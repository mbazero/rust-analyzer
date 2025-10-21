//! Module for handling "move and rename" operations.
//!
//! This module extends the rename functionality to support moving items between modules.
//! When a rename target is a fully-qualified path (e.g., `crate::mod_one::finish::Final`),
//! this module handles:
//! - Parsing the target path to extract module and name components
//! - Creating target module files/directories if they don't exist
//! - Moving the item from source to target location
//! - Updating all references and imports
//!
//! # Current Status
//!
//! This is a work-in-progress implementation. Currently implemented:
//! - Path parsing to detect move operations
//! - Basic integration with the rename flow
//! - Item source extraction
//!
//! Still TODO:
//! - Complete module resolution from ModPath
//! - File/directory creation logic
//! - Module declaration insertion
//! - Reference and import updates
//!
//! See `MOVE_AND_RENAME_FEATURE.md` in the repository root for detailed status and plans.

use hir::{Module, ModuleDef, Semantics};
use hir_expand::{mod_path::{ModPath, PathKind}, name::Name};
use span::Edition;
use syntax::{AstNode, ast};
use crate::{
    RootDatabase,
    defs::Definition,
    source_change::SourceChange,
    rename::{Result, RenameError, bail, format_err},
};

/// Parses a string to determine if it's a fully-qualified path for a move operation.
/// Returns the target module path and the final name if this is a move operation.
pub fn parse_move_target(
    new_name: &str,
    edition: Edition,
) -> Option<(ModPath, Name)> {
    // Check if this looks like a path (contains ::)
    if !new_name.contains("::") {
        return None;
    }

    // Parse the string as a Rust path by wrapping it in a use statement
    let path_str = format!("use {};", new_name);
    let parse = syntax::SourceFile::parse(&path_str, edition);

    // Find the path in the parsed syntax tree
    let use_tree = parse.tree()
        .syntax()
        .descendants()
        .find_map(ast::UseTree::cast)?;

    let path = use_tree.path()?;

    // Extract the final segment as the new name
    let segments: Vec<_> = path.segments().collect();
    if segments.is_empty() {
        return None;
    }

    let last_segment = segments.last()?;
    let target_name = last_segment.name_ref()?.as_name();

    // Build the module path (all segments except the last)
    let mut mod_path_segments = Vec::new();
    let mut kind = PathKind::Plain;

    let mut super_count = 0u8;
    let mut found_non_super = false;

    for (i, segment) in segments.iter().enumerate() {
        if i == segments.len() - 1 {
            // Skip the last segment (that's the item name)
            break;
        }

        match segment.kind()? {
            ast::PathSegmentKind::Name(name_ref) => {
                if i == 0 && segment.coloncolon_token().is_some() {
                    kind = PathKind::Abs;
                }
                found_non_super = true;
                mod_path_segments.push(name_ref.as_name());
            }
            ast::PathSegmentKind::CrateKw => {
                if i == 0 {
                    kind = PathKind::Crate;
                }
            }
            ast::PathSegmentKind::SelfKw => {
                if i == 0 {
                    kind = PathKind::SELF;
                }
            }
            ast::PathSegmentKind::SuperKw => {
                if !found_non_super {
                    super_count += 1;
                } else {
                    // super after a regular segment is not allowed in module paths
                    return None;
                }
            }
            _ => return None,
        }
    }

    if super_count > 0 && !found_non_super {
        kind = PathKind::Super(super_count);
    }

    let mod_path = ModPath::from_segments(kind, mod_path_segments);
    Some((mod_path, target_name))
}

/// Checks if the target path differs from the current item's location, indicating a move.
pub fn is_move_operation(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: &ModPath,
) -> Result<bool> {
    // Get the current module containing the definition
    let current_module = match def {
        Definition::Module(m) => m.parent(sema.db),
        Definition::Function(f) => Some(f.module(sema.db)),
        Definition::Adt(adt) => Some(adt.module(sema.db)),
        Definition::Variant(v) => Some(v.module(sema.db)),
        Definition::Const(c) => Some(c.module(sema.db)),
        Definition::Static(s) => Some(s.module(sema.db)),
        Definition::Trait(t) => Some(t.module(sema.db)),
        Definition::TypeAlias(ta) => Some(ta.module(sema.db)),
        Definition::Macro(m) => Some(m.module(sema.db)),
        _ => None,
    };

    let Some(current_module) = current_module else {
        bail!("Cannot move this type of definition");
    };

    // Get the current module's path
    let current_path = current_module.path_to_root(sema.db);

    // Compare with target path - this is a simplified check
    // A more robust implementation would need to resolve the target path
    // TODO: Implement proper path resolution and comparison

    Ok(true) // Placeholder for now
}

/// Performs the move and rename operation.
pub fn move_and_rename(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: ModPath,
    target_name: Name,
) -> Result<SourceChange> {
    use crate::source_change::FileSystemEdit;
    use crate::text_edit::TextEdit;
    use base_db::AnchoredPathBuf;
    use hir::{HasSource, ModuleSource};
    use syntax::{ast::{self, HasName}, AstNode};
    use span::EditionedFileId;

    // Get the current module containing the definition
    let current_module = match def {
        Definition::Function(f) => f.module(sema.db),
        Definition::Adt(adt) => adt.module(sema.db),
        Definition::Variant(v) => v.module(sema.db),
        Definition::Const(c) => c.module(sema.db),
        Definition::Static(s) => s.module(sema.db),
        Definition::Trait(t) => t.module(sema.db),
        Definition::TypeAlias(ta) => ta.module(sema.db),
        Definition::Macro(m) => m.module(sema.db),
        _ => bail!("Cannot move this type of definition"),
    };

    // Resolve the target module from the path
    let target_module = resolve_target_module(sema, current_module, &target_module_path)?;

    // Get the source range of the item to move
    let item_source = get_item_source(sema, def)?;
    let source_file_id = item_source.file_id;
    let item_text = item_source.value.to_string();

    // Check if we need to rename the item
    let renamed_item_text = if let Some(current_name) = get_item_name(&item_source.value) {
        if current_name.as_str() != target_name.as_str() {
            // Replace the name in the item text
            item_text.replacen(current_name.as_str(), target_name.as_str(), 1)
        } else {
            item_text
        }
    } else {
        item_text
    };

    // Get the target file for the target module
    let (target_file_id, needs_module_declaration) =
        get_or_create_target_file(sema, target_module, &target_module_path)?;

    let mut source_change = SourceChange::default();

    // 1. Remove the item from the source file
    let source_file = sema.parse(source_file_id);
    let item_range = item_source.value.syntax().text_range();
    source_change.insert_source_edit(
        source_file_id.file_id(sema.db),
        TextEdit::delete(item_range),
    );

    // 2. Add the item to the target file
    if let Some(target_file_id) = target_file_id {
        // File exists, append to it
        let target_file = sema.parse(target_file_id);
        let target_text = target_file.syntax().text();
        let insert_position = target_text.len().into();

        source_change.insert_source_edit(
            target_file_id.file_id(sema.db),
            TextEdit::insert(insert_position, format!("\n\n{}", renamed_item_text)),
        );
    } else {
        // Need to create the file
        // This would be handled by get_or_create_target_file returning a FileSystemEdit
        // For now, we'll return an error
        bail!("Target module creation is not yet fully implemented");
    }

    // 3. Update all references to point to the new location
    // This is complex and would require updating imports and qualified paths
    // For now, we'll use the standard rename to update the name, but location updates
    // would need additional logic

    // TODO: Add logic to update imports and module paths

    Ok(source_change)
}

/// Gets the source node for a definition
fn get_item_source(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
) -> Result<hir::InFile<syntax::SyntaxNode>> {
    use hir::HasSource;

    let source = match def {
        Definition::Function(f) => {
            let src = f.source(sema.db).ok_or_else(|| format_err!("No source found"))?;
            src.map(|it| it.syntax().clone())
        }
        Definition::Adt(hir::Adt::Struct(s)) => {
            let src = s.source(sema.db).ok_or_else(|| format_err!("No source found"))?;
            src.map(|it| it.syntax().clone())
        }
        Definition::Adt(hir::Adt::Enum(e)) => {
            let src = e.source(sema.db).ok_or_else(|| format_err!("No source found"))?;
            src.map(|it| it.syntax().clone())
        }
        Definition::Adt(hir::Adt::Union(u)) => {
            let src = u.source(sema.db).ok_or_else(|| format_err!("No source found"))?;
            src.map(|it| it.syntax().clone())
        }
        Definition::Trait(t) => {
            let src = t.source(sema.db).ok_or_else(|| format_err!("No source found"))?;
            src.map(|it| it.syntax().clone())
        }
        Definition::TypeAlias(ta) => {
            let src = ta.source(sema.db).ok_or_else(|| format_err!("No source found"))?;
            src.map(|it| it.syntax().clone())
        }
        Definition::Const(c) => {
            let src = c.source(sema.db).ok_or_else(|| format_err!("No source found"))?;
            src.map(|it| it.syntax().clone())
        }
        Definition::Static(s) => {
            let src = s.source(sema.db).ok_or_else(|| format_err!("No source found"))?;
            src.map(|it| it.syntax().clone())
        }
        _ => bail!("Unsupported definition type for moving"),
    };

    Ok(source)
}

/// Gets the name of an item from its syntax node
fn get_item_name(node: &syntax::SyntaxNode) -> Option<syntax::ast::Name> {
    use syntax::ast;

    if let Some(func) = ast::Fn::cast(node.clone()) {
        func.name()
    } else if let Some(strukt) = ast::Struct::cast(node.clone()) {
        strukt.name()
    } else if let Some(enm) = ast::Enum::cast(node.clone()) {
        enm.name()
    } else if let Some(trt) = ast::Trait::cast(node.clone()) {
        trt.name()
    } else if let Some(type_alias) = ast::TypeAlias::cast(node.clone()) {
        type_alias.name()
    } else if let Some(cnst) = ast::Const::cast(node.clone()) {
        cnst.name()
    } else if let Some(stc) = ast::Static::cast(node.clone()) {
        stc.name()
    } else {
        None
    }
}

/// Resolves the target module from a module path
fn resolve_target_module(
    sema: &Semantics<'_, RootDatabase>,
    current_module: Module,
    target_path: &ModPath,
) -> Result<Module> {
    // For now, we'll just return the current module as a placeholder
    // A full implementation would need to:
    // 1. Start from the crate root or current module depending on the path kind
    // 2. Traverse the path segments to find the target module
    // 3. Create any missing modules along the way

    // TODO: Implement proper module resolution
    bail!("Module resolution is not yet implemented")
}

/// Gets or creates the target file for a module
fn get_or_create_target_file(
    sema: &Semantics<'_, RootDatabase>,
    target_module: Module,
    target_path: &ModPath,
) -> Result<(Option<span::EditionedFileId>, bool)> {
    use hir::ModuleSource;

    // Get the module's file
    let module_file = target_module.definition_source(sema.db);

    match module_file.value {
        ModuleSource::SourceFile(file) => {
            // Module has a dedicated file
            Ok((Some(module_file.file_id), false))
        }
        ModuleSource::Module(_) => {
            // Inline module - would need to create a file
            // TODO: Implement file creation logic
            Ok((None, true))
        }
        ModuleSource::BlockExpr(_) => {
            bail!("Cannot move items into block modules")
        }
    }
}
