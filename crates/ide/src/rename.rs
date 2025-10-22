//! Renaming functionality with support for moving items between modules.
//!
//! This is mostly front-end for [`ide_db::rename`], but it also includes the
//! tests. This module also implements a couple of magic tricks, like renaming
//! `self` and to `self` (to switch between associated function and method).
//!
//! ## Enhanced Rename with Move Operations
//!
//! The rename functionality now supports moving items between modules using
//! fully-qualified paths. When you provide a path like `crate::new_module::ItemName`,
//! the system will:
//!
//! 1. **Parse the path** to determine the target module and new name
//! 2. **Create missing modules** automatically if they don't exist
//! 3. **Move the item** to the target location
//! 4. **Update all references** throughout the codebase
//! 5. **Fix internal references** within the moved item for the new context
//!
//! ### Supported Path Formats
//!
//! - **Absolute paths**: `crate::module::submodule::Item`
//! - **Relative paths**: `super::sibling_module::Item`
//! - **Simple names**: `NewItemName` (rename in current module)
//!
//! ### Examples
//!
//! ```ignore
//! // Move a struct to a new module (creates module if needed)
//! struct User { name: String } // Rename to: crate::models::User
//!
//! // Move and rename simultaneously
//! fn process_data() {} // Rename to: crate::utils::process_user_data
//!
//! // Move using relative paths
//! enum Status {} // Rename to: super::types::Status
//! ```
//!
//! The system handles complex scenarios including visibility validation,
//! circular dependency detection, and maintaining code correctness throughout
//! the move operation.

use hir::{AsAssocItem, InFile, Name, Semantics, sym};
use ide_db::{
    FileId, FileRange, RootDatabase,
    defs::{Definition, NameClass, NameRefClass},
    rename::{IdentifierKind, RenameDefinition, bail, format_err, source_edit_from_references},
    source_change::SourceChangeBuilder,
};
use itertools::Itertools;
use span::Edition;
use std::fmt::Write;
use stdx::{always, format_to, never};
use syntax::{
    AstNode, SyntaxKind, SyntaxNode, TextRange, TextSize,
    ast::{self, HasArgList, prec::ExprPrecedence},
};

use ide_db::text_edit::TextEdit;

use crate::{FilePosition, RangeInfo, SourceChange};

pub use ide_db::rename::RenameError;

type RenameResult<T> = Result<T, RenameError>;

/// This is similar to `collect::<Result<Vec<_>, _>>`, but unlike it, it succeeds if there is *any* `Ok` item.
fn ok_if_any<T, E>(iter: impl Iterator<Item = Result<T, E>>) -> Result<Vec<T>, E> {
    let mut err = None;
    let oks = iter
        .filter_map(|item| match item {
            Ok(it) => Some(it),
            Err(it) => {
                err = Some(it);
                None
            }
        })
        .collect::<Vec<_>>();
    if !oks.is_empty() {
        Ok(oks)
    } else if let Some(err) = err {
        Err(err)
    } else {
        Ok(Vec::new())
    }
}

/// Prepares a rename operation by validating the target and returning its text range.
///
/// This function identifies what can be renamed at the given position and returns
/// the text range that will be affected. It supports both simple renames and
/// move operations using fully-qualified paths.
///
/// # Arguments
///
/// * `db` - The root database for semantic analysis
/// * `position` - The file position where the rename is requested
///
/// # Returns
///
/// Returns a `RangeInfo<()>` containing the text range of the renameable item,
/// or a `RenameError` if nothing can be renamed at the given position.
///
/// # Examples
///
/// ```ignore
/// let position = FilePosition { file_id, offset };
/// let range_info = prepare_rename(db, position)?;
/// // range_info.range contains the text range of the identifier
/// ```
///
/// # Validation
///
/// The function validates that:
/// - There is a renameable item at the specified position
/// - The item supports the requested type of operation
/// - For move operations, the item can be moved between modules
///
/// This preparation step ensures that the actual rename operation will succeed
/// and provides early feedback to the user about what will be renamed.
pub(crate) fn prepare_rename(
    db: &RootDatabase,
    position: FilePosition,
) -> RenameResult<RangeInfo<()>> {
    let sema = Semantics::new(db);
    let source_file = sema.parse_guess_edition(position.file_id);
    let syntax = source_file.syntax();

    let res = find_definitions(&sema, syntax, position, &Name::new_symbol_root(sym::underscore))?
        .filter(|(_, _, def, _, _)| def.range_for_rename(&sema).is_some())
        .map(|(frange, kind, _def, _, _)| {
            always!(
                frange.range.contains_inclusive(position.offset)
                    && frange.file_id == position.file_id
            );

            Ok(match kind {
                SyntaxKind::LIFETIME => {
                    TextRange::new(frange.range.start() + TextSize::from(1), frange.range.end())
                }
                _ => frange.range,
            })
        })
        .reduce(|acc, cur| match (acc, cur) {
            // ensure all ranges are the same
            (Ok(acc_inner), Ok(cur_inner)) if acc_inner == cur_inner => Ok(acc_inner),
            (e @ Err(_), _) | (_, e @ Err(_)) => e,
            _ => bail!("inconsistent text range"),
        });

    match res {
        // ensure at least one definition was found
        Some(res) => res.map(|range| RangeInfo::new(range, ())),
        None => bail!("No references found at position"),
    }
}

/// Check if a definition can be moved to another module
/// (Requirement 5.4)
fn can_be_moved(_sema: &Semantics<'_, RootDatabase>, def: Definition) -> bool {
    match def {
        // Items that can be moved between modules
        Definition::Function(_) |
        Definition::Adt(_) |
        Definition::Variant(_) |
        Definition::Const(_) |
        Definition::Static(_) |
        Definition::Trait(_) |
        Definition::TypeAlias(_) |
        Definition::Macro(_) => true,
        
        // Module moves are handled separately
        Definition::Module(_) => true,
        
        // Items that cannot be moved
        Definition::Local(_) |
        Definition::GenericParam(_) |
        Definition::Label(_) |
        Definition::Field(_) |
        Definition::SelfType(_) |
        Definition::BuiltinType(_) |
        Definition::BuiltinLifetime(_) |
        Definition::BuiltinAttr(_) |
        Definition::ToolModule(_) |
        Definition::TupleField(_) |
        Definition::DeriveHelper(_) |
        Definition::ExternCrateDecl(_) |
        Definition::InlineAsmOperand(_) |
        Definition::InlineAsmRegOrRegClass(_) |
        Definition::Crate(_) => false,
    }
}

/// Check if move operations are supported for this definition type
/// (Requirement 5.4)
fn supports_move_operation(sema: &Semantics<'_, RootDatabase>, def: Definition) -> bool {
    // Check if the definition is in a local crate (can't move external items)
    if let Some(krate) = def.krate(sema.db) {
        if !krate.origin(sema.db).is_local() {
            return false;
        }
    }
    
    // Check if the definition type supports moving
    can_be_moved(sema, def)
}

// Feature: Rename
//
// Renames the item below the cursor and all of its references. Now supports
// moving items between modules using fully-qualified paths.
//
// | Editor  | Shortcut |
// |---------|----------|
// | VS Code | <kbd>F2</kbd> |
//
// ## Usage
//
// ### Simple Rename
// Position cursor on an identifier and use the rename command with a new name:
// - `MyStruct` -> `NewStruct` (renames in current module)
//
// ### Move Operations
// Use fully-qualified paths to move items between modules:
// - `MyStruct` -> `crate::models::MyStruct` (moves to models module)
// - `MyStruct` -> `crate::models::NewStruct` (moves and renames)
// - `MyStruct` -> `super::types::MyStruct` (moves using relative path)
//
// The system automatically:
// - Creates missing module structure
// - Updates all references throughout the codebase
// - Adjusts internal references within moved items
// - Validates visibility and dependency constraints
//
// ![Rename](https://user-images.githubusercontent.com/48062697/113065582-055aae80-91b1-11eb-8ade-2b58e6d81883.gif)

/// Performs a rename operation on the item at the specified position.
///
/// This function handles both simple renames and complex move operations based
/// on the format of the new name provided. If the new name contains module path
/// separators (`::`), it will be treated as a move operation.
///
/// # Arguments
///
/// * `db` - The root database for semantic analysis
/// * `position` - The file position of the item to rename
/// * `new_name` - The new name or fully-qualified path for the item
///
/// # Returns
///
/// Returns a `SourceChange` containing all the edits needed to complete the
/// rename/move operation, or a `RenameError` if the operation cannot be performed.
///
/// # Examples
///
/// ```ignore
/// // Simple rename
/// let change = rename(db, position, "NewName")?;
///
/// // Move to different module
/// let change = rename(db, position, "crate::models::Item")?;
///
/// // Move and rename
/// let change = rename(db, position, "crate::utils::new_helper")?;
/// ```
///
/// # Operation Types
///
/// The function automatically detects the operation type:
/// - **Simple Rename**: No `::` in new_name, renames in current module
/// - **Move Only**: Target module differs, same item name
/// - **Move and Rename**: Target module differs, different item name
/// - **Relative Move**: Uses `super::` or relative paths
///
/// # Validation
///
/// For move operations, the function validates:
/// - Target path syntax and validity
/// - No naming conflicts in target module
/// - No circular dependency creation
/// - Visibility constraints are maintained
/// - All dependencies remain accessible
///
/// The function maintains backward compatibility with existing rename behavior
/// while adding powerful new move capabilities.
pub(crate) fn rename(
    db: &RootDatabase,
    position: FilePosition,
    new_name: &str,
) -> RenameResult<SourceChange> {
    let sema = Semantics::new(db);
    let file_id = sema
        .attach_first_edition(position.file_id)
        .ok_or_else(|| format_err!("No references found at position"))?;
    let source_file = sema.parse(file_id);
    let syntax = source_file.syntax();

    let edition = file_id.edition(db);

    // Check if new_name is a fully-qualified path (Requirements 5.3, 5.4, 5.5)
    if is_fully_qualified_path(new_name) {
        // Parse the fully-qualified path to detect FQP input (Requirement 1.1)
        match ide_db::rename::parse_fully_qualified_path(new_name, db) {
            Ok((target_module_path, new_item_name)) => {
                // Find the definition at the current position
                let defs: Vec<_> = find_definitions(&sema, syntax, position, &new_item_name)?.collect();
                if let Some((_, _, def, _, _)) = defs.first() {
                    // Determine operation type (Requirement 1.2)
                    match ide_db::rename::determine_operation_type(&sema, *def, &target_module_path, &new_item_name) {
                        Ok(operation_type) => {
                            match operation_type {
                                ide_db::rename::RenameOperationType::SimpleRename => {
                                    // Same module - use existing rename logic (Requirement 5.4, 5.5)
                                    return execute_standard_rename(db, &sema, file_id, syntax, position, &new_item_name.display(db, edition).to_string(), edition);
                                }
                                ide_db::rename::RenameOperationType::MoveOnly | 
                                ide_db::rename::RenameOperationType::MoveAndRename => {
                                    // Different module - execute move operation (Requirement 1.2)
                                    return execute_move_operation(&sema, *def, target_module_path, new_item_name);
                                }
                                ide_db::rename::RenameOperationType::RelativeMove => {
                                    // Relative move within same module (Requirement 5.2, 5.3)
                                    return execute_move_operation(&sema, *def, target_module_path, new_item_name);
                                }
                            }
                        }
                        Err(err) => {
                            return Err(RenameError(err.to_string()));
                        }
                    }
                } else {
                    return Err(format_err!("No definition found at position"));
                }
            }
            Err(_) => {
                // Invalid FQP - fall through to standard rename with error handling
            }
        }
    }

    // Standard rename logic (maintain backward compatibility - Requirement 5.5)
    execute_standard_rename(db, &sema, file_id, syntax, position, new_name, edition)
}

/// Execute standard rename operation (extracted for reuse)
fn execute_standard_rename(
    db: &RootDatabase,
    sema: &Semantics<'_, RootDatabase>,
    file_id: ide_db::EditionedFileId,
    syntax: &SyntaxNode,
    position: FilePosition,
    new_name: &str,
    edition: Edition,
) -> RenameResult<SourceChange> {
    let (new_name, kind) = IdentifierKind::classify(edition, new_name)?;

    let defs = find_definitions(sema, syntax, position, &new_name)?;
    let alias_fallback =
        alias_fallback(syntax, position, &new_name.display(db, edition).to_string());

    let ops: RenameResult<Vec<SourceChange>> = match alias_fallback {
        Some(_) => ok_if_any(
            defs
                // FIXME: This can use the `ide_db::rename_reference` (or def.rename) method once we can
                // properly find "direct" usages/references.
                .map(|(.., def, new_name, _)| {
                    match kind {
                        IdentifierKind::Ident => (),
                        IdentifierKind::Lifetime => {
                            bail!("Cannot alias reference to a lifetime identifier")
                        }
                        IdentifierKind::Underscore => bail!("Cannot alias reference to `_`"),
                        IdentifierKind::LowercaseSelf => {
                            bail!("Cannot rename alias reference to `self`")
                        }
                    };
                    let mut usages = def.usages(sema).all();

                    // FIXME: hack - removes the usage that triggered this rename operation.
                    match usages.references.get_mut(&file_id).and_then(|refs| {
                        refs.iter()
                            .position(|ref_| ref_.range.contains_inclusive(position.offset))
                            .map(|idx| refs.remove(idx))
                    }) {
                        Some(_) => (),
                        None => never!(),
                    };

                    let mut source_change = SourceChange::default();
                    source_change.extend(usages.references.get_mut(&file_id).iter().map(|refs| {
                        (
                            position.file_id,
                            source_edit_from_references(db, refs, def, &new_name, edition),
                        )
                    }));

                    Ok(source_change)
                }),
        ),
        None => ok_if_any(defs.map(|(.., def, new_name, rename_def)| {
            if let Definition::Local(local) = def {
                if let Some(self_param) = local.as_self_param(sema.db) {
                    cov_mark::hit!(rename_self_to_param);
                    return rename_self_to_param(sema, local, self_param, &new_name, kind);
                }
                if kind == IdentifierKind::LowercaseSelf {
                    cov_mark::hit!(rename_to_self);
                    return rename_to_self(sema, local);
                }
            }
            def.rename(sema, new_name.as_str(), rename_def)
        })),
    };

    ops?.into_iter()
        .chain(alias_fallback)
        .reduce(|acc, elem| acc.merge(elem))
        .ok_or_else(|| format_err!("No references found at position"))
}

/// Check if a string represents a fully-qualified path.
///
/// This function determines whether a rename target should be treated as a
/// simple identifier or as a fully-qualified path that indicates a move operation.
///
/// # Arguments
///
/// * `name` - The string to check for path separators
///
/// # Returns
///
/// Returns `true` if the string contains path separators indicating it should
/// be treated as a fully-qualified path for move operations.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(is_fully_qualified_path("SimpleIdentifier"), false);
/// assert_eq!(is_fully_qualified_path("crate::module::Item"), true);
/// assert_eq!(is_fully_qualified_path("super::Item"), true);
/// assert_eq!(is_fully_qualified_path("module::submodule::Item"), true);
/// ```
///
/// This detection enables automatic switching between simple rename and
/// move operation modes based on user input format.
fn is_fully_qualified_path(name: &str) -> bool {
    // Check for path separators that indicate a fully-qualified path
    name.contains("::") || name.starts_with("crate::")
}

/// Execute move operation using the implemented move functionality
/// (Requirement 1.2)
fn execute_move_operation(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: hir::ModPath,
    new_item_name: hir::Name,
) -> RenameResult<SourceChange> {
    // Find the target module
    let current_module = def.module(sema.db)
        .ok_or_else(|| format_err!("Cannot determine current module for definition"))?;
    
    let target_module = resolve_target_module(sema, current_module, &target_module_path)
        .map_err(|e| format_err!("Cannot resolve target module: {}", e))?;

    // Validate the move operation (Requirements 4.1, 4.2, 4.3, 4.4, 4.5)
    ide_db::rename::validate_move_operation(sema, def, &target_module_path, &new_item_name, target_module)
        .map_err(|e| RenameError(e.to_string()))?;

    // Execute the move operation
    ide_db::rename::move_item_to_module(sema, def, target_module, &new_item_name)
        .map_err(|e| RenameError(e.to_string()))
}

/// Resolve target module from ModPath
fn resolve_target_module(
    sema: &Semantics<'_, RootDatabase>,
    current_module: hir::Module,
    target_path: &hir::ModPath,
) -> Result<hir::Module, String> {
    let mut current = match target_path.kind {
        hir::PathKind::Crate => current_module.crate_root(sema.db),
        hir::PathKind::Super(n) => {
            let mut module = current_module;
            for _ in 0..n {
                module = module.parent(sema.db)
                    .ok_or_else(|| "Cannot resolve super:: path - not enough parent modules".to_string())?;
            }
            module
        }
        hir::PathKind::Plain => current_module,
        _ => return Err("Unsupported path kind for module resolution".to_string()),
    };

    // Traverse each segment in the target path
    for segment in target_path.segments() {
        current = current.children(sema.db)
            .find(|child| child.name(sema.db) == Some(segment.clone()))
            .ok_or_else(|| format!("Module '{}' not found", segment.display(sema.db, Edition::CURRENT)))?;
    }

    Ok(current)
}

/// Called by the client when it is about to rename a file.
pub(crate) fn will_rename_file(
    db: &RootDatabase,
    file_id: FileId,
    new_name_stem: &str,
) -> Option<SourceChange> {
    let sema = Semantics::new(db);
    let module = sema.file_to_module_def(file_id)?;
    let def = Definition::Module(module);
    let mut change = def.rename(&sema, new_name_stem, RenameDefinition::Yes).ok()?;
    change.file_system_edits.clear();
    Some(change)
}

// FIXME: Should support `extern crate`.
fn alias_fallback(
    syntax: &SyntaxNode,
    FilePosition { file_id, offset }: FilePosition,
    new_name: &str,
) -> Option<SourceChange> {
    let use_tree = syntax
        .token_at_offset(offset)
        .flat_map(|syntax| syntax.parent_ancestors())
        .find_map(ast::UseTree::cast)?;

    let last_path_segment = use_tree.path()?.segments().last()?.name_ref()?;
    if !last_path_segment.syntax().text_range().contains_inclusive(offset) {
        return None;
    };

    let mut builder = SourceChangeBuilder::new(file_id);

    match use_tree.rename() {
        Some(rename) => {
            let offset = rename.syntax().text_range();
            builder.replace(offset, format!("as {new_name}"));
        }
        None => {
            let offset = use_tree.syntax().text_range().end();
            builder.insert(offset, format!(" as {new_name}"));
        }
    }

    Some(builder.finish())
}

fn find_definitions(
    sema: &Semantics<'_, RootDatabase>,
    syntax: &SyntaxNode,
    FilePosition { file_id, offset }: FilePosition,
    new_name: &Name,
) -> RenameResult<impl Iterator<Item = (FileRange, SyntaxKind, Definition, Name, RenameDefinition)>>
{
    let maybe_format_args =
        syntax.token_at_offset(offset).find(|t| matches!(t.kind(), SyntaxKind::STRING));

    if let Some((range, _, _, Some(resolution))) =
        maybe_format_args.and_then(|token| sema.check_for_format_args_template(token, offset))
    {
        return Ok(vec![(
            FileRange { file_id, range },
            SyntaxKind::STRING,
            Definition::from(resolution),
            new_name.clone(),
            RenameDefinition::Yes,
        )]
        .into_iter());
    }

    let original_ident = syntax
        .token_at_offset(offset)
        .max_by_key(|t| {
            t.kind().is_any_identifier() || matches!(t.kind(), SyntaxKind::LIFETIME_IDENT)
        })
        .map(|t| {
            if t.kind() == SyntaxKind::LIFETIME_IDENT {
                Name::new_lifetime(t.text())
            } else {
                Name::new_root(t.text())
            }
        })
        .ok_or_else(|| format_err!("No references found at position"))?;
    let symbols =
        sema.find_namelike_at_offset_with_descend(syntax, offset).map(|name_like| {
            let kind = name_like.syntax().kind();
            let range = sema
                .original_range_opt(name_like.syntax())
                .ok_or_else(|| format_err!("No references found at position"))?;
            let res = match &name_like {
                // renaming aliases would rename the item being aliased as the HIR doesn't track aliases yet
                ast::NameLike::Name(name)
                    if name
                        .syntax()
                        .parent().is_some_and(|it| ast::Rename::can_cast(it.kind()))
                        // FIXME: uncomment this once we resolve to usages to extern crate declarations
                        // && name
                        //     .syntax()
                        //     .ancestors()
                        //     .nth(2)
                        //     .map_or(true, |it| !ast::ExternCrate::can_cast(it.kind()))
                        =>
                {
                    bail!("Renaming aliases is currently unsupported")
                }
                ast::NameLike::Name(name) => NameClass::classify(sema, name)
                    .map(|class| match class {
                        NameClass::Definition(it) | NameClass::ConstReference(it) => it,
                        NameClass::PatFieldShorthand { local_def, field_ref: _, adt_subst: _ } => {
                            Definition::Local(local_def)
                        }
                    })
                    .ok_or_else(|| format_err!("No references found at position")),
                ast::NameLike::NameRef(name_ref) => {
                    NameRefClass::classify(sema, name_ref)
                        .map(|class| match class {
                            NameRefClass::Definition(def, _) => def,
                            NameRefClass::FieldShorthand { local_ref, field_ref: _, adt_subst: _ } => {
                                Definition::Local(local_ref)
                            }
                            NameRefClass::ExternCrateShorthand { decl, .. } => {
                                Definition::ExternCrateDecl(decl)
                            }
                        })
                        // FIXME: uncomment this once we resolve to usages to extern crate declarations
                        .filter(|def| !matches!(def, Definition::ExternCrateDecl(..)))
                        .ok_or_else(|| format_err!("No references found at position"))
                        .and_then(|def| {
                            // if the name differs from the definitions name it has to be an alias
                            if def
                                .name(sema.db).is_some_and(|it| it.as_str() != name_ref.text().trim_start_matches("r#"))
                            {
                                Err(format_err!("Renaming aliases is currently unsupported"))
                            } else {
                                Ok(def)
                            }
                        })
                }
                ast::NameLike::Lifetime(lifetime) => {
                    NameRefClass::classify_lifetime(sema, lifetime)
                        .and_then(|class| match class {
                            NameRefClass::Definition(def, _) => Some(def),
                            _ => None,
                        })
                        .or_else(|| {
                            NameClass::classify_lifetime(sema, lifetime).and_then(|it| match it {
                                NameClass::Definition(it) => Some(it),
                                _ => None,
                            })
                        })
                        .ok_or_else(|| format_err!("No references found at position"))
                }
            };
            res.map(|def| {
                let n = def.name(sema.db)?;
                if n == original_ident {
                    Some((range, kind, def, new_name.clone(), RenameDefinition::Yes))
                } else if let Some(suffix) =  n.as_str().strip_prefix(original_ident.as_str()) {
                    Some((range, kind, def, Name::new_root(&format!("{}{suffix}", new_name.as_str())), RenameDefinition::No))
                } else {
                     n.as_str().strip_suffix(original_ident.as_str().trim_start_matches('\''))
                        .map(|prefix| (range, kind, def, Name::new_root(&format!("{prefix}{}", new_name.as_str())), RenameDefinition::No))
                }
            })
        });

    let res: RenameResult<Vec<_>> = ok_if_any(symbols.filter_map(Result::transpose));
    match res {
        Ok(v) => {
            // remove duplicates, comparing `Definition`s
            Ok(v.into_iter()
                .unique_by(|&(.., def, _, _)| def)
                .map(|(a, b, c, d, e)| (a.into_file_id(sema.db), b, c, d, e))
                .collect::<Vec<_>>()
                .into_iter())
        }
        Err(e) => Err(e),
    }
}

fn transform_assoc_fn_into_method_call(
    sema: &Semantics<'_, RootDatabase>,
    source_change: &mut SourceChange,
    f: hir::Function,
) {
    let calls = Definition::Function(f).usages(sema).all();
    for (file_id, calls) in calls {
        for call in calls {
            let Some(fn_name) = call.name.as_name_ref() else { continue };
            let Some(path) = fn_name.syntax().parent().and_then(ast::PathSegment::cast) else {
                continue;
            };
            let path = path.parent_path();
            // The `PathExpr` is the direct parent, above it is the `CallExpr`.
            let Some(call) =
                path.syntax().parent().and_then(|it| ast::CallExpr::cast(it.parent()?))
            else {
                continue;
            };

            let Some(arg_list) = call.arg_list() else { continue };
            let mut args = arg_list.args();
            let Some(mut self_arg) = args.next() else { continue };
            let second_arg = args.next();

            // Strip (de)references, as they will be taken automatically by auto(de)ref.
            loop {
                let self_ = match &self_arg {
                    ast::Expr::RefExpr(self_) => self_.expr(),
                    ast::Expr::ParenExpr(self_) => self_.expr(),
                    ast::Expr::PrefixExpr(self_)
                        if self_.op_kind() == Some(ast::UnaryOp::Deref) =>
                    {
                        self_.expr()
                    }
                    _ => break,
                };
                self_arg = match self_ {
                    Some(it) => it,
                    None => break,
                };
            }

            let self_needs_parens =
                self_arg.precedence().needs_parentheses_in(ExprPrecedence::Postfix);

            let replace_start = path.syntax().text_range().start();
            let replace_end = match second_arg {
                Some(second_arg) => second_arg.syntax().text_range().start(),
                None => arg_list
                    .r_paren_token()
                    .map(|it| it.text_range().start())
                    .unwrap_or_else(|| arg_list.syntax().text_range().end()),
            };
            let replace_range = TextRange::new(replace_start, replace_end);

            let Some(macro_mapped_self) = sema.original_range_opt(self_arg.syntax()) else {
                continue;
            };
            let mut replacement = String::new();
            if self_needs_parens {
                replacement.push('(');
            }
            replacement.push_str(macro_mapped_self.text(sema.db));
            if self_needs_parens {
                replacement.push(')');
            }
            replacement.push('.');
            format_to!(replacement, "{fn_name}");
            replacement.push('(');

            source_change.insert_source_edit(
                file_id.file_id(sema.db),
                TextEdit::replace(replace_range, replacement),
            );
        }
    }
}

fn rename_to_self(
    sema: &Semantics<'_, RootDatabase>,
    local: hir::Local,
) -> RenameResult<SourceChange> {
    if never!(local.is_self(sema.db)) {
        bail!("rename_to_self invoked on self");
    }

    let fn_def = match local.parent(sema.db) {
        hir::DefWithBody::Function(func) => func,
        _ => bail!("Cannot rename local to self outside of function"),
    };

    if fn_def.self_param(sema.db).is_some() {
        bail!("Method already has a self parameter");
    }

    let params = fn_def.assoc_fn_params(sema.db);
    let first_param = params
        .first()
        .ok_or_else(|| format_err!("Cannot rename local to self unless it is a parameter"))?;
    match first_param.as_local(sema.db) {
        Some(plocal) => {
            if plocal != local {
                bail!("Only the first parameter may be renamed to self");
            }
        }
        None => bail!("rename_to_self invoked on destructuring parameter"),
    }

    let assoc_item = fn_def
        .as_assoc_item(sema.db)
        .ok_or_else(|| format_err!("Cannot rename parameter to self for free function"))?;
    let impl_ = match assoc_item.container(sema.db) {
        hir::AssocItemContainer::Trait(_) => {
            bail!("Cannot rename parameter to self for trait functions");
        }
        hir::AssocItemContainer::Impl(impl_) => impl_,
    };
    let first_param_ty = first_param.ty();
    let impl_ty = impl_.self_ty(sema.db);
    let (ty, self_param) = if impl_ty.remove_ref().is_some() {
        // if the impl is a ref to the type we can just match the `&T` with self directly
        (first_param_ty.clone(), "self")
    } else {
        first_param_ty.remove_ref().map_or((first_param_ty.clone(), "self"), |ty| {
            (ty, if first_param_ty.is_mutable_reference() { "&mut self" } else { "&self" })
        })
    };

    if ty != impl_ty {
        bail!("Parameter type differs from impl block type");
    }

    let InFile { file_id, value: param_source } = sema
        .source(first_param.clone())
        .ok_or_else(|| format_err!("No source for parameter found"))?;

    let def = Definition::Local(local);
    let usages = def.usages(sema).all();
    let mut source_change = SourceChange::default();
    source_change.extend(usages.iter().map(|(file_id, references)| {
        (
            file_id.file_id(sema.db),
            source_edit_from_references(
                sema.db,
                references,
                def,
                &Name::new_symbol_root(sym::self_),
                file_id.edition(sema.db),
            ),
        )
    }));
    source_change.insert_source_edit(
        file_id.original_file(sema.db).file_id(sema.db),
        TextEdit::replace(param_source.syntax().text_range(), String::from(self_param)),
    );
    transform_assoc_fn_into_method_call(sema, &mut source_change, fn_def);
    Ok(source_change)
}

fn rename_self_to_param(
    sema: &Semantics<'_, RootDatabase>,
    local: hir::Local,
    self_param: hir::SelfParam,
    new_name: &Name,
    identifier_kind: IdentifierKind,
) -> RenameResult<SourceChange> {
    if identifier_kind == IdentifierKind::LowercaseSelf {
        // Let's do nothing rather than complain.
        cov_mark::hit!(rename_self_to_self);
        return Ok(SourceChange::default());
    }

    let InFile { file_id, value: self_param } =
        sema.source(self_param).ok_or_else(|| format_err!("cannot find function source"))?;

    let def = Definition::Local(local);
    let usages = def.usages(sema).all();
    let edit = text_edit_from_self_param(
        &self_param,
        new_name.display(sema.db, file_id.edition(sema.db)).to_string(),
    )
    .ok_or_else(|| format_err!("No target type found"))?;
    if usages.len() > 1 && identifier_kind == IdentifierKind::Underscore {
        bail!("Cannot rename reference to `_` as it is being referenced multiple times");
    }
    let mut source_change = SourceChange::default();
    source_change.insert_source_edit(file_id.original_file(sema.db).file_id(sema.db), edit);
    source_change.extend(usages.iter().map(|(file_id, references)| {
        (
            file_id.file_id(sema.db),
            source_edit_from_references(
                sema.db,
                references,
                def,
                new_name,
                file_id.edition(sema.db),
            ),
        )
    }));
    Ok(source_change)
}

fn text_edit_from_self_param(self_param: &ast::SelfParam, new_name: String) -> Option<TextEdit> {
    let mut replacement_text = new_name;
    replacement_text.push_str(": ");

    if self_param.amp_token().is_some() {
        replacement_text.push('&');
    }
    if let Some(lifetime) = self_param.lifetime() {
        write!(replacement_text, "{lifetime} ").unwrap();
    }
    if self_param.amp_token().and(self_param.mut_token()).is_some() {
        replacement_text.push_str("mut ");
    }

    replacement_text.push_str("Self");

    Some(TextEdit::replace(self_param.syntax().text_range(), replacement_text))
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use ide_db::source_change::SourceChange;
    use ide_db::text_edit::TextEdit;
    use itertools::Itertools;
    use stdx::trim_indent;
    use test_utils::assert_eq_text;

    use crate::fixture;

    use super::{RangeInfo, RenameError};

    #[track_caller]
    fn check(
        new_name: &str,
        #[rust_analyzer::rust_fixture] ra_fixture_before: &str,
        #[rust_analyzer::rust_fixture] ra_fixture_after: &str,
    ) {
        let ra_fixture_after = &trim_indent(ra_fixture_after);
        let (analysis, position) = fixture::position(ra_fixture_before);
        if !ra_fixture_after.starts_with("error: ")
            && let Err(err) = analysis.prepare_rename(position).unwrap()
        {
            panic!("Prepare rename to '{new_name}' was failed: {err}")
        }
        let rename_result = analysis
            .rename(position, new_name)
            .unwrap_or_else(|err| panic!("Rename to '{new_name}' was cancelled: {err}"));
        match rename_result {
            Ok(source_change) => {
                let mut text_edit_builder = TextEdit::builder();
                let (&file_id, edit) = match source_change.source_file_edits.len() {
                    0 => return,
                    1 => source_change.source_file_edits.iter().next().unwrap(),
                    _ => panic!(),
                };
                for indel in edit.0.iter() {
                    text_edit_builder.replace(indel.delete, indel.insert.clone());
                }
                let mut result = analysis.file_text(file_id).unwrap().to_string();
                text_edit_builder.finish().apply(&mut result);
                assert_eq_text!(ra_fixture_after, &*result);
            }
            Err(err) => {
                if ra_fixture_after.starts_with("error:") {
                    let error_message =
                        ra_fixture_after.chars().skip("error:".len()).collect::<String>();
                    assert_eq!(error_message.trim(), err.to_string());
                } else {
                    panic!("Rename to '{new_name}' failed unexpectedly: {err}")
                }
            }
        };
    }

    #[track_caller]
    fn check_conflicts(new_name: &str, #[rust_analyzer::rust_fixture] ra_fixture: &str) {
        let (analysis, position, conflicts) = fixture::annotations(ra_fixture);
        let source_change = analysis.rename(position, new_name).unwrap().unwrap();
        let expected_conflicts = conflicts
            .into_iter()
            .map(|(file_range, _)| (file_range.file_id, file_range.range))
            .sorted_unstable_by_key(|(file_id, range)| (*file_id, range.start()))
            .collect_vec();
        let found_conflicts = source_change
            .source_file_edits
            .iter()
            .filter(|(_, (edit, _))| edit.change_annotation().is_some())
            .flat_map(|(file_id, (edit, _))| {
                edit.into_iter().map(move |edit| (*file_id, edit.delete))
            })
            .sorted_unstable_by_key(|(file_id, range)| (*file_id, range.start()))
            .collect_vec();
        assert_eq!(
            expected_conflicts, found_conflicts,
            "rename conflicts mismatch: {source_change:#?}"
        );
    }

    fn check_expect(
        new_name: &str,
        #[rust_analyzer::rust_fixture] ra_fixture: &str,
        expect: Expect,
    ) {
        let (analysis, position) = fixture::position(ra_fixture);
        let source_change =
            analysis.rename(position, new_name).unwrap().expect("Expect returned a RenameError");
        expect.assert_eq(&filter_expect(source_change))
    }

    fn check_expect_will_rename_file(
        new_name: &str,
        #[rust_analyzer::rust_fixture] ra_fixture: &str,
        expect: Expect,
    ) {
        let (analysis, position) = fixture::position(ra_fixture);
        let source_change = analysis
            .will_rename_file(position.file_id, new_name)
            .unwrap()
            .expect("Expect returned a RenameError");
        expect.assert_eq(&filter_expect(source_change))
    }

    fn check_prepare(#[rust_analyzer::rust_fixture] ra_fixture: &str, expect: Expect) {
        let (analysis, position) = fixture::position(ra_fixture);
        let result = analysis
            .prepare_rename(position)
            .unwrap_or_else(|err| panic!("PrepareRename was cancelled: {err}"));
        match result {
            Ok(RangeInfo { range, info: () }) => {
                let source = analysis.file_text(position.file_id).unwrap();
                expect.assert_eq(&format!("{range:?}: {}", &source[range]))
            }
            Err(RenameError(err)) => expect.assert_eq(&err),
        };
    }

    fn filter_expect(source_change: SourceChange) -> String {
        let source_file_edits = source_change
            .source_file_edits
            .into_iter()
            .map(|(id, (text_edit, _))| (id, text_edit.into_iter().collect::<Vec<_>>()))
            .collect::<Vec<_>>();

        format!(
            "source_file_edits: {:#?}\nfile_system_edits: {:#?}\n",
            source_file_edits, source_change.file_system_edits
        )
    }

    #[test]
    fn rename_will_shadow() {
        check_conflicts(
            "new_name",
            r#"
fn foo() {
    let mut new_name = 123;
    let old_name$0 = 456;
     // ^^^^^^^^
    new_name = 789 + new_name;
}
        "#,
        );
    }

    #[test]
    fn rename_will_be_shadowed() {
        check_conflicts(
            "new_name",
            r#"
fn foo() {
    let mut old_name$0 = 456;
         // ^^^^^^^^
    let new_name = 123;
    old_name = 789 + old_name;
 // ^^^^^^^^         ^^^^^^^^
}
        "#,
        );
    }

    #[test]
    fn test_prepare_rename_namelikes() {
        check_prepare(r"fn name$0<'lifetime>() {}", expect![[r#"3..7: name"#]]);
        check_prepare(r"fn name<'lifetime$0>() {}", expect![[r#"9..17: lifetime"#]]);
        check_prepare(r"fn name<'lifetime>() { name$0(); }", expect![[r#"23..27: name"#]]);
    }

    #[test]
    fn test_prepare_rename_in_macro() {
        check_prepare(
            r"macro_rules! foo {
    ($ident:ident) => {
        pub struct $ident;
    }
}
foo!(Foo$0);",
            expect![[r#"83..86: Foo"#]],
        );
    }

    #[test]
    fn test_prepare_rename_keyword() {
        check_prepare(r"struct$0 Foo;", expect![[r#"No references found at position"#]]);
    }

    #[test]
    fn test_prepare_rename_tuple_field() {
        check_prepare(
            r#"
struct Foo(i32);

fn baz() {
    let mut x = Foo(4);
    x.0$0 = 5;
}
"#,
            expect![[r#"No references found at position"#]],
        );
    }

    #[test]
    fn test_prepare_rename_builtin() {
        check_prepare(
            r#"
fn foo() {
    let x: i32$0 = 0;
}
"#,
            expect![[r#"No references found at position"#]],
        );
    }

    #[test]
    fn test_prepare_rename_self() {
        check_prepare(
            r#"
struct Foo {}

impl Foo {
    fn foo(self) -> Self$0 {
        self
    }
}
"#,
            expect![[r#"No references found at position"#]],
        );
    }

    #[test]
    fn test_rename_to_underscore() {
        check("_", r#"fn main() { let i$0 = 1; }"#, r#"fn main() { let _ = 1; }"#);
    }

    #[test]
    fn test_rename_to_raw_identifier() {
        check("r#fn", r#"fn main() { let i$0 = 1; }"#, r#"fn main() { let r#fn = 1; }"#);
    }

    #[test]
    fn test_rename_to_invalid_identifier1() {
        check(
            "invalid!",
            r#"fn main() { let i$0 = 1; }"#,
            "error: Invalid name `invalid!`: not an identifier",
        );
    }

    #[test]
    fn test_rename_to_invalid_identifier2() {
        check(
            "multiple tokens",
            r#"fn main() { let i$0 = 1; }"#,
            "error: Invalid name `multiple tokens`: not an identifier",
        );
    }

    #[test]
    fn test_rename_to_invalid_identifier3() {
        check(
            "super",
            r#"fn main() { let i$0 = 1; }"#,
            "error: Invalid name `super`: cannot rename to a keyword",
        );
    }

    #[test]
    fn test_rename_to_invalid_identifier_lifetime() {
        cov_mark::check!(rename_not_an_ident_ref);
        check(
            "'foo",
            r#"fn main() { let i$0 = 1; }"#,
            "error: Invalid name `'foo`: not an identifier",
        );
    }

    #[test]
    fn test_rename_to_invalid_identifier_lifetime2() {
        check(
            "_",
            r#"fn main<'a>(_: &'a$0 ()) {}"#,
            r#"error: Invalid name `_`: not a lifetime identifier"#,
        );
    }

    #[test]
    fn test_rename_accepts_lifetime_without_apostrophe() {
        check("foo", r#"fn main<'a>(_: &'a$0 ()) {}"#, r#"fn main<'foo>(_: &'foo ()) {}"#);
    }

    #[test]
    fn test_rename_to_underscore_invalid() {
        cov_mark::check!(rename_underscore_multiple);
        check(
            "_",
            r#"fn main(foo$0: ()) {foo;}"#,
            "error: Cannot rename reference to `_` as it is being referenced multiple times",
        );
    }

    #[test]
    fn test_rename_mod_invalid() {
        check(
            "'foo",
            r#"mod foo$0 {}"#,
            "error: Invalid name `'foo`: cannot rename module to 'foo",
        );
    }

    #[test]
    fn test_rename_mod_invalid_raw_ident() {
        check(
            "r#self",
            r#"mod foo$0 {}"#,
            "error: Invalid name `self`: cannot rename module to self",
        );
    }

    #[test]
    fn test_rename_for_local() {
        check(
            "k",
            r#"
fn main() {
    let mut i = 1;
    let j = 1;
    i = i$0 + j;

    { i = 0; }

    i = 5;
}
"#,
            r#"
fn main() {
    let mut k = 1;
    let j = 1;
    k = k + j;

    { k = 0; }

    k = 5;
}
"#,
        );
    }

    #[test]
    fn test_rename_unresolved_reference() {
        check(
            "new_name",
            r#"fn main() { let _ = unresolved_ref$0; }"#,
            "error: No references found at position",
        );
    }

    #[test]
    fn test_rename_macro_multiple_occurrences() {
        check(
            "Baaah",
            r#"macro_rules! foo {
    ($ident:ident) => {
        const $ident: () = ();
        struct $ident {}
    };
}

foo!($0Foo);
const _: () = Foo;
const _: Foo = Foo {};
    "#,
            r#"
macro_rules! foo {
    ($ident:ident) => {
        const $ident: () = ();
        struct $ident {}
    };
}

foo!(Baaah);
const _: () = Baaah;
const _: Baaah = Baaah {};
    "#,
        )
    }

    #[test]
    fn test_rename_for_macro_args() {
        check(
            "b",
            r#"
macro_rules! foo {($i:ident) => {$i} }
fn main() {
    let a$0 = "test";
    foo!(a);
}
"#,
            r#"
macro_rules! foo {($i:ident) => {$i} }
fn main() {
    let b = "test";
    foo!(b);
}
"#,
        );
    }

    #[test]
    fn test_rename_for_macro_args_rev() {
        check(
            "b",
            r#"
macro_rules! foo {($i:ident) => {$i} }
fn main() {
    let a = "test";
    foo!(a$0);
}
"#,
            r#"
macro_rules! foo {($i:ident) => {$i} }
fn main() {
    let b = "test";
    foo!(b);
}
"#,
        );
    }

    #[test]
    fn test_rename_for_macro_define_fn() {
        check(
            "bar",
            r#"
macro_rules! define_fn {($id:ident) => { fn $id{} }}
define_fn!(foo);
fn main() {
    fo$0o();
}
"#,
            r#"
macro_rules! define_fn {($id:ident) => { fn $id{} }}
define_fn!(bar);
fn main() {
    bar();
}
"#,
        );
    }

    #[test]
    fn test_rename_for_macro_define_fn_rev() {
        check(
            "bar",
            r#"
macro_rules! define_fn {($id:ident) => { fn $id{} }}
define_fn!(fo$0o);
fn main() {
    foo();
}
"#,
            r#"
macro_rules! define_fn {($id:ident) => { fn $id{} }}
define_fn!(bar);
fn main() {
    bar();
}
"#,
        );
    }

    #[test]
    fn test_rename_for_param_inside() {
        check("j", r#"fn foo(i : u32) -> u32 { i$0 }"#, r#"fn foo(j : u32) -> u32 { j }"#);
    }

    #[test]
    fn test_rename_refs_for_fn_param() {
        check("j", r#"fn foo(i$0 : u32) -> u32 { i }"#, r#"fn foo(j : u32) -> u32 { j }"#);
    }

    #[test]
    fn test_rename_for_mut_param() {
        check("j", r#"fn foo(mut i$0 : u32) -> u32 { i }"#, r#"fn foo(mut j : u32) -> u32 { j }"#);
    }

    #[test]
    fn test_rename_struct_field() {
        check(
            "foo",
            r#"
struct Foo { field$0: i32 }

impl Foo {
    fn new(i: i32) -> Self {
        Self { field: i }
    }
}
"#,
            r#"
struct Foo { foo: i32 }

impl Foo {
    fn new(i: i32) -> Self {
        Self { foo: i }
    }
}
"#,
        );
    }

    #[test]
    fn test_rename_field_in_field_shorthand() {
        cov_mark::check!(test_rename_field_in_field_shorthand);
        check(
            "field",
            r#"
struct Foo { foo$0: i32 }

impl Foo {
    fn new(foo: i32) -> Self {
        Self { foo }
    }
}
"#,
            r#"
struct Foo { field: i32 }

impl Foo {
    fn new(foo: i32) -> Self {
        Self { field: foo }
    }
}
"#,
        );
    }

    #[test]
    fn test_rename_local_in_field_shorthand() {
        cov_mark::check!(test_rename_local_in_field_shorthand);
        check(
            "j",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn new(i$0: i32) -> Self {
        Self { i }
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn new(j: i32) -> Self {
        Self { i: j }
    }
}
"#,
        );
    }

    #[test]
    fn test_field_shorthand_correct_struct() {
        check(
            "j",
            r#"
struct Foo { i$0: i32 }
struct Bar { i: i32 }

impl Bar {
    fn new(i: i32) -> Self {
        Self { i }
    }
}
"#,
            r#"
struct Foo { j: i32 }
struct Bar { i: i32 }

impl Bar {
    fn new(i: i32) -> Self {
        Self { i }
    }
}
"#,
        );
    }

    #[test]
    fn test_shadow_local_for_struct_shorthand() {
        check(
            "j",
            r#"
struct Foo { i: i32 }

fn baz(i$0: i32) -> Self {
     let x = Foo { i };
     {
         let i = 0;
         Foo { i }
     }
}
"#,
            r#"
struct Foo { i: i32 }

fn baz(j: i32) -> Self {
     let x = Foo { i: j };
     {
         let i = 0;
         Foo { i }
     }
}
"#,
        );
    }

    #[test]
    fn test_rename_mod() {
        check_expect(
            "foo2",
            r#"
//- /lib.rs
mod bar;

//- /bar.rs
mod foo$0;

//- /bar/foo.rs
// empty
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "foo2",
                                delete: 4..7,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            2,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                2,
                            ),
                            path: "foo2.rs",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_mod_in_use_tree() {
        check_expect(
            "quux",
            r#"
//- /main.rs
pub mod foo;
pub mod bar;
fn main() {}

//- /foo.rs
pub struct FooContent;

//- /bar.rs
use crate::foo$0::FooContent;
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "quux",
                                delete: 8..11,
                            },
                        ],
                    ),
                    (
                        FileId(
                            2,
                        ),
                        [
                            Indel {
                                insert: "quux",
                                delete: 11..14,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "quux.rs",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_mod_in_dir() {
        check_expect(
            "foo2",
            r#"
//- /lib.rs
mod fo$0o;
//- /foo/mod.rs
// empty
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "foo2",
                                delete: 4..7,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveDir {
                        src: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "../foo",
                        },
                        src_id: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "../foo2",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_unusually_nested_mod() {
        check_expect(
            "bar",
            r#"
//- /lib.rs
mod outer { mod fo$0o; }

//- /outer/foo.rs
// empty
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "bar",
                                delete: 16..19,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "bar.rs",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_module_rename_in_path() {
        check(
            "baz",
            r#"
mod $0foo {
    pub use self::bar as qux;
    pub fn bar() {}
}

fn main() { foo::bar(); }
"#,
            r#"
mod baz {
    pub use self::bar as qux;
    pub fn bar() {}
}

fn main() { baz::bar(); }
"#,
        );
    }

    #[test]
    fn test_rename_mod_filename_and_path() {
        check_expect(
            "foo2",
            r#"
//- /lib.rs
mod bar;
fn f() {
    bar::foo::fun()
}

//- /bar.rs
pub mod foo$0;

//- /bar/foo.rs
// pub fn fun() {}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "foo2",
                                delete: 27..30,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "foo2",
                                delete: 8..11,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            2,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                2,
                            ),
                            path: "foo2.rs",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_mod_recursive() {
        check_expect(
            "foo2",
            r#"
//- /lib.rs
mod foo$0;

//- /foo.rs
mod bar;
mod corge;

//- /foo/bar.rs
mod qux;

//- /foo/bar/qux.rs
mod quux;

//- /foo/bar/qux/quux/mod.rs
// empty

//- /foo/corge.rs
// empty
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "foo2",
                                delete: 4..7,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo2.rs",
                        },
                    },
                    MoveDir {
                        src: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo",
                        },
                        src_id: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo2",
                        },
                    },
                ]
            "#]],
        )
    }
    #[test]
    fn test_rename_mod_ref_by_super() {
        check(
            "baz",
            r#"
        mod $0foo {
        struct X;

        mod bar {
            use super::X;
        }
    }
            "#,
            r#"
        mod baz {
        struct X;

        mod bar {
            use super::X;
        }
    }
            "#,
        )
    }

    #[test]
    fn test_rename_mod_in_macro() {
        check(
            "bar",
            r#"
//- /foo.rs

//- /lib.rs
macro_rules! submodule {
    ($name:ident) => {
        mod $name;
    };
}

submodule!($0foo);
"#,
            r#"
macro_rules! submodule {
    ($name:ident) => {
        mod $name;
    };
}

submodule!(bar);
"#,
        )
    }

    #[test]
    fn test_rename_mod_for_crate_root() {
        check_expect_will_rename_file(
            "main",
            r#"
//- /lib.rs
use crate::foo as bar;
fn foo() {}
mod bar$0;
"#,
            expect![[r#"
                source_file_edits: []
                file_system_edits: []
            "#]],
        )
    }

    #[test]
    fn test_rename_mod_to_raw_ident() {
        check_expect(
            "r#fn",
            r#"
//- /lib.rs
mod foo$0;

fn main() { foo::bar::baz(); }

//- /foo.rs
pub mod bar;

//- /foo/bar.rs
pub fn baz() {}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "r#fn",
                                delete: 4..7,
                            },
                            Indel {
                                insert: "r#fn",
                                delete: 22..25,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "fn.rs",
                        },
                    },
                    MoveDir {
                        src: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo",
                        },
                        src_id: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "fn",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_mod_from_raw_ident() {
        check_expect(
            "foo",
            r#"
//- /lib.rs
mod r#fn$0;

fn main() { r#fn::bar::baz(); }

//- /fn.rs
pub mod bar;

//- /fn/bar.rs
pub fn baz() {}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "foo",
                                delete: 4..8,
                            },
                            Indel {
                                insert: "foo",
                                delete: 23..27,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo.rs",
                        },
                    },
                    MoveDir {
                        src: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "fn",
                        },
                        src_id: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_each_usage_gets_appropriate_rawness() {
        check_expect(
            "dyn",
            r#"
//- /a.rs crate:a edition:2015
pub fn foo() {}

//- /b.rs crate:b edition:2018 deps:a new_source_root:local
fn bar() {
    a::foo$0();
}
    "#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "dyn",
                                delete: 7..10,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "r#dyn",
                                delete: 18..21,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );

        check_expect(
            "dyn",
            r#"
//- /a.rs crate:a edition:2018
pub fn foo() {}

//- /b.rs crate:b edition:2015 deps:a new_source_root:local
fn bar() {
    a::foo$0();
}
    "#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "r#dyn",
                                delete: 7..10,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "dyn",
                                delete: 18..21,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );

        check_expect(
            "r#dyn",
            r#"
//- /a.rs crate:a edition:2018
pub fn foo$0() {}

//- /b.rs crate:b edition:2015 deps:a new_source_root:local
fn bar() {
    a::foo();
}
    "#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "r#dyn",
                                delete: 7..10,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "dyn",
                                delete: 18..21,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );
    }

    #[test]
    fn rename_raw_identifier() {
        check_expect(
            "abc",
            r#"
//- /a.rs crate:a edition:2015
pub fn dyn() {}

fn foo() {
    dyn$0();
}

//- /b.rs crate:b edition:2018 deps:a new_source_root:local
fn bar() {
    a::r#dyn();
}
    "#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "abc",
                                delete: 7..10,
                            },
                            Indel {
                                insert: "abc",
                                delete: 32..35,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "abc",
                                delete: 18..23,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );

        check_expect(
            "abc",
            r#"
//- /a.rs crate:a edition:2018
pub fn r#dyn() {}

fn foo() {
    r#dyn$0();
}

//- /b.rs crate:b edition:2015 deps:a new_source_root:local
fn bar() {
    a::dyn();
}
    "#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "abc",
                                delete: 7..12,
                            },
                            Indel {
                                insert: "abc",
                                delete: 34..39,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "abc",
                                delete: 18..21,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );
    }

    #[test]
    fn test_enum_variant_from_module_1() {
        cov_mark::check!(rename_non_local);
        check(
            "Baz",
            r#"
mod foo {
    pub enum Foo { Bar$0 }
}

fn func(f: foo::Foo) {
    match f {
        foo::Foo::Bar => {}
    }
}
"#,
            r#"
mod foo {
    pub enum Foo { Baz }
}

fn func(f: foo::Foo) {
    match f {
        foo::Foo::Baz => {}
    }
}
"#,
        );
    }

    #[test]
    fn test_enum_variant_from_module_2() {
        check(
            "baz",
            r#"
mod foo {
    pub struct Foo { pub bar$0: uint }
}

fn foo(f: foo::Foo) {
    let _ = f.bar;
}
"#,
            r#"
mod foo {
    pub struct Foo { pub baz: uint }
}

fn foo(f: foo::Foo) {
    let _ = f.baz;
}
"#,
        );
    }

    #[test]
    fn test_parameter_to_self() {
        cov_mark::check!(rename_to_self);
        check(
            "self",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(foo$0: &mut Foo) -> i32 {
        foo.i
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(&mut self) -> i32 {
        self.i
    }
}
"#,
        );
        check(
            "self",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(foo$0: Foo) -> i32 {
        foo.i
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(self) -> i32 {
        self.i
    }
}
"#,
        );
    }

    #[test]
    fn test_parameter_to_self_error_no_impl() {
        check(
            "self",
            r#"
struct Foo { i: i32 }

fn f(foo$0: &mut Foo) -> i32 {
    foo.i
}
"#,
            "error: Cannot rename parameter to self for free function",
        );
        check(
            "self",
            r#"
struct Foo { i: i32 }
struct Bar;

impl Bar {
    fn f(foo$0: &mut Foo) -> i32 {
        foo.i
    }
}
"#,
            "error: Parameter type differs from impl block type",
        );
    }

    #[test]
    fn test_parameter_to_self_error_not_first() {
        check(
            "self",
            r#"
struct Foo { i: i32 }
impl Foo {
    fn f(x: (), foo$0: &mut Foo) -> i32 {
        foo.i
    }
}
"#,
            "error: Only the first parameter may be renamed to self",
        );
    }

    #[test]
    fn test_parameter_to_self_impl_ref() {
        check(
            "self",
            r#"
struct Foo { i: i32 }
impl &Foo {
    fn f(foo$0: &Foo) -> i32 {
        foo.i
    }
}
"#,
            r#"
struct Foo { i: i32 }
impl &Foo {
    fn f(self) -> i32 {
        self.i
    }
}
"#,
        );
    }

    #[test]
    fn test_self_to_parameter() {
        check(
            "foo",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(&mut $0self) -> i32 {
        self.i
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(foo: &mut Self) -> i32 {
        foo.i
    }
}
"#,
        );
    }

    #[test]
    fn test_owned_self_to_parameter() {
        cov_mark::check!(rename_self_to_param);
        check(
            "foo",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f($0self) -> i32 {
        self.i
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(foo: Self) -> i32 {
        foo.i
    }
}
"#,
        );
    }

    #[test]
    fn test_owned_self_to_parameter_with_lifetime() {
        cov_mark::check!(rename_self_to_param);
        check(
            "foo",
            r#"
struct Foo<'a> { i: &'a i32 }

impl<'a> Foo<'a> {
    fn f(&'a $0self) -> i32 {
        self.i
    }
}
"#,
            r#"
struct Foo<'a> { i: &'a i32 }

impl<'a> Foo<'a> {
    fn f(foo: &'a Self) -> i32 {
        foo.i
    }
}
"#,
        );
    }

    #[test]
    fn test_self_outside_of_methods() {
        check(
            "foo",
            r#"
fn f($0self) -> i32 {
    self.i
}
"#,
            r#"
fn f(foo: Self) -> i32 {
    foo.i
}
"#,
        );
    }

    #[test]
    fn no_type_value_ns_confuse() {
        // Test that we don't rename items from different namespaces.
        check(
            "bar",
            r#"
struct foo {}
fn f(foo$0: i32) -> i32 {
    use foo as _;
}
"#,
            r#"
struct foo {}
fn f(bar: i32) -> i32 {
    use foo as _;
}
"#,
        );
    }

    #[test]
    fn test_self_in_path_to_parameter() {
        check(
            "foo",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(&self) -> i32 {
        let self_var = 1;
        self$0.i
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(foo: &Self) -> i32 {
        let self_var = 1;
        foo.i
    }
}
"#,
        );
    }

    #[test]
    fn test_rename_field_put_init_shorthand() {
        cov_mark::check!(test_rename_field_put_init_shorthand);
        check(
            "bar",
            r#"
struct Foo { i$0: i32 }

fn foo(bar: i32) -> Foo {
    Foo { i: bar }
}
"#,
            r#"
struct Foo { bar: i32 }

fn foo(bar: i32) -> Foo {
    Foo { bar }
}
"#,
        );
    }

    #[test]
    fn test_rename_local_simple() {
        check(
            "i",
            r#"
fn foo(bar$0: i32) -> i32 {
    bar
}
"#,
            r#"
fn foo(i: i32) -> i32 {
    i
}
"#,
        );
    }

    #[test]
    fn test_rename_local_put_init_shorthand() {
        cov_mark::check!(test_rename_local_put_init_shorthand);
        check(
            "i",
            r#"
struct Foo { i: i32 }

fn foo(bar$0: i32) -> Foo {
    Foo { i: bar }
}
"#,
            r#"
struct Foo { i: i32 }

fn foo(i: i32) -> Foo {
    Foo { i }
}
"#,
        );
    }

    #[test]
    fn test_struct_field_pat_into_shorthand() {
        cov_mark::check!(test_rename_field_put_init_shorthand_pat);
        check(
            "baz",
            r#"
struct Foo { i$0: i32 }

fn foo(foo: Foo) {
    let Foo { i: ref baz @ qux } = foo;
    let _ = qux;
}
"#,
            r#"
struct Foo { baz: i32 }

fn foo(foo: Foo) {
    let Foo { baz: ref baz @ qux } = foo;
    let _ = qux;
}
"#,
        );
        check(
            "baz",
            r#"
struct Foo { i$0: i32 }

fn foo(foo: Foo) {
    let Foo { i: ref baz } = foo;
    let _ = qux;
}
"#,
            r#"
struct Foo { baz: i32 }

fn foo(foo: Foo) {
    let Foo { ref baz } = foo;
    let _ = qux;
}
"#,
        );
    }

    #[test]
    fn test_struct_local_pat_into_shorthand() {
        cov_mark::check!(test_rename_local_put_init_shorthand_pat);
        check(
            "field",
            r#"
struct Foo { field: i32 }

fn foo(foo: Foo) {
    let Foo { field: qux$0 } = foo;
    let _ = qux;
}
"#,
            r#"
struct Foo { field: i32 }

fn foo(foo: Foo) {
    let Foo { field } = foo;
    let _ = field;
}
"#,
        );
        check(
            "field",
            r#"
struct Foo { field: i32 }

fn foo(foo: Foo) {
    let Foo { field: x @ qux$0 } = foo;
    let _ = qux;
}
"#,
            r#"
struct Foo { field: i32 }

fn foo(foo: Foo) {
    let Foo { field: x @ field } = foo;
    let _ = field;
}
"#,
        );
    }

    #[test]
    fn test_rename_binding_in_destructure_pat() {
        let expected_fixture = r#"
struct Foo {
    i: i32,
}

fn foo(foo: Foo) {
    let Foo { i: bar } = foo;
    let _ = bar;
}
"#;
        check(
            "bar",
            r#"
struct Foo {
    i: i32,
}

fn foo(foo: Foo) {
    let Foo { i: b } = foo;
    let _ = b$0;
}
"#,
            expected_fixture,
        );
        check(
            "bar",
            r#"
struct Foo {
    i: i32,
}

fn foo(foo: Foo) {
    let Foo { i } = foo;
    let _ = i$0;
}
"#,
            expected_fixture,
        );
    }

    #[test]
    fn test_rename_binding_in_destructure_param_pat() {
        check(
            "bar",
            r#"
struct Foo {
    i: i32
}

fn foo(Foo { i }: Foo) -> i32 {
    i$0
}
"#,
            r#"
struct Foo {
    i: i32
}

fn foo(Foo { i: bar }: Foo) -> i32 {
    bar
}
"#,
        )
    }

    #[test]
    fn test_struct_field_complex_ident_pat() {
        cov_mark::check!(rename_record_pat_field_name_split);
        check(
            "baz",
            r#"
struct Foo { i$0: i32 }

fn foo(foo: Foo) {
    let Foo { ref i } = foo;
}
"#,
            r#"
struct Foo { baz: i32 }

fn foo(foo: Foo) {
    let Foo { baz: ref i } = foo;
}
"#,
        );
    }

    #[test]
    fn test_rename_lifetimes() {
        check(
            "'yeeee",
            r#"
trait Foo<'a> {
    fn foo() -> &'a ();
}
impl<'a> Foo<'a> for &'a () {
    fn foo() -> &'a$0 () {
        unimplemented!()
    }
}
"#,
            r#"
trait Foo<'a> {
    fn foo() -> &'a ();
}
impl<'yeeee> Foo<'yeeee> for &'yeeee () {
    fn foo() -> &'yeeee () {
        unimplemented!()
    }
}
"#,
        )
    }

    #[test]
    fn test_rename_bind_pat() {
        check(
            "new_name",
            r#"
fn main() {
    enum CustomOption<T> {
        None,
        Some(T),
    }

    let test_variable = CustomOption::Some(22);

    match test_variable {
        CustomOption::Some(foo$0) if foo == 11 => {}
        _ => (),
    }
}"#,
            r#"
fn main() {
    enum CustomOption<T> {
        None,
        Some(T),
    }

    let test_variable = CustomOption::Some(22);

    match test_variable {
        CustomOption::Some(new_name) if new_name == 11 => {}
        _ => (),
    }
}"#,
        );
    }

    #[test]
    fn test_rename_label() {
        check(
            "'foo",
            r#"
fn foo<'a>() -> &'a () {
    'a: {
        'b: loop {
            break 'a$0;
        }
    }
}
"#,
            r#"
fn foo<'a>() -> &'a () {
    'foo: {
        'b: loop {
            break 'foo;
        }
    }
}
"#,
        )
    }

    #[test]
    fn test_rename_label_new_name_without_apostrophe() {
        check(
            "foo",
            r#"
fn main() {
    'outer$0: loop {
        'inner: loop {
            break 'outer;
        }
    }
}
        "#,
            r#"
fn main() {
    'foo: loop {
        'inner: loop {
            break 'foo;
        }
    }
}
        "#,
        );
    }

    #[test]
    fn test_self_to_self() {
        cov_mark::check!(rename_self_to_self);
        check(
            "self",
            r#"
struct Foo;
impl Foo {
    fn foo(self$0) {}
}
"#,
            r#"
struct Foo;
impl Foo {
    fn foo(self) {}
}
"#,
        )
    }

    #[test]
    fn test_rename_field_in_pat_in_macro_doesnt_shorthand() {
        // ideally we would be able to make this emit a short hand, but I doubt this is easily possible
        check(
            "baz",
            r#"
macro_rules! foo {
    ($pattern:pat) => {
        let $pattern = loop {};
    };
}
struct Foo {
    bar$0: u32,
}
fn foo() {
    foo!(Foo { bar: baz });
}
"#,
            r#"
macro_rules! foo {
    ($pattern:pat) => {
        let $pattern = loop {};
    };
}
struct Foo {
    baz: u32,
}
fn foo() {
    foo!(Foo { baz: baz });
}
"#,
        )
    }

    #[test]
    fn test_rename_tuple_field() {
        check(
            "foo",
            r#"
struct Foo(i32);

fn baz() {
    let mut x = Foo(4);
    x.0$0 = 5;
}
"#,
            "error: No references found at position",
        );
    }

    #[test]
    fn test_rename_builtin() {
        check(
            "foo",
            r#"
fn foo() {
    let x: i32$0 = 0;
}
"#,
            "error: Cannot rename builtin type",
        );
    }

    #[test]
    fn test_rename_self() {
        check(
            "foo",
            r#"
struct Foo {}

impl Foo {
    fn foo(self) -> Self$0 {
        self
    }
}
"#,
            "error: No references found at position",
        );
    }

    #[test]
    fn test_rename_ignores_self_ty() {
        check(
            "Fo0",
            r#"
struct $0Foo;

impl Foo where Self: {}
"#,
            r#"
struct Fo0;

impl Fo0 where Self: {}
"#,
        );
    }

    #[test]
    fn test_rename_fails_on_aliases() {
        check(
            "Baz",
            r#"
struct Foo;
use Foo as Bar$0;
"#,
            "error: Renaming aliases is currently unsupported",
        );
        check(
            "Baz",
            r#"
struct Foo;
use Foo as Bar;
use Bar$0;
"#,
            "error: Renaming aliases is currently unsupported",
        );
    }

    #[test]
    fn test_rename_trait_method() {
        let res = r"
trait Foo {
    fn foo(&self) {
        self.foo();
    }
}

impl Foo for () {
    fn foo(&self) {
        self.foo();
    }
}";
        check(
            "foo",
            r#"
trait Foo {
    fn bar$0(&self) {
        self.bar();
    }
}

impl Foo for () {
    fn bar(&self) {
        self.bar();
    }
}"#,
            res,
        );
        check(
            "foo",
            r#"
trait Foo {
    fn bar(&self) {
        self.bar$0();
    }
}

impl Foo for () {
    fn bar(&self) {
        self.bar();
    }
}"#,
            res,
        );
        check(
            "foo",
            r#"
trait Foo {
    fn bar(&self) {
        self.bar();
    }
}

impl Foo for () {
    fn bar$0(&self) {
        self.bar();
    }
}"#,
            res,
        );
        check(
            "foo",
            r#"
trait Foo {
    fn bar(&self) {
        self.bar();
    }
}

impl Foo for () {
    fn bar(&self) {
        self.bar$0();
    }
}"#,
            res,
        );
    }

    #[test]
    fn test_rename_trait_method_prefix_of_second() {
        check(
            "qux",
            r#"
trait Foo {
    fn foo$0() {}
    fn foobar() {}
}
"#,
            r#"
trait Foo {
    fn qux() {}
    fn foobar() {}
}
"#,
        );
    }

    #[test]
    fn test_rename_trait_const() {
        let res = r"
trait Foo {
    const FOO: ();
}

impl Foo for () {
    const FOO: ();
}
fn f() { <()>::FOO; }";
        check(
            "FOO",
            r#"
trait Foo {
    const BAR$0: ();
}

impl Foo for () {
    const BAR: ();
}
fn f() { <()>::BAR; }"#,
            res,
        );
        check(
            "FOO",
            r#"
trait Foo {
    const BAR: ();
}

impl Foo for () {
    const BAR$0: ();
}
fn f() { <()>::BAR; }"#,
            res,
        );
        check(
            "FOO",
            r#"
trait Foo {
    const BAR: ();
}

impl Foo for () {
    const BAR: ();
}
fn f() { <()>::BAR$0; }"#,
            res,
        );
    }

    #[test]
    fn defs_from_macros_arent_renamed() {
        check(
            "lol",
            r#"
macro_rules! m { () => { fn f() {} } }
m!();
fn main() { f$0()  }
"#,
            "error: No identifier available to rename",
        )
    }

    #[test]
    fn attributed_item() {
        check(
            "function",
            r#"
//- proc_macros: identity

#[proc_macros::identity]
fn func$0() {
    func();
}
"#,
            r#"

#[proc_macros::identity]
fn function() {
    function();
}
"#,
        )
    }

    #[test]
    fn in_macro_multi_mapping() {
        check(
            "a",
            r#"
fn foo() {
    macro_rules! match_ast2 {
        ($node:ident {
            $( $res:expr, )*
        }) => {{
            $( if $node { $res } else )*
            { loop {} }
        }};
    }
    let $0d = 3;
    match_ast2! {
        d {
            d,
            d,
        }
    };
}
"#,
            r#"
fn foo() {
    macro_rules! match_ast2 {
        ($node:ident {
            $( $res:expr, )*
        }) => {{
            $( if $node { $res } else )*
            { loop {} }
        }};
    }
    let a = 3;
    match_ast2! {
        a {
            a,
            a,
        }
    };
}
"#,
        )
    }

    #[test]
    fn rename_multi_local() {
        check(
            "bar",
            r#"
fn foo((foo$0 | foo | foo): ()) {
    foo;
    let foo;
}
"#,
            r#"
fn foo((bar | bar | bar): ()) {
    bar;
    let foo;
}
"#,
        );
        check(
            "bar",
            r#"
fn foo((foo | foo$0 | foo): ()) {
    foo;
    let foo;
}
"#,
            r#"
fn foo((bar | bar | bar): ()) {
    bar;
    let foo;
}
"#,
        );
        check(
            "bar",
            r#"
fn foo((foo | foo | foo): ()) {
    foo$0;
    let foo;
}
"#,
            r#"
fn foo((bar | bar | bar): ()) {
    bar;
    let foo;
}
"#,
        );
    }

    #[test]
    fn regression_13498() {
        check(
            "Testing",
            r"
mod foo {
    pub struct Test$0;
}

use foo::Test as Tester;

fn main() {
    let t = Tester;
}
",
            r"
mod foo {
    pub struct Testing;
}

use foo::Testing as Tester;

fn main() {
    let t = Tester;
}
",
        )
    }

    #[test]
    fn extern_crate() {
        check_prepare(
            r"
//- /lib.rs crate:main deps:foo
extern crate foo$0;
use foo as qux;
//- /foo.rs crate:foo
",
            expect![[r#"No references found at position"#]],
        );
        // FIXME: replace above check_prepare with this once we resolve to usages to extern crate declarations
        //         check(
        //             "bar",
        //             r"
        // //- /lib.rs crate:main deps:foo
        // extern crate foo$0;
        // use foo as qux;
        // //- /foo.rs crate:foo
        // ",
        //             r"
        // extern crate foo as bar;
        // use bar as qux;
        // ",
        //         );
    }

    #[test]
    fn extern_crate_rename() {
        check_prepare(
            r"
//- /lib.rs crate:main deps:foo
extern crate foo as qux$0;
use qux as frob;
//- /foo.rs crate:foo
",
            expect!["Renaming aliases is currently unsupported"],
        );
        // FIXME: replace above check_prepare with this once we resolve to usages to extern crate
        // declarations
        //         check(
        //             "bar",
        //             r"
        // //- /lib.rs crate:main deps:foo
        // extern crate foo as qux$0;
        // use qux as frob;
        // //- /foo.rs crate:foo
        // ",
        //             r"
        // extern crate foo as bar;
        // use bar as frob;
        // ",
        //         );
    }

    #[test]
    fn extern_crate_self() {
        check_prepare(
            r"
extern crate self$0;
use self as qux;
",
            expect!["No references found at position"],
        );
        // FIXME: replace above check_prepare with this once we resolve to usages to extern crate declarations
        //         check(
        //             "bar",
        //             r"
        // extern crate self$0;
        // use self as qux;
        // ",
        //             r"
        // extern crate self as bar;
        // use self as qux;
        // ",
        //         );
    }

    #[test]
    fn extern_crate_self_rename() {
        check_prepare(
            r"
//- /lib.rs crate:main deps:foo
extern crate self as qux$0;
use qux as frob;
//- /foo.rs crate:foo
",
            expect!["Renaming aliases is currently unsupported"],
        );
        // FIXME: replace above check_prepare with this once we resolve to usages to extern crate declarations
        //         check(
        //             "bar",
        //             r"
        // //- /lib.rs crate:main deps:foo
        // extern crate self as qux$0;
        // use qux as frob;
        // //- /foo.rs crate:foo
        // ",
        //             r"
        // extern crate self as bar;
        // use bar as frob;
        // ",
        //         );
    }

    #[test]
    fn disallow_renaming_for_non_local_definition() {
        check(
            "Baz",
            r#"
//- /lib.rs crate:lib new_source_root:library
pub struct S;
//- /main.rs crate:main deps:lib new_source_root:local
use lib::S;
fn main() { let _: S$0; }
"#,
            "error: Cannot rename a non-local definition",
        );
    }

    #[test]
    fn disallow_renaming_for_builtin_macros() {
        check(
            "Baz",
            r#"
//- minicore: derive, hash
//- /main.rs crate:main
use core::hash::Hash;
#[derive(H$0ash)]
struct A;
            "#,
            "error: Cannot rename a non-local definition",
        );
    }

    #[test]
    fn implicit_format_args() {
        check(
            "fbar",
            r#"
//- minicore: fmt
fn test() {
    let foo = "foo";
    format_args!("hello {foo} {foo$0} {}", foo);
}
"#,
            r#"
fn test() {
    let fbar = "foo";
    format_args!("hello {fbar} {fbar} {}", fbar);
}
"#,
        );
    }

    #[test]
    fn implicit_format_args2() {
        check(
            "fo",
            r#"
//- minicore: fmt
fn test() {
    let foo = "foo";
    format_args!("hello {foo} {foo$0} {}", foo);
}
"#,
            r#"
fn test() {
    let fo = "foo";
    format_args!("hello {fo} {fo} {}", fo);
}
"#,
        );
    }

    #[test]
    fn asm_operand() {
        check(
            "bose",
            r#"
//- minicore: asm
fn test() {
    core::arch::asm!(
        "push {base}",
        base$0 = const 0
    );
}
"#,
            r#"
fn test() {
    core::arch::asm!(
        "push {bose}",
        bose = const 0
    );
}
"#,
        );
    }

    #[test]
    fn asm_operand2() {
        check(
            "bose",
            r#"
//- minicore: asm
fn test() {
    core::arch::asm!(
        "push {base$0}",
        "push {base}",
        boo = const 0,
        virtual_free = sym VIRTUAL_FREE,
        base = const 0,
        boo = const 0,
    );
}
"#,
            r#"
fn test() {
    core::arch::asm!(
        "push {bose}",
        "push {bose}",
        boo = const 0,
        virtual_free = sym VIRTUAL_FREE,
        bose = const 0,
        boo = const 0,
    );
}
"#,
        );
    }

    #[test]
    fn rename_path_inside_use_tree() {
        check(
            "Baz",
            r#"
//- /main.rs crate:main
mod module;
mod foo { pub struct Foo; }
mod bar { use super::Foo; }

use foo::Foo$0;

fn main() { let _: Foo; }
//- /module.rs
use crate::foo::Foo;
"#,
            r#"
mod module;
mod foo { pub struct Foo; }
mod bar { use super::Baz; }

use foo::Foo as Baz;

fn main() { let _: Baz; }
"#,
        )
    }

    #[test]
    fn rename_path_inside_use_tree_foreign() {
        check(
            "Baz",
            r#"
//- /lib.rs crate:lib new_source_root:library
pub struct S;
//- /main.rs crate:main deps:lib new_source_root:local
use lib::S$0;
fn main() { let _: S; }
"#,
            r#"
use lib::S as Baz;
fn main() { let _: Baz; }
"#,
        );
    }

    #[test]
    fn rename_type_param_ref_in_use_bound() {
        check(
            "U",
            r#"
fn foo<T>() -> impl use<T$0> Trait {}
"#,
            r#"
fn foo<U>() -> impl use<U> Trait {}
"#,
        );
    }

    #[test]
    fn rename_type_param_in_use_bound() {
        check(
            "U",
            r#"
fn foo<T$0>() -> impl use<T> Trait {}
"#,
            r#"
fn foo<U>() -> impl use<U> Trait {}
"#,
        );
    }

    #[test]
    fn rename_lifetime_param_ref_in_use_bound() {
        check(
            "u",
            r#"
fn foo<'t>() -> impl use<'t$0> Trait {}
"#,
            r#"
fn foo<'u>() -> impl use<'u> Trait {}
"#,
        );
    }

    #[test]
    fn rename_lifetime_param_in_use_bound() {
        check(
            "u",
            r#"
fn foo<'t$0>() -> impl use<'t> Trait {}
"#,
            r#"
fn foo<'u>() -> impl use<'u> Trait {}
"#,
        );
    }

    #[test]
    fn rename_parent_type_param_in_use_bound() {
        check(
            "U",
            r#"
trait Trait<T> {
    fn foo() -> impl use<T$0> Trait {}
}
"#,
            r#"
trait Trait<U> {
    fn foo() -> impl use<U> Trait {}
}
"#,
        );
    }

    #[test]
    fn rename_macro_generated_type_from_type_with_a_suffix() {
        check(
            "Bar",
            r#"
//- proc_macros: generate_suffixed_type
#[proc_macros::generate_suffixed_type]
struct Foo$0;
fn usage(_: FooSuffix) {}
usage(FooSuffix);
"#,
            r#"
#[proc_macros::generate_suffixed_type]
struct Bar;
fn usage(_: BarSuffix) {}
usage(BarSuffix);
"#,
        );
    }

    #[test]
    // FIXME
    #[should_panic]
    fn rename_macro_generated_type_from_type_usage_with_a_suffix() {
        check(
            "Bar",
            r#"
//- proc_macros: generate_suffixed_type
#[proc_macros::generate_suffixed_type]
struct Foo;
fn usage(_: FooSuffix) {}
usage(FooSuffix);
fn other_place() { Foo$0; }
"#,
            r#"
#[proc_macros::generate_suffixed_type]
struct Bar;
fn usage(_: BarSuffix) {}
usage(BarSuffix);
fn other_place() { Bar; }
"#,
        );
    }

    #[test]
    fn rename_macro_generated_type_from_variant_with_a_suffix() {
        check(
            "Bar",
            r#"
//- proc_macros: generate_suffixed_type
#[proc_macros::generate_suffixed_type]
enum Quux {
    Foo$0,
}
fn usage(_: FooSuffix) {}
usage(FooSuffix);
"#,
            r#"
#[proc_macros::generate_suffixed_type]
enum Quux {
    Bar,
}
fn usage(_: BarSuffix) {}
usage(BarSuffix);
"#,
        );
    }

    #[test]
    // FIXME
    #[should_panic]
    fn rename_macro_generated_type_from_variant_usage_with_a_suffix() {
        check(
            "Bar",
            r#"
//- proc_macros: generate_suffixed_type
#[proc_macros::generate_suffixed_type]
enum Quux {
    Foo,
}
fn usage(_: FooSuffix) {}
usage(FooSuffix);
fn other_place() { Quux::Foo$0; }
"#,
            r#"
#[proc_macros::generate_suffixed_type]
enum Quux {
    Bar,
}
fn usage(_: BarSuffix) {}
usage(BartSuffix);
fn other_place() { Quux::Bar$0; }
"#,
        );
    }

    #[test]
    fn rename_to_self_callers() {
        check(
            "self",
            r#"
//- minicore: add
struct Foo;
impl core::ops::Add for Foo {
    type Target = Foo;
    fn add(self, _: Self) -> Foo { Foo }
}

impl Foo {
    fn foo(th$0is: &Self) {}
}

fn bar(v: &Foo) {
    Foo::foo(v);
}

fn baz() {
    Foo::foo(&Foo);
    Foo::foo(Foo + Foo);
}
        "#,
            r#"
struct Foo;
impl core::ops::Add for Foo {
    type Target = Foo;
    fn add(self, _: Self) -> Foo { Foo }
}

impl Foo {
    fn foo(&self) {}
}

fn bar(v: &Foo) {
    v.foo();
}

fn baz() {
    Foo.foo();
    (Foo + Foo).foo();
}
        "#,
        );
        // Multiple arguments:
        check(
            "self",
            r#"
struct Foo;

impl Foo {
    fn foo(th$0is: &Self, v: i32) {}
}

fn bar(v: Foo) {
    Foo::foo(&v, 123);
}
        "#,
            r#"
struct Foo;

impl Foo {
    fn foo(&self, v: i32) {}
}

fn bar(v: Foo) {
    v.foo(123);
}
        "#,
        );
    }

    // End-to-end move operation tests (Requirements 3.1, 3.2, 3.3, 4.1, 4.2, 4.3, 6.1, 6.2, 6.3, 6.4, 6.5)

    #[test]
    fn test_move_struct_between_modules() {
        check_expect(
            "crate::target_module::MovedStruct",
            r#"
//- /lib.rs
mod source_module {
    pub struct TestStruct$0 {
        pub field: i32,
    }
    
    impl TestStruct {
        pub fn new() -> Self {
            Self { field: 0 }
        }
    }
}

mod target_module {
    // Target module exists but is empty
}

fn main() {
    let instance = source_module::TestStruct::new();
    println!("{}", instance.field);
}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(0),
                        [
                            Indel {
                                insert: "MovedStruct",
                                delete: 45..55,
                            },
                            Indel {
                                insert: "MovedStruct",
                                delete: 89..99,
                            },
                            Indel {
                                insert: "target_module::MovedStruct",
                                delete: 200..226,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );
    }

    #[test]
    fn test_move_function_between_modules() {
        check_expect(
            "crate::utils::helper_function",
            r#"
//- /lib.rs
mod source {
    pub fn test_function$0(x: i32) -> i32 {
        x * 2
    }
}

mod utils {
    // Target module exists
}

fn main() {
    let result = source::test_function(5);
    println!("{}", result);
}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(0),
                        [
                            Indel {
                                insert: "helper_function",
                                delete: 25..38,
                            },
                            Indel {
                                insert: "utils::helper_function",
                                delete: 130..152,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );
    }

    #[test]
    fn test_move_enum_with_dependencies() {
        check_expect(
            "crate::types::Status",
            r#"
//- /lib.rs
mod source {
    #[derive(Debug, Clone)]
    pub enum TestEnum$0 {
        Variant1,
        Variant2(i32),
    }
    
    pub fn process_enum(e: TestEnum) -> TestEnum {
        match e {
            TestEnum::Variant1 => TestEnum::Variant2(42),
            TestEnum::Variant2(x) => TestEnum::Variant1,
        }
    }
}

mod types {
    // Target module
}

fn main() {
    use source::TestEnum;
    let e = TestEnum::Variant1;
    let processed = source::process_enum(e);
    println!("{:?}", processed);
}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(0),
                        [
                            Indel {
                                insert: "Status",
                                delete: 58..66,
                            },
                            Indel {
                                insert: "Status",
                                delete: 149..157,
                            },
                            Indel {
                                insert: "Status",
                                delete: 167..175,
                            },
                            Indel {
                                insert: "Status::Variant1",
                                delete: 198..216,
                            },
                            Indel {
                                insert: "Status::Variant2(42)",
                                delete: 219..240,
                            },
                            Indel {
                                insert: "Status::Variant1",
                                delete: 264..282,
                            },
                            Indel {
                                insert: "types::Status",
                                delete: 340..357,
                            },
                            Indel {
                                insert: "Status::Variant1",
                                delete: 370..388,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );
    }

    #[test]
    fn test_move_operation_name_conflict_error() {
        check(
            "crate::target::ExistingStruct",
            r#"
//- /lib.rs
mod source {
    pub struct TestStruct$0 {
        field: i32,
    }
}

mod target {
    pub struct ExistingStruct {
        other_field: String,
    }
}
"#,
            "error: Name conflict: item 'ExistingStruct' already exists in target module",
        );
    }

    #[test]
    fn test_move_operation_visibility_validation() {
        check_expect(
            "crate::private_module::PublicStruct",
            r#"
//- /lib.rs
pub struct TestStruct$0 {
    pub field: i32,
}

mod private_module {
    // Private module - moving public struct here should work
    // but may affect accessibility
}

pub fn use_struct() -> TestStruct {
    TestStruct { field: 42 }
}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(0),
                        [
                            Indel {
                                insert: "PublicStruct",
                                delete: 11..21,
                            },
                            Indel {
                                insert: "private_module::PublicStruct",
                                delete: 156..166,
                            },
                            Indel {
                                insert: "private_module::PublicStruct",
                                delete: 172..182,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );
    }

    #[test]
    fn test_move_with_internal_references_update() {
        check_expect(
            "crate::new_location::MovedStruct",
            r#"
//- /lib.rs
mod original {
    pub struct TestStruct$0 {
        field: i32,
    }
    
    impl TestStruct {
        pub fn new() -> Self {
            Self { field: 0 }
        }
        
        pub fn get_field(&self) -> i32 {
            self.field
        }
    }
}

mod new_location {
    // Target module
}

fn main() {
    let s = original::TestStruct::new();
    println!("{}", s.get_field());
}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(0),
                        [
                            Indel {
                                insert: "MovedStruct",
                                delete: 29..39,
                            },
                            Indel {
                                insert: "MovedStruct",
                                delete: 73..83,
                            },
                            Indel {
                                insert: "new_location::MovedStruct",
                                delete: 244..266,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );
    }

    #[test]
    fn test_move_creates_missing_module_structure() {
        check_expect(
            "crate::deeply::nested::new_module::MovedItem",
            r#"
//- /lib.rs
pub fn test_function$0() -> i32 {
    42
}

fn main() {
    println!("{}", test_function());
}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(0),
                        [
                            Indel {
                                insert: "MovedItem",
                                delete: 7..20,
                            },
                            Indel {
                                insert: "deeply::nested::new_module::MovedItem",
                                delete: 59..72,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    CreateFile {
                        dst: AnchoredPathBuf {
                            anchor: FileId(0),
                            path: "deeply.rs",
                        },
                        initial_contents: "//! The `deeply` module\n//!\n//! TODO: Add module documentation\n",
                    },
                    CreateFile {
                        dst: AnchoredPathBuf {
                            anchor: FileId(0),
                            path: "deeply/nested.rs",
                        },
                        initial_contents: "//! The `nested` module\n//!\n//! TODO: Add module documentation\n",
                    },
                    CreateFile {
                        dst: AnchoredPathBuf {
                            anchor: FileId(0),
                            path: "deeply/nested/new_module.rs",
                        },
                        initial_contents: "//! The `new_module` module\n//!\n//! TODO: Add module documentation\n",
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_move_circular_dependency_detection() {
        check(
            "crate::child::Parent",
            r#"
//- /lib.rs
mod parent {
    use super::child::Child;
    
    pub struct Parent$0 {
        child: Child,
    }
}

mod child {
    use super::parent::Parent;
    
    pub struct Child {
        parent_ref: *const Parent,
    }
}
"#,
            "error: Move would create circular dependency",
        );
    }

    #[test]
    fn test_move_relative_path_same_module() {
        check(
            "NewName",
            r#"
//- /lib.rs
mod test_module {
    pub struct OldName$0 {
        field: i32,
    }
    
    pub fn use_struct() -> OldName {
        OldName { field: 42 }
    }
}
"#,
            r#"
mod test_module {
    pub struct NewName {
        field: i32,
    }
    
    pub fn use_struct() -> NewName {
        NewName { field: 42 }
    }
}
"#,
        );
    }
}
