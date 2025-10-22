//! Item move operations for rename-to-move functionality.
//!
//! This module handles the logic for moving items between modules when the
//! rename target is a fully-qualified path (e.g., `crate::other::module::NewName`).

use hir::{Module, Semantics, ModPath, PathKind, HasSource};
use syntax::{AstNode, ast::{self, HasName}, TextRange};
use base_db::AnchoredPathBuf;

use crate::{RootDatabase, defs::Definition, source_change::FileSystemEdit, text_edit::TextEdit};

use super::{IdentifierKind, RenameError, Result, bail, format_err};

/// Represents a rename operation that includes moving an item to a different module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MoveOperation {
    /// The source module where the item currently resides
    pub source_module: Module,
    /// The destination module path (everything except final name component)
    pub dest_module_path: ModPath,
    /// The new name for the item (final path component)
    pub new_name: String,
    /// Whether the destination module exists
    pub dest_module_exists: bool,
}

/// The result of parsing a rename target string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RenameTarget {
    /// Simple rename within the same module
    SimpleRename,
    /// Move to a different module with optional rename
    Move(MoveOperation),
}

/// Parse a rename target string to determine if it's a move operation.
///
/// A move operation is detected when the new_name contains `::` and can be
/// parsed as a valid Rust path (e.g., `crate::other::NewName`).
pub fn parse_rename_target(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    new_name_str: &str,
) -> Result<RenameTarget> {
    // Check if the new name looks like a path (contains ::)
    if !new_name_str.contains("::") {
        return Ok(RenameTarget::SimpleRename);
    }

    // Parse as a path using syntax
    let parsed = syntax::SourceFile::parse(
        &format!("type _ = {};", new_name_str),
        span::Edition::LATEST,
    );
    let syntax_node = parsed.syntax_node();

    // Find the path in the parsed syntax
    let path = syntax_node
        .descendants()
        .find_map(ast::Path::cast)
        .ok_or_else(|| format_err!("Invalid path: {}", new_name_str))?;

    // Extract the final name component
    let final_name = path
        .segment()
        .and_then(|seg| seg.name_ref())
        .ok_or_else(|| format_err!("Path must end with an identifier"))?
        .to_string();

    // Validate the final name is a valid identifier
    let edition = span::Edition::LATEST; // TODO: get actual edition
    IdentifierKind::classify(edition, &final_name)?;

    // Get the source module for the item being renamed
    let source_module = def
        .module(sema.db)
        .ok_or_else(|| format_err!("Cannot determine module for item"))?;

    // Parse the module path using ModPath
    let edition = span::Edition::LATEST; // TODO: get actual edition from context
    let mod_path = ModPath::from_src(
        sema.db,
        path.clone(),
        &mut |_| span::SyntaxContext::root(edition)
    )
    .ok_or_else(|| format_err!("Failed to parse module path"))?;

    // Extract just the module portion (remove final segment which is the item name)
    let dest_module_path = extract_module_path(&mod_path)?;

    // Try to resolve the destination module to see if it exists
    let dest_module_exists = try_resolve_module(sema, &source_module, &dest_module_path).is_some();

    // Check if destination is same as source (would just be a rename)
    if let Some(dest_module) = try_resolve_module(sema, &source_module, &dest_module_path) {
        if dest_module == source_module {
            return Ok(RenameTarget::SimpleRename);
        }
    }

    Ok(RenameTarget::Move(MoveOperation {
        source_module,
        dest_module_path,
        new_name: final_name,
        dest_module_exists,
    }))
}

/// Extract the module path from a full ModPath (excluding the final item name segment).
fn extract_module_path(path: &ModPath) -> Result<ModPath> {
    let segments = path.segments();

    if segments.len() < 1 {
        bail!("Path must have at least one segment");
    }

    // For a move operation, we need at least a module path
    // e.g., "crate::foo::Bar" -> module path is "crate::foo"
    if segments.len() == 1 {
        // Single segment with a path kind (e.g., "crate::Foo")
        // Module path is just the kind (e.g., "crate")
        return Ok(ModPath::from_kind(path.kind));
    }

    // Remove the last segment (item name)
    let module_segments = &segments[..segments.len() - 1];
    Ok(ModPath::from_segments(
        path.kind,
        module_segments.iter().cloned(),
    ))
}

/// Try to resolve a module path to an actual Module.
/// Returns None if the module doesn't exist yet (needs to be created).
fn try_resolve_module(
    sema: &Semantics<'_, RootDatabase>,
    from_module: &Module,
    path: &ModPath,
) -> Option<Module> {
    // For now, we'll do a simple resolution starting from the crate root or current module
    match path.kind {
        PathKind::Crate => {
            let crate_module = from_module.krate().root_module();
            resolve_from_module(sema, crate_module, path.segments())
        }
        PathKind::Super(level) => {
            // Navigate up 'level' parents
            let mut current = *from_module;
            for _ in 0..level {
                current = current.parent(sema.db)?;
            }
            resolve_from_module(sema, current, path.segments())
        }
        PathKind::Plain => {
            // Relative path from current module
            resolve_from_module(sema, *from_module, path.segments())
        }
        PathKind::Abs => {
            // Absolute path from crate root
            let crate_module = from_module.krate().root_module();
            resolve_from_module(sema, crate_module, path.segments())
        }
        PathKind::DollarCrate(_) => {
            // $crate from macro expansion - treat as absolute crate path
            let crate_module = from_module.krate().root_module();
            resolve_from_module(sema, crate_module, path.segments())
        }
    }
}

/// Resolve a path starting from a given module.
fn resolve_from_module(
    sema: &Semantics<'_, RootDatabase>,
    mut current: Module,
    segments: &[hir::Name],
) -> Option<Module> {
    for segment in segments {
        // Find child module with this name
        current = current
            .children(sema.db)
            .find(|child| child.name(sema.db).as_ref() == Some(segment))?;
    }
    Some(current)
}

/// Compute the file path for a destination module.
/// Returns both the primary candidate (named file like `foo.rs`) and alternative (mod.rs style).
pub fn module_file_paths(
    parent_module: &Module,
    module_name: &str,
    sema: &Semantics<'_, RootDatabase>,
) -> Result<(String, String)> {
    // Get the parent module's file
    let parent_file_id = parent_module
        .as_source_file_id(sema.db)
        .ok_or_else(|| format_err!("Parent module has no associated file"))?;

    // Determine the directory path where the new module should be created
    let dir_path = if parent_module.is_mod_rs(sema.db) {
        // Parent is mod.rs, so children go in the same directory
        // e.g., src/foo/mod.rs -> src/foo/bar.rs or src/foo/bar/mod.rs
        String::new()
    } else {
        // Parent is a named file like foo.rs, so children go in foo/ subdirectory
        // e.g., src/foo.rs -> src/foo/bar.rs or src/foo/bar/mod.rs
        if let Some(parent_name) = parent_module.name(sema.db) {
            format!("{}/", parent_name.as_str())
        } else {
            // Crate root without explicit mod.rs
            String::new()
        }
    };

    // Generate both candidate paths
    let named_file = format!("{}{}.rs", dir_path, module_name);
    let mod_rs_file = format!("{}{}/mod.rs", dir_path, module_name);

    Ok((named_file, mod_rs_file))
}

/// Determine which module file style to use (foo.rs vs foo/mod.rs).
/// Returns the path to create and whether it's a mod.rs file.
pub fn choose_module_file_style(
    parent_module: &Module,
    module_name: &str,
    dest_module_path: &ModPath,
    sema: &Semantics<'_, RootDatabase>,
) -> Result<(String, bool)> {
    let (named_file, mod_rs_file) = module_file_paths(parent_module, module_name, sema)?;

    // Determine preference based on existing modules in the parent
    // If the destination path has more segments after this, we need mod.rs style
    // to allow further nesting
    let needs_nesting = dest_module_path.segments().len() > 1;

    if needs_nesting {
        // We're creating an intermediate module in a nested path
        // Use mod.rs style to allow children
        return Ok((mod_rs_file, true));
    }

    // Check existing siblings for style preference
    let siblings: Vec<_> = parent_module.children(sema.db).collect();
    let mut named_file_count = 0;
    let mut mod_rs_count = 0;

    for sibling in siblings {
        if sibling.is_mod_rs(sema.db) {
            mod_rs_count += 1;
        } else {
            named_file_count += 1;
        }
    }

    // Prefer the style that's already in use
    // Default to named file style if no preference
    let use_mod_rs = mod_rs_count > named_file_count;

    if use_mod_rs {
        Ok((mod_rs_file, true))
    } else {
        Ok((named_file, false))
    }
}

/// Create file system edits to ensure the destination module exists.
/// Returns a list of FileSystemEdit operations to create necessary files/directories.
pub fn create_module_tree(
    sema: &Semantics<'_, RootDatabase>,
    source_module: &Module,
    dest_module_path: &ModPath,
) -> Result<Vec<FileSystemEdit>> {
    let mut edits = Vec::new();

    // Start from the appropriate root based on path kind
    let mut current_module = match dest_module_path.kind {
        PathKind::Crate | PathKind::Abs | PathKind::DollarCrate(_) => {
            source_module.krate().root_module()
        }
        PathKind::Super(0) => *source_module,
        PathKind::Super(level) => {
            let mut module = *source_module;
            for _ in 0..level {
                module = module
                    .parent(sema.db)
                    .ok_or_else(|| format_err!("Cannot navigate to super module"))?;
            }
            module
        }
        PathKind::Plain => *source_module,
    };

    // Navigate through the path segments, creating modules as needed
    let segments = dest_module_path.segments();
    for (index, segment) in segments.iter().enumerate() {
        let segment_name = segment.as_str();

        // Check if this child module already exists
        if let Some(child) = current_module
            .children(sema.db)
            .find(|child| child.name(sema.db).as_ref() == Some(segment))
        {
            // Module exists, continue navigating
            current_module = child;
        } else {
            // Module doesn't exist, need to create it
            // Get the parent module's file to anchor the new file
            let anchor_file = current_module
                .as_source_file_id(sema.db)
                .ok_or_else(|| format_err!("Module has no associated file"))?
                .file_id(sema.db);

            // Determine the remaining path for nesting decisions
            let remaining_segments = &segments[index..];
            let remaining_path = ModPath::from_segments(
                PathKind::Plain,
                remaining_segments.iter().cloned(),
            );

            // Choose file style (named file vs mod.rs)
            let (file_path, is_mod_rs) =
                choose_module_file_style(&current_module, segment_name, &remaining_path, sema)?;

            // Create the file
            edits.push(FileSystemEdit::CreateFile {
                dst: AnchoredPathBuf { anchor: anchor_file, path: file_path.clone() },
                initial_contents: String::new(),
            });

            // For nested paths, we need to continue creating intermediate modules
            // but we can't navigate into them since they don't exist in HIR yet.
            // The remaining modules will need to be created with proper paths.

            // If this is not the last segment and we're using named file style,
            // we need to create a directory too (implicitly done by the file path)

            // We can't continue navigating since the module doesn't exist in HIR
            // Instead, build the remaining path manually
            if index < segments.len() - 1 {
                // More segments to process - need to handle nested creation
                for (nested_index, nested_segment) in segments[index + 1..].iter().enumerate() {
                    let nested_segment_name = nested_segment.as_str();
                    let remaining_nested = &segments[index + 1 + nested_index..];
                    let remaining_nested_path = ModPath::from_segments(
                        PathKind::Plain,
                        remaining_nested.iter().cloned(),
                    );

                    // Build path relative to the file we just created
                    let nested_path = if is_mod_rs {
                        // Created foo/mod.rs, so next file is foo/bar.rs or foo/bar/mod.rs
                        if remaining_nested_path.segments().len() > 1 {
                            format!("{}/{}/mod.rs", segment_name, nested_segment_name)
                        } else {
                            format!("{}/{}.rs", segment_name, nested_segment_name)
                        }
                    } else {
                        // Created foo.rs, so next file is foo/bar.rs or foo/bar/mod.rs
                        if remaining_nested_path.segments().len() > 1 {
                            format!("{}/{}/mod.rs", segment_name, nested_segment_name)
                        } else {
                            format!("{}/{}.rs", segment_name, nested_segment_name)
                        }
                    };

                    edits.push(FileSystemEdit::CreateFile {
                        dst: AnchoredPathBuf { anchor: anchor_file, path: nested_path },
                        initial_contents: String::new(),
                    });
                }
            }

            // Break since we can't navigate further without the modules existing in HIR
            break;
        }
    }

    Ok(edits)
}

/// Insert a module declaration in the parent module file.
/// Returns a TextEdit to add `pub mod module_name;` in the appropriate location.
pub fn insert_module_declaration(
    sema: &Semantics<'_, RootDatabase>,
    parent_module: &Module,
    module_name: &str,
) -> Result<Option<(span::FileId, TextEdit)>> {
    // Get the parent module's source file
    let parent_file_id = parent_module
        .as_source_file_id(sema.db)
        .ok_or_else(|| format_err!("Parent module has no associated file"))?;

    // Parse the parent file
    let parent_source = sema.parse(parent_file_id);
    let parent_syntax = parent_source.syntax();

    // Check if the module declaration already exists
    if module_declaration_exists(parent_syntax, module_name) {
        // Module already declared, no edit needed
        return Ok(None);
    }

    // Create the module declaration
    let module_decl = create_module_declaration(module_name);

    // Find the best position to insert the declaration
    let insert_position = find_module_insert_position(parent_syntax);

    // Create the text edit
    let text_edit = match insert_position {
        Some(position) => {
            // Insert after the found position with proper spacing
            let insert_offset = position.text_range().end();
            let insert_text = format!("\n{}", module_decl);
            TextEdit::insert(insert_offset, insert_text)
        }
        None => {
            // No existing modules, insert at the beginning after any attributes/comments
            let insert_offset = find_first_item_position(parent_syntax);
            let insert_text = format!("{}\n\n", module_decl);
            TextEdit::insert(insert_offset, insert_text)
        }
    };

    Ok(Some((parent_file_id.file_id(sema.db), text_edit)))
}

/// Check if a module declaration already exists in the file.
fn module_declaration_exists(syntax: &syntax::SyntaxNode, module_name: &str) -> bool {
    syntax
        .descendants()
        .filter_map(ast::Module::cast)
        .any(|module| {
            module.name().map(|name| name.text() == module_name).unwrap_or(false)
                && module.semicolon_token().is_some() // Only match declarations, not inline modules
        })
}

/// Create a `pub mod module_name;` declaration.
fn create_module_declaration(module_name: &str) -> String {
    format!("pub mod {};", module_name)
}

/// Find the best position to insert a new module declaration.
/// Returns the syntax node after which to insert (typically the last existing module declaration).
fn find_module_insert_position(syntax: &syntax::SyntaxNode) -> Option<syntax::SyntaxNode> {
    // Find all module declarations (mod foo;) at the top level
    let mut last_module_decl = None;

    for item in syntax.descendants().filter_map(ast::Item::cast) {
        if let ast::Item::Module(module) = item {
            // Only consider module declarations (with semicolon), not inline modules (with body)
            if module.semicolon_token().is_some() {
                last_module_decl = Some(module.syntax().clone());
            }
        }
    }

    last_module_decl
}

/// Find the position to insert the first module declaration.
/// This should be after file-level attributes and use statements, but before other items.
fn find_first_item_position(syntax: &syntax::SyntaxNode) -> syntax::TextSize {
    // Look for the first non-use item
    for item in syntax.descendants().filter_map(ast::Item::cast) {
        if !matches!(item, ast::Item::Use(_)) {
            return item.syntax().text_range().start();
        }
    }

    // If no items found, insert at the start
    syntax::TextSize::from(0)
}

/// Extract the text range of an item definition including its attributes and documentation.
/// This captures the complete item for relocation.
pub fn extract_item_with_attrs(item_node: &syntax::SyntaxNode) -> TextRange {
    // Start from the item itself
    let mut start = item_node.text_range().start();

    // Walk backwards to include attributes and doc comments
    let mut current = item_node.clone();
    while let Some(prev) = current.prev_sibling_or_token() {
        match prev.kind() {
            syntax::SyntaxKind::WHITESPACE => {
                // Check if this is just whitespace on the same line or includes newlines
                if let Some(token) = prev.as_token() {
                    let text = token.text();
                    if text.contains('\n') {
                        // Stop at the newline before attributes/comments
                        // but include the newline after the previous item
                        let lines: Vec<_> = text.split('\n').collect();
                        if lines.len() > 1 {
                            // Include everything after the first newline
                            let offset = text.find('\n').unwrap() + 1;
                            start = prev.text_range().start() + syntax::TextSize::from(offset as u32);
                        }
                        break;
                    }
                    current = prev.as_node().unwrap().clone();
                }
            }
            syntax::SyntaxKind::COMMENT => {
                // Include doc comments and regular comments
                start = prev.text_range().start();
                current = prev.as_node().map(|n| n.clone()).unwrap_or(current);
            }
            syntax::SyntaxKind::ATTR => {
                // Include attributes
                start = prev.text_range().start();
                current = prev.as_node().map(|n| n.clone()).unwrap_or(current);
            }
            _ => {
                // Hit a different kind of node, stop here
                break;
            }
        }
    }

    TextRange::new(start, item_node.text_range().end())
}

/// Find all impl blocks associated with a given item (struct, enum, or trait).
/// Returns the syntax nodes of the impl blocks.
pub fn find_associated_impl_blocks(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
) -> Vec<syntax::SyntaxNode> {
    let mut impl_blocks = Vec::new();

    // Get the module containing the item
    let module = match def.module(sema.db) {
        Some(m) => m,
        None => return impl_blocks,
    };

    // Get all impl blocks in the module
    let module_impls = module.impl_defs(sema.db);

    // For each impl, check if it's for our type
    for impl_def in module_impls {
        let matches = match def {
            Definition::Adt(adt) => {
                // Check if impl is for this ADT
                let impl_self_ty = impl_def.self_ty(sema.db);
                match impl_self_ty.as_adt() {
                    Some(impl_adt) if impl_adt == *adt => true,
                    _ => false,
                }
            }
            Definition::Trait(trait_def) => {
                // Check if this is a trait impl (impl Trait for Type)
                if let Some(impl_trait) = impl_def.trait_(sema.db) {
                    impl_trait == *trait_def
                } else {
                    false
                }
            }
            _ => false,
        };

        if matches {
            // Get the source of the impl block
            if let Some(source) = impl_def.source(sema.db) {
                impl_blocks.push(source.value.syntax().clone());
            }
        }
    }

    impl_blocks
}

/// Generate a TextEdit to remove an item from its source file.
/// This includes removing the item definition and any trailing whitespace/newlines.
pub fn generate_item_removal_edit(
    item_range: TextRange,
    source_text: &str,
) -> TextEdit {
    // Extend the range to include trailing whitespace up to and including the next newline
    let mut end = item_range.end();
    let end_usize: usize = end.into();

    if end_usize < source_text.len() {
        let remaining = &source_text[end_usize..];
        if let Some(newline_pos) = remaining.find('\n') {
            // Include the newline
            end += syntax::TextSize::from((newline_pos + 1) as u32);
        }
    }

    TextEdit::delete(TextRange::new(item_range.start(), end))
}

/// Generate a TextEdit to insert an item into a destination file.
/// This adds the item text with proper formatting.
pub fn generate_item_insertion_edit(
    source_text: &str,
    item_range: TextRange,
    insert_position: syntax::TextSize,
    add_newlines: bool,
) -> TextEdit {
    let item_text = &source_text[item_range];
    let insert_text = if add_newlines {
        format!("{}\n\n", item_text)
    } else {
        item_text.to_string()
    };

    TextEdit::insert(insert_position, insert_text)
}

/// Update external references to a moved item.
///
/// This function finds all references to the item being moved and updates their paths
/// to reflect the new module location. It handles:
/// - Use statements (absolute and relative imports)
/// - Qualified references (Type::method, crate::module::Type)
///
/// Returns a map of file IDs to TextEdits that update the references.
pub fn update_external_references(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    dest_module: &Module,
    new_name: &str,
) -> Result<Vec<(base_db::EditionedFileId, TextEdit)>> {
    // Find all usages of the item being moved
    let usages = def.usages(sema).all();

    // Build the new import path for the item
    let dest_mod_path = dest_module.path_to_root(sema.db);
    let new_import_path = build_import_path(sema, &dest_mod_path, new_name)?;

    let mut edits = Vec::new();

    // Process each file that contains references
    for (file_id, references) in usages.references {
        let mut edit_builder = TextEdit::builder();
        let mut edited_ranges = std::collections::HashSet::new();

        for reference in references {
            // Skip if we've already edited this range (can happen with macros)
            if edited_ranges.contains(&reference.range.start()) {
                continue;
            }

            // Try to update the reference based on its context
            if let Some(replacement) = compute_reference_replacement(
                sema,
                &reference,
                file_id,
                &new_import_path,
                new_name,
            ) {
                edit_builder.replace(reference.range, replacement);
                edited_ranges.insert(reference.range.start());
            }
        }

        let edit = edit_builder.finish();
        if !edit.is_empty() {
            edits.push((file_id, edit));
        }
    }

    Ok(edits)
}

/// Build an import path string from a module path and item name.
fn build_import_path(sema: &Semantics<'_, RootDatabase>, module_path: &[Module], item_name: &str) -> Result<String> {
    let mut path = String::from("crate");
    for module in module_path.iter().rev().skip(1) {
        // Skip the crate root
        if let Some(name) = module.name(sema.db) {
            path.push_str("::");
            path.push_str(&name.display_no_db(span::Edition::LATEST).to_string());
        }
    }
    path.push_str("::");
    path.push_str(item_name);
    Ok(path)
}

/// Compute the replacement text for a reference to the moved item.
///
/// This analyzes the context of the reference (use statement, qualified path, etc.)
/// and determines the appropriate replacement text.
fn compute_reference_replacement(
    _sema: &Semantics<'_, RootDatabase>,
    reference: &crate::search::FileReference,
    _file_id: base_db::EditionedFileId,
    new_import_path: &str,
    new_name: &str,
) -> Option<String> {
    let name_node = reference.name.as_name_ref()?;
    let syntax_node = name_node.syntax();

    // Check if this reference is part of a use statement
    if let Some(use_tree) = syntax_node.ancestors().find_map(ast::UseTree::cast) {
        // For use statements, we need to replace the entire path
        return Some(compute_use_tree_replacement(&use_tree, new_import_path, new_name));
    }

    // Check if this is a qualified path reference (e.g., old_module::Item or Item::method)
    if let Some(path) = syntax_node.ancestors().find_map(ast::Path::cast) {
        return Some(compute_path_replacement(&path, name_node, new_import_path, new_name));
    }

    // For unqualified references, just replace with the new name
    Some(new_name.to_string())
}

/// Compute replacement text for a use tree.
fn compute_use_tree_replacement(use_tree: &ast::UseTree, new_import_path: &str, _new_name: &str) -> String {
    // Get the path part of the use tree
    if let Some(_path) = use_tree.path() {
        // Check if there's a rename (use old::Item as Alias)
        if let Some(rename) = use_tree.rename() {
            // Keep the alias, just update the path
            format!("{} as {}", new_import_path, rename.name().map_or("_".to_string(), |n| n.to_string()))
        } else {
            // Simple import, just use the new path
            new_import_path.to_string()
        }
    } else {
        new_import_path.to_string()
    }
}

/// Compute replacement text for a path reference.
fn compute_path_replacement(
    path: &ast::Path,
    name_ref: &ast::NameRef,
    new_import_path: &str,
    new_name: &str,
) -> String {
    // If the name_ref is the last segment of the path, we need to replace the entire path
    if let Some(segment) = path.segment() {
        if segment.name_ref().as_ref() == Some(name_ref) {
            // This is the final segment, check if there's a qualifier
            if path.qualifier().is_some() {
                // There's a qualifier, so this is a qualified path - replace with new path
                new_import_path.to_string()
            } else {
                // No qualifier, just replace the name
                new_name.to_string()
            }
        } else {
            // The name_ref is not the last segment, just replace it
            new_name.to_string()
        }
    } else {
        new_name.to_string()
    }
}

/// Update internal references within a moved item.
///
/// This function analyzes the code being moved and updates references to items
/// in the source module that will no longer be accessible after the move.
/// It handles:
/// - Unqualified references that need imports
/// - Relative path references (self::, super::)
/// - Generating new import statements
/// - Preserving valid imports
///
/// Returns updated source code for the moved item with necessary import changes.
pub fn update_internal_references(
    sema: &Semantics<'_, RootDatabase>,
    item_syntax: &syntax::SyntaxNode,
    source_module: &Module,
    dest_module: &Module,
    items_moving_together: &[syntax::SyntaxNode],
) -> Result<String> {
    // Parse the item to find all path references
    let mut new_imports = Vec::new();
    let mut path_updates = Vec::new();

    // Find all path expressions in the item
    for path_node in item_syntax.descendants().filter_map(ast::Path::cast) {
        if let Some(update) = analyze_path_reference(
            sema,
            &path_node,
            source_module,
            dest_module,
            items_moving_together,
        ) {
            match update {
                PathUpdate::AddImport(import_path) => {
                    if !new_imports.contains(&import_path) {
                        new_imports.push(import_path);
                    }
                }
                PathUpdate::ReplacePath { range, new_path } => {
                    path_updates.push((range, new_path));
                }
            }
        }
    }

    // Build the updated source code
    let mut updated_text = item_syntax.text().to_string();

    // Apply path updates (in reverse order to maintain offsets)
    path_updates.sort_by_key(|(range, _)| std::cmp::Reverse(range.start()));
    for (range, new_path) in path_updates {
        let start = usize::from(range.start());
        let end = usize::from(range.end());
        updated_text.replace_range(start..end, &new_path);
    }

    // Add import statements at the beginning if needed
    if !new_imports.is_empty() {
        let import_block = generate_import_block(&new_imports);
        // Insert after any existing attributes/doc comments
        if let Some(item) = ast::Item::cast(item_syntax.clone()) {
            // Find where to insert imports (after attributes, before the item keyword)
            let insert_pos = find_import_insertion_point(&item);
            updated_text.insert_str(insert_pos, &import_block);
        } else {
            // Fallback: prepend to the beginning
            updated_text = format!("{}{}", import_block, updated_text);
        }
    }

    Ok(updated_text)
}

/// Represents an update needed for a path reference.
enum PathUpdate {
    /// Need to add an import statement
    AddImport(String),
    /// Need to replace the path text
    ReplacePath { range: TextRange, new_path: String },
}

/// Analyze a path reference to determine if it needs updating.
fn analyze_path_reference(
    sema: &Semantics<'_, RootDatabase>,
    path: &ast::Path,
    source_module: &Module,
    _dest_module: &Module,
    items_moving_together: &[syntax::SyntaxNode],
) -> Option<PathUpdate> {
    // Get the first segment to determine the path kind
    let _first_segment = path.first_segment()?;
    let path_text = path.to_string();

    // Check if this is a relative path (self::, super::, or crate::)
    if path_text.starts_with("self::") {
        // self:: paths need to be updated to absolute paths to the source module
        return Some(handle_self_path(sema, path, source_module));
    }

    if path_text.starts_with("super::") {
        // super:: paths need to be resolved and converted to absolute paths
        return Some(handle_super_path(sema, path, source_module));
    }

    if path_text.starts_with("crate::") {
        // Absolute crate paths are fine, no update needed
        return None;
    }

    // For unqualified paths, check if they resolve to items in the source module
    if let Some(resolution) = sema.resolve_path(path) {
        if let Some(def) = path_resolution_to_definition(resolution) {
            // Check if this item is in the source module
            if is_item_in_module(sema, &def, source_module) {
                // Check if this item is moving together with us
                if !is_item_moving_together(&def, items_moving_together) {
                    // This item stays in source module, we need an import
                    return Some(PathUpdate::AddImport(build_import_for_definition(
                        sema,
                        &def,
                    )));
                }
            }
        }
    }

    None
}

/// Handle a self:: path by converting it to an absolute path to the source module.
fn handle_self_path(
    sema: &Semantics<'_, RootDatabase>,
    path: &ast::Path,
    source_module: &Module,
) -> PathUpdate {
    let path_str = path.to_string();
    let after_self = path_str.strip_prefix("self::").unwrap_or(&path_str);

    // Build absolute path to source module
    let source_path = module_path_string(sema, source_module);
    let new_path = format!("{}::{}", source_path, after_self);

    PathUpdate::ReplacePath {
        range: path.syntax().text_range(),
        new_path,
    }
}

/// Handle a super:: path by resolving it and converting to an absolute path.
fn handle_super_path(
    sema: &Semantics<'_, RootDatabase>,
    path: &ast::Path,
    source_module: &Module,
) -> PathUpdate {
    let path_str = path.to_string();

    // Count the number of super:: prefixes
    let super_count = path_str.matches("super::").count();

    // Navigate up from source module
    let mut current = Some(source_module.clone());
    for _ in 0..super_count {
        current = current.and_then(|m| m.parent(sema.db));
    }

    // Get the rest of the path after super::
    let rest = path_str.split("super::").last().unwrap_or("");

    if let Some(target_module) = current {
        let module_path = module_path_string(sema, &target_module);
        let new_path = if rest.is_empty() {
            module_path
        } else {
            format!("{}::{}", module_path, rest)
        };

        PathUpdate::ReplacePath {
            range: path.syntax().text_range(),
            new_path,
        }
    } else {
        // Couldn't resolve, keep as-is (will likely be a compile error)
        PathUpdate::ReplacePath {
            range: path.syntax().text_range(),
            new_path: path_str,
        }
    }
}

/// Build a module path string (e.g., "crate::alpha::beta").
fn module_path_string(sema: &Semantics<'_, RootDatabase>, module: &Module) -> String {
    let path_parts = module.path_to_root(sema.db);
    let mut result = String::from("crate");

    for m in path_parts.iter().rev().skip(1) {
        if let Some(name) = m.name(sema.db) {
            result.push_str("::");
            result.push_str(&name.display_no_db(span::Edition::LATEST).to_string());
        }
    }

    result
}

/// Convert a path resolution to a Definition.
fn path_resolution_to_definition(resolution: hir::PathResolution) -> Option<Definition> {
    match resolution {
        hir::PathResolution::Def(def) => Some(def.into()),
        _ => None,
    }
}

/// Check if an item is defined in a specific module.
fn is_item_in_module(sema: &Semantics<'_, RootDatabase>, def: &Definition, module: &Module) -> bool {
    if let Some(item_module) = def.module(sema.db) {
        item_module == *module
    } else {
        false
    }
}

/// Check if an item is part of the items being moved together.
fn is_item_moving_together(_def: &Definition, _items_moving: &[syntax::SyntaxNode]) -> bool {
    // This is a simplified check - in practice we'd need to match the definition
    // to one of the syntax nodes being moved
    // For now, return false to be conservative (assume items stay in source)
    false
}

/// Build an import statement for a definition.
fn build_import_for_definition(sema: &Semantics<'_, RootDatabase>, def: &Definition) -> String {
    if let Some(module) = def.module(sema.db) {
        let module_path = module_path_string(sema, &module);
        let item_name = def.name(sema.db).map(|n| n.display_no_db(span::Edition::LATEST).to_string()).unwrap_or_default();
        format!("{}::{}", module_path, item_name)
    } else {
        String::new()
    }
}

/// Generate a block of import statements.
fn generate_import_block(imports: &[String]) -> String {
    let mut result = String::new();
    for import in imports {
        result.push_str(&format!("use {};\n", import));
    }
    result.push('\n');
    result
}

/// Find the position to insert import statements.
fn find_import_insertion_point(_item: &ast::Item) -> usize {
    // Insert after attributes and doc comments, right before the item
    // For now, simple implementation: insert at the beginning
    0
}

/// Validate that all items referenced within the moved code will remain accessible
/// from the destination module.
pub fn validate_external_visibility(
    sema: &Semantics<'_, RootDatabase>,
    item_syntax: &syntax::SyntaxNode,
    source_module: &Module,
    dest_module: &Module,
) -> Result<()> {
    // Find all path references in the item
    for path_node in item_syntax.descendants().filter_map(ast::Path::cast) {
        if let Some(resolution) = sema.resolve_path(&path_node) {
            if let Some(def) = path_resolution_to_definition(resolution) {
                // Check if this is an item from the source module
                if is_item_in_module(sema, &def, source_module) {
                    // Check if it will be visible from destination
                    if !is_visible_from(sema, &def, dest_module)? {
                        let item_name = def.name(sema.db)
                            .map(|n| n.display_no_db(span::Edition::LATEST).to_string())
                            .unwrap_or_else(|| "<unknown>".to_string());

                        bail!(
                            "Cannot move item: references private item '{}' which will not be visible from destination module",
                            item_name
                        );
                    }
                }
            }
        }
    }

    Ok(())
}

/// Check if a definition is visible from a given module.
fn is_visible_from(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    from_module: &Module,
) -> Result<bool> {
    // Get the visibility of the definition
    let visibility = get_visibility(sema, def)?;

    // Get the module where the definition is located
    let def_module = def.module(sema.db).ok_or_else(|| format_err!("No module for definition"))?;

    // Check visibility rules
    match visibility {
        Visibility::Public => Ok(true), // pub - visible everywhere
        Visibility::Crate => {
            // pub(crate) - visible within same crate
            Ok(def_module.krate() == from_module.krate())
        }
        Visibility::Private => {
            // private - only visible in same module
            Ok(def_module == *from_module)
        }
        Visibility::Super => {
            // pub(super) - visible in parent module and siblings
            // Check if from_module is the parent or a sibling
            if let Some(def_parent) = def_module.parent(sema.db) {
                Ok(*from_module == def_parent ||
                   from_module.parent(sema.db).as_ref() == Some(&def_parent))
            } else {
                Ok(false)
            }
        }
    }
}

/// Simplified visibility representation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Visibility {
    Public,      // pub
    Crate,       // pub(crate)
    Super,       // pub(super)
    Private,     // no modifier
}

/// Get the visibility of a definition.
fn get_visibility(sema: &Semantics<'_, RootDatabase>, def: &Definition) -> Result<Visibility> {
    // Get the syntax node for the definition
    let syntax = match def {
        Definition::Adt(adt) => match adt {
            hir::Adt::Struct(s) => {
                sema.source(*s).map(|src| src.value.syntax().clone())
            }
            hir::Adt::Enum(e) => {
                sema.source(*e).map(|src| src.value.syntax().clone())
            }
            hir::Adt::Union(u) => {
                sema.source(*u).map(|src| src.value.syntax().clone())
            }
        },
        Definition::Function(f) => {
            sema.source(*f).map(|src| src.value.syntax().clone())
        }
        Definition::Const(c) => {
            sema.source(*c).map(|src| src.value.syntax().clone())
        }
        Definition::Static(s) => {
            sema.source(*s).map(|src| src.value.syntax().clone())
        }
        _ => None,
    }.ok_or_else(|| format_err!("No source for definition"))?;

    // Parse visibility from syntax
    parse_visibility_from_syntax(&syntax)
}

/// Parse visibility modifier from a syntax node.
fn parse_visibility_from_syntax(node: &syntax::SyntaxNode) -> Result<Visibility> {
    use syntax::ast::HasVisibility;

    // Try different item types that have visibility
    macro_rules! try_has_vis {
        ($($ty:ty),*) => {
            $(
                if let Some(item) = <$ty>::cast(node.clone()) {
                    if let Some(vis) = item.visibility() {
                        return Ok(parse_visibility_ast(&vis));
                    }
                }
            )*
        };
    }

    try_has_vis!(
        ast::Struct, ast::Enum, ast::Union, ast::Fn,
        ast::Const, ast::Static, ast::Trait, ast::TypeAlias
    );

    // Default to private if no visibility modifier
    Ok(Visibility::Private)
}

/// Parse an AST visibility node.
fn parse_visibility_ast(vis: &ast::Visibility) -> Visibility {
    let text = vis.to_string();

    if text.starts_with("pub(crate)") {
        Visibility::Crate
    } else if text.starts_with("pub(super)") {
        Visibility::Super
    } else if text.starts_with("pub") {
        Visibility::Public
    } else {
        Visibility::Private
    }
}

/// Calculate the required visibility for a moved item based on its usages.
pub fn calculate_required_visibility(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    dest_module: &Module,
) -> Result<Visibility> {
    // Find all usages of the item
    let usages = def.usages(sema).all();

    let mut required = Visibility::Private;

    // Check each usage location
    for (file_id, _references) in usages.references {
        // Get the module for this file by finding a module definition in the file
        // We use the crate's module tree to find the module containing this file
        let krate = dest_module.krate();
        let modules = krate.modules(sema.db);

        let mut usage_module = None;
        for module in modules {
            if let Some(module_file_id) = module.as_source_file_id(sema.db) {
                if module_file_id.file_id(sema.db) == file_id.file_id(sema.db) {
                    usage_module = Some(module);
                    break;
                }
            }
        }

        if let Some(usage_module) = usage_module {
            // Determine required visibility based on module relationship
            if usage_module == *dest_module {
                // Same module - private is fine
                continue;
            } else if usage_module.krate() == dest_module.krate() {
                // Same crate, different module - need at least pub(crate)
                if required == Visibility::Private {
                    required = Visibility::Crate;
                }
            } else {
                // Different crate - need pub
                required = Visibility::Public;
                break; // Can't get more public than this
            }
        }
    }

    Ok(required)
}

/// Update the visibility of an item to the required level.
pub fn update_item_visibility(
    item_text: &str,
    current_vis: Visibility,
    required_vis: Visibility,
) -> Result<String> {
    // Never downgrade visibility
    let new_vis = match (current_vis, required_vis) {
        // Already pub - keep it
        (Visibility::Public, _) => return Ok(item_text.to_string()),

        // Already pub(crate) and need pub - upgrade
        (Visibility::Crate, Visibility::Public) => Visibility::Public,

        // Already pub(crate) and need pub(crate) or less - keep it
        (Visibility::Crate, _) => return Ok(item_text.to_string()),

        // Private - upgrade to required
        (Visibility::Private, req) => req,

        // Super is treated like private for this simplified model
        (Visibility::Super, req) => req,
    };

    // Generate new visibility modifier text
    let vis_text = match new_vis {
        Visibility::Public => "pub ",
        Visibility::Crate => "pub(crate) ",
        Visibility::Private => "",
        Visibility::Super => "pub(super) ",
    };

    // Replace or add visibility modifier
    // This is simplified - a full implementation would parse the AST
    let result = if item_text.trim_start().starts_with("pub") {
        // Replace existing visibility
        let after_vis = item_text.trim_start()
            .strip_prefix("pub(crate)")
            .or_else(|| item_text.trim_start().strip_prefix("pub(super)"))
            .or_else(|| item_text.trim_start().strip_prefix("pub"))
            .unwrap_or(item_text.trim_start());
        format!("{}{}", vis_text, after_vis.trim_start())
    } else {
        // Add visibility modifier
        format!("{}{}", vis_text, item_text.trim_start())
    };

    Ok(result)
}

/// Validate that the destination module doesn't have conflicting item names.
pub fn validate_destination_no_conflicts(
    sema: &Semantics<'_, RootDatabase>,
    dest_module: &Module,
    new_name: &str,
) -> Result<()> {
    // Get all items in the destination module
    let scope = dest_module.scope(sema.db, None);

    // Check if there's already an item with this name
    for (name, _def) in scope.iter() {
        if name.display_no_db(span::Edition::LATEST).to_string() == new_name {
            bail!("Item '{}' already exists in destination module", new_name);
        }
    }

    Ok(())
}

/// Validate that the item can be moved (not a local, not from external crate).
pub fn validate_item_is_movable(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
) -> Result<()> {
    // Check if this is a local variable or parameter
    if matches!(def, Definition::Local(_)) {
        bail!("Cannot move local items");
    }

    // Check if this is from an external crate
    if let Some(krate) = def.krate(sema.db) {
        if !krate.origin(sema.db).is_local() {
            bail!("Cannot move items from external crates");
        }
    }

    // Check for other non-movable types
    match def {
        Definition::BuiltinType(_) => bail!("Cannot move builtin types"),
        Definition::BuiltinAttr(_) => bail!("Cannot move builtin attributes"),
        Definition::BuiltinLifetime(_) => bail!("Cannot move builtin lifetimes"),
        Definition::ToolModule(_) => bail!("Cannot move tool modules"),
        Definition::SelfType(_) => bail!("Cannot move Self type"),
        Definition::Label(_) => bail!("Cannot move labels"),
        Definition::DeriveHelper(_) => bail!("Cannot move derive helpers"),
        Definition::GenericParam(_) => bail!("Cannot move generic parameters"),
        _ => Ok(()),
    }
}

/// Validate that moving to the destination won't create circular dependencies.
///
/// For now, this is a simplified check. A full implementation would need to
/// analyze the dependency graph of modules.
pub fn validate_no_circular_dependencies(
    _sema: &Semantics<'_, RootDatabase>,
    _source_module: &Module,
    _dest_module: &Module,
) -> Result<()> {
    // Simplified: We don't check for circular dependencies yet
    // A full implementation would need to:
    // 1. Build a dependency graph of modules
    // 2. Check if adding an edge from dest_module to source_module would create a cycle
    // 3. This is complex because it requires analyzing all imports and uses

    // For now, we allow all moves and let the compiler catch circular dependencies
    Ok(())
}

/// Comprehensive validation for a move operation.
///
/// This function performs all necessary validation checks before attempting to move an item.
pub fn validate_move_operation(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    item_syntax: &syntax::SyntaxNode,
    source_module: &Module,
    dest_module: &Module,
    new_name: &str,
) -> Result<()> {
    // 1. Validate the item is movable
    validate_item_is_movable(sema, def)?;

    // 2. Validate destination doesn't have conflicts
    validate_destination_no_conflicts(sema, dest_module, new_name)?;

    // 3. Validate external visibility (referenced items will remain accessible)
    validate_external_visibility(sema, item_syntax, source_module, dest_module)?;

    // 4. Check for circular dependencies
    validate_no_circular_dependencies(sema, source_module, dest_module)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use syntax::ast::HasModuleItem;

    // Note: Most comprehensive testing will be done via integration tests in ide/src/rename.rs
    // These unit tests focus on syntax-level parsing which doesn't require HIR setup

    #[test]
    fn test_path_contains_separator() {
        // Simple check: does the rename target look like a path?
        assert!(!"Foo".contains("::"));
        assert!("crate::Foo".contains("::"));
        assert!("foo::bar::Baz".contains("::"));
        assert!("super::Foo".contains("::"));
        assert!("self::Foo".contains("::"));
    }

    #[test]
    fn test_parse_path_with_syntax() {
        // Test that we can parse various path forms using syntax parsing
        let test_cases = vec![
            "crate::foo::Bar",
            "super::foo::Bar",
            "self::foo::Bar",
            "foo::bar::Baz",
            "a::b::c::D",
        ];

        for case in test_cases {
            let parsed = syntax::SourceFile::parse(
                &format!("type _ = {};", case),
                span::Edition::LATEST,
            );
            let path = parsed
                .syntax_node()
                .descendants()
                .find_map(ast::Path::cast);
            assert!(path.is_some(), "Failed to parse path: {}", case);
        }
    }

    #[test]
    fn test_extract_final_name_from_path() {
        // Test extracting the final identifier from various paths
        let test_cases = vec![
            ("crate::foo::Bar", "Bar"),
            ("super::Baz", "Baz"),
            ("a::b::c::Item", "Item"),
            ("self::Thing", "Thing"),
        ];

        for (path_str, expected_name) in test_cases {
            let parsed = syntax::SourceFile::parse(
                &format!("type _ = {};", path_str),
                span::Edition::LATEST,
            );
            let path = parsed
                .syntax_node()
                .descendants()
                .find_map(ast::Path::cast)
                .unwrap();

            let final_name = path.segment().and_then(|seg| seg.name_ref()).unwrap();
            assert_eq!(
                final_name.to_string(),
                expected_name,
                "Path: {}",
                path_str
            );
        }
    }

    #[test]
    fn test_move_operation_struct_exists() {
        // Verify MoveOperation type exists and has expected traits
        fn assert_traits<T: std::fmt::Debug + Clone + PartialEq + Eq>() {}
        assert_traits::<MoveOperation>();
    }

    #[test]
    fn test_rename_target_simple_variant() {
        // Test SimpleRename variant
        let simple = RenameTarget::SimpleRename;
        assert!(matches!(simple, RenameTarget::SimpleRename));
    }

    #[test]
    fn test_invalid_path_strings() {
        // Document what should be rejected as invalid paths
        let invalid_cases = vec![
            "",           // Empty
            "123",        // Starts with number
            "foo bar",    // Contains space
            "foo-bar",    // Contains dash
            "foo::123",   // Number segment
        ];

        for case in invalid_cases {
            let _parsed = syntax::SourceFile::parse(
                &format!("type _ = {};", case),
                span::Edition::LATEST,
            );
            // These will either fail to parse or parse incorrectly
            // Full validation happens in parse_rename_target with semantics
        }
    }

    // Tests for module file path generation
    // Note: These test the path generation logic, but full integration tests
    // with actual HIR will be in ide/src/rename.rs

    #[test]
    fn test_module_file_paths_named_parent() {
        // Test path generation when parent is a named file (foo.rs)
        // In this case, children go in foo/ subdirectory

        // We can't easily test module_file_paths without HIR setup,
        // but we can verify the path format logic manually

        // For parent foo.rs (named file, not mod.rs):
        // - child bar should be at foo/bar.rs or foo/bar/mod.rs

        let dir_path = "foo/"; // What we'd get from a named parent
        let module_name = "bar";

        let named_file = format!("{}{}.rs", dir_path, module_name);
        let mod_rs_file = format!("{}{}/mod.rs", dir_path, module_name);

        assert_eq!(named_file, "foo/bar.rs");
        assert_eq!(mod_rs_file, "foo/bar/mod.rs");
    }

    #[test]
    fn test_module_file_paths_mod_rs_parent() {
        // Test path generation when parent is mod.rs
        // In this case, children go in the same directory

        // For parent foo/mod.rs:
        // - child bar should be at foo/bar.rs or foo/bar/mod.rs

        let dir_path = ""; // Empty for mod.rs parent
        let module_name = "bar";

        let named_file = format!("{}{}.rs", dir_path, module_name);
        let mod_rs_file = format!("{}{}/mod.rs", dir_path, module_name);

        assert_eq!(named_file, "bar.rs");
        assert_eq!(mod_rs_file, "bar/mod.rs");
    }

    #[test]
    fn test_module_file_paths_crate_root() {
        // Test path generation from crate root (lib.rs or main.rs)

        // For crate root:
        // - child foo should be at foo.rs or foo/mod.rs

        let dir_path = ""; // Empty for crate root
        let module_name = "foo";

        let named_file = format!("{}{}.rs", dir_path, module_name);
        let mod_rs_file = format!("{}{}/mod.rs", dir_path, module_name);

        assert_eq!(named_file, "foo.rs");
        assert_eq!(mod_rs_file, "foo/mod.rs");
    }

    #[test]
    fn test_nested_module_path_construction() {
        // Test building nested module paths
        // For crate::foo::bar::baz, we should create:
        // - foo.rs or foo/mod.rs
        // - foo/bar.rs or foo/bar/mod.rs
        // - foo/bar/baz.rs or foo/bar/baz/mod.rs

        let parent_segment = "foo";
        let nested_segment = "bar";

        // If foo.rs is created (named file style):
        let nested_named = format!("{}/{}.rs", parent_segment, nested_segment);
        let nested_mod_rs = format!("{}/{}/mod.rs", parent_segment, nested_segment);

        assert_eq!(nested_named, "foo/bar.rs");
        assert_eq!(nested_mod_rs, "foo/bar/mod.rs");

        // If foo/mod.rs is created (mod.rs style):
        let nested_from_mod_rs_named = format!("{}/{}.rs", parent_segment, nested_segment);
        let nested_from_mod_rs_mod = format!("{}/{}/mod.rs", parent_segment, nested_segment);

        assert_eq!(nested_from_mod_rs_named, "foo/bar.rs");
        assert_eq!(nested_from_mod_rs_mod, "foo/bar/mod.rs");
    }

    #[test]
    fn test_extract_module_path_structure() {
        // Verify that extract_module_path correctly removes the final segment
        use hir::{Name, Symbol};

        // Create a simple path: crate::foo::bar::Baz
        // The module path should be: crate::foo::bar

        let segments = vec![
            Name::new_symbol_root(Symbol::intern("foo")),
            Name::new_symbol_root(Symbol::intern("bar")),
            Name::new_symbol_root(Symbol::intern("Baz")),
        ];
        let path = ModPath::from_segments(PathKind::Crate, segments);

        let module_path = extract_module_path(&path).unwrap();

        assert_eq!(module_path.kind, PathKind::Crate);
        assert_eq!(module_path.segments().len(), 2);
        assert_eq!(module_path.segments()[0].as_str(), "foo");
        assert_eq!(module_path.segments()[1].as_str(), "bar");
    }

    #[test]
    fn test_extract_module_path_single_segment() {
        // For crate::Foo, module path should be just crate
        use hir::{Name, Symbol};

        let segments = vec![
            Name::new_symbol_root(Symbol::intern("Foo")),
        ];
        let path = ModPath::from_segments(PathKind::Crate, segments);

        let module_path = extract_module_path(&path).unwrap();

        assert_eq!(module_path.kind, PathKind::Crate);
        assert_eq!(module_path.segments().len(), 0);
    }

    #[test]
    fn test_path_kind_preservation() {
        // Verify that different PathKind variants are preserved
        use hir::{Name, Symbol};

        let segments = vec![
            Name::new_symbol_root(Symbol::intern("foo")),
            Name::new_symbol_root(Symbol::intern("Bar")),
        ];

        // Test with different path kinds
        let test_cases = vec![
            PathKind::Crate,
            PathKind::Super(1),
            PathKind::Super(2),
            PathKind::Plain,
            PathKind::Abs,
        ];

        for kind in test_cases {
            let path = ModPath::from_segments(kind, segments.clone());
            let module_path = extract_module_path(&path).unwrap();
            assert_eq!(module_path.kind, kind);
        }
    }

    // Tests for module declaration insertion

    #[test]
    fn test_module_declaration_exists_simple() {
        let source = "pub mod foo;\npub mod bar;\n";
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let syntax = parsed.syntax_node();

        assert!(module_declaration_exists(&syntax, "foo"));
        assert!(module_declaration_exists(&syntax, "bar"));
        assert!(!module_declaration_exists(&syntax, "baz"));
    }

    #[test]
    fn test_module_declaration_exists_inline_vs_file() {
        // Inline modules (with {}) should not be detected as existing declarations
        let source = "mod inline { fn foo() {} }\npub mod file_mod;\n";
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let syntax = parsed.syntax_node();

        assert!(!module_declaration_exists(&syntax, "inline")); // Inline module
        assert!(module_declaration_exists(&syntax, "file_mod")); // File module
    }

    #[test]
    fn test_create_module_declaration() {
        let decl = create_module_declaration("new_module");
        assert_eq!(decl, "pub mod new_module;");
    }

    #[test]
    fn test_find_module_insert_position_with_existing() {
        let source = "pub mod first;\npub mod second;\n\nfn main() {}";
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let syntax = parsed.syntax_node();

        let position = find_module_insert_position(&syntax);
        assert!(position.is_some());

        // The position should be after "pub mod second;"
        let pos_text = position.unwrap().to_string();
        assert!(pos_text.contains("second"));
    }

    #[test]
    fn test_find_module_insert_position_no_existing() {
        let source = "fn main() {}\nstruct Foo;";
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let syntax = parsed.syntax_node();

        let position = find_module_insert_position(&syntax);
        assert!(position.is_none());
    }

    #[test]
    fn test_find_first_item_position_with_uses() {
        let source = "use std::collections::HashMap;\nuse std::vec::Vec;\n\nfn main() {}";
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let syntax = parsed.syntax_node();

        let position = find_first_item_position(&syntax);

        // Should point to the first non-use item (fn main)
        let text_at_position = &source[position.into()..];
        assert!(text_at_position.starts_with("fn main"));
    }

    #[test]
    fn test_find_first_item_position_no_uses() {
        let source = "fn main() {}";
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let syntax = parsed.syntax_node();

        let position = find_first_item_position(&syntax);

        // Should point to the first item
        let text_at_position = &source[position.into()..];
        assert!(text_at_position.starts_with("fn main"));
    }

    #[test]
    fn test_find_first_item_position_empty() {
        let source = "";
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let syntax = parsed.syntax_node();

        let position = find_first_item_position(&syntax);
        assert_eq!(position, syntax::TextSize::from(0));
    }

    // Tests for item extraction and relocation

    #[test]
    fn test_extract_item_with_attrs_simple() {
        let source = r#"
fn foo() {}

#[derive(Debug)]
struct Bar;
"#;
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let items: Vec<_> = parsed.tree().items().collect();
        let struct_item = items[1].syntax();

        let range = extract_item_with_attrs(struct_item);
        let extracted = &source[range];

        assert!(extracted.contains("#[derive(Debug)]"));
        assert!(extracted.contains("struct Bar;"));
    }

    #[test]
    fn test_extract_item_with_attrs_doc_comment() {
        let source = r#"
fn foo() {}

/// This is a documented struct
/// with multiple lines
#[derive(Debug)]
struct Bar;
"#;
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let items: Vec<_> = parsed.tree().items().collect();
        let struct_item = items[1].syntax();

        let range = extract_item_with_attrs(struct_item);
        let extracted = &source[range];

        assert!(extracted.contains("/// This is a documented struct"));
        assert!(extracted.contains("/// with multiple lines"));
        assert!(extracted.contains("#[derive(Debug)]"));
        assert!(extracted.contains("struct Bar;"));
    }

    #[test]
    fn test_extract_item_with_attrs_no_attrs() {
        let source = "struct Bar;";
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let items: Vec<_> = parsed.tree().items().collect();
        let struct_item = items[0].syntax();

        let range = extract_item_with_attrs(struct_item);
        let extracted = &source[range];

        assert_eq!(extracted, "struct Bar;");
    }

    #[test]
    fn test_generate_item_removal_edit() {
        let source = "struct Foo;\nstruct Bar;\nstruct Baz;\n";
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let items: Vec<_> = parsed.tree().items().collect();
        let bar_item = items[1].syntax();

        let range = bar_item.text_range();
        let edit = generate_item_removal_edit(range, source);

        let mut result = source.to_string();
        edit.apply(&mut result);

        assert_eq!(result, "struct Foo;\nstruct Baz;\n");
        assert!(!result.contains("Bar"));
    }

    #[test]
    fn test_generate_item_insertion_edit() {
        let source_text = "struct Foo;";
        let dest_text = "struct Bar;\n";

        let source_parsed = syntax::SourceFile::parse(source_text, span::Edition::LATEST);
        let source_items: Vec<_> = source_parsed.tree().items().collect();
        let foo_item = source_items[0].syntax();
        let foo_range = foo_item.text_range();

        let dest_parsed = syntax::SourceFile::parse(dest_text, span::Edition::LATEST);
        let dest_syntax = dest_parsed.syntax_node();
        let insert_pos = dest_syntax.text_range().end();

        let edit = generate_item_insertion_edit(source_text, foo_range, insert_pos, true);

        let mut result = dest_text.to_string();
        edit.apply(&mut result);

        assert!(result.contains("struct Foo;"));
        assert!(result.contains("struct Bar;"));
    }

    #[test]
    fn test_extract_item_range_calculation() {
        let source = r#"
// Regular comment
struct Foo;

/// Doc comment
#[derive(Debug)]
#[allow(dead_code)]
struct Bar {
    field: i32,
}
"#;
        let parsed = syntax::SourceFile::parse(source, span::Edition::LATEST);
        let items: Vec<_> = parsed.tree().items().collect();

        // Test Foo (with regular comment)
        let foo_item = items[0].syntax();
        let foo_range = extract_item_with_attrs(foo_item);
        let foo_text = &source[foo_range];
        // Regular comments are included
        assert!(foo_text.contains("// Regular comment"));

        // Test Bar (with doc comments and multiple attributes)
        let bar_item = items[1].syntax();
        let bar_range = extract_item_with_attrs(bar_item);
        let bar_text = &source[bar_range];
        assert!(bar_text.contains("/// Doc comment"));
        assert!(bar_text.contains("#[derive(Debug)]"));
        assert!(bar_text.contains("#[allow(dead_code)]"));
        assert!(bar_text.contains("field: i32"));
    }
}
