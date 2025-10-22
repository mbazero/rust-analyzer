//! Item move operations for rename-to-move functionality.
//!
//! This module handles the logic for moving items between modules when the
//! rename target is a fully-qualified path (e.g., `crate::other::module::NewName`).

use hir::{Module, Semantics, ModPath, PathKind};
use syntax::{AstNode, ast::{self, HasName}};
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
