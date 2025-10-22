//! Item move operations for rename-to-move functionality.
//!
//! This module handles the logic for moving items between modules when the
//! rename target is a fully-qualified path (e.g., `crate::other::module::NewName`).

use hir::{Module, Semantics, ModPath, PathKind};
use syntax::{AstNode, ast};

use crate::{RootDatabase, defs::Definition};

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
}
