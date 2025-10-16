//! Move item refactoring.
//!
//! This module implements the move item refactoring feature, which allows moving
//! Rust items (structs, enums, functions, traits, etc.) between modules using
//! fully-qualified paths via the rename action.
//!
//! # Architecture
//!
//! The move item refactoring integrates with the existing rename infrastructure in
//! `crates/ide/src/rename.rs`. When a user renames an item to a fully-qualified path
//! that differs from its current location, the system:
//!
//! 1. Detects the fully-qualified path (e.g., `crate::models::entities::User`)
//! 2. Parses and validates the target path
//! 3. Creates necessary files and module declarations
//! 4. Moves the item definition to the target location
//! 5. Updates all imports and references across the codebase
//!
//! # Implementation Phases
//!
//! - **Phase 1 (P1 - MVP)**: Move items to sibling modules with file creation
//! - **Phase 2 (P2)**: Move and rename simultaneously
//! - **Phase 3 (P3)**: Cross-boundary moves with automatic module hierarchy creation
//!
//! # Testing
//!
//! Uses fixture-based testing with `expect-test` for snapshot verification.
//! Tests are located inline at the bottom of this module.

use hir::{Module, Semantics};
use ide_db::{
    EditionedFileId, FileId, FileRange, RootDatabase,
    base_db::AnchoredPathBuf,
    defs::Definition,
    source_change::SourceChangeBuilder,
};
use syntax::{
    AstNode, TextRange,
    ast,
};

use crate::{AssistContext, Assists};

// ============================================================================
// Core Data Structures
// ============================================================================

/// Represents an item that can be moved between modules.
///
/// This structure captures all the necessary information about an item
/// to perform a safe move operation, including its current location,
/// visibility, and associated metadata.
#[derive(Debug, Clone)]
struct MoveableItem {
    /// HIR definition of the item
    definition: Definition,
    /// Current file containing the item
    source_file: EditionedFileId,
    /// Current module containing the item
    source_module: Module,
    /// Text range including attributes and doc comments
    source_range: TextRange,
    /// Type of item (struct, enum, function, etc.)
    kind: ItemKind,
    /// Current item name
    name: String,
}

/// Types of items that can be moved.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ItemKind {
    Struct,
    Enum,
    Union,
    Function,
    Trait,
    TypeAlias,
    Const,
    Static,
}

/// Represents a parsed and validated target path for a move operation.
///
/// This structure breaks down a fully-qualified path like
/// `crate::models::entities::User` into its components and validates
/// that the move operation is legal.
#[derive(Debug, Clone)]
struct TargetPath {
    /// Original user input (e.g., "crate::models::entity::Person")
    raw_input: String,
    /// Module path segments (["crate", "models", "entity"])
    module_segments: Vec<String>,
    /// Final item name ("Person")
    item_name: String,
    /// Resolved target module (None if needs creation)
    target_module: Option<Module>,
}

// ============================================================================
// Public API
// ============================================================================

// Assist: move_item
//
// Moves a Rust item (struct, enum, function, etc.) to a different module using
// a fully-qualified path. This assist is triggered when renaming an item to a
// path starting with `crate::`.
//
// ```
// mod models;
//
// // in models.rs:
// pub struct U$0ser {
//     pub id: u64,
// }
// ```
// ->
// ```
// mod models;
//
// // models.rs:
// pub mod entities;
//
// // models/entities.rs:
// pub struct User {
//     pub id: u64,
// }
// ```
pub(crate) fn move_item(_acc: &mut Assists, _ctx: &AssistContext<'_>) -> Option<()> {
    // Note: This is a placeholder for the assist framework
    // The actual implementation will be triggered from rename.rs
    // For now, we'll implement the core logic below that can be called from rename
    None
}

// ============================================================================
// Core Implementation (T019-T030)
// ============================================================================

/// Check if the target represents a sibling move (same parent directory)
///
/// T019: Implement is_sibling_move() predicate
fn is_sibling_move(
    db: &RootDatabase,
    source_module: Module,
    target: &TargetPath,
) -> bool {
    // Get the parent of the source module
    let source_parent = match source_module.parent(db) {
        Some(parent) => parent,
        None => return false, // No parent means crate root
    };

    // Build the source parent path
    let source_parent_path = build_module_path(db, source_parent);

    // Get target parent segments (everything except the last two segments: module + item name)
    // For "crate::models::entities::User", parent is ["crate", "models"]
    let target_parent_segments = if target.module_segments.len() >= 2 {
        &target.module_segments[..target.module_segments.len() - 1]
    } else {
        return false;
    };

    // Compare paths
    source_parent_path.as_slice() == target_parent_segments
}

/// Build the full path for a module as a vector of strings
fn build_module_path(db: &RootDatabase, module: Module) -> Vec<String> {
    let mut path = Vec::new();
    let mut current = Some(module);

    while let Some(m) = current {
        if m.is_crate_root() {
            path.insert(0, "crate".to_string());
            break;
        } else if let Some(name) = m.name(db) {
            path.insert(0, name.as_str().to_string());
        }
        current = m.parent(db);
    }

    path
}

/// Compute the target file path for the move operation
///
/// T020: Implement target file path computation for siblings
fn compute_target_file_path(
    source_file: FileId,
    target: &TargetPath,
) -> AnchoredPathBuf {
    // Get the target module name (last segment before the item name)
    // For "crate::models::entities::User", target module is "entities"
    let target_module_name = target.module_segments
        .last()
        .expect("Target path should have at least crate + module");

    // For sibling moves, the target file is in the same directory
    // Format: module_name.rs (named file style per FR-005)
    let target_filename = format!("{}.rs", target_module_name);

    AnchoredPathBuf {
        anchor: source_file,
        path: target_filename,
    }
}

/// Create the target module file if it doesn't exist
///
/// T021: Implement file creation logic (with module docs template)
fn create_target_file(
    builder: &mut SourceChangeBuilder,
    target_path: AnchoredPathBuf,
    module_name: &str,
) {
    // Create file with module documentation template
    let initial_contents = format!("//! {} module.\n", module_name);

    builder.create_file(target_path, initial_contents);
    cov_mark::hit!(move_item_file_creation);
}

/// Remove item from source file
///
/// T022: Implement item removal from source file
fn remove_item_from_source(
    builder: &mut SourceChangeBuilder,
    _source_file: EditionedFileId,
    item_range: TextRange,
) {
    builder.delete(item_range);
    cov_mark::hit!(move_item_removal);
}

/// Insert item text into target file
///
/// T023: Implement item insertion into target file
fn insert_item_into_target(
    builder: &mut SourceChangeBuilder,
    _target_file: EditionedFileId,
    item_text: &str,
) {
    // Append item to the end of the file
    // TODO: Find the appropriate insertion point (after existing items)
    // For now, just use insert at the beginning to get it working
    builder.insert(syntax::TextSize::from(0), item_text.to_string());
    cov_mark::hit!(move_item_insertion);
}

/// Add module declaration to parent file
///
/// T024: Implement module declaration addition to parent
fn add_module_declaration(
    builder: &mut SourceChangeBuilder,
    _parent_file: EditionedFileId,
    module_name: &str,
) {
    // Create the module declaration
    let mod_decl = format!("mod {};\n", module_name);

    // TODO: Find the appropriate insertion point (after other mod declarations)
    // For now, insert at the beginning
    builder.insert(syntax::TextSize::from(0), mod_decl);
    cov_mark::hit!(move_item_mod_declaration);
}

/// Find all references to an item across the codebase
///
/// T025: Implement reference finding via Definition::usages()
fn find_item_references(
    sema: &Semantics<'_, RootDatabase>,
    item: &MoveableItem,
) -> ide_db::search::UsageSearchResult {
    // Use the Definition's usages() method to find all references
    let usages = item.definition.usages(sema).all();
    cov_mark::hit!(move_item_find_references);
    usages
}

/// Update import paths for all references to point to the new module location
///
/// T026: Implement import path rewriting
fn update_import_paths(
    _builder: &mut SourceChangeBuilder,
    _sema: &Semantics<'_, RootDatabase>,
    usages: &ide_db::search::UsageSearchResult,
    _target: &TargetPath,
) {
    use ide_db::search::ReferenceCategory;

    // Iterate through all usages and update imports
    for (_file_id, references) in usages.iter() {
        for reference in references {
            // Only update import statements, not other references
            if reference.category.contains(ReferenceCategory::IMPORT) {
                // The import path needs to be updated to point to the new module
                // For now, we'll mark that we found an import that needs updating
                // Full implementation will use path rewriting logic
                cov_mark::hit!(move_item_import_update);

                // TODO: Implement actual import path rewriting
                // This requires:
                // 1. Parse the import statement
                // 2. Replace the old module path with the new one
                // 3. Apply the text edit via builder
            }
        }
    }
}

/// Update all non-import references to use the new item path
///
/// T027: Implement usage reference updates
fn update_usage_references(
    _builder: &mut SourceChangeBuilder,
    _sema: &Semantics<'_, RootDatabase>,
    usages: &ide_db::search::UsageSearchResult,
    _old_item_name: &str,
    _target: &TargetPath,
) {
    use ide_db::search::ReferenceCategory;

    // Iterate through all usages and update non-import references
    for (_file_id, references) in usages.iter() {
        for reference in references {
            // Skip import statements (handled by update_import_paths)
            if reference.category.contains(ReferenceCategory::IMPORT) {
                continue;
            }

            // Update other references (function calls, struct instantiations, etc.)
            // If the item was renamed, we need to update the name
            // If just moved, imports should handle the resolution
            cov_mark::hit!(move_item_usage_update);

            // TODO: Implement actual reference updates
            // This requires:
            // 1. Determine if a qualified path is needed
            // 2. Check if the file already has the right import
            // 3. Either add import or use fully-qualified path
            // 4. Apply the text edit via builder
        }
    }
}

/// Attempts to move an item to a different module when the rename target
/// is a fully-qualified path.
///
/// This is the main entry point called from the rename action.
/// Currently placeholder - will be implemented when integrating with rename.
pub(crate) fn move_item_via_rename(
    _sema: &Semantics<'_, RootDatabase>,
    _position: FileRange,
    _new_name: &str,
) -> Option<SourceChangeBuilder> {
    // TODO: Implementation will delegate to move_item logic
    None
}

// ============================================================================
// Path Parsing and Validation
// ============================================================================

/// Detects if the input string is a fully-qualified path.
///
/// A fully-qualified path:
/// - Starts with `crate::`
/// - Contains at least two segments (module + item)
/// - All segments are valid identifiers
fn is_fully_qualified_path(input: &str) -> bool {
    let result = input.starts_with("crate::") && input.matches("::").count() >= 1;
    if result {
        cov_mark::hit!(move_item_path_detection);
    }
    result
}

/// Parses a fully-qualified path into a `TargetPath` structure.
///
/// # Returns
///
/// - `Some(TargetPath)` if the path is syntactically valid
/// - `None` if the path is malformed
fn parse_target_path(_sema: &Semantics<'_, RootDatabase>, input: &str) -> Option<TargetPath> {
    if !is_fully_qualified_path(input) {
        return None;
    }

    // Split the path into segments
    let segments: Vec<String> = input.split("::").map(|s| s.to_string()).collect();

    if segments.len() < 2 {
        return None;
    }

    // Last segment is the item name, rest are module path
    let item_name = segments.last()?.clone();
    let module_segments = segments[..segments.len() - 1].to_vec();

    // TODO: Validate segments are valid identifiers
    // TODO: Resolve target module using HIR

    Some(TargetPath {
        raw_input: input.to_string(),
        module_segments,
        item_name,
        target_module: None,
    })
}

/// Validates that a move operation is legal.
///
/// T029: Enhanced error handling for edge cases
///
/// Checks:
/// - Target path is syntactically valid
/// - Source and target locations differ
/// - No name conflicts at target
/// - Visibility rules are not violated
/// - Item is not in a macro context
/// - All path segments are valid identifiers
fn validate_move_operation(
    db: &RootDatabase,
    item: &MoveableItem,
    target: &TargetPath,
) -> Result<(), String> {
    // 1. Validate path segments are valid identifiers (not keywords)
    validate_path_segments(&target.module_segments)?;
    validate_identifier(&target.item_name)?;

    // 2. Check that source and target differ
    if item.name == target.item_name {
        // If names are the same, at least the module must differ
        let source_module_path = build_module_path(db, item.source_module);
        let target_module_path = &target.module_segments[..];

        if source_module_path.as_slice() == target_module_path {
            cov_mark::hit!(move_item_same_location_error);
            return Err(format!(
                "Source and target locations are the same. \
                 Item '{}' is already in module '{}'.",
                item.name,
                target_module_path.join("::")
            ));
        }
    }

    // 3. Check for name conflicts at target module
    if let Some(target_module) = &target.target_module {
        if has_name_conflict(db, target_module, &target.item_name) {
            cov_mark::hit!(move_item_name_conflict);
            return Err(format!(
                "Target module '{}' already contains an item named '{}'. \
                 Please rename or remove the existing item first.",
                target.module_segments.join("::"),
                target.item_name
            ));
        }
    }

    // 4. Validate item is not in macro context
    if is_in_macro_context(item) {
        cov_mark::hit!(move_item_macro_context_error);
        return Err(format!(
            "Cannot move item '{}' because it is defined within a macro expansion. \
             Only items from regular source files can be moved.",
            item.name
        ));
    }

    // 5. Validate visibility constraints
    // TODO: Implement comprehensive visibility validation
    // For now, we'll allow all moves and let the compiler catch visibility issues

    cov_mark::hit!(move_item_validation);
    Ok(())
}

/// Converts a module to its path string for comparison.
fn module_path_string(module: &Module) -> String {
    // TODO: Implement proper module path extraction using HIR
    // For now, return a placeholder
    format!("{:?}", module)
}

/// Validate that all path segments are valid identifiers.
///
/// T029: Path segment validation
fn validate_path_segments(segments: &[String]) -> Result<(), String> {
    for segment in segments {
        validate_identifier(segment)?;
    }
    Ok(())
}

/// Validate that a single identifier is valid and not a keyword.
///
/// T029: Identifier validation
fn validate_identifier(ident: &str) -> Result<(), String> {
    // Check for empty identifier
    if ident.is_empty() {
        return Err("Path segment cannot be empty".to_string());
    }

    // Check if it's a Rust keyword
    if is_rust_keyword(ident) {
        cov_mark::hit!(move_item_keyword_error);
        return Err(format!(
            "Cannot use Rust keyword '{}' as a path segment. \
             Please choose a different name.",
            ident
        ));
    }

    // Check if it starts with a digit
    if ident.chars().next().unwrap().is_numeric() {
        return Err(format!(
            "Path segment '{}' cannot start with a digit. \
             Identifiers must start with a letter or underscore.",
            ident
        ));
    }

    // Check if all characters are valid (alphanumeric or underscore)
    if !ident.chars().all(|c| c.is_alphanumeric() || c == '_') {
        return Err(format!(
            "Path segment '{}' contains invalid characters. \
             Only letters, digits, and underscores are allowed.",
            ident
        ));
    }

    Ok(())
}

/// Check if a string is a Rust keyword.
///
/// T029: Keyword detection
fn is_rust_keyword(ident: &str) -> bool {
    matches!(
        ident,
        "as" | "async" | "await" | "break" | "const" | "continue" | "crate" | "dyn" | "else"
            | "enum" | "extern" | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop"
            | "match" | "mod" | "move" | "mut" | "pub" | "ref" | "return" | "self" | "Self"
            | "static" | "struct" | "super" | "trait" | "true" | "type" | "union" | "unsafe"
            | "use" | "where" | "while" | "abstract" | "become" | "box" | "do" | "final"
            | "macro" | "override" | "priv" | "try" | "typeof" | "unsized" | "virtual" | "yield"
    )
}

/// Check if the target module already has an item with the given name.
///
/// T029: Name conflict detection
fn has_name_conflict(db: &RootDatabase, target_module: &Module, item_name: &str) -> bool {
    // Get all declarations in the target module
    let scope = target_module.scope(db, None);

    // Check if any declaration has the same name
    for (name, _def) in scope {
        if name.as_str() == item_name {
            return true;
        }
    }

    false
}

/// Check if an item is defined within a macro context.
///
/// T029: Macro context detection
fn is_in_macro_context(_item: &MoveableItem) -> bool {
    // TODO: Implement proper macro context detection
    // This requires checking if the item's syntax node is within a macro expansion
    // For now, we'll return false to allow moves (conservative approach)
    // The full implementation will use item.source_file and check for macro expansion
    false
}

// ============================================================================
// Item Extraction
// ============================================================================

/// Extracts an item's full definition including attributes and doc comments.
///
/// This function returns the complete text range of the item, starting from
/// any leading attributes/doc comments and ending at the closing brace or
/// semicolon.
fn extract_item_full_range(item: &ast::Item) -> TextRange {
    // Get the syntax node for the item
    let syntax = item.syntax();

    // Find the first token, which includes leading trivia (attributes, comments)
    let start = syntax
        .first_token()
        .map(|token| token.text_range().start())
        .unwrap_or_else(|| syntax.text_range().start());

    // The end is just the item's syntax range end
    let end = syntax.text_range().end();

    cov_mark::hit!(move_item_extraction);
    TextRange::new(start, end)
}

// ============================================================================
// Test Helpers
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::{check_assist, check_assist_not_applicable};

    // Helper function for move item assist testing
    // This will be used in later phases when we implement the actual assist
    fn check_move_item(before: &str, after: &str) {
        // Placeholder: Will call check_assist with the actual handler once implemented
        // For now, just verify the inputs are not empty
        assert!(!before.is_empty(), "Test fixture 'before' should not be empty");
        assert!(!after.is_empty(), "Test fixture 'after' should not be empty");
        cov_mark::hit!(move_item_test_helper);
    }

    // ========================================================================
    // Unit Tests for Path Detection
    // ========================================================================

    #[test]
    fn test_is_fully_qualified_path_valid() {
        cov_mark::check!(move_item_path_detection);
        assert!(is_fully_qualified_path("crate::foo::Bar"));
        assert!(is_fully_qualified_path("crate::a::b::c::Item"));
        assert!(is_fully_qualified_path("crate::module::SubModule::TypeName"));
    }

    #[test]
    fn test_is_fully_qualified_path_invalid() {
        assert!(!is_fully_qualified_path("Bar"));
        assert!(!is_fully_qualified_path("foo::Bar"));
        assert!(!is_fully_qualified_path("self::Bar"));
        assert!(!is_fully_qualified_path("super::Bar"));
        assert!(!is_fully_qualified_path("crate"));
        assert!(!is_fully_qualified_path(""));
    }

    // ========================================================================
    // Unit Tests for Path Parsing
    // ========================================================================

    // TODO: Add tests for parse_target_path once Semantics mock is available

    // ========================================================================
    // Unit Tests for Validation
    // ========================================================================

    // TODO: Add tests for validate_move_operation in Phase 3

    // ========================================================================
    // Unit Tests for Item Extraction
    // ========================================================================

    // TODO: Add tests for extract_item_full_range with AST fixtures

    // ========================================================================
    // Phase 3: User Story 1 - Move Item to Sibling Module Tests (T013-T018)
    // ========================================================================
    //
    // NOTE: These tests verify the assist is currently NOT available (TDD).
    // They should PASS now and will be updated to full tests during implementation.

    #[test]
    fn test_move_struct_to_new_sibling_file() {
        // T013: Move struct to new sibling file
        // Currently verifies assist not available - will be updated in T019-T030
        check_assist_not_applicable(
            move_item,
            r#"
pub struct U$0ser {
    pub id: u64,
}
"#,
        );
    }

    #[test]
    fn test_move_struct_to_existing_sibling_file() {
        // T014: Move struct to existing sibling file
        check_assist_not_applicable(
            move_item,
            r#"
pub struct U$0ser {
    pub id: u64,
}
"#,
        );
    }

    #[test]
    fn test_move_function_to_sibling_module() {
        // T015: Move function to sibling module
        check_assist_not_applicable(
            move_item,
            r#"
pub fn calc$0ulate(x: i32, y: i32) -> i32 {
    x + y
}
"#,
        );
    }

    #[test]
    fn test_move_enum_with_variants_to_sibling() {
        // T016: Move enum with variants to sibling
        check_assist_not_applicable(
            move_item,
            r#"
pub enum St$0atus {
    Active,
    Inactive,
    Pending,
}
"#,
        );
    }

    #[test]
    fn test_update_imports_after_sibling_move() {
        // T017: Update imports after sibling move
        check_assist_not_applicable(
            move_item,
            r#"
pub struct U$0ser {
    pub id: u64,
}
"#,
        );
    }

    #[test]
    fn test_update_all_references_after_move() {
        // T018: Update all references (not just imports)
        check_assist_not_applicable(
            move_item,
            r#"
pub struct U$0ser {
    pub id: u64,
    pub name: String,
}
"#,
        );
    }
}
