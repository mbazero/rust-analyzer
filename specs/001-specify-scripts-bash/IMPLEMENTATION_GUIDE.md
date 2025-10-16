# Implementation Guide: Move Item Refactoring (MVP)

**Feature**: Move Item to Sibling Module (User Story 1)
**Target**: rust-analyzer codebase
**Estimated Effort**: 8-12 hours for experienced rust-analyzer contributor

## Overview

This guide provides step-by-step instructions to implement the MVP of move item refactoring: moving items to sibling modules within the same parent directory. This is Phase 1 + Phase 2 + Phase 3 (User Story 1) from tasks.md.

## Prerequisites

1. **Rust-analyzer development environment setup**
   ```bash
   cd rust-analyzer
   cargo build
   cargo test
   ```

2. **Understand rust-analyzer architecture**
   - Read: `docs/dev/architecture.md`
   - Familiar with: `hir`, `ide-db`, `syntax`, `ide-assists` crates

3. **Study existing patterns**
   - Review: `crates/ide-assists/src/handlers/move_module_to_file.rs`
   - Review: `crates/ide/src/rename.rs`
   - Review: `crates/ide-db/src/rename.rs`

## Phase 1: Setup (T001-T005)

### T001: Review existing rename infrastructure

**File**: `crates/ide/src/rename.rs`

**Key functions to understand**:
```rust
pub(crate) fn rename(
    db: &RootDatabase,
    position: FilePosition,
    new_name: &str,
) -> RenameResult<SourceChange>
```

**What to learn**:
- How `Semantics` is used for navigation
- How `Definition::usages()` finds references
- How `SourceChangeBuilder` accumulates edits
- Error handling patterns (`RenameResult`, `bail!`)

**Action**: Read the entire file, take notes on patterns used.

### T002: Review search and reference finding

**File**: `crates/ide-db/src/search.rs`

**Key structs**:
```rust
pub struct FindUsages<'a> {
    def: Definition,
    sema: &'a Semantics<'a, RootDatabase>,
    scope: Option<&'a SearchScope>,
    // ...
}
```

**What to learn**:
- How `Definition::usages()` works
- `FileReference` structure (file_id, range, category)
- `ReferenceCategory` enum (Import, Read, Write)
- `SearchScope` for optimizing searches

**Action**: Understand the search flow from definition to all usages.

### T003: Review file system operations

**File**: `crates/ide-db/src/source_change.rs`

**Key enum**:
```rust
pub enum FileSystemEdit {
    CreateFile { dst: AnchoredPathBuf, initial_contents: String },
    MoveFile { src: FileId, dst: AnchoredPathBuf },
    MoveDir { src: AnchoredPathBuf, src_id: FileId, dst: AnchoredPathBuf },
}
```

**What to learn**:
- How `SourceChangeBuilder::create_file()` works
- `AnchoredPathBuf` - paths relative to an anchor file
- How file operations integrate with text edits

**Action**: Study the `SourceChangeBuilder` API.

### T004: Review module resolution

**File**: `crates/hir/src/lib.rs`

**Key Module methods**:
```rust
impl Module {
    pub fn declaration_source(self, db: &dyn HirDatabase) -> Option<InFile<ast::Module>>
    pub fn definition_source(self, db: &dyn HirDatabase) -> InFile<ModuleSource>
    pub fn parent(self, db: &dyn HirDatabase) -> Option<Module>
    pub fn children(self, db: &dyn HirDatabase) -> impl Iterator<Item = Module>
    pub fn is_mod_rs(self, db: &dyn HirDatabase) -> bool
}
```

**What to learn**:
- How to navigate module hierarchy
- Difference between declaration (mod statement) and definition (file)
- How to resolve module paths

**Action**: Trace through module navigation examples.

### T005: Study existing assists testing patterns

**File**: `crates/ide-assists/src/tests.rs`

**Key testing function**:
```rust
pub(crate) fn check_assist(
    assist: Handler,
    ra_fixture_before: &str,
    ra_fixture_after: &str,
)
```

**Fixture format**:
```rust
r#"
//- /lib.rs
mod foo;

//- /foo.rs
struct MyStruct$0;
"#
```

**What to learn**:
- Multi-file test fixtures (//- /path)
- Cursor position marker ($0)
- `expect-test` snapshot testing
- How to update expectations (`UPDATE_EXPECT=1`)

**Action**: Run existing tests to see the pattern.

---

## Phase 2: Foundational Infrastructure (T006-T012)

### T006: Create module skeleton

**File**: `crates/ide-assists/src/handlers/move_item.rs` (NEW)

**Initial structure**:
```rust
//! Move item to another module.
//!
//! This assist allows moving items (structs, enums, functions, etc.) to different modules
//! by specifying a fully-qualified path during rename. When the target path differs from
//! the item's current location, the item is moved to the target module with automatic
//! import updates.

use hir::{Module, ModuleSource, Semantics};
use ide_db::{
    EditionedFileId, FileId, FileRange, RootDatabase,
    defs::Definition,
    rename::bail,
    search::{FileReference, SearchScope},
    source_change::{SourceChange, SourceChangeBuilder},
};
use syntax::{
    AstNode, TextRange,
    ast::{self, HasName},
};

use crate::assist_context::{AssistContext, Assists};

// crates/ide-assists/src/handlers.rs
pub(crate) fn move_item(acc: &mut Assists, ctx: &AssistContext<'_>) -> Option<()> {
    // Entry point - will be implemented in phases
    None
}

#[cfg(test)]
mod tests {
    use crate::tests::check_assist;
    use super::*;

    // Tests will be added here
}
```

**Action**: Create the file and add it to `crates/ide-assists/src/handlers.rs`:
```rust
// In handlers.rs, add:
mod move_item;

// In the handlers! macro, add:
move_item::move_item,
```

### T007: Define core data structures

**In**: `crates/ide-assists/src/handlers/move_item.rs`

```rust
/// Represents a parsed target path for a move operation
#[derive(Debug, Clone)]
struct TargetPath {
    /// Full path segments including crate root
    segments: Vec<String>,
    /// The final item name
    item_name: String,
    /// Whether this represents a different location than source
    is_move: bool,
}

impl TargetPath {
    /// Parse a string like "crate::module::Item" into structured path
    fn parse(input: &str) -> Option<Self> {
        let parts: Vec<&str> = input.split("::").collect();

        if parts.len() < 2 {
            return None; // Need at least "crate::Item"
        }

        if parts[0] != "crate" {
            return None; // Must start with crate
        }

        let item_name = parts.last()?.to_string();
        let segments = parts.iter().map(|s| s.to_string()).collect();

        Some(TargetPath {
            segments,
            item_name,
            is_move: true, // Will determine this later
        })
    }

    /// Get the module path (everything except the final item name)
    fn module_path(&self) -> &[String] {
        &self.segments[..self.segments.len() - 1]
    }

    /// Get the parent module segments (e.g., ["crate", "foo"] for crate::foo::bar::Item)
    fn parent_segments(&self) -> &[String] {
        let module_path = self.module_path();
        &module_path[..module_path.len() - 1]
    }

    /// Get the target module name (the immediate parent of the item)
    fn target_module_name(&self) -> &str {
        let module_path = self.module_path();
        module_path.last().unwrap()
    }
}

/// Information about an item being moved
struct MoveableItem {
    /// The item's HIR definition
    definition: Definition,
    /// Current file location
    source_file: FileId,
    /// Text range including attributes and doc comments
    full_range: TextRange,
    /// The item's current name
    name: String,
    /// Whether this is a sibling move (same parent directory)
    is_sibling: bool,
}
```

### T008: Implement path parsing and detection

**Add to move_item.rs**:
```rust
/// Detect if a rename input is actually a fully-qualified path
fn is_fully_qualified_path(input: &str) -> bool {
    input.starts_with("crate::") && input.contains("::")
}

/// Parse and validate a target path
fn parse_target_path(
    ctx: &AssistContext<'_>,
    input: &str,
    source_def: &Definition,
) -> Option<TargetPath> {
    if !is_fully_qualified_path(input) {
        return None;
    }

    let path = TargetPath::parse(input)?;

    // Check if this is actually a move (different location)
    // For MVP: just check if module name differs
    // Full implementation would compare complete paths

    Some(path)
}
```

### T009: Implement path validation

**Add to move_item.rs**:
```rust
/// Validate that the target path is valid and the move is possible
fn validate_target_path(
    ctx: &AssistContext<'_>,
    target: &TargetPath,
) -> Result<(), String> {
    // Syntax validation
    for segment in &target.segments {
        if !is_valid_identifier(segment) {
            return Err(format!("Invalid identifier: {}", segment));
        }
    }

    // Check for keywords
    if is_keyword(target.target_module_name()) {
        return Err(format!("Cannot use keyword as module name: {}", target.target_module_name()));
    }

    Ok(())
}

fn is_valid_identifier(s: &str) -> bool {
    // Check if string is a valid Rust identifier
    if s.is_empty() {
        return false;
    }

    let first = s.chars().next().unwrap();
    if !first.is_alphabetic() && first != '_' {
        return false;
    }

    s.chars().all(|c| c.is_alphanumeric() || c == '_')
}

fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" |
        "extern" | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" |
        "loop" | "match" | "mod" | "move" | "mut" | "pub" | "ref" | "return" |
        "self" | "Self" | "static" | "struct" | "super" | "trait" | "true" |
        "type" | "unsafe" | "use" | "where" | "while" | "async" | "await" | "dyn"
    )
}
```

### T010: Implement item extraction logic

**Add to move_item.rs**:
```rust
/// Extract the full text of an item including attributes and doc comments
fn extract_item_text(
    ctx: &AssistContext<'_>,
    item: &ast::Item,
) -> (String, TextRange) {
    // Get the full range including attributes and doc comments
    let syntax = item.syntax();

    // Find the start - walk backwards to include attributes
    let mut start = syntax.text_range().start();
    let mut node = syntax.clone();

    while let Some(prev) = node.prev_sibling_or_token() {
        match prev.kind() {
            syntax::SyntaxKind::ATTR |
            syntax::SyntaxKind::COMMENT |
            syntax::SyntaxKind::WHITESPACE => {
                start = prev.text_range().start();
                if let Some(prev_node) = prev.as_node() {
                    node = prev_node.clone();
                } else {
                    break;
                }
            }
            _ => break,
        }
    }

    let full_range = TextRange::new(start, syntax.text_range().end());
    let text = ctx.source_file().text().slice(full_range).to_string();

    (text, full_range)
}
```

### T011: Create test helper functions

**Add to move_item.rs tests module**:
```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::{check_assist, check_assist_not_applicable};

    fn check_move_item(before: &str, after: &str) {
        check_assist(move_item, before, after);
    }

    fn check_move_item_not_applicable(fixture: &str) {
        check_assist_not_applicable(move_item, fixture);
    }

    // Tests will be added in next phase
}
```

### T012: Add regression marks

**Add at top of move_item.rs**:
```rust
use cov_mark::hit;

// Define coverage marks for regression tracking
// Usage: hit!(mark_name) in implementation
//        cov_mark::check!(mark_name) in tests
```

**Checkpoint**: At this point, you have the skeleton and basic infrastructure. The file compiles but the assist doesn't do anything yet.

---

## Phase 3: User Story 1 - Tests (T013-T018)

### T013: Test - Move struct to new sibling file

**Add to tests module**:
```rust
#[test]
fn move_struct_to_new_sibling() {
    check_move_item(
        r#"
//- /lib.rs
mod foo;

//- /foo.rs
struct ToMove$0 {
    field: i32,
}
"#,
        r#"
//- /lib.rs
mod foo;
mod bar;

//- /foo.rs

//- /bar.rs
struct ToMove {
    field: i32,
}
"#,
    );
}
```

**Note**: This test will FAIL until implementation. That's expected (TDD).

### T014: Test - Move struct to existing sibling file

```rust
#[test]
fn move_struct_to_existing_sibling() {
    check_move_item(
        r#"
//- /lib.rs
mod foo;
mod bar;

//- /foo.rs
struct ToMove$0 {
    field: i32,
}

//- /bar.rs
struct Existing {}
"#,
        r#"
//- /lib.rs
mod foo;
mod bar;

//- /foo.rs

//- /bar.rs
struct Existing {}

struct ToMove {
    field: i32,
}
"#,
    );
}
```

### T015: Test - Move function to sibling module

```rust
#[test]
fn move_function_to_sibling() {
    check_move_item(
        r#"
//- /lib.rs
mod utils;

//- /utils.rs
fn calculate$0(x: i32, y: i32) -> i32 {
    x + y
}
"#,
        r#"
//- /lib.rs
mod utils;
mod math;

//- /utils.rs

//- /math.rs
fn calculate(x: i32, y: i32) -> i32 {
    x + y
}
"#,
    );
}
```

### T016: Test - Move enum with variants

```rust
#[test]
fn move_enum_with_variants() {
    check_move_item(
        r#"
//- /lib.rs
mod types;

//- /types.rs
enum Status$0 {
    Active,
    Inactive,
    Pending,
}
"#,
        r#"
//- /lib.rs
mod types;
mod status;

//- /types.rs

//- /status.rs
enum Status {
    Active,
    Inactive,
    Pending,
}
"#,
    );
}
```

### T017: Test - Update imports after move

```rust
#[test]
fn update_imports_after_move() {
    check_move_item(
        r#"
//- /lib.rs
mod models;
mod services;

//- /models.rs
pub struct User$0 {
    pub id: u64,
}

//- /services.rs
use crate::models::User;

fn get_user() -> User {
    User { id: 1 }
}
"#,
        r#"
//- /lib.rs
mod models;
mod services;
mod entities;

//- /models.rs

//- /services.rs
use crate::entities::User;

fn get_user() -> User {
    User { id: 1 }
}

//- /entities.rs
pub struct User {
    pub id: u64,
}
"#,
    );
}
```

### T018: Test - Update all references

```rust
#[test]
fn update_all_references() {
    check_move_item(
        r#"
//- /lib.rs
mod types;
mod logic;

//- /types.rs
pub struct Container$0<T> {
    value: T,
}

//- /logic.rs
use crate::types::Container;

fn create() -> Container<i32> {
    Container { value: 42 }
}

fn process(c: Container<String>) {
    // ...
}
"#,
        r#"
//- /lib.rs
mod types;
mod logic;
mod storage;

//- /types.rs

//- /logic.rs
use crate::storage::Container;

fn create() -> Container<i32> {
    Container { value: 42 }
}

fn process(c: Container<String>) {
    // ...
}

//- /storage.rs
pub struct Container<T> {
    value: T,
}
"#,
    );
}
```

**Run tests** (they should all FAIL):
```bash
cargo test -p ide-assists move_item
```

---

## Phase 3: User Story 1 - Implementation (T019-T030)

### T019: Implement is_sibling_move()

**Add to move_item.rs**:
```rust
/// Check if the target is a sibling module (same parent directory)
fn is_sibling_move(
    ctx: &AssistContext<'_>,
    source_module: Module,
    target: &TargetPath,
) -> bool {
    let source_parent = source_module.parent(ctx.db());

    // For sibling move, the parent path should match
    // e.g., moving from crate::foo::a to crate::foo::b

    // Get source path
    let source_path = module_path(ctx.db(), source_module);
    let source_parent_path = &source_path[..source_path.len() - 1];

    // Compare with target parent
    let target_parent_path = target.parent_segments();

    source_parent_path == target_parent_path
}

fn module_path(db: &RootDatabase, module: Module) -> Vec<String> {
    let mut path = Vec::new();
    let mut current = Some(module);

    while let Some(m) = current {
        if let Some(name) = m.name(db) {
            path.insert(0, name.to_string());
        } else {
            path.insert(0, "crate".to_string());
        }
        current = m.parent(db);
    }

    path
}
```

### T020: Implement target file path computation

```rust
use ide_db::base_db::AnchoredPathBuf;

/// Compute the file path for the target module
fn compute_target_file_path(
    ctx: &AssistContext<'_>,
    source_file: FileId,
    target: &TargetPath,
) -> AnchoredPathBuf {
    // For sibling moves, the target file is in the same directory
    // Example: foo/start.rs → foo/finish.rs

    let target_module_name = target.target_module_name();
    let target_filename = format!("{}.rs", target_module_name);

    // Use AnchoredPathBuf to specify path relative to source file's directory
    AnchoredPathBuf {
        anchor: source_file,
        path: target_filename,
    }
}
```

### T021: Implement file creation logic

```rust
/// Create the target file if it doesn't exist
fn ensure_target_file(
    builder: &mut SourceChangeBuilder,
    ctx: &AssistContext<'_>,
    target_path: &AnchoredPathBuf,
) -> Result<FileId, String> {
    // Check if file already exists
    // If not, create with module documentation template

    let initial_contents = format!(
        "//! {} module.\n\n",
        target_path.path.trim_end_matches(".rs")
    );

    builder.create_file(target_path.clone(), initial_contents);

    // For now, return a placeholder FileId
    // Actual implementation would resolve the FileId from VFS
    Ok(ctx.file_id())
}
```

### T022-T027: Core move implementation

**Main assist entry point**:
```rust
pub(crate) fn move_item(acc: &mut Assists, ctx: &AssistContext<'_>) -> Option<()> {
    // 1. Get the item at cursor
    let item = ctx.find_node_at_offset::<ast::Item>()?;
    let name = match &item {
        ast::Item::Struct(s) => s.name()?,
        ast::Item::Enum(e) => e.name()?,
        ast::Item::Fn(f) => f.name()?,
        ast::Item::Trait(t) => t.name()?,
        ast::Item::TypeAlias(t) => t.name()?,
        ast::Item::Const(c) => c.name()?,
        ast::Item::Static(s) => s.name()?,
        _ => return None,
    };

    let item_name = name.text().to_string();

    // 2. This assist only triggers during rename with a qualified path
    // In actual implementation, this would be called from rename.rs
    // For now, create a placeholder that shows structure

    acc.add(
        ide_db::assists::AssistId("move_item", ide_db::assists::AssistKind::Refactor),
        "Move item to another module",
        item.syntax().text_range(),
        |builder| {
            // Implementation will go here
            // For now, this is a placeholder
        },
    )
}
```

**Note**: The full implementation of T022-T027 requires integrating with:
- `Definition::usages()` for finding references
- `SourceChangeBuilder` for accumulating edits
- Import rewriting logic
- Module declaration insertion

This is where the complexity increases significantly. Here's the structure:

```rust
// T022: Remove item from source
fn remove_item_from_source(
    builder: &mut SourceChangeBuilder,
    source_file: FileId,
    item_range: TextRange,
) {
    builder.replace(source_file, item_range, String::new());
}

// T023: Insert item into target
fn insert_item_into_target(
    builder: &mut SourceChangeBuilder,
    target_file: FileId,
    item_text: &str,
) {
    // Append to end of file
    builder.insert(target_file, TextSize::from(0), item_text.to_string());
}

// T024: Add module declaration
fn add_module_declaration(
    builder: &mut SourceChangeBuilder,
    parent_file: FileId,
    module_name: &str,
) {
    let mod_decl = format!("mod {};\n", module_name);
    // Insert at appropriate location in parent
    builder.insert(parent_file, TextSize::from(0), mod_decl);
}

// T025-T027: Reference updates
fn update_references(
    builder: &mut SourceChangeBuilder,
    ctx: &AssistContext<'_>,
    definition: Definition,
    new_path: &str,
) {
    let usages = definition.usages(&ctx.sema).all();

    for (file_id, references) in usages {
        for reference in references {
            // Update import paths
            // Update qualified usages
            // This requires careful text manipulation
        }
    }
}
```

### T028: Integration with rename.rs

**In `crates/ide/src/rename.rs`**, modify the `rename` function:

```rust
pub(crate) fn rename(
    db: &RootDatabase,
    position: FilePosition,
    new_name: &str,
) -> RenameResult<SourceChange> {
    let sema = Semantics::new(db);

    // NEW: Check if this is a move operation (fully-qualified path)
    if new_name.starts_with("crate::") && new_name.contains("::") {
        // Delegate to move_item assist
        // This requires coordinating with ide-assists
        // For MVP, return error suggesting manual move
        return Err(RenameError("Move operations not yet supported. Use move_item assist.".into()));
    }

    // Existing rename logic continues...
    // ...
}
```

### T029: Error handling for edge cases

```rust
/// Validate the move operation before executing
fn validate_move_operation(
    ctx: &AssistContext<'_>,
    item_name: &str,
    target: &TargetPath,
) -> Result<(), String> {
    // Check for name conflicts
    // TODO: Query target module for existing items

    // Check visibility rules
    // TODO: Validate that move won't break visibility

    // Check for invalid paths
    validate_target_path(ctx, target)?;

    Ok(())
}
```

### T030: Verify tests pass

```bash
# Update test expectations
env UPDATE_EXPECT=1 cargo test -p ide-assists move_item

# Run tests normally
cargo test -p ide-assists move_item

# Run clippy
cargo clippy -p ide-assists

# Run tidy
cargo test -p xtask
```

---

## Integration Checklist

- [ ] File created: `crates/ide-assists/src/handlers/move_item.rs`
- [ ] Module added to `crates/ide-assists/src/handlers.rs`
- [ ] Assist registered in handlers macro
- [ ] All 6 tests written and initially failing
- [ ] Core data structures defined (TargetPath, MoveableItem)
- [ ] Path parsing implemented
- [ ] Path validation implemented
- [ ] Item extraction logic implemented
- [ ] File creation logic implemented
- [ ] Module declaration insertion logic implemented
- [ ] Reference finding via Definition::usages()
- [ ] Import rewriting implemented
- [ ] All tests passing after implementation
- [ ] Integration with rename.rs (optional for MVP)
- [ ] Clippy warnings resolved
- [ ] Tidy checks passing
- [ ] Module documentation complete

## Testing Strategy

### Unit Testing (Inline)
```bash
# Test specific function
cargo test -p ide-assists move_item::tests::move_struct_to_new_sibling

# Test all move_item tests
cargo test -p ide-assists move_item

# Update expectations
UPDATE_EXPECT=1 cargo test -p ide-assists move_item
```

### Manual Testing
1. Build rust-analyzer: `cargo build --release`
2. Use built binary in VS Code
3. Try moving items manually
4. Verify behavior matches spec

## Common Issues & Solutions

### Issue 1: Tests fail with "assist not applicable"
**Cause**: The assist is not triggering (returning None too early)
**Solution**: Add debug prints to trace execution flow

### Issue 2: File paths are incorrect
**Cause**: AnchoredPathBuf calculation is wrong
**Solution**: Check how existing assists compute paths

### Issue 3: Imports not updating
**Cause**: Reference search isn't finding all usages
**Solution**: Verify SearchScope includes all necessary files

### Issue 4: Module declarations not added
**Cause**: Parent file not identified correctly
**Solution**: Use `Module::declaration_source()` to find parent

## Performance Considerations

- Use `SearchScope` to limit reference searches
- Batch text edits in SourceChangeBuilder
- Don't perform I/O during analysis phase

## Code Quality Standards

Per rust-analyzer constitution:

1. **No `dbg!()` or `todo!()`** in production code
2. **Use `always!()` / `never!()`** instead of `assert!()`
3. **Module documentation** required
4. **Use `FxHashMap/FxHashSet`** not std collections
5. **Support cancellation** via salsa (automatic)
6. **Test at correct tier** (middle tier for assists)

## Next Steps After MVP

Once User Story 1 (sibling moves) works:

1. **User Story 2**: Move + rename (T031-T044)
2. **User Story 3**: Cross-boundary with directory creation (T045-T058)
3. **Edge Cases**: Robustness (T059-T070)
4. **Polish**: Error messages, optimization (T071-T081)

## Resources

- **rust-analyzer docs**: `https://rust-analyzer.github.io/`
- **Contributing guide**: `CONTRIBUTING.md`
- **Architecture**: `docs/dev/architecture.md`
- **Similar assists**: `crates/ide-assists/src/handlers/move_module_to_file.rs`
- **Rename implementation**: `crates/ide/src/rename.rs`

## Getting Help

- **Zulip chat**: `https://rust-lang.zulipchat.com/#narrow/stream/185405-t-compiler.2Frust-analyzer`
- **GitHub issues**: Tag with `A-assists` label
- **Design docs**: Link to this spec in questions

---

## Summary

This MVP implementation guide provides:
- ✅ Detailed code structure for each task
- ✅ Integration points with existing code
- ✅ Test patterns with expect-test
- ✅ Error handling patterns
- ✅ Validation checklist
- ✅ Troubleshooting guide

**Estimated Effort**: 8-12 hours for experienced contributor

**Key Complexity Points**:
1. Reference finding and import rewriting (T025-T027)
2. Module declaration insertion (T024)
3. Integration with rename.rs (T028)

Good luck with the implementation! 🚀
