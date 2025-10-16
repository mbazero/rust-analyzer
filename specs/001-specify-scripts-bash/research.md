# Research: Move Item Refactoring

**Feature**: Move Item Refactoring
**Date**: 2025-10-16
**Purpose**: Document architectural decisions, technology choices, and implementation patterns

## Overview

This document captures research findings for implementing a move item refactoring feature in rust-analyzer. The feature extends the existing rename action to support moving items between modules using fully-qualified paths.

## Key Architectural Decisions

### Decision 1: Integration Point - Rename vs New Assist

**Chosen Approach**: Extend rename action with path detection

**Rationale**:
- User's mental model: "rename with a path" is intuitive
- Leverages battle-tested rename infrastructure (`ide/src/rename.rs`, `ide-db/src/rename.rs`)
- Maintains consistent UX with existing rename behavior
- Reuses validation, reference finding, and import updating logic

**Alternatives Considered**:
- **New separate assist**: Would require duplicating rename logic, less discoverable
- **Context menu action**: Less accessible, requires more clicks

**Implementation Pattern**:
```rust
// In ide/src/rename.rs
pub fn rename(/* ... */) -> Result<SourceChange> {
    let new_name_str = new_name.text();

    // NEW: Detect if input is a fully-qualified path
    if is_fully_qualified_path(new_name_str) {
        return handle_move_item(/* ... */);
    }

    // Existing: Normal rename logic
    // ...
}
```

### Decision 2: Module File Organization Strategy

**Chosen Approach**: Always use named file style (`module_name.rs`)

**Rationale**:
- Aligns with modern Rust conventions (Rust 2018+)
- Specified in requirements (FR-005)
- Simpler implementation (no need for heuristics)
- Most rust-analyzer code uses this pattern

**Alternatives Considered**:
- **Auto-detect**: Complex heuristics, potential inconsistency
- **Directory style (mod.rs)**: Older convention, less commonly used

**Implementation**:
- When creating `crate::foo::bar::Item`, create `crates/src/foo/bar.rs`
- Never create `crates/src/foo/bar/mod.rs`

### Decision 3: Testing Strategy

**Chosen Approach**: Middle tier (IDE boundary) with fixture tests

**Rationale**:
- Follows constitution: "Middle tier tests (IDE boundary - MOST IMPORTANT)"
- Existing pattern in `ide-assists` crate
- Fast, reliable, maintainable
- Supports multi-file scenarios with fixture format

**Test Location**: `crates/ide-assists/src/handlers/move_item.rs`

**Test Pattern**:
```rust
#[test]
fn test_move_item_sibling_module() {
    check_assist(
        move_item,
        r#"
//- /lib.rs
mod foo;

//- /foo.rs
struct MyStruct$0;
        "#,
        r#"
//- /lib.rs
mod foo;
mod bar;

//- /foo.rs

//- /bar.rs
struct MyStruct;
        "#,
    );
}
```

### Decision 4: Path Parsing and Validation

**Chosen Approach**: Use `syntax::ast::Path` with semantic validation

**Rationale**:
- Leverages existing AST parsing infrastructure
- Semantic validation via `hir::Semantics`
- Consistent with rust-analyzer architecture

**Implementation Steps**:
1. Parse input string as `ast::Path`
2. Extract module segments and final item name
3. Validate path components (valid identifiers, no keywords)
4. Validate target module visibility
5. Check for name conflicts at target

**Error Cases**:
- Invalid syntax: "crate::foo::123::Bar" (invalid identifier)
- Invalid structure: "std::vec::Vec" (can't move to stdlib)
- Visibility violation: moving pub item to private module
- Name conflict: target already has item with same name

### Decision 5: File System Operations Model

**Chosen Approach**: Use `FileSystemEdit` in `SourceChange`

**Rationale**:
- Existing infrastructure in `ide-db/src/source_change.rs`
- Transaction model (all-or-nothing)
- LSP client handles actual file operations
- Supports undo via editor

**Operations Needed**:
```rust
pub enum FileSystemEdit {
    CreateFile { dst: AnchoredPathBuf, initial_contents: String },
    MoveFile { src: FileId, dst: AnchoredPathBuf },  // If moving entire module
    // Note: MoveDir not needed for item moves
}
```

**Pattern**:
1. Extract item definition from source file
2. Create target file if doesn't exist (with module docs)
3. Insert item into target file
4. Remove item from source file
5. Add module declaration to parent if new module
6. Update all imports and references

### Decision 6: Reference Update Strategy

**Chosen Approach**: Use `Definition::usages()` with import rewriting

**Rationale**:
- Existing infrastructure finds all references
- Import utilities handle `use` statement updates
- Handles edge cases (field shorthands, macro contexts)

**Implementation Pattern**:
```rust
let def = Definition::from_item(item);
let usages = def.usages(&sema).all();

for (file_id, references) in usages {
    for reference in references {
        match reference.category {
            ReferenceCategory::Import => {
                // Rewrite import path
                update_import_path(reference, new_module_path);
            }
            _ => {
                // May need qualification adjustments
                handle_reference_update(reference, new_path);
            }
        }
    }
}
```

## Technology Stack Summary

| Component | Technology | Purpose |
|-----------|-----------|---------|
| **Language** | Rust (latest stable) | Core implementation |
| **Semantic Analysis** | `hir` crate | Module resolution, path operations |
| **Search** | `ide-db::search` | Find all references |
| **Rename** | `ide-db::rename` | Base infrastructure |
| **Source Changes** | `ide-db::source_change` | File operations |
| **Imports** | `ide-db::imports` | Import manipulation |
| **Testing** | `expect-test` | Snapshot testing |
| **Incremental** | `salsa` (via base-db) | Performance |

## Best Practices from Codebase

### 1. Two-Phase Pattern

Always separate analysis from mutation:

```rust
// Phase 1: Analyze (read-only)
let item = find_item_at_cursor(ctx)?;
let target_path = parse_target_path(new_name)?;
let usages = item.def.usages(&ctx.sema).all();
validate_move_operation(&item, &target_path)?;

// Phase 2: Mutate (build SourceChange)
let mut builder = SourceChangeBuilder::new(ctx.db());
builder.move_item(&item, &target_path);
builder.update_references(&usages, &target_path);
builder.finish()
```

### 2. Graceful Error Handling

Never panic, always return `Option` or `Result`:

```rust
fn validate_target_path(path: &ast::Path) -> Result<ModulePath, String> {
    let segments = path.segments()
        .ok_or("Invalid path: no segments")?;

    // Check each segment
    for segment in segments {
        if is_keyword(&segment) {
            return Err(format!("Cannot use keyword '{}' in path", segment));
        }
    }

    Ok(ModulePath::from_segments(segments))
}
```

### 3. Preserve Metadata

Always preserve attributes and doc comments:

```rust
let item_range = item.syntax()
    .first_token()  // Includes leading trivia (comments, attrs)
    .text_range()
    .cover(item.syntax().text_range());

let item_text = ctx.source_file().text().slice(item_range).to_string();
```

### 4. Handle Visibility

Adjust visibility when moving items:

```rust
// If moving from private module to public module,
// may need to add `pub` or `pub(crate)`
let new_vis = calculate_required_visibility(
    item.current_vis,
    source_module,
    target_module,
)?;
```

### 5. Cancellation Support

All long operations support cancellation:

```rust
// Automatically supported via salsa queries
// User can cancel during:
// - Path resolution
// - Reference search
// - File operations
```

## Edge Cases and Solutions

### Edge Case 1: Moving Item With Impl Blocks

**Problem**: Item definition in one file, impl blocks elsewhere

**Solution**:
- Move only the item definition
- Impl blocks stay in their files (they reference the new path)
- Update imports in impl block files

### Edge Case 2: Re-exports

**Problem**: Item is re-exported via `pub use`

**Solution**:
- Update re-export path: `pub use old::Item;` → `pub use new::Item;`
- Detected via `ReferenceCategory::Import` in search results

### Edge Case 3: Macro Contexts

**Problem**: Item defined or used in macro expansion

**Solution**:
- Validate that item is not in macro definition
- Handle references in macro invocations (search finds them)
- Special care for attribute macros on the item

### Edge Case 4: Name Conflicts

**Problem**: Target module already has item with same name

**Solution**:
- Detect before moving: check target module's definitions
- Provide clear error: "Target module already contains item 'Foo'"
- User must manually resolve conflict first

### Edge Case 5: Circular Dependencies

**Problem**: Move creates circular module dependency

**Solution**:
- Validate dependency graph before moving
- Detect if target module already imports source module
- Provide error if cycle detected

### Edge Case 6: Empty Source Module

**Problem**: Moving last item leaves module empty

**Solution**:
- Don't automatically delete empty modules
- User may want to add items later
- Can use separate "remove unused module" assist

## Performance Considerations

### Optimization 1: Incremental Search

**Approach**: Use `SearchScope` to limit reference search

```rust
// For same-crate moves, only search current crate
let scope = SearchScope::single_file(ctx.file_id())
    .union(SearchScope::crate_graph(ctx.db()));
```

### Optimization 2: Batch Operations

**Approach**: Group file edits in single `SourceChange`

- All text edits
- All file creations
- All module declarations
- Single transaction to LSP client

### Optimization 3: Lazy Validation

**Approach**: Only validate what's needed

- Parse path: cheap
- Check syntax: cheap
- Find references: expensive (only if validation passes)
- Validate visibility: medium (only after path validated)

## Implementation Phases

Based on user stories from spec:

### Phase 1: Sibling Module Moves (P1 - MVP)

**Scope**: Move items within same parent module
- No directory creation
- Simple path resolution
- File creation only

**Complexity**: Low
**Value**: High (most common case)

### Phase 2: Move + Rename (P2)

**Scope**: Move AND rename item simultaneously
- Extends Phase 1
- Name conflict detection
- Reference updates include rename

**Complexity**: Medium
**Value**: High (natural extension)

### Phase 3: Cross-Boundary with Auto-Creation (P3)

**Scope**: Create entire module hierarchies
- Directory creation
- Module declaration chains
- Complex path operations

**Complexity**: High
**Value**: Medium (less common, but powerful)

## Dependencies and Integration Points

### External Dependencies

- `syntax` crate: Path parsing, AST manipulation
- `hir` crate: Semantic analysis, module operations
- `ide-db` crate: Search, rename, source changes, imports
- `vfs` crate: File system operations (via FileSystemEdit)
- `base-db` crate: Salsa queries, incremental computation

### Internal Integration Points

**Extends**:
- `ide/src/rename.rs`: Add path detection logic
- `ide-db/src/rename.rs`: Add move operation support

**Uses**:
- `ide-db/src/search.rs`: Find all references
- `ide-db/src/source_change.rs`: File operations
- `ide-db/src/imports/insert_use.rs`: Add imports
- `ide-db/src/imports/import_assets.rs`: Import analysis

**Provides**:
- New assist in `ide-assists/src/handlers/move_item.rs`
- Extension to rename functionality (transparent to users)

## Testing Strategy Details

### Test Organization

```rust
// In crates/ide-assists/src/handlers/move_item.rs

#[cfg(test)]
mod tests {
    use super::*;

    // User Story 1: Sibling moves
    #[test] fn move_struct_to_sibling() { /* ... */ }
    #[test] fn move_function_to_sibling() { /* ... */ }
    #[test] fn move_to_existing_file() { /* ... */ }

    // User Story 2: Move + rename
    #[test] fn move_and_rename_struct() { /* ... */ }
    #[test] fn move_and_rename_enum() { /* ... */ }

    // User Story 3: Cross-boundary
    #[test] fn move_across_modules() { /* ... */ }
    #[test] fn create_nested_modules() { /* ... */ }

    // Edge cases
    #[test] fn error_on_name_conflict() { /* ... */ }
    #[test] fn error_on_invalid_path() { /* ... */ }
    #[test] fn handle_macro_contexts() { /* ... */ }
}
```

### Test Coverage Requirements

- ✅ All user stories (P1, P2, P3)
- ✅ All item types (struct, enum, function, trait, etc.)
- ✅ Edge cases (conflicts, validation, errors)
- ✅ Multi-file scenarios
- ✅ Import updates
- ✅ Module declaration creation

### Regression Marks

Use `cov_mark` to track coverage:

```rust
// In implementation
cov_mark::hit!(move_item_sibling);

// In test
cov_mark::check!(move_item_sibling);
```

## References

### Codebase Examples

- `crates/ide-assists/src/handlers/move_module_to_file.rs`: Module extraction pattern
- `crates/ide-assists/src/handlers/extract_module.rs`: Module creation pattern
- `crates/ide/src/rename.rs`: Rename entry point
- `crates/ide-db/src/rename.rs`: Rename implementation

### Related Issues/PRs

(To be added during implementation - search rust-analyzer repo for rename/refactoring work)

## Conclusion

This research establishes the architectural foundation for move item refactoring. The implementation will:

1. Extend existing rename infrastructure (proven pattern)
2. Use middle-tier testing (constitutional requirement)
3. Follow rust-analyzer patterns (two-phase, graceful errors, incremental)
4. Support incremental implementation (P1 → P2 → P3)
5. Maintain performance (salsa, cancellation, no global invalidation)

Next phases will generate data model and implementation tasks based on these decisions.
