# Quickstart Testing Guide: Move Item Refactoring

**Feature**: Move Item Refactoring
**Date**: 2025-10-16
**Purpose**: Manual testing scenarios to validate feature works correctly

## Overview

This guide provides step-by-step manual testing scenarios to validate the move item refactoring feature. Each scenario corresponds to a user story or edge case from the specification.

## Test Environment Setup

### Prerequisites

1. **Build rust-analyzer with the feature**:
   ```bash
   cd /path/to/rust-analyzer
   git checkout 001-specify-scripts-bash
   cargo build --release
   ```

2. **Create a test Rust project**:
   ```bash
   cargo new move_item_test --lib
   cd move_item_test
   ```

3. **Configure your editor** to use the locally built rust-analyzer

### Test Project Structure

Create this initial structure for testing:

```rust
// src/lib.rs
pub mod models;
pub mod services;
pub mod utils;

// src/models.rs
pub struct User {
    pub id: u64,
    pub name: String,
}

pub struct Post {
    pub id: u64,
    pub title: String,
}

// src/services.rs
use crate::models::User;

pub fn authenticate_user(user: &User) -> bool {
    !user.name.is_empty()
}

// src/utils.rs
pub fn calculate(x: i32, y: i32) -> i32 {
    x + y
}
```

## User Story 1: Move Item to Sibling Module (P1)

### Test 1.1: Move struct to new sibling file

**Objective**: Move `User` struct to new `entities.rs` file

**Steps**:
1. Open `src/models.rs`
2. Place cursor on `User` struct name
3. Trigger rename action (F2 or right-click → Rename)
4. Type: `crate::models::entities::User`
5. Press Enter

**Expected Result**:
- ✅ `src/models/entities.rs` is created
- ✅ `User` struct appears in `entities.rs` with all attributes/docs
- ✅ `User` is removed from `models.rs`
- ✅ `src/models.rs` contains `mod entities;` declaration
- ✅ `src/services.rs` import updated to `use crate::models::entities::User;`
- ✅ All references in `services.rs` still work (no compile errors)

**Verification**:
```bash
cargo check  # Should succeed with no errors
```

### Test 1.2: Move function to existing sibling file

**Objective**: Move `calculate` function to existing file

**Steps**:
1. Create `src/math.rs` with some dummy content
2. Add `pub mod math;` to `src/lib.rs`
3. Open `src/utils.rs`
4. Place cursor on `calculate` function name
5. Trigger rename → Type: `crate::math::calculate`

**Expected Result**:
- ✅ `calculate` function moved to `src/math.rs`
- ✅ Function appended to existing file content
- ✅ No duplicate `mod math;` declarations

**Verification**:
```bash
cargo check
```

### Test 1.3: Move enum with variants

**Objective**: Verify enum variants are preserved

**Setup**:
```rust
// Add to src/models.rs
pub enum Status {
    Active,
    Inactive,
    Pending,
}
```

**Steps**:
1. Trigger rename on `Status`
2. Type: `crate::models::state::Status`

**Expected Result**:
- ✅ `src/models/state.rs` created
- ✅ Enum with all variants moved
- ✅ Any pattern matches updated

## User Story 2: Move and Rename Simultaneously (P2)

### Test 2.1: Move struct with rename

**Objective**: Move and rename `User` → `UserAccount`

**Steps**:
1. Open `src/models.rs`
2. Place cursor on `User` struct
3. Trigger rename → Type: `crate::models::accounts::UserAccount`

**Expected Result**:
- ✅ `src/models/accounts.rs` created
- ✅ Struct renamed to `UserAccount`
- ✅ All imports updated: `use crate::models::User;` → `use crate::models::accounts::UserAccount;`
- ✅ All usages updated: `User { id, name }` → `UserAccount { id, name }`
- ✅ Function signatures updated: `fn auth(user: &User)` → `fn auth(user: &UserAccount)`

**Verification**:
```bash
cargo check
# Also manually verify in services.rs that types are correct
```

### Test 2.2: Move trait with rename and impls

**Setup**:
```rust
// In src/models.rs
pub trait Validate {
    fn is_valid(&self) -> bool;
}

impl Validate for User {
    fn is_valid(&self) -> bool {
        !self.name.is_empty()
    }
}
```

**Steps**:
1. Trigger rename on `Validate` trait
2. Type: `crate::models::validation::Validator`

**Expected Result**:
- ✅ Trait moved to `validation.rs` and renamed to `Validator`
- ✅ Impl block updated: `impl Validator for User`
- ✅ All trait imports updated

### Test 2.3: Move with generic parameters

**Setup**:
```rust
// In src/utils.rs
pub struct Container<T> {
    value: T,
}
```

**Steps**:
1. Trigger rename on `Container`
2. Type: `crate::types::storage::Storage`

**Expected Result**:
- ✅ Generic parameters preserved: `Storage<T>`
- ✅ All generic usages updated: `Container<String>` → `Storage<String>`

## User Story 3: Cross-Boundary with Auto-Creation (P3)

### Test 3.1: Create nested module hierarchy

**Objective**: Create multiple directories and modules

**Steps**:
1. Place cursor on `Post` struct in `src/models.rs`
2. Trigger rename → Type: `crate::entities::blog::posts::BlogPost`

**Expected Result**:
- ✅ Directory `src/entities/` created
- ✅ File `src/entities.rs` created with `pub mod blog;`
- ✅ File `src/entities/blog.rs` created with `pub mod posts;`
- ✅ File `src/entities/blog/posts.rs` created with `BlogPost` struct
- ✅ All intermediate `mod` declarations added
- ✅ `src/lib.rs` has `pub mod entities;`

**Verification**:
```bash
tree src/  # Verify directory structure
cargo check
```

### Test 3.2: Deep nesting (5 levels)

**Setup**:
```rust
// Add to src/utils.rs
pub const MAX_SIZE: usize = 1024;
```

**Steps**:
1. Trigger rename on `MAX_SIZE`
2. Type: `crate::config::app::limits::memory::MAX_MEMORY_SIZE`

**Expected Result**:
- ✅ Five-level directory structure created
- ✅ All module declarations added at each level
- ✅ Const moved with rename

### Test 3.3: Multiple moves to same new module

**Objective**: Second move to same module doesn't duplicate declarations

**Steps**:
1. Move `User` to `crate::entities::users::User` (first move)
2. Move `Post` to `crate::entities::posts::Post` (second move)

**Expected Result**:
- ✅ Both moves succeed
- ✅ Only one `mod entities;` declaration in lib.rs
- ✅ `entities.rs` contains both `mod users;` and `mod posts;`
- ✅ No duplicate declarations

## Edge Cases

### Edge Case 1: Name conflict detection

**Setup**:
```rust
// In src/models.rs
pub struct User { /* ... */ }
pub struct Person { /* ... */ }
```

**Steps**:
1. Try to move `Person` to `crate::models::User`

**Expected Result**:
- ❌ Operation blocked with error: "Target module already contains item 'User'"
- ✅ No changes made to codebase
- ✅ Clear error message in editor

### Edge Case 2: Invalid path syntax

**Steps**:
1. Trigger rename on any item
2. Type: `crate::123invalid::Item` (invalid identifier)

**Expected Result**:
- ❌ Error: "Invalid path: '123invalid' is not a valid identifier"
- ✅ No changes made

### Edge Case 3: Visibility preservation

**Setup**:
```rust
// In src/models.rs
pub struct PublicUser { /* ... */ }
pub(crate) struct CrateUser { /* ... */ }
struct PrivateUser { /* ... */ }
```

**Steps**:
1. Move each struct to different modules
2. Verify visibility keywords preserved

**Expected Result**:
- ✅ `pub struct PublicUser` remains `pub`
- ✅ `pub(crate) struct CrateUser` remains `pub(crate)`
- ✅ `struct PrivateUser` remains private

### Edge Case 4: Doc comments and attributes

**Setup**:
```rust
/// This is a user struct.
/// It represents a user in the system.
#[derive(Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct User {
    pub id: u64,
}
```

**Steps**:
1. Move `User` to `crate::models::users::User`

**Expected Result**:
- ✅ All doc comments preserved
- ✅ All attributes preserved (`#[derive(...)]`, `#[serde(...)]`)
- ✅ Comments appear before struct in new file

### Edge Case 5: Re-exports (pub use)

**Setup**:
```rust
// src/lib.rs
pub use models::User;

// src/models.rs
pub struct User { /* ... */ }
```

**Steps**:
1. Move `User` to `crate::models::users::User`

**Expected Result**:
- ✅ Re-export updated: `pub use models::users::User;`
- ✅ Public API unchanged (still `crate::User` works)

### Edge Case 6: Macro usage

**Setup**:
```rust
// src/models.rs
#[derive(Debug)]
pub struct User {
    pub id: u64,
}

// src/services.rs
use crate::models::User;

fn test() {
    println!("{:?}", User { id: 1 });
}
```

**Steps**:
1. Move `User` to new module

**Expected Result**:
- ✅ Macro-generated trait impls still work
- ✅ `println!` usage updated correctly

## Performance Tests

### Performance Test 1: Large codebase

**Setup**: Create project with 100+ files, 1000+ items

**Steps**:
1. Move an item referenced in 50+ files

**Expected Result**:
- ✅ Completion time < 10 seconds
- ✅ Editor remains responsive during operation
- ✅ All 50+ references correctly updated

**Measurement**:
```bash
time <perform move operation>
# Should be < 10s
```

### Performance Test 2: Cancellation

**Steps**:
1. Start a move operation on heavily-referenced item
2. Cancel (Esc or Ctrl+C) during reference search

**Expected Result**:
- ✅ Operation cancelled gracefully
- ✅ No partial changes applied
- ✅ Codebase unchanged

## Regression Tests

### Regression 1: Undo functionality

**Steps**:
1. Perform any successful move
2. Press Undo (Ctrl+Z)

**Expected Result**:
- ✅ All changes reverted in single undo
- ✅ File creation undone
- ✅ Imports restored

### Regression 2: Multiple moves in sequence

**Steps**:
1. Move `User` to `crate::a::User`
2. Move `Post` to `crate::b::Post`
3. Move `User` again to `crate::c::User`

**Expected Result**:
- ✅ All three moves succeed
- ✅ Module declarations correct
- ✅ No stale imports or broken references

## Test Checklist

Use this checklist to verify all tests pass:

### User Story 1 (P1) - Sibling Moves
- [ ] Test 1.1: Move struct to new sibling file
- [ ] Test 1.2: Move function to existing sibling
- [ ] Test 1.3: Move enum with variants

### User Story 2 (P2) - Move + Rename
- [ ] Test 2.1: Move struct with rename
- [ ] Test 2.2: Move trait with rename and impls
- [ ] Test 2.3: Move with generic parameters

### User Story 3 (P3) - Cross-Boundary
- [ ] Test 3.1: Create nested module hierarchy
- [ ] Test 3.2: Deep nesting (5 levels)
- [ ] Test 3.3: Multiple moves to same new module

### Edge Cases
- [ ] Edge Case 1: Name conflict detection
- [ ] Edge Case 2: Invalid path syntax
- [ ] Edge Case 3: Visibility preservation
- [ ] Edge Case 4: Doc comments and attributes
- [ ] Edge Case 5: Re-exports (pub use)
- [ ] Edge Case 6: Macro usage

### Performance
- [ ] Performance Test 1: Large codebase (<10s)
- [ ] Performance Test 2: Cancellation support

### Regression
- [ ] Regression 1: Undo functionality
- [ ] Regression 2: Multiple moves in sequence

## Troubleshooting

### Issue: Move doesn't trigger

**Check**:
- Is cursor on an item name (not whitespace)?
- Is item a top-level module item?
- Is input a fully-qualified path starting with `crate::`?

### Issue: Compilation errors after move

**Check**:
- Run `cargo check` for detailed errors
- Verify all import paths updated
- Check for name conflicts
- Verify visibility rules not violated

### Issue: Module declarations missing

**Check**:
- Parent module has `mod child_name;` statement
- Module files created in correct locations
- No syntax errors in module declarations

## Success Criteria Validation

Map tests to success criteria from spec:

- **SC-001** (< 10 seconds): Performance Test 1
- **SC-002** (100% accuracy): All tests + `cargo check` passes
- **SC-003** (auto file creation): Tests 1.1, 3.1, 3.2
- **SC-004** (95% success rate): Run all tests, should pass without manual fixes
- **SC-005** (single undo): Regression Test 1
- **SC-006** (50% time savings): Qualitative - note time vs manual process

## Reporting Issues

When reporting bugs, include:
1. Rust-analyzer version / commit hash
2. Editor and version
3. Minimal reproduction case (small test project)
4. Expected vs actual behavior
5. Any error messages from rust-analyzer logs

## Notes

- All tests assume rust-analyzer is running correctly
- Use `rust-analyzer.trace.server = "verbose"` in settings for debugging
- Check rust-analyzer logs if operations fail silently
- Test with both single-file and workspace projects
