# Quick Start: Implementing Move Item Refactoring

**Goal**: Get started implementing the MVP in 30 minutes

## Prerequisites (5 minutes)

```bash
# 1. Clone and build rust-analyzer
git clone https://github.com/rust-lang/rust-analyzer.git
cd rust-analyzer
cargo build

# 2. Run tests to verify environment
cargo test

# 3. Check out feature branch
git checkout -b feature/move-item-refactoring
```

## Phase 1: Create the Module (10 minutes)

### Step 1: Create the assist file

```bash
touch crates/ide-assists/src/handlers/move_item.rs
```

### Step 2: Add minimal implementation

Copy this skeleton to `move_item.rs`:

```rust
//! Move item to another module.

use ide_db::assists::{AssistContext, Assists};
use syntax::ast;

pub(crate) fn move_item(acc: &mut Assists, ctx: &AssistContext<'_>) -> Option<()> {
    let _item = ctx.find_node_at_offset::<ast::Item>()?;
    // TODO: Implementation
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::check_assist;

    #[test]
    fn test_move_struct_to_sibling() {
        check_assist(
            move_item,
            r#"
//- /lib.rs
mod foo;

//- /foo.rs
struct ToMove$0;
            "#,
            r#"
//- /lib.rs
mod foo;
mod bar;

//- /foo.rs

//- /bar.rs
struct ToMove;
            "#,
        );
    }
}
```

### Step 3: Register the assist

Edit `crates/ide-assists/src/handlers.rs`:

```rust
// Add at top with other mod declarations
mod move_item;

// Add in handlers! macro
move_item::move_item,
```

### Step 4: Verify it compiles

```bash
cargo check -p ide-assists
```

## Phase 2: Make First Test Pass (15 minutes)

### Step 1: Run the test (it will fail)

```bash
cargo test -p ide-assists move_item::tests::test_move_struct_to_sibling
```

Expected: Test fails because assist returns None

### Step 2: Implement basic detection

```rust
pub(crate) fn move_item(acc: &mut Assists, ctx: &AssistContext<'_>) -> Option<()> {
    let item = ctx.find_node_at_offset::<ast::Item>()?;

    // Get item name
    let name = match &item {
        ast::Item::Struct(s) => s.name()?,
        _ => return None,
    };

    acc.add(
        ide_db::assists::AssistId("move_item", ide_db::assists::AssistKind::Refactor),
        "Move item to another module",
        item.syntax().text_range(),
        |builder| {
            // Placeholder - will implement move logic
        },
    )
}
```

### Step 3: Run test again

```bash
cargo test -p ide-assists move_item::tests::test_move_struct_to_sibling
```

Expected: Test should show assist is available but doesn't do anything yet

## Next Steps

You now have:
- ✅ Module created and compiling
- ✅ Assist registered and discoverable
- ✅ First test written (failing)
- ✅ Basic structure in place

**Continue with**: `IMPLEMENTATION_GUIDE.md` for full MVP implementation

## Common First-Time Issues

**Issue**: `cargo check` fails with "module not found"
- **Fix**: Make sure you added `mod move_item;` to `handlers.rs`

**Issue**: Test fails with "assist not found"
- **Fix**: Make sure you added `move_item::move_item,` to the handlers macro

**Issue**: Cannot find `AssistContext`
- **Fix**: Import from `ide_db::assists`

## Development Loop

```bash
# 1. Write/update test
# 2. Run test (should fail)
cargo test -p ide-assists move_item::tests::test_name

# 3. Implement feature
# 4. Run test (should pass)
cargo test -p ide-assists move_item::tests::test_name

# 5. Update expectations if needed
UPDATE_EXPECT=1 cargo test -p ide-assists move_item::tests::test_name

# 6. Run all tests
cargo test -p ide-assists

# 7. Check code quality
cargo clippy -p ide-assists
```

## Getting Help

- Read: `IMPLEMENTATION_GUIDE.md` for detailed instructions
- Check: Existing assists in `crates/ide-assists/src/handlers/`
- Ask: rust-analyzer Zulip chat

## Time Estimate

- Quick Start: 30 minutes ✅
- Phase 1 (Setup): 1-2 hours
- Phase 2 (Foundation): 2-3 hours
- Phase 3 (MVP Tests): 1 hour
- Phase 3 (MVP Implementation): 4-6 hours
- **Total MVP**: 8-12 hours

Good luck! 🚀
