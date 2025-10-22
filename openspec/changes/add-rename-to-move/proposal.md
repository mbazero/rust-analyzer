# Proposal: Add Rename-to-Move for Items

## Why

Currently, rust-analyzer's rename operation only changes an item's name but cannot relocate it to a different module. Users need to manually cut/paste code, create new files/modules, and update all imports when reorganizing code. This is error-prone and tedious, especially when refactoring large codebases.

## What Changes

Extend the rename LSP operation to interpret fully-qualified paths as "move and rename" instructions:

- Parse new names as potential module paths (e.g., `crate::mod_two::finish::Final`)
- Compare against current item path to detect move operations
- Create destination modules and files as needed (respecting module system conventions)
- Move item definition to destination file
- Rename item if the final name component differs
- Update all imports and references across the workspace (both external and internal)
- Validate visibility of items referenced within moved code
- Automatically update moved item's visibility to minimum required level
- Support both inline modules (`mod.rs`) and file-based modules based on project conventions

**Examples:**
- `ToMove` → `crate::mod_one::finish::Final`: Creates `finish.rs` sibling, moves and renames struct
- `ToMove` → `crate::mod_two::finish::Final`: Creates `mod_two/` directory with `mod.rs` or top-level `mod_two.rs`, creates `finish.rs`, moves and renames struct
- `ToMove` → `crate::mod_one::Final`: Moves to existing `mod_one/mod.rs`, renames to `Final`

**Breaking:** None - extends existing rename behavior with backward compatibility

## Impact

- **Affected specs:** New capability `item-rename-move`
- **Affected code:**
  - `crates/ide/src/rename.rs` - Add path parsing and move orchestration
  - `crates/ide-db/src/rename.rs` - Extend `Definition::rename()` to handle moves
  - `crates/rust-analyzer/src/handlers/request.rs` - LSP handler may need updates for file creation
  - `crates/vfs/` - May need new APIs for creating files/directories
  - `crates/syntax/` - Path parsing for destination validation
- **User experience:** Significantly improves code reorganization workflow
- **Testing:** New test fixtures for various move scenarios (cross-module, nested modules, file creation)
