# Move and Rename Feature for rust-analyzer

## Overview

This feature extends rust-analyzer's rename functionality to support moving items between modules.
When a rename target is a fully-qualified path (e.g., `crate::mod_one::finish::Final`), the system
will move the item to the specified module and optionally rename it.

## Example Usage

### Example 1: Move to sibling module with rename

```rust
// In src/mod_one/start.rs
struct ToMove;
```

Running rename on `ToMove` and typing:
```
crate::mod_one::finish::Final
```

Would:
1. Create `finish.rs` as a sibling to `start.rs`
2. Move the struct to `finish.rs`
3. Rename it to `Final`
4. Update all imports and references

### Example 2: Move to new module hierarchy

```rust
// In src/mod_one/start.rs
struct ToMove;
```

Running rename and typing:
```
crate::mod_two::finish::Final
```

Would:
1. Create the `mod_two` directory if needed
2. Create a `mod.rs` or top-level `mod_two.rs` file (preference-dependent)
3. Create the `finish.rs` file in the module hierarchy
4. Move and rename the struct
5. Update all imports and references

## Implementation Status

### ✅ Completed

1. **Path Parsing** (`crates/ide-db/src/rename_move.rs::parse_move_target`)
   - Detects when a rename target is a fully-qualified path
   - Parses the path to extract module path and final name
   - Supports `crate::`, `self::`, `super::`, and plain paths

2. **Integration** (`crates/ide/src/rename.rs`)
   - Integrated move detection into the main rename flow
   - Redirects to move logic when a path is detected

3. **Basic Move Logic** (`crates/ide-db/src/rename_move.rs::move_and_rename`)
   - Item source extraction
   - Basic rename logic for item names
   - Skeleton for module resolution and file creation

### 🚧 TODO - Critical Components

1. **Module Resolution** (`resolve_target_module`)
   - Need to properly resolve `ModPath` to `hir::Module`
   - Handle `crate::`, `super::`, and relative paths correctly
   - Start from the appropriate root (crate root vs current module)
   - Navigate through the module tree following path segments

2. **Module/File Creation** (`get_or_create_target_file`)
   - Determine whether to create `module_name.rs` or `module_name/mod.rs`
   - Create parent modules if they don't exist
   - Add `mod` declarations to parent modules
   - Handle both inline and file-based modules
   - Return `FileSystemEdit::CreateFile` operations

3. **Reference Updates**
   - Update all imports pointing to the old location
   - Update fully-qualified paths in code
   - Handle re-exports
   - Update documentation links
   - Consider visibility changes (public items moving to private modules)

4. **Edge Cases**
   - Handle items with dependencies (other items that should move together)
   - Handle items referenced in macros
   - Handle moving items that are re-exported
   - Handle moving items between crates (should error)
   - Handle moving into module blocks (should error or have special handling)

### 📝 Implementation Notes

#### Module Resolution Strategy

The module resolution should:
1. Check the `PathKind` to determine the starting point:
   - `PathKind::Crate` → Start from crate root
   - `PathKind::SELF` → Start from current module
   - `PathKind::Super(n)` → Go up n levels from current module
   - `PathKind::Plain` → Start from current module

2. For each segment in the path:
   - Look for a child module with that name
   - If not found, plan to create it
   - Track what files/directories need to be created

3. Return the target module and a list of creation operations

#### File Creation Strategy

When creating new module files:
1. Check project preferences for module style (through `rust-analyzer` config if available)
2. For a module `foo`:
   - Style 1: Create `foo.rs` in the parent module's directory
   - Style 2: Create `foo/mod.rs` directory structure
3. If creating nested modules, use consistent style
4. Add `mod foo;` declarations to parent modules

#### Reference Update Strategy

After moving an item:
1. Use the existing `Definition::usages()` to find all references
2. For each reference:
   - If it's an import, update the path
   - If it's a qualified path, update it
   - If it's an unqualified reference, ensure there's an import
3. Handle glob imports specially (may need to add explicit imports)

## Files Modified

- `crates/ide-db/src/lib.rs` - Added `rename_move` module
- `crates/ide-db/src/rename_move.rs` - New file with move logic
- `crates/ide/src/rename.rs` - Integrated move detection

## Testing Strategy

When implementation is complete, tests should cover:

1. **Basic moves**:
   - Move to existing sibling module
   - Move to existing parent module
   - Move to existing child module

2. **With file creation**:
   - Move to new sibling module
   - Move to new nested module hierarchy
   - Move with both `.rs` and `mod.rs` styles

3. **With renames**:
   - Move and rename
   - Move without rename
   - Rename without move (existing behavior)

4. **Reference updates**:
   - Update use statements
   - Update qualified paths
   - Add necessary imports
   - Handle re-exports

5. **Error cases**:
   - Invalid target paths
   - Circular module dependencies
   - Moving built-in types
   - Moving to block modules

## Future Enhancements

1. **Batch moves**: Move multiple related items together
2. **Auto-import cleanup**: Remove unused imports from source file
3. **Visibility analysis**: Warn about visibility issues
4. **Dependency analysis**: Suggest moving dependent items
5. **Preview**: Show a preview of all changes before applying

## References

- Current rename implementation: `crates/ide-db/src/rename.rs`
- Module system: `crates/hir/src/lib.rs` (Module type)
- Path representation: `crates/hir-expand/src/mod_path.rs`
- File operations: `crates/ide-db/src/source_change.rs` (FileSystemEdit)
