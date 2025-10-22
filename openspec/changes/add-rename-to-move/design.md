# Design: Rename-to-Move Implementation

## Context

Currently, rust-analyzer's rename operation (crates/ide-db/src/rename.rs and crates/ide/src/rename.rs) can rename items and update all references, including moving files when renaming modules. However, it cannot move arbitrary items (structs, enums, functions, traits) between modules. This forces users to manually reorganize code.

**Key stakeholders:** Rust developers using rust-analyzer for code refactoring and organization.

**Constraints:**
- Must integrate with existing rename infrastructure (Definition::rename, find_usages)
- Must maintain LSP compatibility (WorkspaceEdit format)
- Must be atomic (all-or-nothing operation)
- Performance: Should handle large workspaces without significant slowdown

## Goals / Non-Goals

### Goals
- Enable moving items between modules using fully-qualified paths in rename
- Automatically create module files and directories as needed
- Update all imports and references across the workspace
- Support common item types (structs, enums, functions, traits, type aliases, constants)
- Preserve backward compatibility with simple rename operations

### Non-Goals
- Moving macro definitions (complex expansion semantics)
- Moving items between crates (only intra-crate moves)
- Interactive UI for choosing module structure (follow conventions, provide defaults)
- Automatic code style enforcement beyond import updates
- Moving items with macro-generated code (defer to future work)

## Decisions

### Decision 1: Path Parsing Strategy

**What:** Parse the rename target string to detect fully-qualified paths using existing syntax parsing infrastructure.

**Why:**
- Leverage existing `syntax` crate for robust path parsing
- Distinguish between simple names (`Foo`), qualified names (`bar::Foo`), and absolute paths (`crate::bar::Foo`)
- Validate path components as legal identifiers

**Implementation:**
```rust
// In ide/src/rename.rs or ide-db/src/rename.rs
fn parse_rename_target(new_name: &str) -> RenameTarget {
    // Use syntax::ast::Path to parse
    if looks_like_path(new_name) {
        if let Some(path) = parse_as_path(new_name) {
            return RenameTarget::Move { path, final_name };
        }
    }
    RenameTarget::SimpleRename(new_name)
}
```

**Alternatives considered:**
1. String-based parsing (split on `::`) - Rejected: fragile, doesn't handle edge cases
2. Requiring special syntax (e.g., `move:crate::foo`) - Rejected: not intuitive, breaks existing rename UX
3. Separate LSP command - Rejected: duplicates rename logic, worse UX

### Decision 2: Module File Structure Detection

**What:** Prefer file-based modules (`foo.rs`) for simple cases and directory modules (`foo/mod.rs`) for nested structures. Detect existing project style when possible.

**Why:**
- Edition 2018+ prefers file-based modules for simplicity
- Directory modules needed for modules with submodules
- Consistency with project conventions improves maintainability

**Implementation:**
```rust
fn choose_module_structure(target_path: &ModPath, project_root: &Path) -> ModuleStructure {
    // 1. Check if target module needs submodules
    if has_submodules(target_path) {
        return ModuleStructure::Directory; // foo/mod.rs
    }

    // 2. Detect existing project style
    if let Some(style) = detect_existing_style(project_root) {
        return style;
    }

    // 3. Default to file-based for Edition 2018+
    ModuleStructure::File // foo.rs
}
```

**Alternatives considered:**
1. Always use file-based modules - Rejected: doesn't work for modules with submodules
2. Always use directory modules - Rejected: unnecessary complexity for simple modules
3. Prompt user for preference - Rejected: interrupts flow, adds complexity

### Decision 3: Associated Item Handling

**What:** Move impl blocks defined in the same file along with the type definition.

**Why:**
- Impl blocks are semantically associated with the type
- Splitting type and impls across files is poor Rust style
- Users expect methods to move with their types

**Implementation:**
```rust
fn collect_associated_items(def: &Definition, file: &SourceFile) -> Vec<ImplBlock> {
    // Find all impl blocks in the same file that implement `def`
    file.syntax()
        .descendants()
        .filter_map(ast::Impl::cast)
        .filter(|impl_| impl_for_type(impl_) == def)
        .collect()
}
```

**Edge case:** Impl blocks in different files (e.g., spread across module hierarchy) - Keep in original files, only move those in the same source file.

**Alternatives considered:**
1. Never move impl blocks - Rejected: breaks code, poor UX
2. Move all impl blocks workspace-wide - Rejected: may break intentional organization
3. Prompt user for each impl - Rejected: too verbose

### Decision 4: External Reference Update Strategy

**What:** Reuse existing `find_usages` infrastructure to locate all references to the moved item from other files, then update import paths and qualified references.

**Why:**
- Existing code already finds all usages reliably
- Handles complex cases (re-exports, glob imports, etc.)
- Consistent with current rename behavior

**Implementation:**
```rust
fn update_external_references(
    def: &Definition,
    old_path: &ModPath,
    new_path: &ModPath,
    sema: &Semantics<RootDatabase>,
) -> Vec<TextEdit> {
    let usages = def.find_usages(sema);

    usages.iter().map(|usage| {
        match usage.kind {
            UsageKind::Import => rewrite_import(usage, old_path, new_path),
            UsageKind::QualifiedReference => rewrite_qualified_path(usage, old_path, new_path),
            _ => TextEdit::replace(usage.range, new_path.to_string()),
        }
    }).collect()
}
```

**Challenges:**
- Relative imports (super::, self::) need context-aware rewriting
- Re-exports may need updating if they re-export the moved item

**Alternatives considered:**
1. Only update direct imports, let user fix the rest - Rejected: incomplete operation
2. Use import optimization pass - Considered: may be useful for cleanup but orthogonal

### Decision 5: Internal Reference Update Strategy

**What:** Analyze references within the moved item's code to items in the source module and add/update imports to maintain correctness after the move.

**Why:**
- Moving an item changes its module context, breaking previously-valid unqualified references
- References like `helper()` or `Helper` that worked in the source module need explicit imports in the destination
- Relative paths (`super::`, `self::`) become invalid when module hierarchy changes
- Critical for maintaining compilable code after the move

**Implementation:**
```rust
fn update_internal_references(
    moved_syntax: &SyntaxNode,
    source_module: &Module,
    dest_module: &Module,
    sema: &Semantics<RootDatabase>,
) -> (SyntaxNode, Vec<Use>) {
    let mut new_imports = Vec::new();
    let updated_syntax = moved_syntax.clone();

    // Find all path references within the moved code
    for path in updated_syntax.descendants().filter_map(ast::Path::cast) {
        if let Some(resolution) = sema.resolve_path(&path) {
            // Check if reference is to an item in the source module
            if is_in_module(&resolution, source_module) && !is_being_moved(&resolution) {
                // Add import for this item
                new_imports.push(Use::new(resolution.canonical_path()));
            }

            // Update relative paths (super::, self::) that are now incorrect
            if path.is_relative() {
                let new_path = recalculate_relative_path(
                    &resolution,
                    source_module,
                    dest_module,
                );
                // Update path in syntax tree
            }
        }
    }

    (updated_syntax, new_imports)
}
```

**Key cases handled:**
1. **Unqualified references** - Add `use crate::source_mod::Item;`
2. **Qualified references** - Update `source_mod::helper()` → `crate::source_mod::helper()`
3. **Self references** - Update `self::Item` → `crate::source_mod::Item`
4. **Super references** - Recalculate based on new module hierarchy
5. **Preserve external imports** - Keep `use std::*` unchanged
6. **Self/self within impl** - No change needed (still valid)

**Challenges:**
- Need to distinguish between items being moved together (e.g., struct + impl) vs staying behind
- Glob imports (`use source_mod::*;`) may hide which items need explicit imports
- Macros may expand to use items from source module (difficult to detect statically)

**Alternatives considered:**
1. Don't update internal references, let user fix - Rejected: results in broken code
2. Always use absolute paths - Considered: valid approach, may be verbose
3. Run import optimization after move - Considered: complementary but doesn't solve correctness

### Decision 6: File System Operations Representation

**What:** Extend `SourceChange` to include file creation operations, similar to existing module rename support.

**Why:**
- SourceChange already supports file system edits for module renames
- LSP WorkspaceEdit supports file creation (CreateFile resource operation)
- Maintains consistency with existing architecture

**Implementation:**
```rust
// In ide_db::source_change
pub struct SourceChange {
    pub source_file_edits: Vec<TextEdit>,
    pub file_system_edits: Vec<FileSystemEdit>, // Already exists
    // ...
}

pub enum FileSystemEdit {
    CreateFile { dst: FileId, initial_contents: String },
    MoveFile { src: FileId, dst: FileId },
    // ... existing variants
}
```

**Alternatives considered:**
1. Return file ops separately from SourceChange - Rejected: requires API changes
2. Let client handle file creation - Rejected: breaks atomicity

### Decision 7: Visibility Validation and Updates

**What:** Validate that external items referenced by moved code remain visible (without changing their visibility), and update the moved item's visibility to minimum required level for all its external references.

**Why:**
- Moving an item changes module boundaries, affecting Rust's privacy rules
- Cannot change visibility of items we don't own (external dependencies)
- Must maintain compilation after move
- Should preserve principle of least privilege (minimal visibility)

**Implementation:**

```rust
fn validate_external_visibility(
    moved_item: &Definition,
    source_module: &Module,
    dest_module: &Module,
    sema: &Semantics<RootDatabase>,
) -> Result<()> {
    // Find all items referenced within moved_item
    for referenced_item in find_internal_references(moved_item, sema) {
        // Skip items that are moving together
        if is_being_moved(&referenced_item) {
            continue;
        }

        let visibility = get_visibility(&referenced_item, sema);

        // Check if referenced_item will be visible from dest_module
        if !is_visible_from(referenced_item, dest_module, visibility, sema) {
            return Err(RenameError(format!(
                "Cannot move '{}': references {} item '{}' which will not be visible from '{}'",
                moved_item.name(),
                visibility_description(visibility),
                referenced_item.name(),
                dest_module.name(),
            )));
        }
    }
    Ok(())
}

fn calculate_required_visibility(
    moved_item: &Definition,
    dest_module: &Module,
    sema: &Semantics<RootDatabase>,
) -> Visibility {
    let usages = moved_item.find_usages(sema);
    let mut required = Visibility::Private;

    for usage in usages {
        let usage_module = usage.module();

        // Calculate minimum visibility needed for this usage
        let needed = min_visibility_between(dest_module, usage_module);

        // Take maximum of all required visibilities
        required = max_visibility(required, needed);
    }

    required
}

fn min_visibility_between(from_module: &Module, to_module: &Module) -> Visibility {
    if from_module == to_module {
        return Visibility::Private; // Same module
    }

    if same_crate(from_module, to_module) {
        return Visibility::PubCrate; // Different modules, same crate
    }

    Visibility::Pub // Different crates
}

fn update_visibility_modifier(
    item_syntax: &SyntaxNode,
    new_visibility: Visibility,
) -> TextEdit {
    let current_vis = extract_visibility(item_syntax);

    // If current visibility is already sufficient, don't change
    if visibility_is_sufficient(current_vis, new_visibility) {
        return TextEdit::empty();
    }

    // Replace or insert visibility modifier
    match current_vis {
        Some(vis_syntax) => TextEdit::replace(vis_syntax.range(), new_visibility.as_syntax()),
        None => TextEdit::insert(item_syntax.range().start(), format!("{} ", new_visibility.as_syntax())),
    }
}
```

**Key cases handled:**

1. **External visibility (items referenced by moved code):**
   - Private items in source module → Error (will be inaccessible)
   - `pub` items → OK (always visible)
   - `pub(crate)` items → OK within same crate
   - `pub(super)` items → Validate parent module relationship
   - `pub(in path)` items → Validate path accessibility

2. **Internal visibility (moved item itself):**
   - Private + no external refs → Stay private
   - Private + refs from other modules in same crate → Update to `pub(crate)`
   - Private + refs from other crates → Update to `pub` (if inter-crate moves supported)
   - `pub(crate)` + all refs within crate → Keep `pub(crate)`
   - Already `pub` → Keep `pub` (never downgrade)
   - Simple rule: Private < `pub(crate)` < `pub`

3. **Associated items in impl blocks:**
   - Methods/functions follow same rules
   - Update each associated item independently based on its usage

**Simplifications:**
- No `pub(super)` or `pub(in path)` generation (validation only)
- Only three visibility levels for updates: private, `pub(crate)`, `pub`
- Avoids complex common ancestor calculations

**Challenges:**
- Handling `pub(in path)` validation with complex module hierarchies
- Trait impl visibility rules (trait methods must match trait visibility)
- Field visibility in structs (may be referenced via patterns)

**Alternatives considered:**
1. Always make moved items `pub` - Rejected: violates least privilege
2. Support `pub(super)` and `pub(in path)` updates - Rejected: complex common ancestor calculation, rare benefit
3. Prompt user to manually set visibility - Considered: valid fallback for ambiguous cases
4. Don't update visibility, let compiler errors guide user - Rejected: poor UX
5. Allow changing external item visibility - Rejected: breaks encapsulation, unexpected side effects

### Decision 8: Transaction Atomicity

**What:** Validate all operations before creating any SourceChange. If validation fails, return error without partial edits.

**Why:**
- Prevents inconsistent workspace state
- LSP clients apply WorkspaceEdit atomically
- Matches user expectations (all-or-nothing)

**Implementation:**
```rust
fn rename_to_move(...) -> Result<SourceChange> {
    // Phase 1: Validate everything
    validate_destination_available()?;
    validate_no_circular_deps()?;
    validate_item_movable()?;
    validate_external_visibility()?;  // NEW

    // Phase 2: Build all edits
    let mut change = SourceChange::default();
    change.file_system_edits.extend(create_modules()?);
    change.source_file_edits.extend(move_definition()?);
    change.source_file_edits.extend(update_imports()?);
    change.source_file_edits.extend(update_visibility()?);  // NEW

    Ok(change)
}
```

**Alternatives considered:**
1. Apply changes incrementally - Rejected: hard to rollback, breaks atomicity
2. Use transactions at VFS level - Considered: may be useful but out of scope

## Architecture

### Component Interaction

```
LSP Client (textDocument/rename)
    ↓
handlers/request.rs::handle_rename()
    ↓
ide/lib.rs::Analysis::rename()
    ↓
ide/src/rename.rs::rename()
    ├─ detect_move_operation()
    │   └─ parse_rename_target() → RenameTarget::Move
    │
    ├─ [IF MOVE] orchestrate_move()
    │   ├─ validate_move_operation()
    │   │   ├─ validate_destination_available()
    │   │   ├─ validate_item_movable()
    │   │   └─ validate_external_visibility() ← NEW
    │   ├─ plan_module_structure()
    │   ├─ calculate_required_visibility() ← NEW
    │   ├─ create_file_system_edits()
    │   ├─ create_definition_relocation_edits()
    │   │   ├─ collect_associated_items() (impl blocks)
    │   │   ├─ update_internal_references()
    │   │   └─ update_visibility_modifiers() ← NEW
    │   ├─ create_external_reference_update_edits()
    │   └─ create_module_declaration_edits()
    │
    └─ [ELSE] existing simple rename flow
        └─ Definition::rename() (ide-db)
```

### New Modules/Functions

**ide/src/rename.rs:**
- `detect_move_operation(new_name: &str) -> Option<MoveOperation>`
- `orchestrate_move(def: Definition, move_op: MoveOperation) -> Result<SourceChange>`
- `validate_move_operation(...)`
- `plan_module_structure(...)`

**ide-db/src/rename.rs (or new ide-db/src/move_items.rs):**
- `struct MoveOperation { source_module, dest_module, new_name }`
- `create_module_files(dest_path: &ModPath) -> Vec<FileSystemEdit>`
- `relocate_item_definition(...) -> Vec<TextEdit>`
- `update_module_declarations(...) -> Vec<TextEdit>`
- `update_internal_references(...) -> (SyntaxNode, Vec<Use>)`
- `validate_external_visibility(...) -> Result<()>` ← NEW
- `calculate_required_visibility(...) -> Visibility` ← NEW
- `update_visibility_modifiers(...) -> Vec<TextEdit>` ← NEW

### Data Flow

1. User invokes rename with `crate::new_mod::NewName`
2. Parse target → detect it's a move operation
3. Resolve current item module path
4. Compare paths → determine source and destination modules
5. Validate:
   - Destination available (no name conflict)
   - Item is movable (not local, not external)
   - Module path is valid
   - External visibility: items referenced by moved code are accessible ← NEW
6. Calculate required visibility:
   - Find all usages of moved item ← NEW
   - Determine minimum visibility needed for each usage ← NEW
   - Select maximum (most permissive) required visibility ← NEW
7. Plan file operations:
   - What files/dirs to create
   - Where to insert module declarations
8. Generate edits:
   - FileSystemEdit for new files
   - TextEdit to remove item from source
   - Analyze internal references within moved item
   - Generate imports for internal references
   - Update visibility modifiers on moved item and associated items ← NEW
   - TextEdit to insert item (with updated references and visibility) in destination
   - TextEdit to update external imports/references
9. Return SourceChange → LSP WorkspaceEdit → Client applies

## Risks / Trade-offs

### Risk: Complex module structures edge cases
**Mitigation:** Start with simple cases (flat modules), expand iteratively. Comprehensive test coverage for edge cases.

### Risk: Performance impact on large workspaces
**Mitigation:** Reuse existing find_usages infrastructure (already optimized). Profile and optimize if needed.

### Risk: Breaking changes to SourceChange API
**Mitigation:** Extend existing structures rather than replacing. Maintain backward compatibility.

### Risk: Ambiguous user intent (move vs rename)
**Mitigation:** Clear heuristics (if path contains `::` and resolves differently, it's a move). Document behavior.

### Trade-off: Limited support for macros
**Rationale:** Macro expansion semantics are complex. Defer to future work rather than ship incomplete solution.

### Trade-off: No interactive module structure selection
**Rationale:** Simplifies UX and implementation. Follow conventions and project style detection. Power users can manually adjust afterward.

## Migration Plan

### Rollout
1. Ship feature behind experimental flag initially
2. Gather feedback on common workflows and edge cases
3. Enable by default once stable

### Backward Compatibility
- Existing simple renames unaffected (no `::` in new name)
- No breaking changes to LSP API (uses standard WorkspaceEdit)
- Existing rename tests continue to pass

### Rollback
- Feature can be disabled via config flag
- No persistent state changes (all edits via LSP WorkspaceEdit)

## Open Questions

1. **Should we support moving multiple items at once?**
   - Current: No, rename is single-item operation
   - Future: Could extend to multi-select rename

2. **How to handle re-exports when moving items?**
   - Option A: Update re-exports automatically
   - Option B: Leave re-exports, may cause breakage (forces user to decide)
   - **Lean toward A:** Update re-exports for smoother UX

3. **Should we clean up empty source files after moving last item?**
   - Option A: Delete empty files automatically
   - Option B: Leave cleanup to user
   - **Lean toward B:** Less risky, user may have comments/docs to preserve

4. **How to handle glob imports (`use parent::*;`)?**
   - Current behavior: find_usages may not catch these
   - Need to investigate: Does existing rename handle this correctly?

5. **Should we support moving to existing files with `pub use` re-export pattern?**
   - Scenario: User wants to move into `lib.rs` but keep old module path working via re-export
   - Defer to future enhancement

## Success Metrics

- Feature successfully moves items in 95%+ of test cases
- No performance regression on existing rename operations
- User feedback indicates improved code organization workflow
- Integration tests cover all documented scenarios
