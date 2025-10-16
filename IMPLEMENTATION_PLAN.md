# Move-and-Rename Enhancement Implementation Plan

## Current Status ✅
- **Path Detection**: Qualified paths like `crate::foo::bar::Item` are detected ✅
- **Item Movement**: Items move between existing modules ✅
- **Basic Reference Updates**: All references updated to use fully-qualified paths ✅
- **Tests**: 2 basic tests passing ✅

## Phase 1: Smart Import Management 🎯 **NEXT**

### Goal
Replace fully-qualified paths (`crate::bar::Final`) with proper use statements and short names (`Final`).

### Implementation Steps

#### 1.1 Update Reference Strategy (crates/ide/src/rename.rs, lines 685-709)

**Current Code** (lines 685-709):
```rust
// Find all usages of the moved item
let usages = def.usages(sema).all();

// Build the fully qualified path to the new location
let mut new_path = String::new();
for segment in module_path {
    if !new_path.is_empty() {
        new_path.push_str("::");
    }
    new_path.push_str(segment.as_str());
}
if !new_path.is_empty() {
    new_path.push_str("::");
}
new_path.push_str(item_name.as_str());

// Update all references to use the new path
for (file_id, references) in usages.iter() {
    builder.edit_file(file_id.file_id(db));

    for reference in references {
        // Replace the reference with the fully qualified path
        builder.replace(reference.range, new_path.clone());
    }
}
```

**New Implementation**:
```rust
use hir::{ItemInNs, ModuleDefId};
use ide_db::imports::insert_use::{insert_use, InsertUseConfig, ImportGranularity};
use ide_db::imports::ImportScope;
use hir_def::FindPathConfig;

// Find all usages of the moved item
let usages = def.usages(sema).all();

// Convert definition to ItemInNs for find_path
let item_in_ns = match def {
    Definition::Adt(adt) => ItemInNs::Types(ModuleDefId::from(adt)),
    Definition::Function(func) => ItemInNs::Values(ModuleDefId::from(func)),
    Definition::Const(c) => ItemInNs::Values(ModuleDefId::from(c)),
    Definition::Static(s) => ItemInNs::Values(ModuleDefId::from(s)),
    Definition::Trait(t) => ItemInNs::Types(ModuleDefId::from(t)),
    Definition::TypeAlias(ta) => ItemInNs::Types(ModuleDefId::from(ta)),
    Definition::Macro(m) => ItemInNs::Macros(m.id),
    _ => bail!("Unsupported item type for move-and-rename"),
};

// Group references by file
for (file_id, references) in usages.iter() {
    let file_id = file_id.file_id(db);
    builder.edit_file(file_id);

    // Parse the file to get syntax tree
    let source_file = sema.parse_guess_edition(file_id);

    // Find import scope for this file
    if let Some(import_scope) = ImportScope::find_insert_use_container(
        source_file.syntax(),
        sema
    ) {
        // Find the module containing the first reference
        let first_ref_module = {
            let token = source_file.syntax()
                .token_at_offset(references[0].range.start())
                .next();
            token.and_then(|t| sema.scope(t.parent()?).module())
        };

        if let Some(ref_module) = first_ref_module {
            // Find optimal path from this module to the moved item
            let find_path_config = FindPathConfig {
                prefer_no_std: false,
                prefer_prelude: false,
                prefer_absolute: false,
                allow_unstable: true,
            };

            if let Some(mod_path) = ref_module.find_path(
                db,
                item_in_ns,
                find_path_config
            ) {
                // Add use statement
                let use_tree = make::use_tree(
                    make::path_from_segments(
                        mod_path.segments().iter().map(|s| s.to_string()),
                        false
                    ),
                    None,
                    None,
                    false
                );

                let config = InsertUseConfig {
                    granularity: ImportGranularity::Crate,
                    prefix_kind: hir::PrefixKind::Plain,
                    enforce_granularity: true,
                    group: true,
                    skip_glob_imports: true,
                };

                insert_use(&import_scope, use_tree, &config);

                // Replace references with short name
                for reference in references {
                    builder.replace(reference.range, item_name.to_string());
                }
            } else {
                // Fallback to fully qualified path if find_path fails
                let full_path = format!("{}::{}",
                    module_path.iter().map(|n| n.as_str()).collect::<Vec<_>>().join("::"),
                    item_name.as_str()
                );
                for reference in references {
                    builder.replace(reference.range, full_path.clone());
                }
            }
        }
    } else {
        // No import scope (e.g., in expression context), use qualified path
        let full_path = format!("{}::{}",
            module_path.iter().map(|n| n.as_str()).collect::<Vec<_>>().join("::"),
            item_name.as_str()
        );
        for reference in references {
            builder.replace(reference.range, full_path.clone());
        }
    }
}
```

#### 1.2 Add Required Imports

At the top of `crates/ide/src/rename.rs`, add:
```rust
use ide_db::imports::insert_use::{insert_use, InsertUseConfig, ImportGranularity, ImportScope};
use hir_def::FindPathConfig;
use syntax::ast::make;
```

#### 1.3 Handle Definition Conversion Helper

Add this helper function before `move_and_rename`:
```rust
/// Converts a Definition to ItemInNs for path finding
fn def_to_item_in_ns(def: Definition) -> RenameResult<ItemInNs> {
    use hir::{ItemInNs, ModuleDefId};

    Ok(match def {
        Definition::Adt(adt) => ItemInNs::Types(ModuleDefId::from(adt)),
        Definition::Function(func) => ItemInNs::Values(ModuleDefId::from(func)),
        Definition::Const(c) => ItemInNs::Values(ModuleDefId::from(c)),
        Definition::Static(s) => ItemInNs::Values(ModuleDefId::from(s)),
        Definition::Trait(t) => ItemInNs::Types(ModuleDefId::from(t)),
        Definition::TypeAlias(ta) => ItemInNs::Types(ModuleDefId::from(ta)),
        Definition::Macro(m) => ItemInNs::Macros(m.id),
        _ => bail!("Unsupported item type for move-and-rename: {:?}", def),
    })
}
```

### Testing Phase 1

Add these tests to `crates/ide/src/rename.rs`:

```rust
#[test]
fn test_move_with_use_statement() {
    let (analysis, position) = fixture::position(
        r#"
//- /lib.rs
struct Fi$0nal { value: i32 }

fn usage() {
    let x = Final { value: 42 };
}

mod bar;
//- /bar.rs
// Empty module
        "#,
    );

    let result = analysis.rename(position, "crate::bar::Final").unwrap();
    assert!(result.is_ok());

    let source_change = result.unwrap();

    // Apply edits and verify
    // Expected: lib.rs has "use crate::bar::Final;" and uses "Final { value: 42 }"
    // Expected: bar.rs has "struct Final { value: i32 }"
}

#[test]
fn test_move_multiple_references_with_imports() {
    let (analysis, position) = fixture::position(
        r#"
//- /lib.rs
struct Fi$0nal;

fn usage1() {
    let x = Final;
}

fn usage2() {
    let y = Final;
}

mod bar;
//- /bar.rs
// Empty module
        "#,
    );

    let result = analysis.rename(position, "crate::bar::Final").unwrap();
    assert!(result.is_ok());

    // Should have use statement and both references using short name
}
```

---

## Phase 2: Automatic Module Creation 🎯

### Goal
Create missing modules and files automatically when moving to non-existent paths.

### Implementation Steps

#### 2.1 Update `resolve_target_module` Function

**Current** (lines 714-755):
```rust
fn resolve_target_module(
    db: &RootDatabase,
    from_module: hir::Module,
    path: &[Name],
) -> RenameResult<hir::Module> {
    // ... traverses existing modules, bails if not found
}
```

**New** - Rename and extend:
```rust
fn resolve_or_create_target_module(
    db: &RootDatabase,
    sema: &Semantics<'_, RootDatabase>,
    builder: &mut SourceChangeBuilder,
    from_module: hir::Module,
    path: &[Name],
) -> RenameResult<hir::Module> {
    use ide_db::base_db::AnchoredPathBuf;
    use syntax::ast::make;

    if path.is_empty() {
        return Ok(from_module);
    }

    let mut current_module = if path.first().map(|n| n.as_str()) == Some("crate") {
        from_module.crate_root(db)
    } else {
        from_module
    };

    let path_segments = if path.first().map(|n| n.as_str()) == Some("crate") {
        &path[1..]
    } else {
        path
    };

    // Traverse and create missing modules
    for (idx, segment_name) in path_segments.iter().enumerate() {
        let children = current_module.children(db);
        let existing = children.into_iter()
            .find(|child| child.name(db).as_ref() == Some(segment_name));

        match existing {
            Some(child) => {
                current_module = child;
            }
            None => {
                // Module doesn't exist - create it
                current_module = create_module(
                    db,
                    sema,
                    builder,
                    current_module,
                    segment_name,
                    &path_segments[idx+1..],  // remaining path
                )?;
            }
        }
    }

    Ok(current_module)
}
```

#### 2.2 Implement Module Creation Helper

```rust
/// Creates a new module file and adds module declaration to parent
fn create_module(
    db: &RootDatabase,
    sema: &Semantics<'_, RootDatabase>,
    builder: &mut SourceChangeBuilder,
    parent_module: hir::Module,
    module_name: &Name,
    _remaining_path: &[Name],  // for future nested creation
) -> RenameResult<hir::Module> {
    use hir::ModuleSource;
    use ide_db::base_db::AnchoredPathBuf;
    use syntax::ast::make;
    use stdx::format_to;

    // Get parent module's file
    let parent_source = parent_module.definition_source(db);
    let parent_file_id = parent_source.file_id.original_file(db).file_id(db);

    // Determine the path for the new module file
    let module_path = match parent_source.value {
        ModuleSource::SourceFile(_) => {
            // Parent is a file-based module
            // Determine if parent is mod.rs or name.rs
            let is_mod_rs = parent_module.is_mod_rs(db);

            let mut path = String::from("./");

            if !is_mod_rs {
                // Parent is foo.rs, create foo/bar.rs
                if let Some(parent_name) = parent_module.name(db) {
                    format_to!(path, "{}/", parent_name.as_str());
                }
            }
            // Parent is mod.rs, create bar.rs in same directory

            format_to!(path, "{}.rs", module_name.as_str());
            path
        }
        ModuleSource::Module(_) => {
            // Parent is inline module - not supported yet
            bail!("Cannot create modules inside inline modules");
        }
        ModuleSource::BlockExpr(_) => {
            bail!("Cannot create modules inside block expressions");
        }
    };

    // Create the new module file
    let anchor = parent_file_id;
    let dst = AnchoredPathBuf { anchor, path: module_path };
    builder.create_file(dst, String::new());

    // Add module declaration to parent file
    builder.edit_file(parent_file_id);

    let parent_syntax = sema.parse_guess_edition(parent_file_id);

    // Find insertion point (after existing mod declarations, or at end)
    let insert_position = parent_syntax.syntax()
        .children()
        .filter_map(|node| syntax::ast::Item::cast(node))
        .filter_map(|item| match item {
            syntax::ast::Item::Module(_) => Some(item.syntax().text_range().end()),
            _ => None,
        })
        .last()
        .unwrap_or_else(|| parent_syntax.syntax().text_range().end());

    // Insert module declaration
    let mod_decl = format!("\nmod {};", module_name.as_str());
    builder.insert(insert_position, mod_decl);

    // Return a placeholder - HIR won't be updated until after this transaction
    // The caller should handle this limitation
    bail!("Module created successfully, but HIR not yet updated. Please re-run the operation.")
}
```

#### 2.3 Update `move_and_rename` Call Site

Change line 631-635 from:
```rust
let target_module = resolve_target_module(
    db,
    current_module,
    module_path,
)?;
```

To:
```rust
let target_module = resolve_or_create_target_module(
    db,
    sema,
    &mut builder,
    current_module,
    module_path,
)?;
```

Note: Move `builder` creation earlier (before this call).

### Testing Phase 2

```rust
#[test]
fn test_create_single_module() {
    let (analysis, position) = fixture::position(
        r#"
//- /lib.rs
struct Fi$0nal;

fn usage() {
    let x = Final;
}
        "#,
    );

    let result = analysis.rename(position, "crate::newmod::Final").unwrap();

    // First attempt should create the module
    // May need to be a two-step process due to HIR limitations
}

#[test]
fn test_create_nested_modules() {
    let (analysis, position) = fixture::position(
        r#"
//- /lib.rs
struct Fi$0nal;
        "#,
    );

    let result = analysis.rename(position, "crate::a::b::c::Final").unwrap();

    // Should create a.rs, a/b.rs, a/b/c.rs and proper mod declarations
}
```

---

## Phase 3: Enhanced Testing 🎯

### Additional Test Cases

```rust
// Different item types
#[test]
fn test_move_enum() { /* move enum with variants */ }

#[test]
fn test_move_trait() { /* move trait definition */ }

#[test]
fn test_move_function() { /* move standalone function */ }

// Edge cases
#[test]
fn test_move_with_generics() { /* struct with type parameters */ }

#[test]
fn test_move_with_attributes() { /* #[derive], #[cfg], etc. */ }

#[test]
fn test_move_with_doc_comments() { /* preserve /// comments */ }

#[test]
fn test_move_deep_nesting() { /* crate::a::b::c::d::e::Item */ }

// Error cases
#[test]
fn test_move_conflict_detection() { /* item exists in target */ }

#[test]
fn test_move_visibility_check() { /* pub item to private module */ }
```

---

## Phase 4: Inline Module Support (Optional) 🎯

### Goal
Support moving into `mod foo { }` style modules

### Implementation

In `move_and_rename`, modify the `ModuleSource::Module` case:

```rust
ModuleSource::Module(module_ast) => {
    // Find the item list
    let item_list = module_ast.item_list()
        .ok_or_else(|| format_err!("Inline module has no item list"))?;

    // Get insertion point (before closing brace)
    let insert_pos = item_list.r_curly_token()
        .map(|t| t.text_range().start())
        .unwrap_or_else(|| item_list.syntax().text_range().end());

    // Insert with proper indentation
    let indent = IndentLevel::from_node(module_ast.syntax());
    let indented_item = format!("\n{}{}\n{}",
        indent.indent_str(),
        item_text,
        indent.indent_str()
    );

    builder.insert(insert_pos, indented_item);

    // Continue with reference updates...
}
```

---

## Implementation Order & Time Estimates

1. **Phase 1: Smart Imports** - 2-3 hours
   - Most impactful improvement
   - Builds on existing foundation
   - Clear testing strategy

2. **Phase 2: Module Creation** - 3-4 hours
   - More complex due to HIR limitations
   - May require two-step user workflow
   - File path logic is intricate

3. **Phase 3: Enhanced Testing** - 2-3 hours
   - Ensures robustness
   - Catches edge cases
   - Validates all item types

4. **Phase 4: Inline Modules** - 1-2 hours
   - Nice-to-have feature
   - Simpler than file-based
   - Lower priority

**Total Estimated Time**: 8-12 hours

---

## Key Technical Challenges

### Challenge 1: HIR Update Lag
**Problem**: Created modules don't exist in HIR until after the transaction.
**Solution**:
- Detect this scenario and return a helpful error
- User re-runs the operation (modules now exist)
- Or implement a two-phase approach

### Challenge 2: Import Scope Detection
**Problem**: References in different contexts may not have an import scope.
**Solution**:
- Fallback to qualified paths when no import scope found
- Handle block-level code appropriately

### Challenge 3: Path Ambiguity
**Problem**: Multiple valid paths might exist to the same item.
**Solution**:
- Trust `find_path()` to return the best one
- It already handles prelude, re-exports, etc.

---

## Testing Strategy

1. **Unit Tests**: Test each helper function individually
2. **Integration Tests**: Test full move-and-rename workflows
3. **Edge Case Tests**: Cover all item types, nesting levels, conflicts
4. **Regression Tests**: Ensure existing functionality still works

---

## Success Criteria

### Must Have ✅
- References use `use` statements instead of qualified paths
- Can create missing modules (even if two-step)
- 15+ comprehensive tests passing
- No regressions in existing rename functionality

### Nice to Have 🎯
- Single-step module creation (if HIR allows)
- Inline module support
- Conflict detection
- Visibility warnings

---

## Getting Started

1. **Start with Phase 1** - It provides the most value and builds on existing code
2. **Test incrementally** - Add tests as you implement each feature
3. **Commit frequently** - Keep commits small and focused
4. **Document limitations** - Be clear about HIR update lag and workarounds

---

## References

- `crates/hir-def/src/find_path.rs` - Path computation
- `crates/ide-db/src/imports/insert_use.rs` - Import insertion
- `crates/ide-assists/src/handlers/move_module_to_file.rs` - Module file creation patterns
- Current implementation: `crates/ide/src/rename.rs` lines 587-755

Good luck! 🚀
