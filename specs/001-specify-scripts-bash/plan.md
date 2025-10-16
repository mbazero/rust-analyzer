# Implementation Plan: Move Item Refactoring

**Branch**: `001-specify-scripts-bash` | **Date**: 2025-10-16 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-specify-scripts-bash/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Extend rust-analyzer's rename action to support moving items between modules using fully-qualified paths. When a developer renames an item to a fully-qualified path that differs from its current location, the system will move the item to the target module, create necessary files and directories, update all imports and references, and optionally rename the item. This enables seamless code reorganization without manual file operations or import updates.

## Technical Context

**Language/Version**: Rust (rust-analyzer workspace uses latest stable)
**Primary Dependencies**:
- `hir` crate (semantic analysis, module resolution)
- `ide-db` crate (search, rename infrastructure, source changes)
- `syntax` crate (AST parsing and manipulation)
- `vfs` crate (virtual file system operations)
- `base-db` crate (salsa incremental computation)

**Storage**: N/A (operates on in-memory representation with file system side effects)
**Testing**: `expect-test` for snapshot testing at IDE boundary (middle tier)
**Target Platform**: Cross-platform (Linux, macOS, Windows via rust-analyzer LSP)
**Project Type**: Single workspace (rust-analyzer monorepo)
**Performance Goals**: Complete move operations in under 10 seconds, maintain IDE responsiveness during operation
**Constraints**:
- Must use salsa for incremental computation
- Must support cancellation via revision counters
- Must handle broken code gracefully (return errors, don't fail)
- Must preserve all item metadata (attributes, doc comments, visibility)

**Scale/Scope**:
- Single-crate moves (cross-crate moves not in scope)
- Support all moveable item types (structs, enums, functions, traits, type aliases, constants, statics)
- Handle projects from small (100s of files) to large (10,000+ files)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### I. Code Quality First ✅

- **Correctness**: Feature extends existing rename infrastructure (battle-tested)
- **Performance**: Uses salsa for incremental computation, no blocking operations
- **Collections**: Will use `FxHashMap`/`FxHashSet` as required
- **Module docs**: All new modules will have `//!` documentation
- **No debug macros**: Production code will avoid `dbg!`, `todo!`, etc.

**Status**: PASS - Feature aligns with existing quality standards

### II. Three-Tier Testing Architecture ✅

- **Primary testing tier**: Middle tier (IDE boundary) using `expect-test`
- **Test location**: `crates/ide-assists/src/handlers/move_item.rs` with inline tests
- **Test format**: Fixture-based multi-file tests with before/after snapshots
- **Pattern**: Follows existing assists testing (e.g., `move_module_to_file`)

**Status**: PASS - Will use appropriate testing tier

### III. Performance-First Design ✅

- **Incremental**: Uses `Semantics` and salsa queries (no global invalidation)
- **Cancellable**: All operations support cancellation via `Canceled::throw()`
- **Memory-resident**: Operates on in-memory AST and HIR
- **No I/O after startup**: File operations happen via VFS layer
- **Profiling support**: Compatible with `RA_PROFILE` instrumentation

**Status**: PASS - Design preserves incremental computation invariant

### IV. Robust Error Handling ✅

- **Graceful degradation**: Validates before making changes, provides clear errors
- **No assertions**: Uses `always!()` and `never!()` for conditions
- **Partial results**: Returns diagnostics for validation failures
- **Rollback**: Uses `SourceChange` transaction model

**Status**: PASS - Error handling follows rust-analyzer patterns

### V. User Experience Consistency ✅

- **Editor terminology**: Uses `FileRange`, `TextEdit`, LSP types in APIs
- **Resilient**: Validates path syntax and visibility before executing
- **Stateless**: Operates as single LSP request/response
- **Atomic**: All changes bundled in single `SourceChange`

**Status**: PASS - UX consistent with existing features

### Overall Gate Status: ✅ PASS

All constitutional principles satisfied. Proceed to Phase 0 research.

## Project Structure

### Documentation (this feature)

```
specs/001-specify-scripts-bash/
├── plan.md              # This file (/speckit.plan command output)
├── spec.md              # Feature specification (complete)
├── research.md          # Phase 0 output (generated below)
├── data-model.md        # Phase 1 output (generated below)
├── quickstart.md        # Phase 1 output (generated below)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```
crates/
├── ide/
│   └── src/
│       └── rename.rs              # Entry point: detect fully-qualified paths
├── ide-assists/
│   └── src/
│       └── handlers/
│           └── move_item.rs       # NEW: Core move item logic
├── ide-db/
│   └── src/
│       ├── rename.rs              # Extend: Support cross-file moves
│       ├── search.rs              # Use: Find all references
│       ├── source_change.rs       # Use: FileSystemEdit operations
│       └── imports/
│           ├── insert_use.rs      # Use: Add imports
│           └── import_assets.rs   # Use: Import analysis
├── hir/
│   └── src/
│       └── lib.rs                 # Use: Module resolution, path operations
└── syntax/
    └── src/
        └── ast.rs                 # Use: AST manipulation

tests/ (via inline tests in assists)
```

**Structure Decision**: This is an enhancement to existing rust-analyzer infrastructure. Core logic will live in `ide-assists/src/handlers/move_item.rs` as a new assist, extending the existing rename infrastructure in `ide/src/rename.rs` and `ide-db/src/rename.rs`. Testing will follow the established pattern of inline fixture tests within the assist module.

## Complexity Tracking

*No constitutional violations - this section is not needed*
