# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**rust-analyzer** is a language server implementing the Language Server Protocol (LSP) for Rust. It provides IDE functionality including code completion, goto-definition, refactorings, and diagnostics. The codebase is a Cargo workspace with ~38 crates organized in a layered architecture.

**Minimum Rust version**: 1.88 (edition 2024)

## Common Commands

### Building & Running

```bash
# Build the project
cargo build

# Build release version
cargo build --release

# Run the language server
cargo run --bin rust-analyzer -- lsp-server

# Build specific crate
cargo check -p ide
cargo build -p rust-analyzer
```

### Testing

```bash
# Run all tests
cargo test --all

# Run tests for specific crate
cargo test -p hir-ty
cargo test -p ide-completion
cargo test -p ide-assists

# Update snapshot tests (IMPORTANT: use this when test expectations need updating)
UPDATE_EXPECT=1 cargo test

# Run single test
cargo test -p hir-ty --test main -- test_name

# Run fuzzer (requires nightly)
cargo xtask fuzz-tests
```

### Code Quality

```bash
# Format code
cargo fmt

# Run lints
cargo clippy --all

# Run tidy checks (custom project lints)
cargo xtask tidy
```

### xtask Commands

The `xtask` directory contains a custom build system for specialized tasks:

```bash
# Install rust-analyzer locally
cargo xtask install

# Build distributable binary
cargo xtask dist

# Regenerate code from grammar definitions
cargo xtask codegen

# Release management
cargo xtask release
```

## High-Level Architecture

### The Analysis Pipeline

```
Source Code → Parser → Syntax Tree → HIR → Type Checking → IDE Features → LSP Response
```

### Salsa Database (Core Concept)

The entire architecture is built on **Salsa**, an incremental computation framework that provides:
- On-demand, lazy evaluation
- Automatic dependency tracking
- Efficient re-computation when files change
- Full in-memory representation (no filesystem I/O after initialization)

### Layered Crate Structure

**Layer 1: Foundation**
- `parser` - Hand-written recursive descent parser (produces token stream → AST events)
- `syntax` - Full-fidelity syntax tree using rowan library
- `span`, `vfs`, `vfs-notify`, `paths`, `edition` - Infrastructure for source locations and file handling

**Layer 2: Database**
- `base-db` - Salsa database setup and input queries
- `intern`, `query-group-macro` - Support for Salsa queries

**Layer 3: Semantic Analysis (HIR)**
- `hir-expand` - Macro expansion (procedural and declarative)
- `hir-def` - Definition trees: functions, types, traits, modules
- `hir-ty` - Type system: inference, checking, trait solving
- `hir` - Public API wrapping hir-def/hir-ty in OOP style

**Layer 4: IDE Features**
- `ide-db` - Core IDE database: symbol indexing, search
- `ide-completion` - Code completion engine
- `ide-assists` - Refactoring suggestions (quick fixes)
- `ide-diagnostics` - Diagnostic generation
- `ide-ssr` - Structural Search and Replace
- `ide` - Main IDE API (hover, goto-def, document symbols, highlighting)

**Layer 5: LSP Server**
- `rust-analyzer` - LSP server implementation, main loop, request handlers
- `lsp-server` (lib/) - Generic LSP protocol handling

**Supporting Crates**
- `project-model` - Cargo workspace parsing, crate graph construction
- `load-cargo` - Loads Cargo projects into analyzer
- `proc-macro-api`, `proc-macro-srv` - Procedural macro expansion (separate process)
- `mbe`, `tt`, `syntax-bridge` - Macro-by-example and token tree handling

### Key Entry Points

- **Binary**: `/crates/rust-analyzer/src/bin/main.rs` - CLI parsing, LSP initialization
- **Main Loop**: `/crates/rust-analyzer/src/main_loop.rs` - Event multiplexing (LSP, VFS, flycheck)
- **Request Handlers**: `/crates/rust-analyzer/src/handlers/request.rs` - Implements ~50 LSP request types
- **Global State**: `/crates/rust-analyzer/src/global_state.rs` - Mutable server state

## Testing Infrastructure

### Snapshot Testing with expect-test

rust-analyzer uses snapshot tests extensively. Tests compare actual output against expected snapshots:

```rust
#[test]
fn my_test() {
    check_infer(
        r#"
fn main() {
    let x = 1;
}
        "#,
        expect![[r#"
            10..28 '{     ...= 1; }': ()
            20..21 'x': i32
            24..25 '1': i32
        "#]],
    );
}
```

**Updating snapshots**: Set `UPDATE_EXPECT=1` environment variable before running tests.

### Test Fixtures

Tests use a mini-language to express multiple files and crates:

```rust
check_types(
    r#"
//- minicore: sized, fn
// /lib.rs crate:foo
pub mod bar;
// /bar.rs
pub struct Bar;
// /main.rs crate:main deps:foo
use foo::Bar;
let x = Bar;
 // ^ Bar
    "#,
);
```

**Important fixture directives**:
- `//- minicore: flag1, flag2` - Include parts of mock stdlib (required for most tests)
- `// /path.rs crate:name deps:dep1,dep2` - Define crate structure
- `$0` - Cursor position (for hover/completion tests)
- `// ^^ label` - Attach type assertion to range

**Common minicore flags**: `sized`, `fn`, `iterator`, `option`, `result`, `unsize`, `coerce_unsized`

All minicore flags are documented in `crates/test-utils/src/minicore.rs`.

### Test Locations by Feature

- Type inference: `crates/hir-ty/src/tests/`
- Completion: `crates/ide-completion/src/tests/`
- Assists (refactorings): `crates/ide-assists/src/tests/`
- Diagnostics: `crates/ide-diagnostics/src/tests/`
- Syntax parsing: `crates/syntax/src/tests/`

## Architecture Invariants

These design principles guide all development:

1. **Parser Independence**: Parser is separate from tree representation. Same parser handles files and macro input.

2. **Syntax is Value**: Syntax trees are immutable values with no semantic info. No global state.

3. **HIR is OOP**: The `hir` crate provides clean OOP APIs, hiding ECS-style implementation details.

4. **IDE is Boundary**: Only the `ide` crate should be used by the LSP layer. It's the public API.

5. **Salsa for Incrementality**: All computation uses Salsa. Dependencies tracked automatically.

6. **No I/O in Analysis**: All I/O happens at boundaries. Core analyzer is pure functions.

7. **Parser Never Fails**: Parser produces `(Tree, Vec<Error>)` not `Result<Tree, Error>`.

## Development Workflow

### Adding a New IDE Feature

1. Implement core logic in `crates/ide/src/my_feature.rs`
2. Export function from `crates/ide/src/lib.rs`
3. Add LSP handler in `crates/rust-analyzer/src/handlers/request.rs`
4. Add tests using test fixtures
5. Update LSP capabilities in `crates/rust-analyzer/src/lsp/capabilities.rs` if needed

### Fixing a Bug

1. Write failing test with test fixture
2. Add tracing with `tracing::debug!()`
3. Run with `RA_LOG=debug cargo test -p <crate> -- test_name`
4. Fix in appropriate layer:
   - Parsing → `crates/parser` or `crates/syntax`
   - Name resolution → `crates/hir-def`
   - Type errors → `crates/hir-ty`
   - IDE feature → `crates/ide`
   - LSP protocol → `crates/rust-analyzer/src/handlers`

### Understanding a Component

1. Read `lib.rs` for module documentation
2. Look at tests for usage patterns
3. Check `Cargo.toml` dependencies
4. Follow imports to trace data structures
5. Run with logging: `RA_LOG=debug,salsa=info`

## Important Files

| File | Purpose | Location |
|------|---------|----------|
| Main entry point | Binary entry | `crates/rust-analyzer/src/bin/main.rs:361` |
| Main event loop | LSP event handling | `crates/rust-analyzer/src/main_loop.rs` |
| LSP request handlers | All LSP requests | `crates/rust-analyzer/src/handlers/request.rs` |
| Global state | Server state | `crates/rust-analyzer/src/global_state.rs` |
| IDE API | Public IDE interface | `crates/ide/src/lib.rs` |
| HIR API | High-level semantic model | `crates/hir/src/lib.rs` |
| Type system | Type inference/checking | `crates/hir-ty/src/lib.rs` |
| Definitions | Function/type/trait defs | `crates/hir-def/src/lib.rs` |
| Syntax tree | CST and AST | `crates/syntax/src/lib.rs` |
| Parser | Recursive descent parser | `crates/parser/src/lib.rs` |
| Base database | Salsa setup | `crates/base-db/src/lib.rs` |

## LSP Protocol Notes

### UTF-16 ↔ UTF-8 Conversion

LSP uses UTF-16 code units for positions; Rust uses UTF-8 bytes. Conversion happens in:
- `crates/rust-analyzer/src/lsp/from_proto.rs` - LSP → internal types
- `crates/rust-analyzer/src/lsp/to_proto.rs` - Internal → LSP types

### Request Dispatch Strategies

Three dispatch modes in `crates/rust-analyzer/src/handlers/dispatch.rs`:
- `on_sync_mut` - Synchronous with `&mut GlobalState` (for state modifications)
- `on_sync` - Synchronous with snapshot (latency-sensitive: hover, completion)
- `on` - Asynchronous on threadpool (read-only analysis)

## Resources

- Architecture docs: `docs/book/src/contributing/architecture.md`
- Testing guide: `docs/book/src/contributing/testing.md`
- YouTube series: [Explaining Rust Analyzer](https://www.youtube.com/playlist?list=PLhb66M_x9UmrqXhQuIpWC5VgTdrGxMx3y)
- Blog posts: https://rust-analyzer.github.io/blog/
- API docs: https://rust-lang.github.io/rust-analyzer/ide/
