# Project Context

## Purpose
rust-analyzer is a language server that provides IDE functionality for writing Rust programs. It implements the Language Server Protocol (LSP) and can be used with any editor that supports LSP (VS Code, Vim, Emacs, Zed, etc).

**Core Features:**
- Go-to-definition and find-all-references
- Code refactorings and intelligent code completion
- Integrated formatting (with rustfmt)
- Integrated diagnostics (with rustc and clippy)

**Architecture Philosophy:**
Internally structured as a set of libraries for analyzing Rust code. See [Architecture docs](https://rust-analyzer.github.io/book/contributing/architecture.html) for details.

## Tech Stack
- **Language:** Rust (edition 2024, MSRV 1.88)
- **Parser:** Custom parser using rowan (green/red tree)
- **Type System:** Salsa-based incremental computation framework
- **LSP:** Custom lsp-server implementation
- **Build System:** Cargo workspace with 30+ crates
- **Key Dependencies:**
  - rowan 0.15.17 (syntax tree library)
  - salsa 0.24.0 (incremental computation)
  - ra-ap-rustc_* crates (rustc internals at version 0.133)
  - crossbeam-channel, rayon (concurrency)
  - serde/serde_json (serialization)

## Project Conventions

### Code Style
- **Linting:** Comprehensive clippy lints configured in workspace Cargo.toml
  - Deny: correctness, perf
  - Warn: complexity, style, suspicious
  - Special warnings: dbg_macro, todo, print_stdout, print_stderr (raised to deny in CI)
- **Rust Lints:**
  - Warn on: elided_lifetimes_in_paths, explicit_outlives_requirements, unsafe_op_in_unsafe_fn, unused_extern_crates, unused_lifetimes, unreachable_pub
- **Naming:** Standard Rust conventions (snake_case for functions/variables, PascalCase for types)
- **Safety:** unsafe_op_in_unsafe_fn enforced - all unsafe operations must be in unsafe blocks with comments

### Architecture Patterns
- **Layered Architecture:** Clear separation between crates
  - Parser layer (parser, syntax, tt)
  - HIR layer (hir, hir-def, hir-expand, hir-ty)
  - IDE layer (ide, ide-assists, ide-completion, ide-diagnostics, ide-db, ide-ssr)
  - Base infrastructure (base-db, vfs, stdx, paths, intern, span)
- **Query-Based:** Salsa framework for incremental, cached computation
- **Immutable Data Structures:** Heavy use of Arcs and persistent data structures
- **Intern Everything:** String interning and other interning patterns for memory efficiency

### Testing Strategy
- **Unit Tests:** Per-crate test modules
- **Fixture-Based Testing:** test-fixture and test-utils crates for creating test scenarios
- **expect-test:** Snapshot testing using expect-test crate
- **Performance:** Profile-guided optimization for critical paths
- **CI:** Tests run on multiple platforms, clippy lints enforced

### Git Workflow
- **Main Branch:** `master`
- **PR Process:** Pull requests required, reviewed before merge
- **Issue Labels:**
  - C-bug: Bug fixes
  - C-enhancement: Feature improvements
- **Communication:**
  - Bug reports/troubleshooting: Rust forum IDEs and Editors category
  - Development discussion: Zulip stream (t-compiler/rust-analyzer)
- **Feature Development:** Open issue first for new features to confirm scope/acceptance

## Domain Context
**Language Server Protocol:**
rust-analyzer implements LSP to provide IDE features. Understanding LSP request/response patterns and capabilities is essential.

**Rust Compiler Internals:**
Uses rustc internal APIs (via ra-ap-rustc_* crates) for:
- Lexing and parsing
- Type analysis and pattern matching
- ABI and trait solving

**Incremental Compilation:**
Salsa framework enables incremental, demand-driven computation with automatic dependency tracking and memoization.

**Syntax Trees:**
Uses rowan's lossless, immutable syntax trees (red/green tree pattern):
- Green nodes: Shared, immutable syntax data
- Red nodes: Provide parent pointers and absolute positions

## Important Constraints
- **Performance:** Must handle large codebases (100k+ LOC) with low latency
- **Memory:** Careful memory management due to IDE long-running nature
- **Compatibility:** Must work across multiple editors and platforms
- **Stability:** API stability for published lib crates (line-index, la-arena, lsp-server)
- **Rust Version:** MSRV 1.88, edition 2024
- **Security:** See security and privacy sections in manual
- **License:** Dual MIT OR Apache-2.0

## External Dependencies
- **rustc:** Uses rustc internals via ra-ap-rustc_* family of crates
- **rustfmt:** Integrated for code formatting
- **clippy:** Integrated for diagnostics
- **cargo:** cargo_metadata crate for reading Cargo.toml and workspace structure
- **Editors:** VS Code, Vim, Emacs, Zed, and other LSP clients
- **Documentation:** Hosted at https://rust-analyzer.github.io/
- **Metrics:** https://rust-analyzer.github.io/metrics/
- **API Docs:** https://rust-lang.github.io/rust-analyzer/ide/
