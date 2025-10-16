<!--
Sync Impact Report:
- Version change: (initial) → 1.0.0
- Principles added:
  * Code Quality First (correctness and performance over style)
  * Three-Tier Testing Architecture (integration, IDE, HIR boundaries)
  * Performance-First Design (incremental computation, memory-resident)
  * Robust Error Handling (compute errors, never fail)
  * User Experience Consistency (IDE responsiveness, incremental updates)
- Sections added:
  * Quality Assurance Standards
  * Development Workflow
  * Governance
- Templates requiring updates:
  ✅ plan-template.md (Constitution Check section aligns)
  ✅ spec-template.md (Requirements and testing align)
  ✅ tasks-template.md (Test-first and quality tasks align)
- Follow-up TODOs: None
-->

# rust-analyzer Constitution

## Core Principles

### I. Code Quality First

**Code correctness and performance are non-negotiable; style and complexity are important but secondary.**

All code MUST:
- Pass clippy's `correctness` and `perf` lints (deny level)
- Use `FxHashMap`/`FxHashSet` instead of standard collections
- Avoid `unsafe` operations without explicit `unsafe` blocks
- Never use `dbg_macro`, `todo`, `print_stdout`, or `print_stderr` in production code
- Have module-level documentation (`//!` comments) except in test directories

**Rationale**: rust-analyzer is infrastructure-level software used by thousands of developers. Bugs directly impact developer productivity. Performance issues cascade across entire editing sessions. Correctness and speed must be guaranteed before considering stylistic improvements.

### II. Three-Tier Testing Architecture

**All features MUST be tested at the appropriate boundary: heavy integration (LSP), middle tier (IDE API), or inner tier (HIR/compiler).**

Testing requirements:
- **Heavy integration tests** (outermost boundary): Test LSP interface via stdio, gated behind `RUN_SLOW_TESTS`, deliberately minimal
- **Middle tier tests** (IDE boundary - MOST IMPORTANT): Test via Rust API using `AnalysisHost` and `Analysis`, use snapshot testing with `expect-test`
- **Inner tier tests** (HIR/compiler boundary): Test semantic analysis with rich type vocabulary
- Use `cov_mark` system (`hit!()` and `check!()` macros) to prevent regressions
- Tests MUST be data-driven and reproducible without external dependencies
- Update test expectations with: `env UPDATE_EXPECT=1 cargo qt`

**Rationale**: rust-analyzer's architecture has three clear API boundaries. Testing at the correct boundary ensures tests are fast, reliable, and maintainable. The middle tier (IDE) is the sweet spot: rich enough to test real behavior, fast enough to run constantly. Heavy integration tests are intentionally minimal because they're slow.

### III. Performance-First Design

**Typing inside a function body MUST NEVER invalidate global derived data. All operations MUST be incremental and cancellable.**

Performance requirements:
- Use salsa for incremental computation
- Entire codebase kept in memory (tens of megabytes typical)
- Never do I/O after startup (except Cargo reads)
- Support cancellation via revision counters
- Track performance with metrics: build time, analysis-stats benchmarks (ripgrep, webrender, diesel, hyper, self-analysis)
- Profile with hprof: `RA_PROFILE='*>50'` logs actions taking >50ms
- Typing in function `foo` MUST keep all facts about `bar` intact (core invariant)

**Rationale**: IDE responsiveness is the #1 user experience requirement. Users expect instant feedback as they type. The incremental computation invariant ensures that local edits don't trigger global recomputation. This makes rust-analyzer scale to projects with millions of lines of code.

### IV. Robust Error Handling

**Core crates (`ide`, `hir`) MUST compute `(T, Vec<Error>)` for broken code rather than failing. Use graceful recovery, not assertions.**

Error handling rules:
- Return `(T, Vec<Error>)` not `Result<T, Error>` in core computation
- Use `always!()` and `never!()` macros instead of `assert!` for conditions that should hold but might not
- Each LSP request wrapped in `catch_unwind` for panic isolation
- Use special `Canceled::throw()` panic for graceful cancellation
- `ide` crate catches cancel panics and converts to `Result<T, Cancelled>`
- Parser MUST NEVER fail; always produces `(T, Vec<Error>)`

**Rationale**: rust-analyzer must work gracefully with broken, incomplete code because developers spend most of their time writing code that doesn't compile yet. Failing fast is the wrong strategy for IDE infrastructure. Users need partial results and helpful diagnostics, not crashes or missing features.

### V. User Experience Consistency

**IDE features MUST remain available even with broken builds. API boundaries MUST use editor terminology (offsets, labels) not compiler terminology.**

User experience requirements:
- Partially available features even with build errors
- Stateless LSP server design (HTTP-like request/response)
- `ide` crate API uses POD types with public fields
- Use editor concepts (offsets, ranges, labels) in public APIs
- Use compiler concepts (spans, HIR nodes) only in internal implementation
- Support `textDocument/foldingRange`, `textDocument/semanticTokens`, etc. even with compilation errors
- Clear separation: `rust-analyzer` crate knows LSP/JSON, `ide` crate doesn't

**Rationale**: Users interact with rust-analyzer hundreds of times per minute. The experience must be predictable, fast, and resilient. Using editor terminology in APIs prevents confusion and makes the tool approachable. Remaining operational during broken builds is critical—developers need the most help when their code doesn't compile.

## Quality Assurance Standards

### Lint Configuration

**Workspace lints** (enforced via `Cargo.toml`):
- Rust lints (warn): `elided_lifetimes_in_paths`, `explicit_outlives_requirements`, `unsafe_op_in_unsafe_fn`, `unexpected_cfgs`, `unused_extern_crates`, `unused_lifetimes`, `unreachable_pub`
- Clippy groups: `correctness` (DENY), `perf` (DENY), `complexity` (WARN), `style` (WARN), `suspicious` (WARN)
- Specific denies: `dbg_macro`, `todo`, `print_stdout`, `print_stderr`, `rc_buffer`, `str_to_string`

**Disallowed types** (enforced via `clippy.toml`):
- `std::collections::HashMap` → use `FxHashMap`
- `std::collections::HashSet` → use `FxHashSet`
- `std::process::Command::new` → use `toolchain::command`

### Tidy Checks (via `cargo xtask`)

All code MUST pass tidy validation:
- Module documentation required (all modules must have `//!` doc comments)
- No `#[should_panic]` tests (use explicit assertions)
- No trailing whitespace
- Cargo.toml validation (section headers, dependency versions, path dependencies)
- License validation (only approved licenses allowed)
- Marks consistency (all `hit!()` marks must have corresponding `check!()` marks)
- LSP extensions documentation hash-validated with source

### Formatting Standards

**rustfmt configuration** (`rustfmt.toml`):
- `reorder_modules = true`
- `use_small_heuristics = "Max"`

**CI enforcement**:
```
RUSTFLAGS: "-D warnings -W unreachable-pub -W bare-trait-objects"
CARGO_INCREMENTAL: 0
RUST_BACKTRACE: short
```

## Development Workflow

### Feature Development Process

1. **Proposal phase**: Open GitHub issue FIRST to discuss feature desirability with team
2. **Acceptance**: Get explicit team feedback before implementation work begins
3. **Implementation**: Follow test-first approach at appropriate tier (heavy integration / IDE / HIR)
4. **Review**: Ensure constitution compliance (quality, testing, performance, error handling, UX)
5. **Integration**: Run full test suite: `cargo test`, including tidy checks: `cargo test -p xtask`

**Rationale**: Prevents wasted effort on out-of-scope features. Ensures team capacity for maintenance.

### Test Development Workflow

1. **Write test FIRST** using `expect-test` snapshot testing
2. **Verify test FAILS** before implementation
3. **Implement feature** following relevant principle (quality, performance, error handling)
4. **Update expectations**: `env UPDATE_EXPECT=1 cargo qt`
5. **Add regression marks** using `cov_mark::hit!()` and corresponding `cov_mark::check!()` in test

### Code Generation

Auto-generated code from:
- `ungrammar` grammar descriptions → AST APIs
- Inline tests (`// test test_name` comments) → collected into `test_data/parser/inline/`
- Updates triggered by: `cargo test` or explicit `cargo test -p xtask`

**Important**: Bootstrap avoidance—uses `syn` + manual parsing, not rust-analyzer itself.

### Profiling and Debugging

**Hierarchical profiler**:
```bash
RA_PROFILE='*>50'  # Log all actions taking >50ms
```

**Object counting** (expensive, development only):
```bash
RA_COUNT=1  # Live object counting
```

**Metrics collection**:
```bash
cargo xtask metrics  # Run performance benchmarks
```

## Governance

### Amendment Process

Constitution amendments require:
1. **Documentation**: Proposed change with rationale in GitHub issue
2. **Team approval**: Consensus from rust-analyzer maintainers
3. **Version bump**: Semantic versioning of constitution
   - **MAJOR**: Backward-incompatible governance/principle removal or redefinition
   - **MINOR**: New principle/section added or materially expanded guidance
   - **PATCH**: Clarifications, wording, typo fixes, non-semantic refinements
4. **Sync propagation**: Update all dependent templates and documentation
5. **Migration plan**: If affecting existing code, document migration path

### Compliance Review

All pull requests MUST:
- Pass CI checks (lints, tests, tidy, formatting)
- Comply with relevant principles (quality, testing, performance, error handling, UX)
- Include tests at appropriate tier if adding/changing behavior
- Update documentation if changing public APIs
- Justify any complexity that violates simplicity principles

### Architectural Invariants

These invariants MUST be preserved (violations require constitutional amendment):
- **Parser**: Independent of tree structure; never fails
- **Syntax crate**: Independent of salsa/LSP; can be used standalone
- **Base-db**: Uses salsa; doesn't know about filesystem (uses FileIds) or build system
- **HIR crates**: Not an API boundary; explicitly care about being incremental
- **IDE crate**: API boundary with POD types; uses editor terminology
- **rust-analyzer crate**: Only crate knowing about LSP and JSON; stateless design

### Dispute Resolution

When constitution interpretation is unclear:
1. Discuss in rust-analyzer Zulip stream
2. Maintainers provide interpretation
3. If recurring issue, add clarification via PATCH version update

**Version**: 1.0.0 | **Ratified**: 2025-10-16 | **Last Amended**: 2025-10-16
