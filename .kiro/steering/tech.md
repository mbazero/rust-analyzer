# Technology Stack

## Build System & Language
- **Language**: Rust (Edition 2024, MSRV 1.88)
- **Build System**: Cargo workspace with 40+ crates
- **Package Manager**: Cargo with workspace dependencies

## Key Dependencies
- **Parsing**: `rowan` (Red-Green tree), `ra-ap-rustc_lexer`
- **Query System**: `salsa` (incremental computation framework)
- **Concurrency**: `rayon` (data parallelism), `crossbeam-channel`
- **Serialization**: `serde`, `serde_json`
- **Collections**: `rustc-hash` (FxHashMap/FxHashSet), `smallvec`, `indexmap`

## Development Tools
- **Linting**: Clippy with strict configuration
- **Formatting**: rustfmt with `reorder_modules = true`
- **Testing**: Standard Rust testing + fuzz testing
- **Task Runner**: Custom `xtask` binary for build automation

## Common Commands

### Building
```bash
cargo build                    # Build all crates
cargo build --release         # Release build
cargo xtask dist              # Create distribution
```

### Testing
```bash
cargo test                     # Run all tests
cargo tq                       # Quick tests (alias for test -- -q)
cargo xtask fuzz-tests         # Run fuzz tests (requires nightly)
```

### Development
```bash
cargo xtask codegen           # Generate code
cargo lint                    # Run clippy (alias)
cargo xtask install           # Install rust-analyzer
cargo xtask tidy              # Code formatting and cleanup
```

### Performance
```bash
cargo xtask metrics           # Generate performance metrics
```

## Code Quality Standards
- Use `FxHashMap`/`FxHashSet` instead of std collections
- Prefer `toolchain::command` over `std::process::Command::new`
- Enable extensive clippy lints with custom configuration
- 100 character line limit
- 4-space indentation (2 for markdown/yaml)