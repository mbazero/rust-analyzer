# Project Structure

## Workspace Organization

rust-analyzer follows a multi-crate workspace structure with clear separation of concerns:

```
rust-analyzer/
├── crates/           # Core implementation crates
├── lib/              # Reusable libraries (published separately)
├── xtask/            # Build automation and tooling
├── editors/code/     # VS Code extension
├── docs/book/        # Documentation
└── assets/           # Logo and branding assets
```

## Core Crates (`crates/`)

### Foundation Layer
- **`syntax`** - Rust parser and syntax tree (CST)
- **`parser`** - Low-level parsing logic
- **`span`** - Source location tracking
- **`tt`** - Token tree representation

### Database Layer  
- **`base-db`** - Database foundation and file management
- **`hir-expand`** - Macro expansion
- **`hir-def`** - High-level IR definitions
- **`hir-ty`** - Type inference and checking
- **`hir`** - High-level semantic model

### IDE Layer
- **`ide-db`** - IDE database and utilities
- **`ide`** - Core IDE features
- **`ide-assists`** - Code refactoring and quick fixes
- **`ide-completion`** - Code completion
- **`ide-diagnostics`** - Error reporting and diagnostics

### Language Server
- **`rust-analyzer`** - Main LSP server binary
- **`load-cargo`** - Cargo project loading
- **`project-model`** - Project structure modeling

### Utilities
- **`stdx`** - Standard library extensions
- **`paths`** - Path handling utilities
- **`vfs`** - Virtual file system
- **`profile`** - Performance profiling

## Library Crates (`lib/`)

Published separately with semantic versioning:
- **`la-arena`** - Arena-based memory allocation
- **`line-index`** - Line/column indexing
- **`lsp-server`** - LSP protocol implementation

## Naming Conventions

- Crate names use kebab-case (`hir-def`, `ide-assists`)
- Module names use snake_case
- Follow standard Rust naming conventions
- Use descriptive names that reflect the crate's purpose

## Dependency Flow

```
IDE Layer (ide, ide-*)
    ↓
Semantic Layer (hir, hir-*)  
    ↓
Database Layer (base-db, hir-expand)
    ↓
Syntax Layer (syntax, parser)
```

## Testing Structure

- Unit tests alongside source code
- Integration tests in `tests/` directories
- Test data in `test_data/` directories
- Fuzz tests in `crates/syntax/fuzz/`