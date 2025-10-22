# Product Overview

rust-analyzer is a language server that provides IDE functionality for writing Rust programs. It implements the Language Server Protocol (LSP) and can be used with any compatible editor (VS Code, Vim, Emacs, Zed, etc).

## Core Features

- Go-to-definition and find-all-references
- Code completion and refactoring
- Integrated formatting (rustfmt) and diagnostics (rustc/clippy)
- Syntax highlighting and semantic analysis
- Macro expansion and procedural macro support

## Architecture

rust-analyzer is structured as a set of libraries for analyzing Rust code, following a layered architecture with clear separation between parsing, semantic analysis, and IDE features.

## Target Users

- Rust developers using various editors and IDEs
- Tool developers building on top of rust-analyzer's APIs
- Contributors to the Rust ecosystem requiring advanced code analysis