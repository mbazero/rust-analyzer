# Data Model: Move Item Refactoring

**Feature**: Move Item Refactoring
**Date**: 2025-10-16
**Purpose**: Define the domain entities and their relationships for move item refactoring

## Overview

This document defines the core data structures and domain model for the move item refactoring feature. These entities represent the conceptual model used during the move operation, built on top of rust-analyzer's existing HIR and syntax abstractions.

## Core Entities

### 1. MoveableItem

Represents an item that can be moved between modules.

**Attributes**:
- `definition: Definition` - HIR definition (from `ide_db::defs`)
- `source_file: FileId` - Current file location
- `source_module: Module` - Current module (from HIR)
- `source_range: TextRange` - Complete range including attributes and doc comments
- `item_kind: ItemKind` - Type of item (struct, enum, function, etc.)
- `visibility: Visibility` - Current visibility (pub, pub(crate), pub(super), private)
- `name: Name` - Current item name
- `has_impl_blocks: bool` - Whether item has associated impl blocks

**Invariants**:
- Must have a definition resolvable by HIR
- Must not be in a macro definition context
- Must be a top-level module item (not a local item in function)

**Relationships**:
- Belongs to exactly one `SourceModule`
- Has zero or more `ItemReference`s
- May have associated `ImplBlock`s (tracked separately)

**Example**:
```rust
// In crates/src/models/user.rs
pub struct User {
    id: u64,
    name: String,
}

// MoveableItem represents this struct:
// - source_file: FileId for "models/user.rs"
// - source_module: Module for "crate::models"
// - item_kind: ItemKind::Struct
// - visibility: Visibility::Public
// - name: Name::new("User")
```

### 2. TargetPath

Represents the parsed and validated target location for a move operation.

**Attributes**:
- `raw_input: String` - Original user input (e.g., "crate::models::entity::Person")
- `module_path: Vec<Name>` - Module segments (["crate", "models", "entity"])
- `item_name: Name` - Final item name ("Person")
- `target_module: Option<Module>` - Resolved target module (None if needs creation)
- `requires_directory: bool` - Whether intermediate directories needed
- `requires_module_decls: Vec<(Module, Name)>` - Parent modules needing `mod` statements

**Invariants**:
- Must start with `crate::`
- All segments must be valid identifiers (not keywords or invalid chars)
- Must have at least two segments (crate + module or item)
- Target module path must be different from source (or item name must differ)

**Validation**:
- Syntax validation: Parse as `ast::Path`
- Semantic validation: Check module visibility, name conflicts
- Cross-reference validation: No circular dependencies

**Example**:
```rust
// Input: "crate::entities::user::UserAccount"
// TargetPath:
// - module_path: ["crate", "entities", "user"]
// - item_name: "UserAccount"
// - target_module: None (needs creation)
// - requires_directory: true (entities/user/ needed)
// - requires_module_decls: [(crate, "entities"), (crate::entities, "user")]
```

### 3. ItemReference

Represents a usage of an item in the codebase that needs updating.

**Attributes**:
- `file_id: EditionedFileId` - File containing the reference
- `range: TextRange` - Location of the reference
- `category: ReferenceCategory` - Type of reference (Import, Read, Write)
- `syntax_node: SyntaxNode` - AST node for the reference
- `is_in_macro: bool` - Whether reference is in macro expansion

**Reference Categories**:
- `Import`: `use` statement that imports the item
- `Read`: Usage of the item (e.g., function call, type reference)
- `Write`: Mutation (rare for our use case)

**Relationships**:
- References exactly one `MoveableItem`
- Located in a specific file and range
- May be part of an `ImportStatement`

**Example**:
```rust
// In crates/src/services/auth.rs
use crate::models::User;

fn authenticate(user: &User) -> bool {
    // user is a Read reference
    // ...
}

// ItemReference for import:
// - category: ReferenceCategory::Import
// - range: TextRange covering "use crate::models::User;"

// ItemReference for parameter type:
// - category: ReferenceCategory::Read
// - range: TextRange covering "User" in parameter
```

### 4. ModuleInfo

Represents module metadata for both source and target modules.

**Attributes**:
- `module: Module` - HIR module handle
- `file_id: Option<FileId>` - Associated file (None if module needs creation)
- `parent: Option<Module>` - Parent module
- `children: Vec<Name>` - Child module names
- `is_mod_rs: bool` - Whether module uses mod.rs style
- `path_from_root: Vec<Name>` - Full path from crate root
- `declarations: Vec<Name>` - Items currently declared in module

**Operations**:
- `exists()` - Whether module exists in codebase
- `file_path()` - Compute file system path
- `needs_mod_declaration()` - Whether parent needs `mod name;`
- `has_name_conflict(name: Name)` - Check if name already used

**Relationships**:
- Has zero or one parent `ModuleInfo`
- Has zero or more child `ModuleInfo`s
- Contains zero or more `MoveableItem`s

**Example**:
```rust
// For module: crate::models
// ModuleInfo:
// - file_id: Some(FileId for "models.rs" or "models/mod.rs")
// - parent: Some(crate root module)
// - children: ["user", "post", "comment"]
// - path_from_root: ["crate", "models"]
// - declarations: ["User", "Post", "Comment", "ModelError"]
```

### 5. MoveOperation

Represents the complete move operation with all computed changes.

**Attributes**:
- `item: MoveableItem` - Item being moved
- `target: TargetPath` - Parsed target location
- `source_module: ModuleInfo` - Current module info
- `target_module: ModuleInfo` - Target module info (may need creation)
- `references: Vec<ItemReference>` - All references to update
- `file_operations: Vec<FileSystemEdit>` - Files to create/modify
- `text_edits: FxHashMap<FileId, Vec<TextEdit>>` - Text changes per file
- `new_item_name: Option<Name>` - If renaming (None = keep same name)

**Validation State**:
- `is_valid: bool` - Whether operation can proceed
- `validation_errors: Vec<String>` - Errors that prevent operation
- `warnings: Vec<String>` - Non-blocking warnings

**Computed Properties**:
- `involves_rename()` - Whether item name changes
- `creates_new_module()` - Whether target module needs creation
- `creates_directories()` - Whether directories need creation
- `updates_import_count()` - Number of imports to update

**Example**:
```rust
// Moving struct User from crate::models to crate::entities::user::UserAccount
// MoveOperation:
// - item: MoveableItem(User, models.rs)
// - target: TargetPath("crate::entities::user", "UserAccount")
// - involves_rename(): true (User → UserAccount)
// - creates_new_module(): true (entities/user doesn't exist)
// - creates_directories(): true (entities/ needed)
// - references: [10 import refs, 25 usage refs]
// - file_operations: [
//     CreateFile(entities.rs),
//     CreateFile(entities/user.rs)
//   ]
// - text_edits: {
//     models.rs: [Delete(User definition)],
//     entities/user.rs: [Insert(UserAccount definition)],
//     services/auth.rs: [Replace("User" → "UserAccount")],
//     ...
//   }
```

## Relationships

```
MoveableItem
    │
    ├─── located_in ──> ModuleInfo (source)
    ├─── has_many ────> ItemReference
    └─── defines ─────> Definition (HIR)

TargetPath
    │
    └─── resolves_to ─> ModuleInfo (target)

MoveOperation
    │
    ├─── operates_on ─> MoveableItem
    ├─── targets ─────> TargetPath
    ├─── from ────────> ModuleInfo (source)
    ├─── to ──────────> ModuleInfo (target)
    └─── updates ─────> Vec<ItemReference>

ModuleInfo
    │
    ├─── parent_of ──> Vec<ModuleInfo> (children)
    ├─── child_of ───> Option<ModuleInfo> (parent)
    └─── contains ───> Vec<MoveableItem>

ItemReference
    │
    ├─── references ─> MoveableItem
    └─── located_in ─> FileId
```

## Enums and Types

### ItemKind

```rust
pub enum ItemKind {
    Struct,
    Enum,
    Union,
    Function,
    Trait,
    TypeAlias,
    Const,
    Static,
    // Not supported initially:
    // Macro, Module (use separate assists)
}
```

### Visibility

```rust
pub enum Visibility {
    Public,              // pub
    Crate,               // pub(crate)
    Super,               // pub(super)
    InModule(Module),    // pub(in path::to::module)
    Private,             // no pub keyword
}
```

### ReferenceCategory

```rust
pub enum ReferenceCategory {
    Import,    // use statement
    Read,      // usage
    Write,     // mutation (rare)
}
```

### FileSystemEdit

```rust
pub enum FileSystemEdit {
    CreateFile {
        dst: AnchoredPathBuf,
        initial_contents: String,
    },
    MoveFile {
        src: FileId,
        dst: AnchoredPathBuf,
    },
    // Note: MoveDir not used for item moves
}
```

### ValidationError

```rust
pub enum ValidationError {
    InvalidPath(String),
    NameConflict { target_module: ModulePath, name: Name },
    VisibilityViolation { item: Name, reason: String },
    CircularDependency { from: Module, to: Module },
    InMacroContext,
    CrossCrateMove,
}
```

## Domain Invariants

### Move Operation Invariants

1. **Source != Target**: Item's current location must differ from target
   - Either module path differs, or item name differs, or both

2. **Valid Path**: Target path must be syntactically and semantically valid
   - All segments are valid identifiers
   - Target module is reachable from crate root

3. **No Name Conflicts**: Target module must not already contain item with same name
   - Unless moving to same module with rename

4. **Visibility Consistency**: Move must not violate visibility rules
   - Private item can't be moved to more visible module without visibility adjustment
   - Public item must remain accessible from its usage sites

5. **No Macro Definitions**: Item must not be defined inside a macro
   - Can be used by macros, but not defined by them

6. **Single Crate**: Source and target must be in same crate
   - Cross-crate moves not supported

### Module Creation Invariants

1. **Parent Exists**: When creating module, parent must exist
   - Create modules depth-first (parent before child)

2. **Unique Declaration**: Each module declared exactly once in parent
   - Check existing `mod` statements before adding

3. **File System Consistency**: File path matches module path
   - `crate::foo::bar` → `crates/src/foo/bar.rs` (named file style)

## Data Flow

### Phase 1: Parse and Validate

```
User Input (string)
    ↓
TargetPath (parsed)
    ↓
Validation (errors?)
    ↓
MoveOperation (validated)
```

### Phase 2: Collect References

```
MoveableItem
    ↓
Definition::usages()
    ↓
Vec<ItemReference> (all usages)
    ↓
MoveOperation (with references)
```

### Phase 3: Compute Changes

```
MoveOperation
    ↓
Compute FileSystemEdit (files to create)
    ↓
Compute TextEdit (text changes)
    ↓
MoveOperation (complete)
```

### Phase 4: Apply Changes

```
MoveOperation
    ↓
SourceChange
    ↓
LSP Client (apply changes)
```

## Usage Examples

### Example 1: Simple Sibling Move

**Before**:
```
crates/src/
├── lib.rs
├── models/
│   ├── user.rs (contains User struct)
│   └── mod.rs
```

**Operation**: Move `User` to `crate::models::entities::User`

**Data**:
- `MoveableItem`: User in models/user.rs
- `TargetPath`: ["crate", "models", "entities"] + "User"
- `target_module.exists()`: false (needs creation)
- `file_operations`: [CreateFile(models/entities.rs)]
- `text_edits`: Remove from user.rs, insert into entities.rs, update imports

**After**:
```
crates/src/
├── lib.rs
├── models/
│   ├── user.rs (empty or removed)
│   ├── entities.rs (contains User struct)
│   └── mod.rs (has "mod entities;")
```

### Example 2: Move + Rename

**Before**:
```rust
// In crates/src/types.rs
pub struct Status {
    code: u32,
}
```

**Operation**: Move to `crate::models::user_status::UserAccountStatus`

**Data**:
- `MoveableItem`: Status in types.rs
- `TargetPath`: ["crate", "models", "user_status"] + "UserAccountStatus"
- `new_item_name`: Some("UserAccountStatus") (rename)
- `involves_rename()`: true
- `creates_new_module()`: true

**After**:
```rust
// In crates/src/models/user_status.rs
pub struct UserAccountStatus {
    code: u32,
}

// All references updated:
// - Imports: use crate::types::Status; → use crate::models::user_status::UserAccountStatus;
// - Usage: Status { code } → UserAccountStatus { code }
```

## Notes

- This model is conceptual; actual implementation uses rust-analyzer's existing types (Definition, Module, Semantics, etc.)
- Entities map to combinations of HIR, syntax, and ide-db types
- Validation happens incrementally (cheap checks first, expensive checks only if needed)
- All operations designed to support cancellation via salsa revision counters
