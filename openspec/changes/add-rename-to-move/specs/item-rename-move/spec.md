# Item Rename-to-Move Specification

## ADDED Requirements

### Requirement: Path-Based Rename Detection

The rename operation SHALL detect when the new name is a fully-qualified path and interpret it as a move-and-rename instruction.

#### Scenario: Detect simple rename vs move
- **GIVEN** an item `struct Foo` in module `crate::alpha`
- **WHEN** rename is invoked with new name `Bar`
- **THEN** the operation SHALL be treated as a simple rename (no move)

#### Scenario: Detect move to different module
- **GIVEN** an item `struct Foo` in module `crate::alpha`
- **WHEN** rename is invoked with new name `crate::beta::Bar`
- **THEN** the operation SHALL be detected as a move from `alpha` to `beta` with rename to `Bar`

#### Scenario: Detect move within same module
- **GIVEN** an item `struct Foo` in module `crate::alpha`
- **WHEN** rename is invoked with new name `crate::alpha::Bar`
- **THEN** the operation SHALL be treated as a simple rename (module unchanged)

### Requirement: Module File Creation

When moving an item to a non-existent module, the system SHALL create the necessary module files and directories.

#### Scenario: Create sibling module file
- **GIVEN** item in `src/mod_one/start.rs`
- **WHEN** moving to `crate::mod_one::finish::Item`
- **THEN** the system SHALL create `src/mod_one/finish.rs`

#### Scenario: Create new top-level module directory
- **GIVEN** item in `src/mod_one/start.rs`
- **WHEN** moving to `crate::mod_two::finish::Item`
- **THEN** the system SHALL create `src/mod_two/` directory and appropriate module file structure

#### Scenario: Create nested module directory
- **GIVEN** item in `src/alpha.rs`
- **WHEN** moving to `crate::beta::gamma::delta::Item`
- **THEN** the system SHALL create `src/beta/gamma/delta.rs` (or appropriate directory structure)

#### Scenario: Reuse existing module file
- **GIVEN** existing file `src/existing.rs` with content
- **WHEN** moving an item to `crate::existing::NewItem`
- **THEN** the system SHALL append the item to the existing file without creating a new one

### Requirement: Module Declaration Updates

The system SHALL add or update module declarations in parent modules when creating new module files.

#### Scenario: Add module declaration to parent
- **GIVEN** `src/lib.rs` with no declaration of `new_mod`
- **WHEN** creating a new module file `src/new_mod.rs` via rename-to-move
- **THEN** the system SHALL insert `pub mod new_mod;` in `src/lib.rs`

#### Scenario: Skip declaration for existing modules
- **GIVEN** `src/lib.rs` already contains `pub mod existing;`
- **WHEN** moving an item to `crate::existing::Item`
- **THEN** the system SHALL NOT add duplicate module declaration

#### Scenario: Add nested module declaration
- **GIVEN** creating `src/parent/child.rs`
- **WHEN** `src/parent/mod.rs` exists but lacks `pub mod child;`
- **THEN** the system SHALL add `pub mod child;` to `src/parent/mod.rs`

### Requirement: Item Definition Relocation

The system SHALL move the item's definition from the source file to the destination file.

#### Scenario: Move struct definition
- **GIVEN** `struct Foo;` in `src/alpha.rs`
- **WHEN** moving to `crate::beta::Bar`
- **THEN** the struct SHALL be removed from `src/alpha.rs` and added to `src/beta.rs` as `struct Bar;`

#### Scenario: Preserve item attributes and documentation
- **GIVEN** item with doc comments and attributes
  ```rust
  /// Documentation
  #[derive(Debug)]
  pub struct Foo;
  ```
- **WHEN** moving to another module
- **THEN** all attributes, doc comments, and visibility modifiers SHALL be preserved

#### Scenario: Move with associated items
- **GIVEN** struct with impl blocks in the same file
  ```rust
  struct Foo;
  impl Foo { fn bar() {} }
  ```
- **WHEN** moving `Foo` to another module
- **THEN** the implementation blocks SHALL move with the struct definition

### Requirement: External Reference Updates

All import statements and qualified paths referencing the moved item from other files SHALL be updated to reflect the new location.

#### Scenario: Update use statements
- **GIVEN** `use crate::alpha::Foo;` in multiple files
- **WHEN** moving `Foo` from `alpha` to `beta`
- **THEN** all use statements SHALL be updated to `use crate::beta::Foo;`

#### Scenario: Update qualified references
- **GIVEN** code referencing `alpha::Foo::new()`
- **WHEN** moving and renaming `Foo` to `beta::Bar`
- **THEN** references SHALL be updated to `beta::Bar::new()`

#### Scenario: Update relative imports
- **GIVEN** `use super::Foo;` in a sibling module
- **WHEN** moving `Foo` changes the relative path
- **THEN** imports SHALL be updated to maintain correct reference (e.g., `use super::super::beta::Foo;` or absolute paths)

### Requirement: Internal Reference Updates

References within the moved item to other items in the source module SHALL be updated to maintain correct imports after the move.

#### Scenario: Add imports for references to source module items
- **GIVEN** struct `Foo` in `crate::alpha` that references `Helper` also in `crate::alpha`
  ```rust
  // In src/alpha.rs
  struct Helper;
  struct Foo {
      helper: Helper  // No import needed, same module
  }
  ```
- **WHEN** moving `Foo` to `crate::beta`
- **THEN** the moved code SHALL add the necessary import
  ```rust
  // In src/beta.rs
  use crate::alpha::Helper;
  struct Foo {
      helper: Helper
  }
  ```

#### Scenario: Update relative paths within moved item
- **GIVEN** function in `crate::alpha` referencing `alpha::helper()`
  ```rust
  // In src/alpha.rs
  fn helper() {}
  pub fn foo() {
      helper();  // Unqualified call works in same module
  }
  ```
- **WHEN** moving `foo` to `crate::beta`
- **THEN** the reference SHALL be updated to qualify the path
  ```rust
  // In src/beta.rs
  pub fn foo() {
      crate::alpha::helper();
  }
  ```

#### Scenario: Update super references in moved impl blocks
- **GIVEN** struct with impl block referencing items via `super::`
  ```rust
  // In src/parent/child.rs
  impl Foo {
      fn new() -> Self {
          super::parent_fn();  // References parent module
      }
  }
  ```
- **WHEN** moving `Foo` from `crate::parent::child` to `crate::other`
- **THEN** `super::parent_fn()` SHALL be updated to `crate::parent::parent_fn()`

#### Scenario: Preserve imports that remain valid after move
- **GIVEN** item importing from external crate
  ```rust
  use std::collections::HashMap;
  struct Foo {
      map: HashMap<String, i32>
  }
  ```
- **WHEN** moving `Foo` to another module
- **THEN** the `use std::collections::HashMap;` SHALL remain unchanged

#### Scenario: Update qualified self references
- **GIVEN** item referencing other items in source module using `self::`
  ```rust
  // In src/alpha.rs
  struct Helper;
  struct Foo {
      helper: self::Helper
  }
  ```
- **WHEN** moving `Foo` to `crate::beta`
- **THEN** `self::Helper` SHALL be updated to `crate::alpha::Helper`

#### Scenario: Handle moved item referencing another moved associated item
- **GIVEN** struct and impl block both being moved together
  ```rust
  // In src/alpha.rs
  struct Foo;
  impl Foo {
      fn method(&self) {
          Self::associated()  // References own type
      }
      fn associated() {}
  }
  ```
- **WHEN** moving `Foo` to another module
- **THEN** `Self` references SHALL remain unchanged (still valid within impl block)

### Requirement: Module File Structure Preference

The system SHALL respect project conventions when choosing between `mod.rs` and named module files.

#### Scenario: Default to file-based modules for simple cases
- **GIVEN** no existing preference detected
- **WHEN** creating module `crate::new_mod`
- **THEN** the system SHALL create `src/new_mod.rs` by default

#### Scenario: Use directory modules for nested structures
- **GIVEN** moving to `crate::parent::child::Item`
- **WHEN** `parent` requires submodules
- **THEN** the system SHALL create `src/parent/mod.rs` and `src/parent/child.rs`

#### Scenario: Follow existing project style
- **GIVEN** existing modules all use `mod.rs` pattern
- **WHEN** creating new module structure
- **THEN** the system SHALL prefer `mod.rs` style to maintain consistency

### Requirement: External Visibility Validation

Before moving an item, the system SHALL validate that all items referenced within the moved item will remain visible from the destination module, without modifying the visibility of those external items.

#### Scenario: Reject move when referenced private item becomes inaccessible
- **GIVEN** struct `Foo` in `crate::alpha` referencing private `Helper` in `crate::alpha`
  ```rust
  // In src/alpha.rs
  struct Helper;  // private
  pub struct Foo {
      helper: Helper  // OK - same module
  }
  ```
- **WHEN** attempting to move `Foo` to `crate::beta`
- **THEN** the operation SHALL fail with error "Cannot move 'Foo': references private item 'Helper' which will not be visible from 'beta'"

#### Scenario: Allow move when referenced items are pub
- **GIVEN** struct `Foo` referencing `pub struct Helper` in same module
- **WHEN** moving `Foo` to different module
- **THEN** the operation SHALL succeed and add `use crate::alpha::Helper;`

#### Scenario: Allow move when referenced items are pub(crate)
- **GIVEN** struct `Foo` referencing `pub(crate) struct Helper`
- **WHEN** moving `Foo` to different module within same crate
- **THEN** the operation SHALL succeed and add appropriate import

#### Scenario: Reject move when pub(super) item becomes inaccessible
- **GIVEN** struct `Foo` in `crate::parent::child` referencing `pub(super) Helper`
- **WHEN** moving `Foo` to `crate::other` (different parent)
- **THEN** the operation SHALL fail with error indicating `Helper` will not be visible

#### Scenario: Allow move when pub(super) item remains accessible
- **GIVEN** struct `Foo` in `crate::parent::child` referencing `pub(super) Helper`
- **WHEN** moving `Foo` to `crate::parent::other` (same parent)
- **THEN** the operation SHALL succeed

#### Scenario: Validate visibility of nested referenced items
- **GIVEN** struct `Foo` calling `Helper::internal_method()` where `Helper` is public but `internal_method` is private
- **WHEN** attempting to move `Foo` to different module
- **THEN** the operation SHALL fail with error indicating `internal_method` is not accessible

### Requirement: Internal Visibility Updates

The moved item's visibility SHALL be updated to the minimum level required to maintain accessibility from all locations that reference it after the move, using private, pub(crate), or pub.

#### Scenario: Keep private when no external references exist
- **GIVEN** private struct `Foo` with no references outside its module
- **WHEN** moving `Foo` to different module
- **THEN** `Foo` SHALL remain private (no visibility change needed)

#### Scenario: Update to pub(crate) when referenced from other modules in same crate
- **GIVEN** private struct `Foo` in `crate::alpha` referenced from `crate::beta`
  ```rust
  // In src/alpha.rs
  struct Foo;  // private, but used in beta via alpha::Foo
  ```
- **WHEN** moving `Foo` to `crate::gamma`
- **THEN** `Foo` SHALL be updated to `pub(crate) struct Foo;` to remain visible within crate

#### Scenario: Keep pub visibility unchanged when already public
- **GIVEN** `pub struct Foo`
- **WHEN** moving to any module
- **THEN** visibility SHALL remain `pub` (already maximally visible)

#### Scenario: Preserve pub(crate) when already sufficient
- **GIVEN** `pub(crate) struct Foo` referenced only within crate
- **WHEN** moving to different module
- **THEN** visibility SHALL remain `pub(crate)` (already sufficient)

#### Scenario: Update visibility for associated items in impl blocks
- **GIVEN** struct with private methods referenced externally
  ```rust
  pub struct Foo;
  impl Foo {
      fn private_method(&self) {}  // referenced from another module
  }
  ```
- **WHEN** moving `Foo` (with impl block) to different module
- **THEN** `private_method` SHALL be updated to `pub fn` if referenced externally

#### Scenario: Preserve pub on items already pub when pub(crate) would suffice
- **GIVEN** `pub struct Foo` referenced only within crate
- **WHEN** moving to different module
- **THEN** visibility SHALL remain `pub` (do not downgrade existing pub to pub(crate))

### Requirement: Rename Validation for Move Operations

Path-based rename targets SHALL be validated as legal module paths and item names.

#### Scenario: Reject invalid module paths
- **GIVEN** rename target `crate::123invalid::Item`
- **WHEN** attempting rename-to-move
- **THEN** the operation SHALL fail with error "Invalid module name"

#### Scenario: Reject invalid item names
- **GIVEN** rename target `crate::valid::123Invalid`
- **WHEN** attempting rename-to-move
- **THEN** the operation SHALL fail with error "Invalid identifier"

#### Scenario: Reject keywords in paths
- **GIVEN** rename target `crate::super::Item`
- **WHEN** attempting rename-to-move
- **THEN** the operation SHALL fail with error "Cannot use keyword 'super' as module name"

### Requirement: Supported Item Types for Move

The system SHALL support moving the following item types via rename-to-move.

#### Scenario: Move struct definitions
- **GIVEN** a struct definition
- **WHEN** rename-to-move is invoked
- **THEN** the struct SHALL be moved successfully

#### Scenario: Move enum definitions
- **GIVEN** an enum definition
- **WHEN** rename-to-move is invoked
- **THEN** the enum SHALL be moved successfully

#### Scenario: Move function definitions
- **GIVEN** a standalone function
- **WHEN** rename-to-move is invoked
- **THEN** the function SHALL be moved successfully

#### Scenario: Move trait definitions
- **GIVEN** a trait definition
- **WHEN** rename-to-move is invoked
- **THEN** the trait SHALL be moved successfully

#### Scenario: Move type aliases and constants
- **GIVEN** a type alias or const definition
- **WHEN** rename-to-move is invoked
- **THEN** the item SHALL be moved successfully

#### Scenario: Reject moving local variables
- **GIVEN** a local variable or function parameter
- **WHEN** rename-to-move is invoked
- **THEN** the operation SHALL fail with error "Cannot move local items"

#### Scenario: Reject moving items from external crates
- **GIVEN** an item from a dependency crate
- **WHEN** rename-to-move is invoked
- **THEN** the operation SHALL fail with error "Cannot move items from external crates"

### Requirement: Transaction Atomicity

Rename-to-move operations SHALL be atomic - either all changes succeed or none are applied.

#### Scenario: Rollback on validation failure
- **GIVEN** a move operation that would create invalid module structure
- **WHEN** validation fails during planning
- **THEN** no files SHALL be created or modified

#### Scenario: All-or-nothing workspace edits
- **GIVEN** a complex move operation with file creation and reference updates
- **WHEN** the operation completes successfully
- **THEN** all edits SHALL be returned in a single LSP WorkspaceEdit

### Requirement: User Feedback During Move Operations

The system SHALL provide clear feedback about the scope and effects of move operations.

#### Scenario: Preview shows file operations
- **GIVEN** rename-to-move will create new files
- **WHEN** prepare rename is invoked
- **THEN** the response SHOULD indicate file system changes will occur (implementation-dependent on LSP client support)

#### Scenario: Error messages indicate move conflict
- **GIVEN** destination module already contains an item with the target name
- **WHEN** attempting rename-to-move
- **THEN** the error SHALL specify "Item 'Foo' already exists in module 'beta'"
