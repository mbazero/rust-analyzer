# Feature Specification: Move Item Refactoring

**Feature Branch**: `001-specify-scripts-bash`
**Created**: 2025-10-16
**Status**: Draft
**Input**: User description: "I want to implement move item refactoring for rust-analyzer following the pattern below. One could potentially update the rename action to take a fully-qualified path, which, if it differed from the item's current fully-qualified path, could be a 'move and potentially rename' instruction, moving the item to the appropriate module, creating the module if needed, renaming the module if the name is different from the original, and then updating all imports."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Move Item to Sibling Module (Priority: P1)

A developer has a struct defined in one module and wants to move it to a sibling module (same parent directory) to better organize their code. They use the rename action with a fully-qualified path, and the system automatically creates the target file if needed, moves the item, and updates all references.

**Why this priority**: This is the most common refactoring scenario and the simplest case. Developers frequently reorganize code within the same module hierarchy. This delivers immediate value as a standalone feature and serves as the foundation for more complex move operations.

**Independent Test**: Can be fully tested by creating a struct in one file, invoking rename with a sibling module path, and verifying the struct appears in the new file with all imports updated. No cross-module directory creation required.

**Acceptance Scenarios**:

1. **Given** a struct `ToMove` exists in `src/mod_one/start.rs`, **When** developer invokes rename on `ToMove` and enters `crate::mod_one::finish::ToMove`, **Then** system creates `src/mod_one/finish.rs` if it doesn't exist, moves the struct definition to that file, and updates all import statements across the codebase to reference the new location.

2. **Given** a struct `Person` exists in `src/models/user.rs` and `src/models/entity.rs` already exists, **When** developer renames to `crate::models::entity::Person`, **Then** the struct is appended to the existing `entity.rs` file and all imports are updated.

3. **Given** a function `calculate` exists in `src/utils/math.rs`, **When** developer renames to `crate::utils::advanced_math::calculate`, **Then** system creates `advanced_math.rs` as a sibling, moves the function, and updates all call sites.

---

### User Story 2 - Move and Rename Item Simultaneously (Priority: P2)

A developer wants to both move an item to a different module and give it a better name in one operation. They use the rename action with a fully-qualified path containing the new name, and the system performs both operations atomically while updating all references.

**Why this priority**: This is a natural extension of basic move functionality. Developers often realize they need both better naming and better organization simultaneously. Building on P1's infrastructure, this adds significant value with minimal additional complexity.

**Independent Test**: Can be fully tested by creating an item with one name, invoking rename with a different module path and different name, and verifying both the location and name change with all references updated. This can be tested independently of complex directory creation.

**Acceptance Scenarios**:

1. **Given** a struct `ToMove` exists in `src/mod_one/start.rs`, **When** developer invokes rename and enters `crate::mod_one::finish::Final`, **Then** system creates `finish.rs`, moves the struct, renames it to `Final`, and updates all references to use the new module path and name.

2. **Given** an enum `Status` exists in `src/types.rs`, **When** developer renames to `crate::models::user_status::UserAccountStatus`, **Then** the enum is moved to `models/user_status.rs` (creating the file and directory as needed), renamed to `UserAccountStatus`, and all pattern matches and references are updated.

3. **Given** a trait `Handler` exists in `src/lib.rs`, **When** developer renames to `crate::handlers::request::RequestHandler`, **Then** the trait is moved to the new location with the new name, and all implementations (`impl Handler`) are updated to reference `RequestHandler`.

---

### User Story 3 - Move Across Module Boundaries with Auto-Creation (Priority: P3)

A developer wants to move an item to a module that doesn't exist yet, potentially in a different part of the module hierarchy. They use the rename action with a fully-qualified path, and the system automatically creates all necessary directories, module files, and module declarations while moving the item and updating references.

**Why this priority**: This represents the most sophisticated use case, enabling developers to restructure their codebase more freely. While valuable, it builds on P1 and P2 and is less frequently needed than basic moves. It's independently valuable for major refactoring work.

**Independent Test**: Can be fully tested by creating an item in one module hierarchy, invoking rename with a path to a non-existent module hierarchy, and verifying all directories, files, and module declarations are created correctly with the item moved and references updated.

**Acceptance Scenarios**:

1. **Given** a struct `ToMove` exists in `src/mod_one/start.rs` and `mod_two` directory doesn't exist, **When** developer renames to `crate::mod_two::finish::Final`, **Then** system creates `src/mod_two/` directory, creates appropriate module file(s), creates `finish.rs` within it, moves and renames the struct, adds necessary `mod` declarations, and updates all imports.

2. **Given** a function `helper` exists in `src/utils.rs` and a deeply nested path is specified, **When** developer renames to `crate::utilities::internal::helpers::math::calculate_helper`, **Then** system creates all intermediate directories (`utilities/internal/helpers/math/`), creates necessary module files at each level, moves and renames the function, and updates all references.

3. **Given** multiple related items exist in `src/old_module.rs`, **When** developer moves them one by one to `crate::new_structure::*`, **Then** each move operation correctly maintains module structure, avoids duplicate module declarations, and keeps all cross-references working.

---

### Edge Cases

- What happens when the target file already exists and contains an item with the same name as the item being moved?
- What happens when the fully-qualified path is syntactically invalid (e.g., invalid identifiers, incorrect module structure)?
- What happens when the item being moved has dependencies on private items in its current module that wouldn't be accessible from the target module?
- What happens when moving an item that is re-exported from its current module (e.g., `pub use`)?
- What happens when the project uses different module organization styles (mod.rs vs named files) in different parts of the codebase?
- What happens when moving an item from a module that will become empty after the move?
- What happens when the target path would violate module visibility rules?
- What happens when multiple items need to be moved together due to tight coupling?
- What happens if the operation is cancelled mid-execution?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST detect when rename input is a fully-qualified path that differs from the item's current fully-qualified path
- **FR-002**: System MUST parse fully-qualified paths to extract crate root, module hierarchy, and target item name
- **FR-003**: System MUST create target module files if they don't exist
- **FR-004**: System MUST create target directories if they don't exist
- **FR-005**: System MUST use named file style (e.g., `module_name.rs`) when creating new modules, consistent with modern Rust conventions (Rust 2018+)
- **FR-006**: System MUST move complete item definition (including attributes, doc comments, and visibility modifiers) from source to target location
- **FR-007**: System MUST rename item if the final identifier in the path differs from the original item name
- **FR-008**: System MUST update all imports and usages of the moved item across the entire codebase
- **FR-009**: System MUST add module declarations (`mod` statements) to parent modules for newly created modules
- **FR-010**: System MUST preserve item visibility modifiers (pub, pub(crate), pub(super), private)
- **FR-011**: System MUST validate that the target path is syntactically valid before making any changes
- **FR-012**: System MUST validate that the move operation won't violate visibility or dependency rules
- **FR-013**: System MUST detect and warn about name conflicts at the target location before making changes
- **FR-014**: System MUST handle the operation atomically or provide rollback capability if errors occur
- **FR-015**: System MUST provide clear error messages when operations cannot be completed
- **FR-016**: System MUST support moving various item types (structs, enums, functions, traits, type aliases, constants, statics)
- **FR-017**: System MUST update re-exports (`pub use` statements) that reference the moved item
- **FR-018**: System MUST handle moving items that are referenced in macro invocations
- **FR-019**: System MUST preserve relative imports within the moved item (if it contains child items or impl blocks)

### Key Entities

- **Item**: A code element that can be moved (struct, enum, function, trait, type alias, constant, static). Has a name, location (file and module path), visibility, definition, and set of references throughout the codebase.

- **Module**: A logical grouping of items corresponding to files or directories. Has a name, parent module, path from crate root, and style (directory with mod.rs vs named file).

- **Path**: A fully-qualified identifier specifying an item's location. Composed of crate root (`crate`), sequence of module names, and item name. Used to specify both current and target locations.

- **Reference**: A usage of an item in code, including imports (`use` statements), direct qualified usage, and unqualified usage within scope. Must be updated when item location or name changes.

- **Module Declaration**: A `mod` statement that makes a module visible to its parent. Must be added when creating new modules.

### Assumptions

- **Module style detection**: When project uses inconsistent module styles, system will default to creating named files (e.g., `module_name.rs`) rather than directories with `mod.rs`, as this is more commonly used in modern Rust projects.

- **File encoding**: All source files use UTF-8 encoding, consistent with Rust standards.

- **Workspace structure**: The feature operates within a single crate; cross-crate moves are not supported in initial implementation.

- **Import style**: System will use fully-qualified paths in generated imports when ambiguity exists, and will prefer `use` statements over inline qualification for better readability.

- **Cancellation behavior**: Operations can be cancelled via rust-analyzer's standard cancellation mechanism (revision counter) and will leave the codebase in a consistent state.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can move items between modules in under 10 seconds from initiating rename to seeing all updates complete
- **SC-002**: All imports and references are automatically updated with 100% accuracy (no broken references after move operation)
- **SC-003**: Files and directories are created automatically based on the target path without requiring any manual file system operations
- **SC-004**: 95% of attempted move operations complete successfully on first try without errors or requiring manual fixes
- **SC-005**: Move operations can be undone in a single step using standard editor undo functionality
- **SC-006**: Developers report that code reorganization tasks take 50% less time compared to manual cut-paste-update workflows
