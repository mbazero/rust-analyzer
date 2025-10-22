# Implementation Tasks

## 1. Path Parsing and Move Detection

- [x] 1.1 Add path parsing logic to detect fully-qualified paths in rename requests
- [x] 1.2 Implement comparison between current item path and target path
- [x] 1.3 Create `MoveOperation` struct to represent source/destination modules and item names
- [x] 1.4 Add validation for module path components (valid identifiers, no keywords)
- [x] 1.5 Write unit tests for path parsing and move detection

## 2. Module File System Operations

- [x] 2.1 Implement module path resolution to file system paths
- [x] 2.2 Add logic to choose between `mod.rs` and named module files
- [x] 2.3 Create file creation operations for new modules
- [x] 2.4 Implement directory creation for nested module structures
- [x] 2.5 Add detection for existing project module style preferences
- [x] 2.6 Write tests for various module creation scenarios

## 3. Module Declaration Management

- [x] 3.1 Implement parent module detection for new modules
- [x] 3.2 Add logic to insert `pub mod` declarations in parent modules
- [x] 3.3 Handle visibility modifiers for module declarations (pub vs private)
- [x] 3.4 Detect and skip duplicate module declarations
- [x] 3.5 Write tests for module declaration insertion

## 4. Item Definition Relocation

- [x] 4.1 Extract item definition with attributes, docs, and visibility
- [x] 4.2 Identify associated impl blocks for structs/enums/traits
- [x] 4.3 Generate removal TextEdit for source file
- [x] 4.4 Generate insertion TextEdit for destination file
- [x] 4.5 Preserve formatting and comments around moved items
- [x] 4.6 Handle edge cases (last item in file, imports to clean up)
- [x] 4.7 Write tests for definition relocation with various item types

## 5. External Reference Updates

- [x] 5.1 Leverage existing reference-finding infrastructure (find_usages)
- [x] 5.2 Update absolute import paths in other files (use statements)
- [x] 5.3 Update relative import paths in other files (super::, self::)
- [x] 5.4 Update qualified references in other files (Type::method calls)
- [x] 5.5 Generate TextEdits for all external reference updates
- [x] 5.6 Write tests for external reference updates across workspace

## 6. Internal Reference Updates

- [x] 6.1 Analyze references within moved item to items in source module
- [x] 6.2 Detect unqualified references that need imports (e.g., `Helper` → needs `use crate::alpha::Helper;`)
- [x] 6.3 Update relative path references (self::, super::) based on new module context
- [x] 6.4 Generate import statements for source module items referenced by moved code
- [x] 6.5 Preserve imports that remain valid (external crates, unchanged paths)
- [x] 6.6 Handle qualified references within moved code (update if source-module-relative)
- [x] 6.7 Distinguish items moving together vs staying in source (no import needed for co-moved items)
- [x] 6.8 Write tests for internal reference updates in various scenarios

## 7. Integration with Existing Rename Infrastructure

- [ ] 7.1 Extend `Definition::rename()` in `ide-db/src/rename.rs` to handle move operations
- [ ] 7.2 Update `rename()` function in `ide/src/rename.rs` to detect and orchestrate moves
- [ ] 7.3 Integrate file system operations with existing `SourceChange` struct
- [ ] 7.4 Ensure backward compatibility with simple rename operations
- [ ] 7.5 Update LSP handler if needed for file creation notifications

## 8. Visibility Analysis and Updates

- [ ] 8.1 Implement external visibility validation
- [ ] 8.2 Check visibility of all items referenced within moved code
- [ ] 8.3 Validate `pub`, `pub(crate)`, `pub(super)`, `pub(in path)` accessibility rules
- [ ] 8.4 Reject moves when private items become inaccessible
- [ ] 8.5 Implement required visibility calculation for moved item (simplified: private, pub(crate), pub only)
- [ ] 8.6 Find all usages of moved item to determine visibility requirements
- [ ] 8.7 Calculate minimum visibility using simple rule: same module → private, same crate → pub(crate), else → pub
- [ ] 8.8 Update visibility modifiers on moved item definition (never downgrade)
- [ ] 8.9 Update visibility modifiers on associated items (methods in impl blocks)
- [ ] 8.10 Handle struct field visibility (preserve or update as needed)
- [ ] 8.11 Write tests for visibility validation (reject inaccessible references)
- [ ] 8.12 Write tests for visibility updates (private → pub(crate), private → pub, preserve existing)

## 9. Validation and Error Handling

- [ ] 9.1 Validate destination doesn't have conflicting item names
- [ ] 9.2 Check for circular module dependencies
- [ ] 9.3 Validate item types are movable (reject locals, external crate items)
- [ ] 9.4 Return clear error messages for validation failures
- [ ] 9.5 Write tests for all error conditions

## 10. Supported Item Types

- [ ] 10.1 Add support for moving structs
- [ ] 10.2 Add support for moving enums
- [ ] 10.3 Add support for moving functions
- [ ] 10.4 Add support for moving traits
- [ ] 10.5 Add support for moving type aliases
- [ ] 10.6 Add support for moving constants and statics
- [ ] 10.7 Add support for moving unions
- [ ] 10.8 Explicitly reject unsupported types with clear errors

## 11. Testing

- [ ] 11.1 Create test fixtures for simple cross-module moves
- [ ] 11.2 Create test fixtures for moves with file/directory creation
- [ ] 11.3 Create test fixtures for moves with impl blocks
- [ ] 11.4 Create test fixtures for internal reference updates (unqualified, self::, super::)
- [ ] 11.5 Create test fixtures for visibility validation (reject inaccessible items)
- [ ] 11.6 Create test fixtures for visibility updates (private → pub(crate), private → pub, preserve existing)
- [ ] 11.7 Add tests for edge cases (relative imports, re-exports, glob imports)
- [ ] 11.8 Add tests for error conditions
- [ ] 11.9 Add integration tests using existing rename test infrastructure

## 12. Documentation

- [ ] 12.1 Document new rename-to-move feature in user-facing docs
- [ ] 12.2 Add code documentation for new public APIs
- [ ] 12.3 Update CONTRIBUTING.md if needed for testing guidance
- [ ] 12.4 Create examples for common use cases
- [ ] 12.5 Document visibility behavior (validation and automatic updates)
