# Implementation Plan

- [x] 1. Set up core data structures and path parsing using existing infrastructure
  - Create `RenameContext` and `RenameOperationType` enums in `crates/ide-db/src/rename.rs`
  - Implement `parse_rename_target` function leveraging existing `ModPath::from_src`
  - Add `parse_fully_qualified_path` to extract module path and item name from input
  - _Requirements: 1.1, 1.3, 1.4, 1.5_

- [x] 2. Implement path comparison and operation type detection
  - Create `PathComparison` enum and `determine_operation_type` function
  - Add logic to compare current item's `ModPath` with target `ModPath`
  - Implement detection of simple rename vs move operations vs relative moves
  - Support both absolute (`crate::`) and relative path specifications
  - _Requirements: 1.2, 5.1, 5.2, 5.3_

- [x] 3. Create module structure analysis and planning
  - [x] 3.1 Implement `ModuleStructure` analysis
    - Add function to traverse current module hierarchy using existing rust-analyzer APIs
    - Identify existing vs missing module segments in target path
    - Create `ModuleCreationPlan` with required directories and files
    - Integrate with user preferences for module organization
    - _Requirements: 2.1, 2.2, 2.3_

  - [x] 3.2 Add module file creation logic
    - Implement `ModuleOrganizationPreferences` and `ModuleFileSpec` structures
    - Add logic to determine mod.rs vs module_name.rs file structure based on preferences
    - Generate appropriate module file content and declarations
    - Ensure proper module integration into existing module tree
    - _Requirements: 2.2, 2.3, 2.4, 2.5_

- [x] 4. Implement item movement and module creation
  - [x] 4.1 Create file system operations for module structure
    - Add directory creation logic with proper error handling
    - Implement module file creation with correct content
    - Add module declarations to parent modules
    - _Requirements: 2.1, 2.4, 2.5_

  - [x] 4.2 Implement item extraction and insertion
    - Extract item definition from source module
    - Insert item into target module with correct formatting
    - Handle visibility modifiers and attributes preservation
    - _Requirements: 4.4, 4.5_

- [ ] 5. Enhance reference updating for moved items
  - [ ] 5.1 Update external references and imports
    - Implement `update_external_references` function to handle module changes
    - Update import statements to reflect new item location
    - Handle fully-qualified path references throughout codebase
    - Update re-exports and maintain existing import aliases where applicable
    - _Requirements: 3.1, 3.2, 3.3, 3.4, 3.5_

  - [ ] 5.2 Implement internal reference updates
    - Create `InternalReference` and `InternalReferenceType` structures
    - Analyze imports and references within moved item's source code
    - Update relative module references (`super::`, `crate::`) for new module context
    - Adjust sibling module references when item moves to different hierarchy level
    - Handle self-referential paths within the moved item
    - _Requirements: 6.1, 6.2, 6.3, 6.4, 6.5_

- [ ] 6. Add validation and conflict detection (pre-operation checks)
  - [ ] 6.1 Implement name conflict detection
    - Check for existing items with same name in target module
    - Validate that move operation won't create naming conflicts
    - Report conflicts with detailed error messages
    - _Requirements: 4.1_

  - [ ] 6.2 Add dependency and visibility validation
    - Implement `validate_move_operation` with comprehensive checks
    - Check for circular dependency creation
    - Validate visibility constraints are maintained after move
    - Ensure moved items remain accessible to existing code
    - Validate that move won't violate Rust's module privacy rules
    - _Requirements: 4.2, 4.3, 4.4, 4.5_

- [ ] 7. Integrate move functionality into existing rename system
  - [ ] 7.1 Modify main rename entry point
    - Update `rename` function in `crates/ide/src/rename.rs` to detect FQP input
    - Add routing logic between simple rename and move operations
    - Maintain backward compatibility with existing rename behavior
    - _Requirements: 5.3, 5.4, 5.5_

  - [ ] 7.2 Update prepare_rename for move operations
    - Extend `prepare_rename` to handle fully-qualified path validation
    - Add preview information for move operations
    - Ensure consistent behavior with existing rename preparation
    - _Requirements: 5.4_

- [ ] 8. Add comprehensive error handling
  - Create `MoveRenameError` enum with specific error types for all failure scenarios
  - Implement detailed error messages for path validation, conflicts, and visibility issues
  - Add rollback logic for partial operation failures to maintain code integrity
  - Handle module creation failures and reference update errors gracefully
  - _Requirements: 4.1, 4.2, 4.3, 4.5, plus error handling for Requirements 2.1, 2.5, 3.1-3.5, 6.1-6.5_

- [ ] 9. Write comprehensive tests
  - [ ] 9.1 Add unit tests for path parsing and validation
    - Test valid and invalid fully-qualified path formats
    - Test edge cases with keywords and special characters
    - Test relative vs absolute path handling
    - _Requirements: 1.1, 1.3_

  - [ ] 9.2 Add integration tests for module creation
    - Test creating nested module structures
    - Test handling of existing vs new modules
    - Test different module file organization preferences
    - _Requirements: 2.1, 2.2, 2.3, 2.4, 2.5_

  - [ ] 9.3 Add end-to-end move operation tests
    - Test moving structs, enums, functions between modules
    - Test complex scenarios with dependencies and visibility
    - Test error cases and conflict detection
    - _Requirements: 3.1, 3.2, 3.3, 4.1, 4.2, 4.3, 6.1, 6.2, 6.3, 6.4, 6.5_

- [ ] 10. Documentation and polish
  - Add inline documentation for new public APIs
  - Update existing rename documentation to mention move capability
  - Add examples of fully-qualified path usage in comments