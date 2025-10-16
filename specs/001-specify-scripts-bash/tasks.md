# Tasks: Move Item Refactoring

**Input**: Design documents from `/specs/001-specify-scripts-bash/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, quickstart.md

**Tests**: Tests are included as this is infrastructure-level code requiring TDD approach per rust-analyzer constitution.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

**Implementation Guides**:
- 📚 **[IMPLEMENTATION_GUIDE.md](./IMPLEMENTATION_GUIDE.md)** - Detailed step-by-step guide with code examples for all 30 MVP tasks
- 🚀 **[QUICK_START.md](./QUICK_START.md)** - Get started in 30 minutes with skeleton and first test

## Format: `[ID] [P?] [Story] Description`
- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions
- **rust-analyzer monorepo**: `crates/` at repository root
- Core logic: `crates/ide-assists/src/handlers/move_item.rs` (new file)
- Extensions: `crates/ide/src/rename.rs`, `crates/ide-db/src/rename.rs`
- Tests: Inline in `move_item.rs` using `expect-test`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic understanding of existing codebase

- [X] T001 Review existing rename infrastructure in crates/ide/src/rename.rs
- [X] T002 [P] Review search and reference finding in crates/ide-db/src/search.rs
- [X] T003 [P] Review file system operations in crates/ide-db/src/source_change.rs
- [X] T004 [P] Review module resolution in crates/hir/src/lib.rs
- [X] T005 [P] Study existing assists testing patterns in crates/ide-assists/src/tests.rs

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T006 Create module skeleton with module docs in crates/ide-assists/src/handlers/move_item.rs
- [X] T007 [P] Define core data structures (MoveableItem, TargetPath) in crates/ide-assists/src/handlers/move_item.rs
- [X] T008 [P] Implement path parsing function to detect fully-qualified paths in crates/ide-assists/src/handlers/move_item.rs
- [X] T009 Implement path validation (syntax + semantics) in crates/ide-assists/src/handlers/move_item.rs
- [X] T010 [P] Implement item extraction logic (with attributes, docs, visibility) in crates/ide-assists/src/handlers/move_item.rs
- [X] T011 [P] Create test helper functions for fixture-based testing in crates/ide-assists/src/handlers/move_item.rs
- [X] T012 Add regression marks using cov_mark in crates/ide-assists/src/handlers/move_item.rs

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Move Item to Sibling Module (Priority: P1) 🎯 MVP

**Goal**: Enable moving items to sibling modules (same parent directory) with file creation and import updates

**Independent Test**: Create struct in one file, invoke rename with sibling module path, verify struct in new file with imports updated

### Tests for User Story 1 (TDD - write tests FIRST)

**NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T013 [P] [US1] Test: Move struct to new sibling file in crates/ide-assists/src/handlers/move_item.rs
- [X] T014 [P] [US1] Test: Move struct to existing sibling file in crates/ide-assists/src/handlers/move_item.rs
- [X] T015 [P] [US1] Test: Move function to sibling module in crates/ide-assists/src/handlers/move_item.rs
- [X] T016 [P] [US1] Test: Move enum with variants to sibling in crates/ide-assists/src/handlers/move_item.rs
- [X] T017 [P] [US1] Test: Update imports after sibling move in crates/ide-assists/src/handlers/move_item.rs
- [X] T018 [P] [US1] Test: Update all references (not just imports) in crates/ide-assists/src/handlers/move_item.rs

### Implementation for User Story 1

- [X] T019 [US1] Implement is_sibling_move() predicate in crates/ide-assists/src/handlers/move_item.rs
- [X] T020 [US1] Implement target file path computation for siblings in crates/ide-assists/src/handlers/move_item.rs
- [X] T021 [US1] Implement file creation logic (with module docs template) in crates/ide-assists/src/handlers/move_item.rs
- [X] T022 [US1] Implement item removal from source file in crates/ide-assists/src/handlers/move_item.rs
- [X] T023 [US1] Implement item insertion into target file in crates/ide-assists/src/handlers/move_item.rs
- [X] T024 [US1] Implement module declaration addition to parent in crates/ide-assists/src/handlers/move_item.rs
- [X] T025 [US1] Implement reference finding via Definition::usages() in crates/ide-assists/src/handlers/move_item.rs
- [X] T026 [US1] Implement import path rewriting in crates/ide-assists/src/handlers/move_item.rs
- [X] T027 [US1] Implement usage reference updates in crates/ide-assists/src/handlers/move_item.rs
- [X] T028 [US1] Integrate with rename action entry point in crates/ide/src/rename.rs
- [X] T029 [US1] Add error handling for edge cases (name conflicts, invalid paths) in crates/ide-assists/src/handlers/move_item.rs
- [X] T030 [US1] Verify all US1 tests pass with implementation

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently (MVP complete!)

---

## Phase 4: User Story 2 - Move and Rename Simultaneously (Priority: P2)

**Goal**: Enable moving items to different modules while renaming them in one atomic operation

**Independent Test**: Create item with one name, invoke rename with different module + name, verify both location and name change

### Tests for User Story 2 (TDD - write tests FIRST)

- [ ] T031 [P] [US2] Test: Move and rename struct in crates/ide-assists/src/handlers/move_item.rs
- [ ] T032 [P] [US2] Test: Move and rename enum with pattern matches in crates/ide-assists/src/handlers/move_item.rs
- [ ] T033 [P] [US2] Test: Move and rename trait with impls in crates/ide-assists/src/handlers/move_item.rs
- [ ] T034 [P] [US2] Test: Move and rename with generic parameters in crates/ide-assists/src/handlers/move_item.rs
- [ ] T035 [P] [US2] Test: Update impl blocks after rename in crates/ide-assists/src/handlers/move_item.rs
- [ ] T036 [P] [US2] Test: Detect name conflicts before rename in crates/ide-assists/src/handlers/move_item.rs

### Implementation for User Story 2

- [ ] T037 [US2] Implement rename detection (compare target name vs source name) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T038 [US2] Extend reference updates to handle name changes in crates/ide-assists/src/handlers/move_item.rs
- [ ] T039 [US2] Implement item text replacement with new name in crates/ide-assists/src/handlers/move_item.rs
- [ ] T040 [US2] Handle impl block updates (impl OldName → impl NewName) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T041 [US2] Handle trait impl updates (impl Trait for OldName) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T042 [US2] Extend name conflict detection for rename scenarios in crates/ide-assists/src/handlers/move_item.rs
- [ ] T043 [US2] Update import rewriting to include name changes in crates/ide-assists/src/handlers/move_item.rs
- [ ] T044 [US2] Verify all US2 tests pass with implementation

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently

---

## Phase 5: User Story 3 - Cross-Boundary with Auto-Creation (Priority: P3)

**Goal**: Enable creating entire module hierarchies automatically when moving items

**Independent Test**: Create item in one hierarchy, move to non-existent hierarchy, verify all dirs/files/declarations created

### Tests for User Story 3 (TDD - write tests FIRST)

- [ ] T045 [P] [US3] Test: Create nested module hierarchy (2 levels) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T046 [P] [US3] Test: Create deeply nested hierarchy (5 levels) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T047 [P] [US3] Test: Multiple moves to same new module (no duplicate decls) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T048 [P] [US3] Test: Create directories with named file style in crates/ide-assists/src/handlers/move_item.rs
- [ ] T049 [P] [US3] Test: Add mod declarations at all intermediate levels in crates/ide-assists/src/handlers/move_item.rs
- [ ] T050 [P] [US3] Test: Handle existing partial hierarchy in crates/ide-assists/src/handlers/move_item.rs

### Implementation for User Story 3

- [ ] T051 [US3] Implement module hierarchy analysis (which modules exist/missing) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T052 [US3] Implement directory path computation from module path in crates/ide-assists/src/handlers/move_item.rs
- [ ] T053 [US3] Implement create_intermediate_modules() function in crates/ide-assists/src/handlers/move_item.rs
- [ ] T054 [US3] Implement module declaration chain insertion in crates/ide-assists/src/handlers/move_item.rs
- [ ] T055 [US3] Implement duplicate declaration prevention logic in crates/ide-assists/src/handlers/move_item.rs
- [ ] T056 [US3] Integrate directory creation with FileSystemEdit::CreateFile in crates/ide-assists/src/handlers/move_item.rs
- [ ] T057 [US3] Add module doc comments to generated module files in crates/ide-assists/src/handlers/move_item.rs
- [ ] T058 [US3] Verify all US3 tests pass with implementation

**Checkpoint**: All user stories should now be independently functional

---

## Phase 6: Edge Cases & Robustness

**Purpose**: Handle edge cases and ensure robustness across all user stories

- [ ] T059 [P] Test: Name conflict detection and error message in crates/ide-assists/src/handlers/move_item.rs
- [ ] T060 [P] Test: Invalid path syntax error in crates/ide-assists/src/handlers/move_item.rs
- [ ] T061 [P] Test: Visibility preservation (pub, pub(crate), private) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T062 [P] Test: Doc comments and attributes preservation in crates/ide-assists/src/handlers/move_item.rs
- [ ] T063 [P] Test: Re-export (pub use) updates in crates/ide-assists/src/handlers/move_item.rs
- [ ] T064 [P] Test: Macro usage handling in crates/ide-assists/src/handlers/move_item.rs
- [ ] T065 [P] Test: Multiple item types (struct, enum, fn, trait, const, static) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T066 Implement visibility validation logic in crates/ide-assists/src/handlers/move_item.rs
- [ ] T067 Implement attribute and doc comment preservation in crates/ide-assists/src/handlers/move_item.rs
- [ ] T068 Implement re-export detection and updating in crates/ide-assists/src/handlers/move_item.rs
- [ ] T069 Add validation for macro contexts (prevent moving macro-defined items) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T070 Implement support for all item types per FR-016 in crates/ide-assists/src/handlers/move_item.rs

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories and final quality assurance

- [ ] T071 [P] Add comprehensive error messages with actionable guidance in crates/ide-assists/src/handlers/move_item.rs
- [ ] T072 [P] Add cov_mark regression marks for all major code paths in crates/ide-assists/src/handlers/move_item.rs
- [ ] T073 [P] Optimize reference search with SearchScope in crates/ide-assists/src/handlers/move_item.rs
- [ ] T074 [P] Add cancellation support verification (via salsa) in crates/ide-assists/src/handlers/move_item.rs
- [ ] T075 [P] Run clippy and fix all warnings in crates/ide-assists/src/handlers/move_item.rs
- [ ] T076 [P] Run rustfmt on all modified files
- [ ] T077 [P] Verify module-level documentation exists and is complete in crates/ide-assists/src/handlers/move_item.rs
- [ ] T078 Run full test suite: cargo test in repository root
- [ ] T079 Run tidy checks: cargo test -p xtask in repository root
- [ ] T080 Performance test with quickstart.md scenarios (< 10 second requirement)
- [ ] T081 Manual smoke testing with real rust-analyzer (build and test locally)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-5)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2 → P3)
- **Edge Cases (Phase 6)**: Can run in parallel with user stories or after
- **Polish (Phase 7)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Foundational (Phase 2) - Builds on US1 infrastructure but independently testable
- **User Story 3 (P3)**: Can start after Foundational (Phase 2) - Builds on US1 + US2 but independently testable

### Within Each User Story

- Tests (TDD) MUST be written and FAIL before implementation
- Implementation tasks are mostly sequential within a story
- Tests can be written in parallel (marked with [P])
- Story complete before moving to next priority

### Parallel Opportunities

- **Setup tasks (Phase 1)**: All marked [P] can run in parallel (T002-T005)
- **Foundational (Phase 2)**: T007-T008, T010-T012 can run in parallel
- **User Story Tests**: All tests within a story marked [P] can be written in parallel
- **Edge Case Tests**: All marked [P] can run in parallel (T059-T065)
- **Polish**: Tasks T071-T077 can run in parallel

---

## Parallel Example: User Story 1

```bash
# Write all US1 tests in parallel (before implementation):
Task: "Test: Move struct to new sibling file" (T013)
Task: "Test: Move struct to existing sibling file" (T014)
Task: "Test: Move function to sibling module" (T015)
Task: "Test: Move enum with variants" (T016)
Task: "Test: Update imports after move" (T017)
Task: "Test: Update all references" (T018)

# Then implement sequentially:
Task: "Implement is_sibling_move()" (T019)
Task: "Implement target file path computation" (T020)
# ... and so on
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (write tests → verify fail → implement → verify pass)
4. **STOP and VALIDATE**: Test User Story 1 independently with quickstart.md scenarios
5. **MVP COMPLETE**: Can ship sibling module moves

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → **Ship MVP** (sibling moves)
3. Add User Story 2 → Test independently → Ship (move + rename)
4. Add User Story 3 → Test independently → Ship (full hierarchy creation)
5. Add Edge Cases + Polish → Ship (production-ready)

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (tests + implementation)
   - Developer B: User Story 2 (tests + implementation)
   - Developer C: User Story 3 (tests + implementation)
   - Developer D: Edge cases (T059-T070)
3. Stories complete and integrate independently
4. Team collaborates on Polish phase

---

## Notes

- [P] tasks = different files/aspects, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- **TDD is mandatory** per rust-analyzer constitution: Write tests → Verify fail → Implement → Verify pass
- Use `expect-test` for snapshot testing (update with `env UPDATE_EXPECT=1 cargo qt`)
- Use `cov_mark` for regression tracking
- Commit after each major milestone (phase completion, story completion)
- Stop at any checkpoint to validate story independently
- All tests must pass before moving to next phase: `cargo test && cargo test -p xtask`
- Performance target: < 10 seconds per move operation
- Avoid: vague tasks, same file conflicts (minimize), cross-story dependencies that break independence
