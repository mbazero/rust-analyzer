# Move Item Refactoring - Feature Implementation Package

**Status**: ✅ Specification Complete | 📘 Implementation Guides Ready | ⏳ Implementation Pending

## Overview

Complete specification and implementation plan for adding move item refactoring to rust-analyzer. This feature allows developers to move Rust items (structs, enums, functions, etc.) between modules by specifying fully-qualified paths during rename operations.

## Quick Navigation

### For Product Managers / Stakeholders
- 📋 **[spec.md](./spec.md)** - Feature specification with user stories and requirements
- ✅ **[checklists/requirements.md](./checklists/requirements.md)** - Quality validation (all checks passed)

### For Developers / Implementers
- 🚀 **[QUICK_START.md](./QUICK_START.md)** - Get started in 30 minutes ⭐ START HERE
- 📚 **[IMPLEMENTATION_GUIDE.md](./IMPLEMENTATION_GUIDE.md)** - Detailed step-by-step guide with code examples
- 📝 **[tasks.md](./tasks.md)** - Complete task breakdown (81 tasks, 30 for MVP)

### For Architects / Reviewers
- 🏗️ **[plan.md](./plan.md)** - Technical architecture and design decisions
- 🔬 **[research.md](./research.md)** - Architectural research and technology choices
- 📊 **[data-model.md](./data-model.md)** - Domain entities and relationships

### For QA / Testers
- 🧪 **[quickstart.md](./quickstart.md)** - Manual testing guide (18+ test scenarios)
- ✅ **[checklists/requirements.md](./checklists/requirements.md)** - Quality checklist

## Feature Summary

### User Stories (Prioritized)

**P1 - Move Item to Sibling Module** (MVP) 🎯
- Move items within same parent directory
- Automatic file creation and import updates
- **Estimated effort**: 8-12 hours

**P2 - Move and Rename Simultaneously**
- Move AND rename in one atomic operation
- All references updated automatically

**P3 - Cross-Boundary with Auto-Creation**
- Create entire module hierarchies automatically
- Deep nesting support

### Success Criteria

- ✅ Operations complete in under 10 seconds
- ✅ 100% reference update accuracy
- ✅ Automatic file/directory creation
- ✅ 95% first-try success rate
- ✅ Single-step undo support

## Implementation Status

### Completed ✅
- [x] Feature specification (spec.md)
- [x] Implementation plan (plan.md)
- [x] Research and architectural decisions (research.md)
- [x] Data model design (data-model.md)
- [x] Task breakdown (tasks.md)
- [x] Testing guide (quickstart.md)
- [x] **Implementation guides (IMPLEMENTATION_GUIDE.md + QUICK_START.md)**
- [x] Quality validation (all checklists passed)

### Pending ⏳
- [ ] MVP Implementation (Phase 1 + 2 + 3 = User Story 1)
  - [ ] Phase 1: Setup (T001-T005)
  - [ ] Phase 2: Foundational (T006-T012)
  - [ ] Phase 3: User Story 1 (T013-T030)
- [ ] User Story 2 Implementation (T031-T044)
- [ ] User Story 3 Implementation (T045-T058)
- [ ] Edge Cases & Polish (T059-T081)

## Getting Started

### For First-Time Contributors

1. **Read the Quick Start** (30 minutes)
   ```bash
   open specs/001-specify-scripts-bash/QUICK_START.md
   ```

2. **Set up environment** (5 minutes)
   ```bash
   git clone https://github.com/rust-lang/rust-analyzer.git
   cd rust-analyzer
   cargo build
   ```

3. **Create feature branch**
   ```bash
   git checkout -b feature/move-item-refactoring
   ```

4. **Follow the Implementation Guide**
   ```bash
   open specs/001-specify-scripts-bash/IMPLEMENTATION_GUIDE.md
   ```

### For Experienced rust-analyzer Contributors

1. Review [spec.md](./spec.md) for requirements
2. Check [plan.md](./plan.md) for architecture
3. Jump to [IMPLEMENTATION_GUIDE.md](./IMPLEMENTATION_GUIDE.md)
4. Start implementing from [tasks.md](./tasks.md)

## Technical Stack

- **Language**: Rust (rust-analyzer workspace)
- **Core crates**: `hir`, `ide-db`, `syntax`, `vfs`, `base-db`
- **Testing**: `expect-test` (snapshot testing at IDE boundary)
- **Pattern**: Extend existing rename infrastructure
- **Integration**: `crates/ide-assists/src/handlers/move_item.rs` (new file)

## Constitutional Compliance

All rust-analyzer development principles satisfied:

✅ **Code Quality First** - Extends battle-tested infrastructure
✅ **Three-Tier Testing** - Middle tier (IDE boundary) with expect-test
✅ **Performance-First** - Salsa queries, incremental computation
✅ **Robust Error Handling** - Graceful degradation, clear errors
✅ **UX Consistency** - Editor terminology, stateless, atomic operations

See [plan.md](./plan.md) for detailed compliance analysis.

## Documentation Structure

```
specs/001-specify-scripts-bash/
├── README.md                      # This file - navigation hub
├── spec.md                        # Feature specification
├── plan.md                        # Implementation plan
├── tasks.md                       # Task breakdown (81 tasks)
├── IMPLEMENTATION_GUIDE.md        # Detailed guide with code examples ⭐
├── QUICK_START.md                 # 30-minute quickstart ⭐
├── research.md                    # Architectural research
├── data-model.md                  # Domain model
├── quickstart.md                  # Testing guide
└── checklists/
    └── requirements.md            # Quality validation (16/16 passed)
```

## Time Estimates

| Phase | Tasks | Effort | Deliverable |
|-------|-------|--------|-------------|
| **MVP (US1)** | T001-T030 | 8-12 hours | Sibling module moves |
| User Story 2 | T031-T044 | 4-6 hours | Move + rename |
| User Story 3 | T045-T058 | 4-6 hours | Hierarchy creation |
| Edge Cases | T059-T070 | 3-4 hours | Robustness |
| Polish | T071-T081 | 2-3 hours | Production-ready |
| **Total** | 81 tasks | 21-31 hours | Full feature |

## Key Files to Implement

### New Files
- `crates/ide-assists/src/handlers/move_item.rs` - Core logic (MVP: ~500-800 lines)

### Modified Files
- `crates/ide-assists/src/handlers.rs` - Register new assist
- `crates/ide/src/rename.rs` - Integration (optional for MVP)

### Test Files
- Inline tests in `move_item.rs` using `expect-test`
- 6 tests for MVP, 18 tests total

## Quality Assurance

### Checklist Status
- [x] Content Quality (4/4)
- [x] Requirement Completeness (8/8)
- [x] Feature Readiness (4/4)
- [x] **Overall**: ✅ ALL CHECKS PASSED (16/16)

### Testing Strategy
- **TDD Approach**: Write tests first, verify fail, implement, verify pass
- **Middle Tier**: Test at IDE boundary (constitutional requirement)
- **Snapshot Testing**: Use `expect-test` with fixture format
- **Update Command**: `UPDATE_EXPECT=1 cargo test`

### Performance Targets
- < 10 seconds per move operation
- No global invalidation (incremental computation)
- Cancellation support via salsa

## Contributing

### Before You Start
1. Read [QUICK_START.md](./QUICK_START.md)
2. Review existing assists in `crates/ide-assists/src/handlers/`
3. Study rename infrastructure in `crates/ide/src/rename.rs`

### Development Loop
```bash
# 1. Write test
# 2. Verify it fails
cargo test -p ide-assists move_item::tests::test_name

# 3. Implement
# 4. Verify it passes
cargo test -p ide-assists move_item::tests::test_name

# 5. Update expectations if needed
UPDATE_EXPECT=1 cargo test -p ide-assists move_item

# 6. Check quality
cargo clippy -p ide-assists
cargo test -p xtask  # tidy checks
```

### Commit Checklist
- [ ] All tests passing
- [ ] Clippy warnings resolved
- [ ] Tidy checks passing (`cargo test -p xtask`)
- [ ] Module documentation complete
- [ ] No `dbg!()` or `todo!()` in production code

## Resources

- **rust-analyzer docs**: https://rust-analyzer.github.io/
- **Architecture**: https://rust-analyzer.github.io/book/contributing/architecture.html
- **Contributing**: https://github.com/rust-lang/rust-analyzer/blob/master/CONTRIBUTING.md
- **Zulip chat**: https://rust-lang.zulipchat.com/#narrow/stream/185405-t-compiler.2Frust-analyzer

## Support

### Getting Help
- **Quick questions**: Rust-analyzer Zulip stream
- **Bug reports**: GitHub issues with `A-assists` label
- **Design discussions**: Link to this specification

### Common Issues
See [IMPLEMENTATION_GUIDE.md](./IMPLEMENTATION_GUIDE.md#common-issues--solutions) for:
- Tests failing with "assist not applicable"
- File paths incorrect
- Imports not updating
- Module declarations not added

## License

This specification follows rust-analyzer's dual license:
- MIT License
- Apache License (Version 2.0)

## Version History

- **v1.0.0** (2025-10-16): Initial specification complete
  - All planning artifacts created
  - Constitution compliance verified
  - Implementation guides added
  - Quality validation passed (16/16 checks)

---

**Ready to implement?** → Start with [QUICK_START.md](./QUICK_START.md) 🚀
