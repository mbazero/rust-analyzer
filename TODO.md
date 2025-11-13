# Active
- Add a bunch of tests for external ref updates and all different conditions
- Clean up stuff below

- Resolve glom external refs
    - Looks like there's existing functionality in resolver to get glob imports
- Clean up external ref updating logic
- Get tests to pass with new code path
- Implement internal ref updates
    - Write a basic test
    - Tree traversal to build data structure

# Inbox
- Implement smart qualified ref update
- Add visibility for import update
- Make RM data structures operate on raw ast::Items
- Remove any external ref mod imports that are no longer used
- Add generalized add import conflict detection
  - This is really only an issue when you add a smart module import
  
# Qualified external ref update
Qualified external ref means that the origin mod is a descendent of the import mod.
```rust
use crate::foo::bar;
let x = bar::baz::origin::OriginStruct;
```
Origin mod `origin` is a descendent of import mod `bar`. Thus the qualified ref can be relative to the import mod.

## Qualified ref is single-qualifier mod import
- Import new target mod
- Update qualified ref to be single-qualifier import from the new target mod

## Target mod is descendent
If target mod is also a descendent of the import mod:
- Don't change the import
- Update qualified ref to reflect the new descendent path

# Random Notes
Rename-move (RM) is triggered by normal rename code action with a fully qualified rename path--e.g.
crate::foo::bar::NewStructName.

Everything below is a tentative sketch for the implementation and is subject to change.

Tentative rename-move steps:
- Parse new_path into a ModPath
- Find definition and ensure it can be rename-moved
  - To start, only support Module, Function, Adt, Const, and Static definitions
- Find associated impls in file
- Construct RenameMoveComponents struct with definitions and associated impl
- Get or create the destination module
  - If destination module doesn't exist, we need to add source edits to create it
  - Also need to support creating intermediate modules for nested paths
- Move all RenameMoveComponents to target module
  - See extract_module.rs and move_mod_to_file.rs for some guidance here
- Pull in required imports
  - Update internal use statements
  - Find any unresolved internal refs
     - Refs can be resolved by internal use statements or use statements already in the module
- Update internal visibility
     - Struct fields
     - Impl constants
     - Function defs
- Update all external usages (rename)
  - This can probably be done with existing rename.rs infrastructure with some slight
  modifications

Below are some random notes on the implementation:

Possible pre-compilation approach:
- Parse the mod path
- Find the definition, ensure it can be renamed
- Collect inbound references (in_refs)
- Collect outbound references (out_refs)

Validation:
- Determine minimum visibility required of the struct after move based on inbound references
- Ensure all internal refs are still accessible
- If destination module already exists, ensure new name won't clash with an existing name

New module changes:
- Create module if it doesn't exist
- Copy over text range of move item
- Satisfy out_refs (resolve imports)
  - Insert use items for all out_refs
  - De-dup against existing use statements
- Satisfy in_refs
  - Change visibility of definition + decords
  - Update external use statements (simple rename)

Old module changes:
- Remove any use statements that only satisfied imports within moved selection

Other module changes:
- Update external use statements (simple rename)

struct OutRef {
   used_in_original_mod: bool,
   used_in_new_mod: bool,
}

Edge cases:
- Proper alias updating
- Ensure you can't rename-move non-top-level functions
  - At least those within an impl block
- Support raw identifiers
- Derive macros, etc must be moved as well
- Adt defined within function etc
  - Do we want to support this?
- Adt defined within inline module
- Delete src module if it becomes empty
- Handle inline module targets
  - Need to indent appropriately
- Handle inline module origins
  - Need to unindent
- Actually, do need to handle multiple defintions since the def could be annotated by a proc macro which creates some additional defs, maybe
- Test that illegal idents and mod paths are disallowed

Misc:
- Once a functional implementation is complete, see how much functionality can be merged with:
  - rename.rs
  - extract_module.rs
  - move_mod_to_file.rs
- extract_module and move_mod_to_file are both special cases of rename_move
  - These should be updated to call into rename_move utils
- Ensure that associated functions, consts, static, cannot be moved
     - Maybe just check that parent is a module?
- Ensure that struct embedded within a function cannot be moved
     - Maybe just check that the parent is a module?

