use ide_db::{FilePosition, RootDatabase, source_change::SourceChange};

use crate::rename::RenameResult;

pub(crate) fn rename_move(
    db: &RootDatabase,
    position: FilePosition,
    new_name: &str,
) -> RenameResult<SourceChange> {
    // TODO:
    //
    // Compilation:
    // - Parse the mod path
    // - Find the definition, ensure it can be renamed
    // - Collect inbound references (in_refs)
    // - Collect outbound references (out_refs)
    //
    // Validation:
    // - Determine minimum visibility required of the struct after move based on inbound references
    // - Ensure all internal refs are still accessible
    // - If destination module already exists, ensure new name won't clash with an existing name
    //
    // New module changes:
    // - Create module if it doesn't exist
    // - Copy over text range of move item
    // - Satisfy out_refs (resolve imports)
    //   - Insert use items for all out_refs
    //   - De-dup against existing use statements
    // - Satisfy in_refs
    //   - Change visibility of definition + decords
    //   - Update external use statements (simple rename)
    //
    // Old module changes:
    // - Remove any use statements that only satisfied imports within moved selection
    //
    // Other module changes:
    // - Update external use statements (simple rename)
    //
    // struct OutRef {
    //    used_in_original_mod: bool,
    //    used_in_new_mod: bool,
    //
    // }
    //
    todo!()
}
