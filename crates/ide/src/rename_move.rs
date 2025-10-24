use ide_db::{FilePosition, RootDatabase, source_change::SourceChange};

use crate::rename::RenameResult;

pub(crate) fn rename_move(
    db: &RootDatabase,
    position: FilePosition,
    new_name: &str,
) -> RenameResult<SourceChange> {
    todo!()
}
