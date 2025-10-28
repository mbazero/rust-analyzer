use crate::rename::Result;
use hir::{ModPath, Semantics};

use crate::{RootDatabase, defs::Definition, source_change::SourceChange};

impl Definition {
    pub fn rename_move(
        &self,
        _sema: &Semantics<'_, RootDatabase>,
        _new_path: ModPath,
    ) -> Result<SourceChange> {
        todo!()
    }
}
