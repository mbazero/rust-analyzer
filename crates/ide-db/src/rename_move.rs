use crate::rename::Result;
use hir::{ModPath, Semantics};
use std::fmt::{self, Display};

use crate::{
    RootDatabase, defs::Definition, rename::RenameDefinition, source_change::SourceChange,
};

impl Definition {
    pub fn rename_move(
        &self,
        sema: &Semantics<'_, RootDatabase>,
        // TODO: accept (name, kind) so we don't have to recompute
        new_path: ModPath,
    ) -> Result<SourceChange> {
        todo!()
    }
}

// TESTS:
// - Won't rename macro
// - Won't rename non def
