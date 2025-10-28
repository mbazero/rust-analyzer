use crate::{rename::Result, search::UsageSearchResult};
use hir::{ModPath, Module, Semantics};
use syntax::SyntaxNode;

use crate::{RootDatabase, defs::Definition, source_change::SourceChange};

pub struct RenameMoveDefinition {
    definition: Definition,
    new_name: String,
    src_mod: Module,
    dst_mod: Module,
    // NameRefClass::Definition
    internal_usages: Vec<(Definition, SyntaxNode)>,
    // Need to update these to point to the moved def
    external_usages: UsageSearchResult,
    // Need to up visibility on these
    record_fields: Vec<SyntaxNode>,
}

impl Definition {
    pub fn rename_move(
        &self,
        sema: &Semantics<'_, RootDatabase>,
        new_name: &str,
        dst_module: Module,
    ) -> Result<SourceChange> {
        todo!()
    }
}
