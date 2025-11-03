use crate::source_change::SourceChangeBuilder;
use hir::HasSource;
use hir::ModuleSource;
use hir::{Module, Name, Semantics};
use itertools::chain;
use parser::T;
use syntax::AstNode;
use syntax::SyntaxElement;
use syntax::SyntaxElementExt;
use syntax::algo::merge_element_ranges;
use syntax::ast;
use syntax::ast::edit::IndentLevel;
use syntax::ast::make::tokens;

use crate::{RootDatabase, defs::Definition, source_change::SourceChange};

pub struct RenameMoveAdt {
    // Adt to rename-move.
    adt: hir::Adt,
    // Associated impls to move along with the adt.
    impls_to_move: Vec<hir::Impl>,
    // New name for the adt.
    new_name: Name,
    // Source module of the adt.
    src_mod: Module,
    // Destination module to move to.
    dst_mod: Module,
}

impl RenameMoveAdt {
    pub fn new(
        sema: &Semantics<'_, RootDatabase>,
        adt: hir::Adt,
        new_name: Name,
        src_mod: Module,
        dst_mod: Module,
    ) -> Self {
        // TODO:
        // - Do we need to sort by syntax start range, or does all_in_module already return in that order?
        // - Do we need to have special filtering for impls defined in inline modules, or does all_in_module already perform that filtering?
        let impls = hir::Impl::all_in_module(sema.db, src_mod)
            .into_iter()
            .filter(|imp| {
                imp.self_ty(sema.db).as_adt() == Some(adt)
                    && imp.source(sema.db).is_some_and(|src| !src.file_id.is_macro())
            })
            .collect();
        Self { adt, impls_to_move: impls, new_name, src_mod, dst_mod }
    }

    pub fn into_source_change(self, sema: &Semantics<'_, RootDatabase>) -> Option<SourceChange> {
        let adt_source = self.adt.source(sema.db)?;
        let src_file_id = self.src_mod.as_source_file_id(sema.db)?.file_id(sema.db);

        let src_mod_node = self.src_mod.definition_source(sema.db).value.node();

        let dst_mod_source = self.dst_mod.definition_source(sema.db);
        let dst_mod_node = dst_mod_source.value.node();
        let dst_file_id = dst_mod_source.file_id.file_id()?.file_id(sema.db);

        // TODO: Remove
        #[allow(clippy::print_stdout)]
        if std::env::var("SS") == Ok("1".to_owned()) {
            println!("{src_mod_node:#?}");
            println!("{dst_mod_node:#?}");
        }

        let adt_ast = adt_source.value;
        let impl_asts = self
            .impls_to_move
            .into_iter()
            .map(|imp| imp.source(sema.db).map(|s| s.value))
            .collect::<Option<Vec<_>>>()?;

        let mut builder = SourceChangeBuilder::new(src_file_id);

        // Delete items from origin
        // TODO: use edit::remove item style pattern
        builder.apply_file_edits(src_file_id, &src_mod_node, |editor| {
            // TODO: Do we need clone_for_update?
            let ranges =
                chain![[adt_ast.syntax()], impl_asts.iter().map(ast::Impl::syntax)].map(|node| {
                    SyntaxElement::from(node.clone_for_update()).range_with_neighboring_ws()
                });

            merge_element_ranges(ranges).into_iter().for_each(|range| {
                // TODO: Fix whitespace handling for inline mods
                // - Need to match against both prev and next tokens as curlies
                // - Will need to add proper indent if there's something after a l_curly because the indent will have been deleted above
                // - Logic will be easiest if you split by Module or SourceFile like for the addition edits

                let indent = IndentLevel::from_element(range.start());

                let replacement = match (
                    range.start().prev_sibling_or_token().map(|s| s.kind()),
                    range.end().next_sibling_or_token().map(|s| s.kind()),
                ) {
                    (Some(T!['{']), Some(T!['}'])) => vec![],
                    (Some(T!['{']), Some(_)) => {
                        vec![tokens::whitespace_indent("\n", indent).into()]
                    }
                    (Some(_), Some(T!['}'])) => {
                        vec![tokens::whitespace_indent("\n", indent).into()]
                    }
                    (Some(_), Some(_)) => {
                        vec![tokens::whitespace_indent("\n\n", indent).into()]
                    }
                    _ => vec![],
                };

                editor.replace_all(range, replacement);
            });
        });
        
        // TODO: Update internal references

        // Move items to target
        let items = chain![[adt_ast.into()], impl_asts.into_iter().map(ast::Item::from)];
        match dst_mod_source.value {
            ModuleSource::SourceFile(source_file) => {
                builder.apply_file_edits(dst_file_id, &dst_mod_node, move |editor| {
                    source_file.add_item_after_headers(editor, items);
                })
            }
            ModuleSource::Module(module) => {
                builder.apply_file_edits(dst_file_id, &dst_mod_node, move |editor| {
                    module.add_item_after_headers(editor, items);
                })
            }
            ModuleSource::BlockExpr(_) => {
                // TODO: Validate earlier
                return None;
            }
        }
        
        // Update definition usages
        let def = Definition::Adt(self.adt);
        let usages = def.usages(sema).all();
        for (file_id, refs) in usages.iter() {
            // Special case to update refs in origin module
            // - Add imports if there are any uncovered refs in the module (scope?)
            // - Update names as normal
            
            // Unqualified name ref update
            // - Just swap out the name for the ident
            
            // Qualified name ref update
            // - Merge the paths somehow
            dbg!(file_id, refs);
        }
        

        Some(builder.finish())
    }
}

// TODO: Remove and use existing method or at least update algo based on those methods
fn get_lca_mod(sema: &Semantics<'_, RootDatabase>, mod_a: Module, mod_b: Module) -> Option<Module> {
    let [mod_a, mod_b] = [mod_a, mod_b].map(Definition::Module);
    let [mut mod_a_path, mut mod_b_path] =
        [&mod_a, &mod_b].map(|d| d.canonical_module_path(sema.db).into_iter().flatten());

    let mut lca = None;
    while let (Some(mod_a_segment), Some(mod_b_segment)) = (mod_a_path.next(), mod_b_path.next()) {
        if mod_a_segment == mod_b_segment {
            lca = Some(mod_a_segment);
        } else {
            break;
        }
    }
    lca
}
