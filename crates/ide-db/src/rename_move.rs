use crate::source_change::SourceChangeBuilder;
use hir::HasSource;
use hir::ModuleSource;
use hir::{Module, Name, Semantics};
use itertools::Itertools;
use itertools::chain;
use parser::SyntaxKind;
use syntax::AstNode;
use syntax::Direction;
use syntax::SyntaxElement;
use syntax::SyntaxElementExt;
use syntax::SyntaxNode;
use syntax::SyntaxToken;
use syntax::algo::merge_element_ranges;
use syntax::ast;
use syntax::ast::HasModuleItem;
use syntax::ast::edit::AstNodeEdit;
use syntax::ast::edit::IndentLevel;
use syntax::ast::make::tokens;
use syntax::syntax_editor::Position;

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

    // OLD VERSION: Ignore and delete before push
    // pub fn into_source_change(self, sema: &Semantics<'_, RootDatabase>) -> Option<SourceChange> {
    //     let adt_source = self.adt.source(sema.db)?;
    //     let src_file_id = self.src_mod.as_source_file_id(sema.db)?.file_id(sema.db);

    //     let dst_mod_source = self.dst_mod.definition_source(sema.db);
    //     let dst_file_id = self.dst_mod.as_source_file_id(sema.db)?.file_id(sema.db);

    //     let adt_ast = adt_source.value;
    //     let impl_asts = self
    //         .impls_to_move
    //         .into_iter()
    //         .map(|imp| imp.source(sema.db).map(|s| s.value))
    //         .collect::<Option<Vec<_>>>()?;

    //     // Delete items from origin
    //     let mut builder = SourceChangeBuilder::new(src_file_id);
    //     builder.delete(adt_ast.syntax().text_range());
    //     for impl_ast in &impl_asts {
    //         // TODO: Handle whitespace between impls
    //         builder.delete(impl_ast.syntax().text_range());
    //     }

    //     // Add items to target
    //     let TargetModInsert { offset, position, indent_level, include_prefix, include_postfix } =
    //         TargetModInsert::new(&dst_mod_source.value)?;
    //     let insert_buf = chain![
    //         include_prefix.then_some(String::new()),
    //         [adt_ast.indent(indent_level).syntax().to_string()],
    //         impl_asts.into_iter().map(|ast| ast.indent(indent_level).syntax().to_string()),
    //         include_postfix.then_some(String::new()),
    //     ]
    //     .join("\n\n");
    //     builder.edit_file(dst_file_id);
    //     builder.insert(offset, insert_buf);

    //     Some(builder.finish())
    // }

    pub fn into_source_change(self, sema: &Semantics<'_, RootDatabase>) -> Option<SourceChange> {
        let adt_source = self.adt.source(sema.db)?;
        let src_file_id = self.src_mod.as_source_file_id(sema.db)?.file_id(sema.db);

        let src_mod_node = self.src_mod.definition_source(sema.db).value.node();

        let dst_mod_source = self.dst_mod.definition_source(sema.db);
        let dst_mod_node = dst_mod_source.value.node();
        let dst_file_id = self.dst_mod.as_source_file_id(sema.db)?.file_id(sema.db);

        let adt_ast = adt_source.value;
        let impl_asts = self
            .impls_to_move
            .into_iter()
            .map(|imp| imp.source(sema.db).map(|s| s.value))
            .collect::<Option<Vec<_>>>()?;

        let mut builder = SourceChangeBuilder::new(src_file_id);

        // Delete items from origin
        builder.apply_file_edits(src_file_id, &src_mod_node, |editor| {
            let ranges = chain![[adt_ast.syntax()], impl_asts.iter().map(ast::Impl::syntax)]
                .map(|node| SyntaxElement::from(node.clone()).element_range_with_ws());
            merge_element_ranges(ranges)
                .into_iter()
                .for_each(|range| editor.replace_all(range, vec![tokens::blank_line().into()]));
        });

        // Add items to target
        // TODO: Handle indent level
        // TODO: Add blankline handling to a common helper method insert_with_blanklines
        let TargetModInsert { position, indent_level, include_prefix, include_postfix } =
            TargetModInsert::new(&dst_mod_source.value)?;
        builder.apply_file_edits(dst_file_id, &dst_mod_node, move |editor| {
            editor.insert_all(
                position,
                chain![
                    include_prefix.then_some(tokens::blank_line().into()),
                    Itertools::intersperse_with(
                        chain![
                            [adt_ast.syntax().clone_for_update().into()],
                            impl_asts.into_iter().map(|ast| ast.syntax().clone_for_update().into()),
                        ],
                        || tokens::blank_line().into(),
                    ),
                    include_postfix.then_some(tokens::blank_line().into()),
                ]
                .collect(),
            );
        });

        Some(builder.finish())
    }
}

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

struct TargetModInsert {
    position: Position,
    indent_level: IndentLevel,
    include_prefix: bool,
    include_postfix: bool,
}

impl TargetModInsert {
    fn new(mod_source: &ModuleSource) -> Option<Self> {
        let (mod_syntax, mod_items, indent_level) = match mod_source {
            ModuleSource::SourceFile(source_file) => {
                (source_file.syntax(), Some(source_file.items()), IndentLevel::from(0))
            }
            ModuleSource::Module(module) => {
                // TODO: Ensure indent handled properly
                (module.syntax(), module.item_list().map(|l| l.items()), module.indent_level() + 1)
            }
            ModuleSource::BlockExpr(_) => {
                // TODO: Maybe validate that destination module is not a block expression earlier
                return None;
            }
        };

        let mut mod_items = mod_items.into_iter().flatten().peekable();
        let last_header_item = mod_items
            .peeking_take_while(|item| {
                matches!(
                    item,
                    ast::Item::Use(_)
                        | ast::Item::Const(_)
                        | ast::Item::ExternBlock(_)
                        | ast::Item::ExternCrate(_)
                        | ast::Item::Static(_)
                )
            })
            .last();
        let first_body_item = mod_items.next();

        let position = match &last_header_item {
            Some(item) => Position::after(item.syntax()),
            None => Position::first_child_of(mod_syntax),
        };

        let include_prefix = last_header_item.is_some();
        let include_postfix = if let Some(i) = last_header_item {
            adj_blanklines(Direction::Next, i.syntax()).next().is_none()
        } else if let Some(i) = first_body_item {
            adj_blanklines(Direction::Prev, i.syntax()).next().is_none()
        } else {
            false
        };

        Some(Self { position, indent_level, include_prefix, include_postfix })
    }
}

fn adj_blanklines(direction: Direction, node: &SyntaxNode) -> impl Iterator<Item = SyntaxToken> {
    node.siblings_with_tokens(direction).skip(1).map_while(|item| {
        let t = item.into_token()?;
        if t.kind() == SyntaxKind::WHITESPACE
            && t.text().chars().filter(|&c| c == '\n').count() >= 2
        {
            Some(t)
        } else {
            None
        }
    })
}
