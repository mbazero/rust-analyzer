use crate::imports::insert_use::ImportGranularity;
use crate::imports::insert_use::ImportScope;
use crate::imports::insert_use::InsertUseConfig;
use crate::imports::insert_use::insert_use;
use crate::search::FileReference;
use crate::search::FileReferenceNode;
use crate::search::ReferenceCategory;
use crate::source_change::SourceChangeBuilder;
use crate::syntax_helpers::node_ext::full_path_of_name_ref;
use hir::HasSource;
use hir::ModuleSource;
use hir::PrefixKind;
use hir::{Module, Name, Semantics};
use itertools::chain;
use parser::T;
use syntax::AstNode;
use syntax::SyntaxElement;
use syntax::SyntaxElementExt;
use syntax::algo::merge_element_ranges;
use syntax::ast;
use syntax::ast::HasName;
use syntax::ast::PathSegmentKind;
use syntax::ast::edit::IndentLevel;
use syntax::ast::make;
use syntax::ast::make::path_from_text_with_edition;
use syntax::ast::make::tokens;
use syntax::syntax_editor::Detach;
use syntax::ted;

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
    dst_path: String, // HACK!
}

impl RenameMoveAdt {
    pub fn new(
        sema: &Semantics<'_, RootDatabase>,
        adt: hir::Adt,
        new_name: Name,
        src_mod: Module,
        dst_mod: Module,
        dst_path: &str, // HACK: Fix this
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
        Self {
            adt,
            impls_to_move: impls,
            new_name,
            src_mod,
            dst_mod,
            dst_path: dst_path.to_owned(),
        }
    }

    pub fn into_source_change(self, sema: &Semantics<'_, RootDatabase>) -> Option<SourceChange> {
        let edition = self.src_mod.krate().edition(sema.db);
        let adt_source = self.adt.source(sema.db)?;
        let src_file_id = self.src_mod.as_source_file_id(sema.db)?.file_id(sema.db);

        let src_mod_node = self.src_mod.definition_source(sema.db).value.node();

        let dst_mod_source = self.dst_mod.definition_source(sema.db);
        let dst_mod_node = dst_mod_source.value.node();
        let dst_file_id = dst_mod_source.file_id.file_id()?.file_id(sema.db);

        dbg!(src_file_id);
        dbg!(dst_file_id);

        // TODO: Remove
        #[allow(clippy::print_stdout)]
        if std::env::var("SS") == Ok("1".to_owned()) {
            println!("{src_mod_node:#?}");
            println!("{dst_mod_node:#?}");
        }

        let adt_ast = adt_source.value;
        // // TODO: Rename with SyntaxEditor
        // let renamed_adt_ast = {
        //     let clone = adt_ast.clone_for_update();
        //     if let Some(name) = clone.name() {
        //         let replacement =
        //             syntax::ast::make::name(&self.new_name.display(sema.db, edition).to_string())
        //                 .clone_for_update();
        //         ted::replace(name.syntax(), replacement.syntax());
        //     }
        //     clone
        // };
        let impl_asts = self
            .impls_to_move
            .into_iter()
            .map(|imp| imp.source(sema.db).map(|s| s.value))
            .collect::<Option<Vec<_>>>()?;

        let mut builder = SourceChangeBuilder::new(src_file_id);

        // Delete items from origin
        // TODO: use edit::remove item style pattern
        builder.apply_file_edits(src_file_id, &src_mod_node, |editor| {
            // // Rename ast
            if let Some(name) = adt_ast.name() {
                let replacement =
                    syntax::ast::make::name(&self.new_name.display(sema.db, edition).to_string())
                        .clone_for_update();
                editor.replace(name.syntax(), replacement.syntax());
            }

            // TODO: Do we need clone_for_update?
            let ranges = chain![[adt_ast.syntax()], impl_asts.iter().map(ast::Impl::syntax)]
                .map(|node| SyntaxElement::from(node.clone()).range_with_neighboring_ws());

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

                editor.replace_all_detach(range, replacement, Detach(true));
            });
        });

        // TODO: Update internal references
        dbg!(adt_ast.name());

        // Update definition usages
        let def = Definition::Adt(self.adt);
        let total_usages = def.usages(sema).all().iter().flat_map(|(_, refs)| refs).count();
        dbg!(total_usages);
        let usages = def.usages(sema).all();
        for (file_id, refs) in usages.iter() {
            // Special case to update refs in origin module
            // - Add imports if there are any uncovered refs in the module (scope?)
            // - Update names as normal

            // Unqualified name ref update
            // - Just swap out the name for the ident

            // Qualified name ref update
            // - Merge the paths somehow
            let file_id = file_id.file_id(sema.db);
            // println!("Before edit_file: {file_id:?}");
            // builder.edit_file(file_id);
            // println!("After edit_file: {file_id:?}");

            for FileReference { range, name, category } in refs {
                println!("Usage: {range:?} {name:?} {category:?}");
                // println!("Usage: ")
                match name {
                    FileReferenceNode::Name(name) => {
                        println!("FileRef::Name");
                        let new_name =
                            make::name(&format!("{}", self.new_name.display(sema.db, edition)))
                                .clone_for_update();
                        dbg!(&new_name);
                        builder.apply_file_edits(file_id, name.syntax(), |editor| {
                            editor.replace(name.syntax(), new_name.syntax());
                        });
                    }
                    FileReferenceNode::NameRef(name_ref) => {
                        println!("FileRef::NameRef");
                        let path = full_path_of_name_ref(name_ref);
                        dbg!(&path);
                        let import_scope =
                            ImportScope::find_insert_use_container(name_ref.syntax(), sema);

                        // TODO: If the import is non-tree normal import, maybe just replace the full path in the path case
                        if category.contains(ReferenceCategory::IMPORT) {
                            println!("Ref contains import");
                            if let Some(import_scope) = import_scope {
                                println!("Import scope found");
                                let import_scope = builder.make_import_scope_mut(import_scope);

                                if let Some(leaf_use_tree) =
                                    name_ref.syntax().ancestors().find_map(ast::UseTree::cast)
                                {
                                    let leaf_use_tree = builder.make_mut(leaf_use_tree);
                                    leaf_use_tree.remove_recursive();
                                }

                                let cfg = InsertUseConfig {
                                    granularity: ImportGranularity::Crate, // or Module/Item/One
                                    enforce_granularity: false, // infer style from existing file when false
                                    prefix_kind: PrefixKind::ByCrate, // ByCrate | BySelf | Plain
                                    group: true, // group into std/external/local blocks
                                    skip_glob_imports: false, // allow merging into `*` imports
                                };
                                let path = path_from_text_with_edition(&self.dst_path, edition)
                                    .clone_for_update();
                                insert_use(&import_scope, path, &cfg);
                            }
                        } else if let Some(path) = path {
                            println!("Path is some: {path}");
                            if path.qualifier().is_some() {
                                println!("Path qualifier is some");
                                let mut segments = Vec::new();
                                let mut current = Some(path.clone());
                                while let Some(p) = current {
                                    if let Some(segment) = p.segment() {
                                        segments.push(segment);
                                    }
                                    current = p.qualifier();
                                }
                                segments.reverse();

                                let dst_module_name = self
                                    .dst_mod
                                    .name(sema.db)
                                    .map(|name| name.display(sema.db, edition).to_string());
                                let old_type_name =
                                    self.adt.name(sema.db).display(sema.db, edition).to_string();
                                let new_type_name =
                                    self.new_name.display(sema.db, edition).to_string();

                                let mut new_segments = Vec::with_capacity(segments.len());
                                for segment in &segments {
                                    let segment_text = match segment.kind() {
                                        Some(PathSegmentKind::CrateKw) => "crate".to_string(),
                                        Some(PathSegmentKind::SuperKw) => "super".to_string(),
                                        Some(PathSegmentKind::SelfKw) => "self".to_string(),
                                        Some(PathSegmentKind::SelfTypeKw) => "Self".to_string(),
                                        Some(PathSegmentKind::Name(name_ref)) => {
                                            let mut text = name_ref.text().to_string();
                                            if text == old_type_name {
                                                text = new_type_name.clone();
                                            }
                                            text
                                        }
                                        _ => segment.syntax().to_string(),
                                    };
                                    new_segments.push(segment_text);
                                }

                                if let Some(dst_module_name) = dst_module_name {
                                    if new_segments.len() >= 2 {
                                        for idx in (0..new_segments.len() - 1).rev() {
                                            if matches!(
                                                segments[idx].kind(),
                                                Some(PathSegmentKind::Name(_))
                                            ) {
                                                new_segments[idx] = dst_module_name.clone();
                                                break;
                                            }
                                        }
                                    }
                                }

                                let new_text = new_segments.join("::");
                                let new_path = make::path_from_text(&new_text);
                                dbg!(&new_text, &new_path);
                                builder.apply_file_edits(file_id, path.syntax(), |editor| {
                                    editor.replace(path.syntax(), new_path.syntax());
                                });
                            } else {
                                println!("Path qualifier is none");
                                let new_name_ref = make::name_ref(&format!(
                                    "{}",
                                    self.new_name.display(sema.db, edition)
                                ))
                                .clone_for_update();
                                dbg!(name_ref.syntax());
                                dbg!(new_name_ref.syntax());
                                builder.apply_file_edits(file_id, name_ref.syntax(), |editor| {
                                    editor.replace(name_ref.syntax(), new_name_ref.syntax());
                                });
                            }
                        } else {
                            println!("Not import and path none");
                            let new_name_ref = make::name_ref(&format!(
                                "{}",
                                self.new_name.display(sema.db, edition)
                            ))
                            .clone_for_update();
                            builder.apply_file_edits(file_id, name_ref.syntax(), |editor| {
                                editor.replace(name_ref.syntax(), new_name_ref.syntax());
                            });
                        }
                    }
                    FileReferenceNode::Lifetime(_) | FileReferenceNode::FormatStringEntry(_, _) => {
                        // TODO: Log something
                        return None;
                    }
                }
            }
        }
        
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
