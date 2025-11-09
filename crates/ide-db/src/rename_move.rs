use crate::imports::insert_use::ImportGranularity;
use crate::imports::insert_use::ImportScope;
use crate::imports::insert_use::InsertUseConfig;
use crate::imports::insert_use::insert_use;
use crate::search::FileReference;
use crate::search::FileReferenceNode;
use crate::search::ReferenceCategory;
use crate::source_change::SourceChangeBuilder;
use crate::source_change::TreeMutator;
use crate::syntax_helpers::node_ext::full_path_of_name_ref;
use hir::EditionedFileId;
use hir::HasSource;
use hir::ImportResolution;
use hir::InFile;
use hir::InFileWrapper;
use hir::ModuleSource;
use hir::PrefixKind;
use hir::Visibility;
use hir::{Module, Name, Semantics};
use itertools::chain;
use parser::T;
use rustc_hash::FxHashMap;
use span::Edition;
use syntax::AstNode;
use syntax::SyntaxElement;
use syntax::SyntaxElementExt;
use syntax::algo::merge_element_ranges;
use syntax::ast;
use syntax::ast::AnyHasVisibility;
use syntax::ast::HasName;
use syntax::ast::NameRef;
use syntax::ast::PathSegmentKind;
use syntax::ast::edit::IndentLevel;
use syntax::ast::make;
use syntax::ast::make::path_from_text_with_edition;
use syntax::ast::make::tokens;
use syntax::ast::syntax_factory::SyntaxFactory;
use syntax::syntax_editor::SetName;
use syntax::syntax_editor::SetVisibility;
use syntax::syntax_editor::SyntaxEditor;
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

pub struct RenameMoveConfig<'a> {
    sema: &'a Semantics<'a, RootDatabase>,
    new_name: Name,
    origin_mod: Module,
    target_mod: Module,
    target_path: &'a str, // HACK: Fix this
    required_vis: Option<ast::Visibility>,
    include_impls: bool,
}

struct Context<'a> {
    sema: &'a Semantics<'a, RootDatabase>,
    edition: Edition,
}

pub enum RenameMoveDefinition {
    Module(hir::Module),
    Function(hir::Function),
    Adt(hir::Adt),
    Const(hir::Const),
    Static(hir::Static),
    Trait(hir::Trait),
}

impl RenameMoveDefinition {
    pub fn new(def: Definition) -> Option<Self> {
        match def {
            Definition::Module(m) => Self::Module(m).into(),
            Definition::Function(f) => Self::Function(f).into(),
            Definition::Adt(a) => Self::Adt(a).into(),
            Definition::Const(c) => Self::Const(c).into(),
            Definition::Static(s) => Self::Static(s).into(),
            Definition::Trait(t) => Self::Trait(t).into(),
            _ => None,
        }
    }

    pub fn rename_move(self, config: RenameMoveConfig) -> Option<SourceChange> {
        match self {
            RenameMoveDefinition::Module(module) => todo!(),
            RenameMoveDefinition::Function(function) => todo!(),
            RenameMoveDefinition::Adt(adt) => rename_move_adt(adt, config),
            RenameMoveDefinition::Const(_) => todo!(),
            RenameMoveDefinition::Static(_) => todo!(),
            RenameMoveDefinition::Trait(_) => todo!(),
        }
    }
}

pub struct ItemUpdate {
    new_name: String,
    new_visibilities: FxHashMap<AnyHasVisibility, Visibility>,
}

struct AdtEditor<'a> {
    ctx: Context<'a>,
    editor: SyntaxEditor,
    make: SyntaxFactory,
    hir: hir::Adt,
    ast: ast::Adt,
}

impl<'a> AdtEditor<'a> {
    fn new(ctx: Context<'a>, hir: hir::Adt) -> Option<Self> {
        let ast = hir.source(ctx.sema.db)?.value;
        let editor = SyntaxEditor::from_last_ancestor(ast.syntax());
        let make = SyntaxFactory::without_mappings();
        Self { ctx, editor, make, hir, ast }.into()
    }

    fn set_name(&mut self, new_name: impl AsRef<str>) -> Option<&mut Self> {
        let new_name = self.make.name(new_name.as_ref());
        self.editor.replace(self.ast.name()?.syntax(), new_name.syntax());
        Some(self)
    }

    fn set_def_vis(&mut self, vis: hir::Visibility) -> Option<&mut Self> {
        todo!()
    }

    fn set_record_vis(&mut self, vis: hir::Visibility) -> Option<&mut Self> {
        todo!()
    }
}

fn rename_move_adt(
    adt: hir::Adt,
    RenameMoveConfig {
        sema,
        new_name,
        origin_mod,
        target_mod,
        target_path,
        required_vis,
        include_impls,
    }: RenameMoveConfig,
) -> Option<SourceChange> {
    // TODO: Can get all of these directly from the adt
    let InFileWrapper { file_id: adt_file_id, value: adt_ast } = adt.source(sema.db)?;
    let origin_mod = adt.module(sema.db);
    let origin_file_id = origin_mod.as_source_file_id(sema.db)?.file_id(sema.db);
    let origin_mod_node = origin_mod.definition_source(sema.db).value.node();

    let target_mod_source = target_mod.definition_source(sema.db);
    let target_mod_node = target_mod_source.value.node();
    let target_file_id = target_mod_source.file_id.file_id()?.file_id(sema.db);

    let edition = origin_mod.krate().edition(sema.db);
    let new_name_str = format!("{}", new_name.display(sema.db, edition));

    let mut scb = SourceChangeBuilder::new(origin_file_id);
    let make = SyntaxFactory::without_mappings();

    // Update adt
    let new_adt = scb.apply_edits_and_detach_mut(&adt_ast, |editor| {
        adt_ast.set_name(editor, make::name(&new_name_str));
        adt_ast.set_visibility(editor, required_vis.as_ref());
        // TODO: Replace set_vis_if_some with intelligent visibility upgrading
        match &adt_ast {
            ast::Adt::Enum(_) => {
                // Enum variant fields inherit definition visibility
            }
            ast::Adt::Struct(strukt) => strukt.field_list().into_iter().for_each(|fl| match fl {
                ast::FieldList::RecordFieldList(rfl) => {
                    rfl.fields()
                        .for_each(|f| f.set_visibility_if_some(editor, required_vis.as_ref()));
                }
                ast::FieldList::TupleFieldList(tfl) => {
                    tfl.fields()
                        .for_each(|f| f.set_visibility_if_some(editor, required_vis.as_ref()));
                }
            }),
            ast::Adt::Union(union) => {
                union.record_field_list().into_iter().flat_map(|rfl| rfl.fields()).for_each(|f| {
                    f.set_visibility_if_some(editor, required_vis.as_ref());
                })
            }
        }
    });

    // Update impls
    let adt_impls: Vec<_> = hir::Impl::all_in_module(sema.db, origin_mod)
        .into_iter()
        .filter_map(|imp| {
            if imp.self_ty(sema.db).as_adt() == Some(adt)
                && imp.source(sema.db).is_some_and(|src| !src.file_id.is_macro())
            {
                Some(imp.source(sema.db)?.value)
            } else {
                None
            }
        })
        .collect();

    let new_adt_impls: Vec<_> = adt_impls
        .iter()
        .map(|imp_ast| {
            if imp_ast.trait_().is_some() {
                return imp_ast.clone();
            }
            scb.apply_edits_and_detach_mut(imp_ast, |editor| {
                imp_ast.assoc_item_list().into_iter().flat_map(|ail| ail.assoc_items()).for_each(
                    |assoc| match assoc {
                        ast::AssocItem::Const(c) => {
                            c.set_visibility_if_some(editor, required_vis.as_ref())
                        }
                        ast::AssocItem::Fn(f) => {
                            f.set_visibility_if_some(editor, required_vis.as_ref())
                        }
                        ast::AssocItem::MacroCall(_) => {}
                        ast::AssocItem::TypeAlias(ta) => {
                            ta.set_visibility_if_some(editor, required_vis.as_ref())
                        }
                    },
                )
            })
        })
        .collect();

    // Delete items from origin
    // TODO: use edit::remove_items style pattern as below
    scb.apply_edits_in_file(origin_file_id, &origin_mod_node, |editor| {
        // TODO: Do we need clone_for_update?
        let ranges = chain![[adt_ast.syntax()], adt_impls.iter().map(ast::Impl::syntax)]
            .map(|node| SyntaxElement::from(node.clone_for_update()).range_with_neighboring_ws());

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

    // Move items to target
    let items = chain![[new_adt.into()], new_adt_impls.into_iter().map(ast::Item::from)];
    match target_mod_source.value {
        ModuleSource::SourceFile(source_file) => {
            scb.apply_edits_in_file(target_file_id, &target_mod_node, move |editor| {
                source_file.add_item_after_headers(editor, items);
            })
        }
        ModuleSource::Module(module) => {
            scb.apply_edits_in_file(target_file_id, &target_mod_node, move |editor| {
                module.add_item_after_headers(editor, items);
            })
        }
        ModuleSource::BlockExpr(_) => {
            // TODO: Validate earlier
            return None;
        }
    }

    todo!()
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

        // TODO: Remove
        #[allow(clippy::print_stdout)]
        if std::env::var("SS") == Ok("1".to_owned()) {
            println!("{src_mod_node:#?}");
            println!("{dst_mod_node:#?}");
        }

        let adt_ast = adt_source.value;
        // TODO: Rename with SyntaxEditor
        let renamed_adt_ast = {
            let clone = adt_ast.clone_for_update();
            if let Some(name) = clone.name() {
                let replacement =
                    syntax::ast::make::name(&self.new_name.display(sema.db, edition).to_string())
                        .clone_for_update();
                ted::replace(name.syntax(), replacement.syntax());
            }
            clone
        };
        let impl_asts = self
            .impls_to_move
            .into_iter()
            .map(|imp| imp.source(sema.db).map(|s| s.value))
            .collect::<Option<Vec<_>>>()?;

        let mut builder = SourceChangeBuilder::new(src_file_id);

        // Out ref resolution
        // Compute mapping between each out-ref and it's resolution item
        //   - Itself for fully qualified path
        //   - An inner use statement
        //   - An outer use statement
        //
        // As final step after the move:
        // - Loop through each mapping and apply appropriate resolution
        //   - Fully qualified path
        //      - Update inline
        //   - Inner use statement
        //      - Remove old and insert new
        //   - Outer use
        //      - Remove old from origin file
        //          - Only if there are no other refs using the use!
        //      - Insert new into target file

        // Update mutable clone of items to move
        //
        // SKIP OUT-REF RESOLUTION HERE, THAT WILL BE HANDLED AT THE END
        //
        // ADT def edits
        // - Def name
        // - Def visibilty
        // - Record visibility
        //
        // Trait def edits
        // - Def name
        // - Def visibiilty
        //
        // Const def edits
        // - Def name
        // - Def visibilty
        //
        // Static def edits
        // - Def name
        // - Def visibilty
        //
        // Impl block edits
        // - Assoc const visibility
        // - Assoc fn visibility
        // - Method visibility
        //
        // Impl trait block edits
        // - NOTHING! -> visibility inherited from def

        // Delete items from origin
        // TODO: use edit::remove item style pattern
        builder.apply_edits_in_file(src_file_id, &src_mod_node, |editor| {
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
        let items = chain![[renamed_adt_ast.into()], impl_asts.into_iter().map(ast::Item::from)];
        match dst_mod_source.value {
            ModuleSource::SourceFile(source_file) => {
                builder.apply_edits_in_file(dst_file_id, &dst_mod_node, move |editor| {
                    source_file.add_item_after_headers(editor, items);
                })
            }
            ModuleSource::Module(module) => {
                builder.apply_edits_in_file(dst_file_id, &dst_mod_node, move |editor| {
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
            let file_id = file_id.file_id(sema.db);
            for FileReference { range, name, category } in refs {
                builder.edit_file(file_id);
                match name {
                    FileReferenceNode::Name(_) => {
                        builder.replace(
                            *range,
                            format!("{}", self.new_name.display(sema.db, edition)),
                        );
                    }
                    FileReferenceNode::NameRef(name_ref) => {
                        let path = full_path_of_name_ref(name_ref);
                        let import_scope =
                            ImportScope::find_insert_use_container(name_ref.syntax(), sema);

                        // TODO: If the import is non-tree normal import, maybe just replace the full path in the path case
                        if category.contains(ReferenceCategory::IMPORT) {
                            if let Some(import_scope) = import_scope {
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
                            if path.qualifier().is_some() {
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
                                builder.replace(path.syntax().text_range(), new_text);
                            } else {
                                builder.replace(
                                    *range,
                                    format!("{}", self.new_name.display(sema.db, edition)),
                                );
                            }
                        } else {
                            builder.replace(
                                *range,
                                format!("{}", self.new_name.display(sema.db, edition)),
                            );
                        }
                    }
                    FileReferenceNode::Lifetime(_) | FileReferenceNode::FormatStringEntry(_, _) => {
                        // TODO: Log something
                        return None;
                    }
                }
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

#[derive(Debug)]
enum NameRefKind {
    InUseTree(ast::UseTree),
    Qualified(ast::Path),
    UnQualified(Option<ImportResolution>),
}

#[derive(Debug)]
struct RefsInFile {
    names: Vec<Name>,
    name_refs: Vec<(NameRef, NameRefKind)>,
}

#[derive(Debug)]
enum ExternalRef {
    Name(ast::Name, Option<ImportResolution>),
    NameRef(ast::NameRef, NameRefKind),
}

#[derive(Debug)]
struct ExternalRefs {
    refs: FxHashMap<EditionedFileId, Vec<ExternalRef>>,
}

impl ExternalRefs {
    fn compute(sema: &Semantics<'_, RootDatabase>, def: Definition) -> Self {
        let refs = def
            .usages(sema)
            .all()
            .into_iter()
            .map(|(file_id, file_refs)| {
                let file_refs = file_refs
                    .into_iter()
                    .filter_map(|FileReference { name, .. }| match name {
                        FileReferenceNode::Name(name) => {
                            println!("FOUND NAME!!! {name}");
                            let path = name.syntax().ancestors().find_map(ast::Path::cast)?;
                            println!("Name path: {path}");
                            ExternalRef::Name(name, sema.resolve_import(&path)).into()
                        }
                        FileReferenceNode::NameRef(name_ref) => {
                            // TODO: Better in use stament check, per codebase convention
                            let kind = if let Some(use_tree) =
                                name_ref.syntax().ancestors().filter_map(ast::UseTree::cast).last()
                            {
                                NameRefKind::InUseTree(use_tree)
                            } else {
                                let path = full_path_of_name_ref(&name_ref)?;
                                println!("NameRef path: {path}");
                                if let Some(qualifier) = path.qualifier() {
                                    NameRefKind::Qualified(qualifier)
                                } else {
                                    NameRefKind::UnQualified(sema.resolve_import(&path))
                                }
                            };
                            ExternalRef::NameRef(name_ref, kind).into()
                        }
                        FileReferenceNode::Lifetime(_) => None,
                        FileReferenceNode::FormatStringEntry(_, _) => None,
                    })
                    .collect();
                (file_id, file_refs)
            })
            .collect();

        Self { refs }
    }
}

#[cfg(test)]
mod tests {
    use hir::Semantics;
    use rustc_hash::FxHashMap;
    use syntax::{
        AstNode,
        ast::{self, HasModuleItem},
    };
    use test_fixture::WithFixture;

    use crate::{RootDatabase, defs::Definition, rename_move::ExternalRefs};

    #[test]
    fn test_print_syntax() {
        let fixture = r#"
//- /main.rs
pub mod foo;
pub mod bar;
pub mod baz;
pub mod buz;

pub fn main() {}

//- /foo.rs
pub struct Alpha;
pub struct Beta;
pub struct Gamma;
pub struct Delta;

pub enum Epsilon {
    VariantA,
    VariantB,
}

//- /bar.rs
use crate::foo::Alpha;
use super::foo::Beta;
use crate::foo::Epsilon;

fn my_fun(a: Alpha, b: Beta) -> (Alpha, Beta) {
    (a, b)
}

fn eps_fun(eps: Epsilon) {
    match eps {
        Epsilon::VariantA => {}
        Epsilon::VariantB => {}
    }

    if let Epsilon::VariantA = eps {}
}

//- /baz.rs
use crate::foo::{Alpha, Beta};

fn my_fun(a: Alpha, b: Beta) -> (Alpha, Beta) {
    (a, b)
}

//- /buz.rs
use crate::foo::*;

fn my_fun(a: Alpha, b: Beta) -> (Alpha, Beta) {
    (a, b)
}
"#;

        let (db, files) = RootDatabase::with_many_files(fixture);
        let krate = db.test_crate();
        let sema = Semantics::new(&db);

        // TODO: Find in refs in glob
        // - Actually, we could potentially determine if

        let sfs: FxHashMap<_, _> = ["main", "foo", "bar", "baz", "buz"]
            .into_iter()
            .zip(&files)
            .map(|(name, file)| (name, sema.parse(*file)))
            .collect();

        dbg!(sfs["bar"].syntax());

        let file_names: FxHashMap<_, _> = sfs
            .iter()
            .map(|(name, sf)| (sema.hir_file_for(sf.syntax()).file_id().unwrap(), *name))
            .collect();

        let defs: Vec<_> = sfs["foo"]
            .items()
            .filter_map(|i| ast::Adt::cast(i.syntax().clone()))
            .map(|adt| sema.to_adt_def(&adt).unwrap())
            .map(|adt| Definition::Adt(adt))
            .collect();

        let [alpha, beta, gamma, delta, epsilon] = defs.try_into().unwrap();

        dbg!(&alpha);
        dbg!(&epsilon);

        let refs: FxHashMap<_, _> = ExternalRefs::compute(&sema, alpha)
            .refs
            .into_iter()
            .map(|(file_id, refs)| (file_names[&file_id], refs))
            .collect();

        dbg!(refs);

        // dbg!(defs);

        // dbg!(&bar.syntax());
        // dbg!(&baz.syntax());
    }
}
