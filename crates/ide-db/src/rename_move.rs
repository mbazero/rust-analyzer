use std::iter;

use crate::defs::NameClass;
use crate::defs::NameRefClass;

use crate::helpers::mod_path_to_ast;
use crate::imports::insert_use::ImportGranularity;
use crate::imports::insert_use::ImportScope;
use crate::imports::insert_use::InsertUseConfig;
use crate::imports::insert_use::insert_use;
use crate::imports::insert_use::insert_use_tree;
use crate::search::FileReference;
use crate::search::FileReferenceNode;
use crate::search::ReferenceCategory;
use crate::source_change::SourceChangeBuilder;
use crate::source_change::TreeMutator;
use crate::syntax_helpers::node_ext::full_path_of_name_ref;
use base_db::SourceDatabase;
use hir::EditionedFileId;
use hir::HasSource;
use hir::ImportResolution;
use hir::InFile;
use hir::InFileWrapper;
use hir::ModPath;
use hir::ModuleDef;
use hir::ModuleSource;
use hir::PathResolution;
use hir::PrefixKind;
use hir::Visibility;
use hir::db::ExpandDatabase;
use hir::{Module, Name, Semantics};
use itertools::Itertools;
use itertools::chain;
use parser::T;
use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;
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
use syntax::ast::edit::AstNodeEdit;
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
    pub sema: &'a Semantics<'a, RootDatabase>,
    pub new_name: Name,
    pub origin_mod: Module,
    pub target_mod: Module,
    pub target_path: &'a str, // HACK: Fix this
    pub required_vis: Option<ast::Visibility>,
    pub include_impls: bool,
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

    pub fn rename_move(self, config: RenameMoveConfig<'_>) -> Option<SourceChange> {
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

impl From<RenameMoveDefinition> for Definition {
    fn from(value: RenameMoveDefinition) -> Self {
        match value {
            RenameMoveDefinition::Module(m) => Definition::Module(m),
            RenameMoveDefinition::Function(f) => Definition::Function(f),
            RenameMoveDefinition::Adt(a) => Definition::Adt(a),
            RenameMoveDefinition::Const(c) => Definition::Const(c),
            RenameMoveDefinition::Static(s) => Definition::Static(s),
            RenameMoveDefinition::Trait(t) => Definition::Trait(t),
        }
    }
}

// HACK: Fix this
#[derive(Debug)]
struct NewPath {
    name: Name,
    name_str: String,      // TODO: Remove
    full_path_str: String, // TODO: Remove
    edition: Edition,
    module: Module,
}

impl NewPath {
    fn name_path(&self) -> ast::Path {
        make::path_from_text_with_edition(&self.name_str, self.edition)
    }

    fn full_path(&self) -> ast::Path {
        make::path_from_text_with_edition(&self.full_path_str, self.edition)
    }

    fn relative_path_from(&self, db: &RootDatabase, root: &Module) -> Option<ast::Path> {
        let modules: Vec<_> = std::iter::successors(Some(self.module), |m| m.parent(db))
            .take_while_inclusive(|m| m != root)
            .collect();
        if modules.last() != Some(root) {
            return None;
        }
        let segments =
            modules.into_iter().rev().filter_map(|m| m.name(db)).chain([self.name.clone()]);
        let mod_path = ModPath::from_segments(hir::PathKind::Plain, segments);
        Some(mod_path_to_ast(&mod_path, self.edition))
    }

    fn rel_path_from_target_mod(&self, db: &RootDatabase) -> ast::Path {
        let mod_path = ModPath::from_segments(
            hir::PathKind::Plain,
            [self.module.name(db).unwrap(), self.name.clone()],
        );
        mod_path_to_ast(&mod_path, self.edition)
    }

    // HACK!
    fn mod_full_path(&self) -> ast::Path {
        let mod_path = self.full_path_str.rsplit_once("::").unwrap().0;
        make::path_from_text_with_edition(&mod_path, self.edition)
    }

    fn full_path_for_import(&self, use_tree_list: Option<ast::UseTreeList>) -> ast::Path {
        let path = self.full_path();
        let use_tree = make::use_tree(path, use_tree_list, None, false);
        use_tree.syntax().first_child().and_then(ast::Path::cast).unwrap()
    }

    fn name_str(&self, db: &RootDatabase) -> String {
        format!("{}", self.name.display(db, self.edition))
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
    }: RenameMoveConfig<'_>,
) -> Option<SourceChange> {
    // TODO: Can get all of these directly from the adt
    let InFileWrapper { file_id: adt_file_id, value: adt_ast } = adt.source(sema.db)?;
    let adt_file_id = adt_file_id.file_id()?;
    let origin_file_id_ed = origin_mod.definition_source(sema.db).file_id.file_id()?;
    let origin_file_id = origin_file_id_ed.file_id(sema.db);
    let origin_mod_node = origin_mod.definition_source(sema.db).value.node();

    let target_mod_source = target_mod.definition_source(sema.db);
    let target_mod_node = target_mod_source.value.node();
    let target_file_id_ed = target_mod_source.file_id.file_id()?;
    let target_file_id = target_file_id_ed.file_id(sema.db);

    let edition = origin_mod.krate().edition(sema.db);

    let new_path = NewPath {
        name_str: format!("{}", new_name.display(sema.db, edition)),
        full_path_str: target_path.to_owned(),
        name: new_name,
        module: target_mod,
        edition,
    };

    let mut scb = SourceChangeBuilder::new(origin_file_id);
    let make = SyntaxFactory::without_mappings();

    // Update adt
    let new_adt = scb.apply_edits_and_detach_mut(&adt_ast, |editor| {
        adt_ast.set_name(editor, make.name(&new_path.name_str));
        // TODO: Overhaul vis upgrade
        adt_ast.set_visibility_if_none(editor, required_vis.as_ref());
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

    // Update internal refs
    let items: Vec<_> =
        chain![[new_adt.into()], new_adt_impls.into_iter().map(ast::Item::from)].collect();
    // let internal_refs = items.iter().map(|item| item.sy)

    // Move items to target
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

    // Update external refs
    // TODO: Decide about filtering files
    // - If you remove the filter, you'll have to pass the mapped source file nodes from the previous edits
    RenameExternalRefs::new(sema, adt.into(), origin_file_id_ed, target_file_id_ed)?
        .update_inline(&mut scb, &new_path)?;

    Some(scb.finish())
}

enum ImportLocation {
    InsideMoveRange,
    OutsideMoveRange,
}

#[derive(Debug, Clone)]
enum NameRefKind {
    InUseTree(ast::UseTree),
    Qualified { ref_path: ast::Path, first_qual_import: Option<ImportResolution> },
    UnQualified { name_ref_import: ImportResolution },
}

#[derive(Debug, Clone)]
struct RichNameRef {
    name_ref: ast::NameRef,
    kind: NameRefKind,
}

impl RichNameRef {
    fn into_mut(self, scb: &mut SourceChangeBuilder) -> Self {
        Self {
            name_ref: scb.make_mut(self.name_ref),
            kind: match self.kind {
                NameRefKind::InUseTree(use_tree) => NameRefKind::InUseTree(scb.make_mut(use_tree)),
                NameRefKind::Qualified { ref_path, first_qual_import } => NameRefKind::Qualified {
                    ref_path: scb.make_mut(ref_path),
                    first_qual_import: first_qual_import.map(|i| scb.make_import_res_mut(i)),
                },
                NameRefKind::UnQualified { name_ref_import } => NameRefKind::UnQualified {
                    name_ref_import: scb.make_import_res_mut(name_ref_import),
                },
            },
        }
    }
}

struct RenameExternalRefs<'a> {
    sema: &'a Semantics<'a, RootDatabase>,
    refs: FxHashMap<EditionedFileId, Vec<RichNameRef>>,
}

impl<'a> RenameExternalRefs<'a> {
    // TODO: Accept RenameMoveDefinition
    // TODO: Maybe exclude origin and target files?
    fn new(
        sema: &'a Semantics<'a, RootDatabase>,
        def: Definition,
        origin_file_id: EditionedFileId,
        target_file_id: EditionedFileId,
    ) -> Option<Self> {
        let refs = def
            .usages(sema)
            .all()
            .into_iter()
            // .filter(|(file_id, _)| ![origin_file_id, target_file_id].contains(file_id))
            .map(|(file_id, file_refs)| {
                let refs = file_refs
                    .into_iter()
                    .filter_map(|FileReference { name, category, .. }| match name {
                        FileReferenceNode::Name(_) => unreachable!("Got FileReferenceNode::Name"),
                        FileReferenceNode::NameRef(name_ref) => {
                            let ref_path =
                                name_ref.syntax().ancestors().find_map(ast::Path::cast).unwrap();
                            let kind = if category.contains(ReferenceCategory::IMPORT) {
                                NameRefKind::InUseTree(
                                    name_ref
                                        .syntax()
                                        .ancestors()
                                        .find_map(ast::UseTree::cast)
                                        .unwrap(),
                                )
                            } else if let Some(first_qual) = ref_path.first_qualifier() {
                                NameRefKind::Qualified {
                                    ref_path,
                                    first_qual_import: sema.resolve_import(&first_qual),
                                }
                            } else {
                                NameRefKind::UnQualified {
                                    // TODO: What is the only case where we will fail to resolve an
                                    // import for an unqualified ref? Answer, for a usage within the
                                    // same scope as the definition. For now, we can just skip
                                    // these, but we really need to do a better job of this when we
                                    // properly handle internal refs. The proper answer is perhaps
                                    // to exclude the move items range from the search scope.
                                    name_ref_import: sema.resolve_import(&ref_path)?,
                                }
                            };
                            RichNameRef { name_ref, kind }.into()
                        }
                        FileReferenceNode::Lifetime(_) => unreachable!(),
                        FileReferenceNode::FormatStringEntry(_, _) => unreachable!(),
                    })
                    .collect();

                (file_id, refs)
            })
            .collect();

        Some(Self { sema, refs })
    }

    fn update_inline(self, scb: &mut SourceChangeBuilder, new_path: &NewPath) -> Option<()> {
        let RenameExternalRefs { sema, refs } = self;

        for (file_id, refs) in refs {
            // TODO: Wrap in per-file function that returns option
            scb.edit_file(file_id.file_id(sema.db));

            let use_trees: Vec<_> = refs
                .iter()
                .map(|RichNameRef { kind, .. }| match kind {
                    NameRefKind::InUseTree(use_tree) => Some(use_tree.clone()),
                    NameRefKind::Qualified { first_qual_import, .. } => {
                        first_qual_import.as_ref().and_then(|i| i.use_tree())
                    }
                    NameRefKind::UnQualified { name_ref_import, .. } => {
                        Some(name_ref_import.use_tree().unwrap())
                    }
                })
                .collect();

            let mut import_scope_muts: FxHashMap<_, _> = use_trees
                .iter()
                .flatten()
                .unique()
                .filter_map(|use_tree| {
                    let import_scope = scb.make_import_scope_mut(
                        ImportScope::find_insert_use_container(use_tree.syntax(), sema)?,
                    );
                    Some((use_tree.clone(), import_scope))
                })
                .collect();

            let ref_muts: Vec<_> = refs.iter().cloned().map(|r| r.into_mut(scb)).collect();

            // TODO: Prioritize updating in-use-tree first
            // - This might already happen as a consequence of the return order of usages (it probably does)
            // - Actually, maybe better to split this into import (use) updates and normal ref updates
            for (use_tree, ref_mut) in iter::zip(use_trees, ref_muts) {
                match ref_mut.kind {
                    NameRefKind::InUseTree(use_tree_mut) => {
                        let use_tree = use_tree.unwrap();
                        if let Some(import_scope_mut) = import_scope_muts.remove(&use_tree) {
                            // TODO: Unify this with import update for unqualified refs
                            // - Also, discriminate between definition import updates and module import updates
                            // - This branch will always hit def import updates
                            // - The unqualified branch will only hit defs as well
                            // - The qualified branch will only hit module updates, I think, if we add the smart module updating

                            let qualifiers: Vec<_> = use_tree_mut
                                .syntax()
                                .first_child()
                                .and_then(ast::Path::cast)
                                .as_ref()
                                .map(ast::Path::qualifiers)
                                .into_iter()
                                .flatten()
                                .collect();

                            if qualifiers.is_empty()
                                && use_tree_mut.parent_use_tree_list().is_none()
                            {
                                // Unqualified use without a use tree can be treated as a normal unqualified ref
                                // The name must be imported in another use statement, which will be updated appropriately
                                ted::replace(
                                    ref_mut.name_ref.syntax(),
                                    make::name_ref(&new_path.name_str(sema.db))
                                        .clone_for_update()
                                        .syntax(),
                                );
                            // TODO: Handle module-qualified path
                            } else {
                                // Insert new use tree
                                insert_use_tree(
                                    &import_scope_mut,
                                    // TODO: Does this find the star token on the leaf use tree?
                                    // Mention that only case where we can be a glob here is if the def is an enum
                                    make::use_tree(
                                        new_path.full_path(),
                                        use_tree.use_tree_list(),
                                        use_tree.rename(),
                                        use_tree.star_token().is_some(),
                                    ),
                                    &InsertUseConfig {
                                        granularity: ImportGranularity::Module,
                                        enforce_granularity: false,
                                        prefix_kind: PrefixKind::ByCrate,
                                        group: true,
                                        skip_glob_imports: false,
                                    },
                                );

                                // Remove old use tree
                                use_tree_mut.remove_recursive();
                            }
                        }
                    }
                    NameRefKind::Qualified { ref_path: ref_path_mut, .. } => {
                        // TODO: Remove import if it becomes unused

                        let ref_module = (|| {
                            let path = use_tree.as_ref().map(|ut| ut.path().unwrap())?;
                            match self.sema.resolve_path(&path)? {
                                PathResolution::Def(ModuleDef::Module(m)) => Some(m),
                                _ => None,
                            }
                        })();

                        if let Some(ref_module) = ref_module
                            && new_path.module.ancestors(self.sema.db).contains(&ref_module)
                        {
                            // If the target module is a descendent of the imported module, we don't
                            // touch the import and just update the qualified reference to reflect
                            // the new relative path.
                            ted::replace(
                                ref_path_mut.syntax(),
                                new_path
                                    .relative_path_from(&sema.db, &ref_module)
                                    .unwrap()
                                    .clone_for_update()
                                    .syntax(),
                            );
                        } else if ref_module.is_some()
                            && ref_path_mut.qualifiers().exactly_one().is_ok()
                        {
                            // TODO: Explain why the ref_module.is_some() check ensures that we have
                            // a relative path instead of a crate modpath. Actually, better yet,
                            // convert the ref_path into a ModPath and get the type from that.

                            // If the qualified ref is single-module relative import from the origin
                            // module, we mirror this behavior in the updated import. That is, we
                            // import the target module and update the reference to be relative to
                            // the target module.
                            ted::replace(
                                ref_path_mut.syntax(),
                                new_path
                                    .rel_path_from_target_mod(sema.db)
                                    .clone_for_update()
                                    .syntax(),
                            );

                            if let Some(import_scope_mut) =
                                import_scope_muts.remove(&use_tree.unwrap())
                            {
                                // Add new import
                                insert_use_tree(
                                    &import_scope_mut,
                                    // TODO: Explain with a comment why this use tree is different from above
                                    make::use_tree(new_path.mod_full_path(), None, None, false),
                                    &InsertUseConfig {
                                        granularity: ImportGranularity::Module,
                                        enforce_granularity: false,
                                        prefix_kind: PrefixKind::ByCrate,
                                        group: true,
                                        skip_glob_imports: false,
                                    },
                                );
                            }
                        } else {
                            // Otherwise, we don't attempt to be smart about things and just update
                            // the ref to be a fullly-qualified path.
                            ted::replace(
                                ref_path_mut.syntax(),
                                new_path.full_path().clone_for_update().syntax(),
                            );
                        }
                    }
                    NameRefKind::UnQualified { name_ref_import: import_mut } => {
                        // Rename name ref
                        // TODO: Use make passed as a param
                        ted::replace(
                            ref_mut.name_ref.syntax(),
                            make::name_ref(&new_path.name_str(sema.db)).clone_for_update().syntax(),
                        );

                        // Update import if not already updated
                        if let Some(import_scope_mut) = import_scope_muts.remove(&use_tree.unwrap())
                        {
                            // Add new import
                            insert_use_tree(
                                &import_scope_mut,
                                // TODO: Explain with a comment why this use tree is different from above
                                make::use_tree(new_path.full_path(), None, None, false),
                                &InsertUseConfig {
                                    granularity: ImportGranularity::Module,
                                    enforce_granularity: false,
                                    prefix_kind: PrefixKind::ByCrate,
                                    group: true,
                                    skip_glob_imports: false,
                                },
                            );

                            // Remove old import if not glob
                            // If the import is a glob, it must be a glob on the origin module of the definition,
                            // which might resolve other usages in the current file. We could check for other usages
                            // to determine if we can safely remove the glob import, but for now we just leave it.
                            if !import_mut.is_glob() {
                                import_mut.use_tree().unwrap().remove_recursive();
                            }
                        }
                    }
                }
            }
        }

        Some(())
    }
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
struct ExternalRefs<'a> {
    refs: FxHashMap<EditionedFileId, Vec<NameRefInfo<'a>>>,
}

#[derive(Debug)]
struct NameRefInfo<'a> {
    full_path: String,
    immediate_path: ast::Path,
    first_seg: ast::Path,
    name_ref: NameRef,
    name_ref_class: NameRefClass<'a>,
    mod_path: Option<ModPath>,
    path_res: Option<PathResolution>,
    import_res: Option<ImportResolution>,
    import_res_first_seg: Option<ImportResolution>,
    in_use_tree: Option<ast::UseTree>,
}

impl<'a> ExternalRefs<'a> {
    fn compute(sema: &Semantics<'a, RootDatabase>, def: Definition) -> Self {
        let refs = def
            .usages(sema)
            .all()
            .into_iter()
            .map(|(file_id, file_refs)| {
                let file_refs = file_refs
                    .into_iter()
                    .filter_map(|FileReference { name, .. }| match name {
                        FileReferenceNode::Name(name) => {
                            // NOTE: Should not have to worry about Names for rename-moves for supported subset of definitions
                            panic!("Found name: {name:?}");
                        }
                        FileReferenceNode::NameRef(name_ref) => {
                            let name_ref_class = NameRefClass::classify(sema, &name_ref)?;
                            // NOTE: Immediate path seems to get everything we want excluding potential variant suffix
                            let immediate_path =
                                name_ref.syntax().ancestors().find_map(ast::Path::cast)?;
                            // NOTE: This gets the entire path, like "Epsilon::VariantA" for NameRef "Epsilon", which we don't actually want I don't think
                            let full_path = full_path_of_name_ref(&name_ref)?;
                            let mod_path = {
                                let file_id = sema.hir_file_for(immediate_path.syntax());
                                let span_map = sema.db.span_map(file_id);
                                ModPath::from_src(sema.db, immediate_path.clone(), &mut |range| {
                                    span_map.span_for_range(range).ctx
                                })
                            };
                            let path_res = sema.resolve_path(&immediate_path);
                            let import_res = sema.resolve_import(&immediate_path);

                            let first_seg = immediate_path.first_qualifier_or_self();
                            let import_res_first_seg = sema.resolve_import(&first_seg);

                            // TODO: Better in use stament check, per codebase convention
                            let in_use_tree =
                                name_ref.syntax().ancestors().filter_map(ast::UseTree::cast).last();

                            NameRefInfo {
                                full_path: format!("{full_path}"),
                                first_seg,
                                name_ref,
                                name_ref_class,
                                immediate_path,
                                mod_path,
                                path_res,
                                import_res,
                                import_res_first_seg,
                                in_use_tree,
                            }
                            .into()
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

// TODO: Remove
pub mod debug {
    use super::*;

    pub fn print_fname(db: &RootDatabase, file_id: EditionedFileId) -> Option<()> {
        let fid = file_id.file_id(db);
        let sr_id = db.file_source_root(fid).source_root_id(db);
        let sr = db.source_root(sr_id).source_root(db);
        let vpath = sr.path_for_file(&fid)?;
        let (stem, ext) = vpath.name_and_extension()?;
        let name = Some(match ext {
            Some(ext) => format!("{stem}.{ext}"),
            None => stem.to_string(),
        });
        println!("{name:?}");
        Some(())
    }
}

#[cfg(test)]
mod tests {
    use hir::{Semantics, attach_db};
    use rustc_hash::FxHashMap;
    use span::Edition;
    use syntax::{
        AstNode,
        ast::{self, HasModuleItem},
    };
    use test_fixture::WithFixture;

    use crate::{RootDatabase, defs::Definition, rename_move::ExternalRefs};

    #[test]
    #[ignore]
    fn test_print_syntax() {
        let fixture = r#"
//- /main.rs
pub mod foo;
pub mod bar;
pub mod baz;
pub mod buz;
pub mod blast;
pub mod destruct;

//- /foo.rs
pub struct Alpha;
pub struct Beta;
pub struct Gamma;

pub struct Delta;

impl Delta {
    pub fn delta_assoc() -> Self {
        Self
    }

    pub fn delta_method(&self) {}
}

pub enum Epsilon {
    VariantA,
    VariantB,
}

pub struct ForDestruct {
    inner: i32,
}

//- /bar.rs
use crate::foo::Alpha;
use super::foo::Beta;
use crate::foo::Epsilon;
use crate::foo::Delta;

fn my_fun(a: Alpha, b: Beta) -> (Alpha, Beta) {
    (a, b)
}

fn eps_fun(eps: Epsilon) {
    match eps {
        Epsilon::VariantA => {}
        crate::foo::Epsilon::VariantB => {}
    }

    if let Epsilon::VariantA = eps {}
}

fn use_delta(delta: Delta) {
    Delta::delta_assoc();
    delta.delta_method();
}

//- /baz.rs
use crate::foo::{Alpha, Beta, Epsilon};

fn my_fun(a: Alpha, b: Beta) -> (Alpha, Beta) {
    (a, b)
}

//- /buz.rs
use crate::foo::*;

fn my_fun(a: Alpha, b: Beta) -> (Alpha, Beta) {
    (a, b)
}

//- /blast.rs
use crate::foo;

// NOTE: Fail to get import_res for foo::Epsilon
fn my_fun(eps: foo::Epsilon) {
    use crate::foo::{Alpha, Epsilon::*};

    match eps {
        VariantA => {}
        VariantB => {}
    }

    if let foo::Epsilon::VariantA = eps {}
}

mod inner {
    use crate::foo as fud;

    const EPS: fud::Epsilon = fud::Epsilon::VariantA;
}

mod other_inner {
    use crate::foo::Epsilon as EpsRename;

    const EPS: EpsRename = EpsRename::VariantA;
}

//- /destruct.rs
use crate::foo::ForDestruct;

fn do_destruct(d: ForDestruct) -> i32 {
    let ForDestruct { inner } = d;
    inner
}

fn do_destruct_param(ForDestruct { inner }: ForDestruct) -> i32 {
    inner
}


"#;

        let (dbg_file, dbg_def) = std::env::var("DD")
            .expect("no debug specified")
            .split_once(':')
            .map(|(x, y)| (x.to_owned(), y.to_owned()))
            .expect("malformed debug");

        let (db, files) = RootDatabase::with_many_files(fixture);

        attach_db(&db, || {
            let sema = Semantics::new(&db);
            let edition = Edition::LATEST;

            // TODO: Find in refs in glob
            // - Actually, we could potentially determine if

            let source_files: FxHashMap<_, _> =
                ["main", "foo", "bar", "baz", "buz", "blast", "destruct"]
                    .into_iter()
                    .zip(&files)
                    .map(|(name, file)| (name, sema.parse(*file)))
                    .collect();

            dbg!(&source_files[dbg_file.as_str()]);

            let file_names: FxHashMap<_, _> = source_files
                .iter()
                .map(|(name, sf)| (sema.hir_file_for(sf.syntax()).file_id().unwrap(), *name))
                .collect();

            let defs: FxHashMap<_, _> = source_files["foo"]
                .items()
                .filter_map(|i| ast::Adt::cast(i.syntax().clone()))
                .map(|adt| sema.to_adt_def(&adt).unwrap())
                .map(|adt| Definition::Adt(adt))
                .map(|adt| {
                    let name = format!("{}", adt.name(sema.db).unwrap().display(sema.db, edition));
                    (name, adt)
                })
                .collect();

            let refs: FxHashMap<_, _> = defs
                .iter()
                .map(|(name, def)| {
                    let refs = ExternalRefs::compute(&sema, def.clone())
                        .refs
                        .into_iter()
                        .map(|(file_id, refs)| (file_names[&file_id], refs))
                        .collect::<FxHashMap<_, _>>();
                    (name.as_str(), refs)
                })
                .collect();

            dbg!(&refs[dbg_def.as_str()][dbg_file.as_str()]);
        });
    }
}
