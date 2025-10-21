use syntax::{
    AstNode,
    SyntaxNode,
    ast::{self, HasVisibility, edit::AstNodeEdit, make},
};
use syntax::ast::edit::IndentLevel;
use ide_db::source_change::SourceChangeBuilder;
use crate::AssistContext;
use ide_db::span::TextRange;
use ide_db::FileId;
use ide_db::RootDatabase;
use hir::{EditionedFileId, Semantics};
use syntax::Edition;
use crate::AssistConfig;
use syntax::algo::find_node_at_range;
use ide_db::{FileRange, FxHashMap, FxHashSet};
use syntax::SyntaxKind::WHITESPACE;
use itertools::Itertools;

/// Build the SourceChange for extracting `module` into an inline module definition at `module_text_range`.
/// Mirrors the assist logic but returns a SourceChange for reuse by other features.
pub fn build_extract_change(
    ctx: &AssistContext<'_>,
    mut module: Module,
    module_text_range: TextRange,
    impl_parent: &Option<ast::Impl>,
    impl_child_count: usize,
    selected_node: &SyntaxNode,
    curr_parent_module: Option<ast::Module>,
    old_item_indent: IndentLevel,
) -> ide_db::source_change::SourceChange {
    let (usages_to_be_processed, record_fields, use_stmts_to_be_inserted) =
        module.get_usages_and_record_fields(ctx, module_text_range);

    let mut builder = SourceChangeBuilder::new(ctx.vfs_file_id());
    builder.edit_file(ctx.vfs_file_id());
    use_stmts_to_be_inserted.into_iter().for_each(|(_, use_stmt)| {
        builder.insert(ctx.selection_trimmed().end(), format!("\n{use_stmt}"));
    });

    let import_items = module.resolve_imports(curr_parent_module, ctx);
    module.change_visibility(record_fields);

    let module_def = generate_module_def(impl_parent, module, old_item_indent).to_string();

    let mut usages_to_be_processed_for_cur_file = vec![];
    for (file_id, usages) in usages_to_be_processed {
        if file_id == ctx.vfs_file_id() {
            usages_to_be_processed_for_cur_file = usages;
            continue;
        }
        builder.edit_file(file_id);
        for (text_range, usage) in usages {
            builder.replace(text_range, usage)
        }
    }

    builder.edit_file(ctx.vfs_file_id());
    for (text_range, usage) in usages_to_be_processed_for_cur_file {
        builder.replace(text_range, usage);
    }

    if let Some(impl_) = impl_parent {
        let node_to_be_removed = if impl_child_count == 1 { impl_.syntax() } else { selected_node };

        builder.delete(node_to_be_removed.text_range());
        if let Some(range) = indent_range_before_given_node(node_to_be_removed) {
            builder.delete(range);
        }

        builder.insert(impl_.syntax().text_range().end(), format!("\n\n{module_def}"));
    } else {
        for import_item in import_items {
            if !module_text_range.contains_range(import_item) {
                builder.delete(import_item);
            }
        }
        builder.replace(module_text_range, module_def)
    }

    builder.finish()
}

/// Public wrapper to extract items selected by `frange` into a child module named `module_name`.
/// Returns `(change_without_file_creation, Some(body_contents))` for emit_inline = false.
pub struct RenameSpec {
    pub keyword: &'static str,
    pub old: String,
    pub new_name: String,
}

pub struct ExtractOpts {
    pub emit_inline: bool,
    pub renames: Vec<RenameSpec>,
}

pub fn extract_items_to_child_module_from_range(
    db: &RootDatabase,
    config: &AssistConfig,
    frange: FileRange,
    module_name: &str,
    opts: &ExtractOpts,
) -> Result<(ide_db::source_change::SourceChange, String), String> {
    let sema = Semantics::new(db);
    let file_id = sema
        .attach_first_edition(frange.file_id)
        .unwrap_or_else(|| EditionedFileId::new(db, frange.file_id, Edition::CURRENT));
    let ctx = AssistContext::new(sema, config, hir::FileRange { file_id, range: frange.range });

    // Establish selection context
    let node = ctx.covering_element();
    let node = match node {
        syntax::NodeOrToken::Node(n) => n,
        syntax::NodeOrToken::Token(t) => t.parent().ok_or_else(|| "No parent".to_string())?,
    };

    // Detect impl parent
    let mut impl_parent: Option<ast::Impl> = None;
    let mut impl_child_count: usize = 0;
    if let Some(parent_assoc_list) = node.parent()
        && let Some(parent_impl) = parent_assoc_list.parent()
        && let Some(impl_) = ast::Impl::cast(parent_impl)
    {
        impl_child_count = parent_assoc_list.children().count();
        impl_parent = Some(impl_);
    }

    let mut curr_parent_module: Option<ast::Module> = None;
    if let Some(mod_syn_opt) = node.ancestors().find(|it| ast::Module::can_cast(it.kind())) {
        curr_parent_module = ast::Module::cast(mod_syn_opt);
    }

    // Build Module from selection
    let selection_range = ctx.selection_trimmed();
    let (mut module, module_text_range) = if let Some(item) = ast::Item::cast(node.clone()) {
        let module = extract_single_target_named(&item, module_name);
        (module, node.text_range())
    } else {
        let (module, range) = extract_child_target_named(&node, selection_range, module_name)
            .ok_or_else(|| "No items to extract".to_string())?;
        let module_text_range = range.start().text_range().cover(range.end().text_range());
        (module, module_text_range)
    };
    if module.body_items.is_empty() {
        return Err("No items to extract".to_string());
    }
    let old_item_indent = module.body_items[0].indent_level();

    let (change, body) = extract_items_to_module(
        &ctx,
        module,
        module_text_range,
        &impl_parent,
        impl_child_count,
        &node,
        curr_parent_module,
        old_item_indent,
        opts,
    )?;
    Ok((change, body.expect("engine returned no body for non-inline extraction")))
}

fn extract_single_target_named(node: &ast::Item, module_name: &str) -> Module {
    let (body_items, use_items) = if matches!(node, ast::Item::Use(_)) {
        (Vec::new(), vec![node.clone()])
    } else {
        (vec![node.clone()], Vec::new())
    };
    Module { name: unsafe { std::mem::transmute::<&str, &'static str>(module_name) }, body_items, use_items }
}

fn extract_child_target_named(
    node: &SyntaxNode,
    selection_range: TextRange,
    module_name: &str,
) -> Option<(Module, std::ops::RangeInclusive<SyntaxNode>)> {
    let selected_nodes = node
        .children()
        .filter(|node| selection_range.contains_range(node.text_range()))
        .filter_map(ast::Item::cast)
        .collect_vec();
    let start = selected_nodes.first()?.syntax().clone();
    let end = selected_nodes.last()?.syntax().clone();
    let (use_items, body_items): (Vec<ast::Item>, Vec<ast::Item>) =
        selected_nodes.into_iter().partition(|item| matches!(item, ast::Item::Use(..)));
    Some((
        Module { name: unsafe { std::mem::transmute::<&str, &'static str>(module_name) }, body_items, use_items },
        start..=end,
    ))
}

/// High-level engine API: extracts `module` into a module definition.
/// If `emit_inline` is true, returns the inline change.
/// If `emit_inline` is false, replaces the original selection with `mod <module_name>;`
/// and returns the module body as `Some(String)` to be written to a new file by the caller.
pub fn extract_items_to_module(
    ctx: &AssistContext<'_>,
    module: Module,
    module_text_range: TextRange,
    impl_parent: &Option<ast::Impl>,
    impl_child_count: usize,
    selected_node: &SyntaxNode,
    curr_parent_module: Option<ast::Module>,
    old_item_indent: IndentLevel,
    opts: &ExtractOpts,
) -> Result<(ide_db::source_change::SourceChange, Option<String>), String> {
    let module_name = module.name;
    let change = build_extract_change(
        ctx,
        module,
        module_text_range,
        impl_parent,
        impl_child_count,
        selected_node,
        curr_parent_module,
        old_item_indent,
    );
    if opts.emit_inline {
        return Ok((change, None));
    }

    // Transform inline change into declaration + body
    let mut result = ide_db::source_change::SourceChange::default();
    let mut created_body: Option<String> = None;
    for (fid, (edit, snip)) in change.source_file_edits.into_iter() {
        let mut builder = ide_db::text_edit::TextEdit::builder();
        for indel in edit.into_iter() {
            let insert = indel.insert;
            if fid == ctx.vfs_file_id() && indel.delete == module_text_range {
                // Extract body and replace with declaration
                let mut body = crate::core::extract_core::extract_module_body(&insert)
                    .ok_or_else(|| "Unexpected extract_module payload".to_string())?;
                // Apply renames to moved items in the body if requested
                for r in &opts.renames {
                    body = crate::core::extract_core::rename_item_in_body(&body, r.keyword, &r.old, &r.new_name);
                }
                created_body = Some(body);
                let mod_decl = format!("mod {};", module_name);
                builder.replace(indel.delete, mod_decl);
            } else {
                builder.replace(indel.delete, insert);
            }
        }
        result.insert_source_and_snippet_edit(fid, builder.finish(), snip);
    }
    let contents = created_body.ok_or_else(|| "Failed to extract module body".to_string())?;
    Ok((result, Some(contents)))
}

// Helpers moved from handler for reuse
use syntax::TextRange as SynTextRange;
use syntax::ted;

pub fn check_intersection_and_push(
    import_paths_to_be_removed: &mut Vec<SynTextRange>,
    mut import_path: SynTextRange,
) {
    use itertools::Itertools;
    use smallvec::SmallVec;
    let s: SmallVec<[_; 2]> = import_paths_to_be_removed
        .iter_mut()
        .positions(|it| it.intersect(import_path).is_some())
        .collect();
    for pos in s.into_iter().rev() {
        let intersecting_path = import_paths_to_be_removed.swap_remove(pos);
        import_path = import_path.cover(intersecting_path);
    }
    import_paths_to_be_removed.push(import_path);
}

pub fn get_replacements_for_visibility_change(
    items: &mut [ast::Item],
    is_clone_for_updated: bool,
) -> (
    Vec<(Option<ast::Visibility>, SyntaxNode)>,
    Vec<(Option<ast::Visibility>, SyntaxNode)>,
    Vec<ast::Impl>,
) {
    let mut replacements = Vec::new();
    let mut record_field_parents = Vec::new();
    let mut impls = Vec::new();

    macro_rules! push_to_replacement {
        ($it:ident) => {
            replacements.push(($it.visibility(), $it.syntax().clone()))
        };
    }

    for item in items {
        if !is_clone_for_updated {
            *item = item.clone_for_update();
        }
        match item {
            ast::Item::Const(it) => push_to_replacement!(it),
            ast::Item::Enum(it) => push_to_replacement!(it),
            ast::Item::ExternCrate(it) => push_to_replacement!(it),
            ast::Item::Fn(it) => push_to_replacement!(it),
            ast::Item::Impl(it) if it.for_token().is_none() => impls.push(it.clone()),
            ast::Item::MacroDef(it) => push_to_replacement!(it),
            ast::Item::Module(it) => push_to_replacement!(it),
            ast::Item::Static(it) => push_to_replacement!(it),
            ast::Item::Struct(it) => {
                push_to_replacement!(it);
                record_field_parents.push((it.visibility(), it.syntax().clone()));
            }
            ast::Item::Trait(it) => push_to_replacement!(it),
            ast::Item::TypeAlias(it) => push_to_replacement!(it),
            ast::Item::Union(it) => {
                push_to_replacement!(it);
                record_field_parents.push((it.visibility(), it.syntax().clone()));
            }
            _ => (),
        }
    }

    (replacements, record_field_parents, impls)
}

pub fn add_change_vis(vis: Option<ast::Visibility>, node_or_token_opt: Option<syntax::SyntaxElement>) {
    if vis.is_none()
        && let Some(node_or_token) = node_or_token_opt
    {
        let pub_crate_vis = make::visibility_pub_crate().clone_for_update();
        ted::insert(ted::Position::before(node_or_token), pub_crate_vis.syntax());
    }
}

pub fn indent_range_before_given_node(node: &SyntaxNode) -> Option<SynTextRange> {
    node.siblings_with_tokens(syntax::Direction::Prev)
        .find(|x| x.kind() == WHITESPACE)
        .map(|x| x.text_range())
}


/// Shared module representation used by extract engine and assists.
#[derive(Debug, Clone)]
pub struct Module {
    pub name: &'static str,
    /// All items except use items.
    pub body_items: Vec<ast::Item>,
    /// Use items are kept separately as they help when the selection is inside an impl block,
    /// we can directly take these items and keep them outside generated impl block inside
    /// generated module.
    pub use_items: Vec<ast::Item>,
}

/// Generate an `ast::Module` node from a `Module` description.
pub fn generate_module_def(
    parent_impl: &Option<ast::Impl>,
    module: Module,
    old_indent: IndentLevel,
) -> ast::Module {
    let Module { name, body_items, use_items } = module;
    let items = if let Some(self_ty) = parent_impl.as_ref().and_then(|imp| imp.self_ty()) {
        let assoc_items = body_items
            .into_iter()
            .map(|item| item.syntax().clone())
            .filter_map(ast::AssocItem::cast)
            .map(|it| it.indent(IndentLevel(1)))
            .collect::<Vec<_>>();
        let assoc_item_list = make::assoc_item_list(Some(assoc_items));
        let impl_ = make::impl_(None, None, None, self_ty.clone(), None, Some(assoc_item_list));
        // Add the import for enum/struct corresponding to given impl block
        let use_impl = make_use_stmt_of_node_with_super(self_ty.syntax());
        let mut module_body_items = use_items;
        module_body_items.insert(0, use_impl);
        module_body_items.push(ast::Item::Impl(impl_));
        module_body_items
    } else {
        [use_items, body_items].concat()
    };

    let items = items
        .into_iter()
        .map(|it| it.reset_indent().indent(IndentLevel(1)))
        .collect::<Vec<_>>();
    let module_body = make::item_list(Some(items));

    let module_name = make::name(name);
    make::mod_(module_name, Some(module_body)).indent(old_indent)
}

pub fn make_use_stmt_of_node_with_super(node_syntax: &SyntaxNode) -> ast::Item {
    let super_path = make::ext::ident_path("super");
    let node_path = make::ext::ident_path(&node_syntax.to_string());
    let use_ = make::use_(
        None,
        None,
        make::use_tree(make::join_paths(vec![super_path, node_path]), None, None, false),
    );

    ast::Item::from(use_)
}
