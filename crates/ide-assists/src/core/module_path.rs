use hir::{HasAttrs, Module};
use ide_db::{
    RootDatabase,
    base_db::AnchoredPathBuf,
    FileId,
};
use syntax::ast::HasName;
use stdx::format_to;

/// Compute file path for a direct child module of `parent_module` named `child`.
///
/// Mirrors path rules used by assists:
/// - If parent is `mod.rs` or has `#[path]`, place child alongside.
/// - Otherwise, place child under a directory named after the parent module.
/// - Special-case `r#mod` to use `mod/mod.rs`, else `<child>.rs` (trimming `r#`).
pub fn compute_child_module_file_path(
    db: &RootDatabase,
    parent_module: Module,
    anchor_file: FileId,
    child: &str,
) -> AnchoredPathBuf {
    let mut path = String::new();
    let is_parent_mod_rs = parent_module.is_mod_rs(db);
    let has_path_attr = parent_module
        .attrs(db)
        .by_key(hir::sym::path)
        .string_value_unescape()
        .is_some();
    if !is_parent_mod_rs && !has_path_attr {
        if let Some(name) = parent_module.name(db) {
            path.push_str(name.as_str());
            path.push('/');
        }
    }
    if child == "r#mod" {
        path.push_str("mod/mod.rs");
    } else {
        let child_fs = child.strip_prefix("r#").unwrap_or(child);
        path.push_str(child_fs);
        path.push_str(".rs");
    }
    AnchoredPathBuf { anchor: anchor_file, path }
}

/// Compute the file path for moving an inline module `module_ast` into its own file.
///
/// This is the path building logic extracted from the `move_module_to_file` assist.
pub fn compute_move_module_file_path(
    db: &RootDatabase,
    parent_module: Module,
    module_ast: &syntax::ast::Module,
    anchor_file: FileId,
) -> AnchoredPathBuf {
    use itertools::Itertools;
    use syntax::{AstNode, SmolStr};

    let mut buf = String::new();
    match parent_module.name(db) {
        Some(name)
            if !parent_module.is_mod_rs(db)
                && parent_module
                    .attrs(db)
                    .by_key(hir::sym::path)
                    .string_value_unescape()
                    .is_none() =>
        {
            buf.push_str(name.as_str());
            buf.push('/');
        }
        _ => (),
    }

    let segments = std::iter::successors(Some(module_ast.clone()), |module| module.parent())
        .filter_map(|it| it.name())
        .map(|name| SmolStr::from(name.text().trim_start_matches("r#")))
        .collect::<Vec<_>>();

    format_to!(buf, "{}", segments.into_iter().rev().format("/"));

    if module_ast.name().is_some_and(|n| n.text() == "r#mod") {
        buf.push_str("/mod.rs");
    } else {
        buf.push_str(".rs");
    }

    AnchoredPathBuf { anchor: anchor_file, path: buf }
}
