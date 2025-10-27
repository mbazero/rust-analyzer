use std::iter;

use hir::ModPath;
use hir::Module;
use hir::ModuleDef;
use hir::Name;
use hir::Semantics;
use hir::db::ExpandDatabase;
use ide_db::FileRange;
use ide_db::defs::Definition;
use ide_db::rename::RenameDefinition;
use ide_db::rename::RenameError;
use ide_db::rename::bail;
use ide_db::rename::format_err;
use ide_db::source_change::SourceChangeBuilder;
use ide_db::{FilePosition, RootDatabase, source_change::SourceChange};
use itertools::Itertools;
use span::Edition;
use syntax::AstNode;
use syntax::SourceFile;
use syntax::SyntaxKind;
use syntax::SyntaxNode;
use syntax::ast;
use syntax::ast::HasModuleItem;

use crate::TryToNav;
use crate::rename::find_definitions;

pub type Result<T> = crate::rename::RenameResult<T>;

// Compilation:
// - Parse the mod path
// - Find the definition, ensure it can be renamed
// - Collect inbound references (in_refs)
// - Collect outbound references (out_refs)
//
// Validation:
// - Determine minimum visibility required of the struct after move based on inbound references
// - Ensure all internal refs are still accessible
// - If destination module already exists, ensure new name won't clash with an existing name
//
// New module changes:
// - Create module if it doesn't exist
// - Copy over text range of move item
// - Satisfy out_refs (resolve imports)
//   - Insert use items for all out_refs
//   - De-dup against existing use statements
// - Satisfy in_refs
//   - Change visibility of definition + decords
//   - Update external use statements (simple rename)
//
// Old module changes:
// - Remove any use statements that only satisfied imports within moved selection
//
// Other module changes:
// - Update external use statements (simple rename)
//
// struct OutRef {
//    used_in_original_mod: bool,
//    used_in_new_mod: bool,
// }
//
//
// Edge cases:
// - Support raw identifiers
// - Derive macros, etc must be moved as well
// - Adt defined within function etc
//   - Do we want to support this?
// - Adt defined within inline module
// - Delete src module if it becomes empty

pub(crate) fn rename_move(
    db: &RootDatabase,
    position: FilePosition,
    new_path: &str,
) -> Result<SourceChange> {
    dbg!(&position);

    let sema = Semantics::new(db);
    let file_id = sema
        .attach_first_edition(position.file_id)
        .ok_or_else(|| format_err!("No references found at position"))?;
    let raw_file_id = file_id.file_id(db);
    let source_file = sema.parse(file_id);
    let syntax = source_file.syntax();
    let edition = file_id.edition(db);

    let src_mod = sema.file_to_module_def(raw_file_id).expect("Failed to get src mod");
    let krate = src_mod.krate();
    let all_mod_names: Vec<_> =
        krate.modules(db).into_iter().map(|m| m.canonical_path(db, edition)).collect();
    dbg!(all_mod_names);
    let crate_root = src_mod.crate_root(db);
    let src_mod_path = src_mod.path_to_root(db);
    dbg!(&src_mod_path);

    let dst_path = parse_move_target(new_path, &sema, edition)?;
    dbg!(&dst_path);

    // TODO: Only pop last if the move def is not a module
    let dst_mod_path = {
        let mut p = dst_path.clone();
        p.pop_segment();
        p
    };
    dbg!(&dst_mod_path);

    let Some(dst_mod) = crate_root.resolve_mod_path(db, dst_mod_path.segments().iter().cloned())
    else {
        bail!("Coudln't find dst_mod");
    };

    let dst_resolved_mod_path: Vec<_> = dst_mod.map(|x| x.into_module_def().name(db)).collect();
    dbg!(dst_resolved_mod_path);

    // let dst_res_path = crate_root
    //     .resolve_mod_path(db, dst_mod_path.segments().iter().cloned())
    //     .map(|x| x.collect::<Vec<_>>());

    // Use this for relative paths
    // let dst_res_path = sema.resolve_mod_path(syntax, &dst_mod_path).map(|x| x.collect::<Vec<_>>());

    // Bail if target is within a macro

    // Find the range of the thing you're trying to move
    // - Need to make sure you have the thing used for the range in prepare_rename

    let (file_range, syntax_kind, def, new_name, rename_def) =
        find_definitions(&sema, syntax, position, dst_mod_path.segments().last().unwrap())?
            .exactly_one()
            .map_err(|_| {
                // Multiple found definitions indicates macro involvement, which is currently unsupported
                format_err!("Rename-move unsupported")
            })?;

    match def {
        Definition::Module(module) => {
            // TODO: Handle module moves
            bail!("Module moves not yet supported");
        }
        Definition::Adt(_)
        | Definition::Const(_)
        | Definition::Static(_)
        | Definition::Trait(_) => {}
        _ => bail!("Rename-move unsupported"),
    };

    // NOTE: Test if this fails if the definition is within a function etc
    let parent_mod = Definition::Module(def.module(db).unwrap());
    let parent_mod_path = parent_mod.canonical_module_path(db).unwrap();

    let def_nav_target = def.try_to_nav(&sema).unwrap().call_site;

    let contents = source_file.syntax().text().slice(def_nav_target.full_range).to_string();
    println!("contents:\n{contents}");

    let new_mod_path =
        dst_mod_path.segments().iter().map(|name| name.display(db, edition)).format("/");
    println!("new_mod_path: {new_mod_path}");

    let builder = SourceChangeBuilder::new(raw_file_id);

    // Check target type

    bail!("Testing");
}

fn get_or_create_dst_mod(
    sema: &Semantics<'_, RootDatabase>,
    mod_path: &ModPath,
    anchor_mod: Module,
    source_change: &SourceChangeBuilder,
) -> Option<Module> {
    // let root_mod = match mod_path.kind {
    //     hir::PathKind::Super(n) => (0..n).fold(anchor_mod, |m, _| m.parent(sema.db).unwrap()),
    //     _ => anchor_mod.crate_root(sema.db),
    // };
    todo!()
}

fn get_mod_lca(a: Module, b: Module) -> Option<Module> {
    todo!()
}

fn parse_move_target(
    path: &str,
    sema: &Semantics<'_, RootDatabase>,
    edition: Edition,
) -> Result<ModPath> {
    let mod_path =
        parse_mod_path(path, sema, edition).ok_or_else(|| format_err!("Invalid move target"))?;

    match mod_path.kind {
        hir::PathKind::Crate => Ok(mod_path),
        _ => bail!("Only crate move targets are supported"),
    }
}

fn parse_mod_path(
    path: &str,
    sema: &Semantics<'_, RootDatabase>,
    edition: Edition,
) -> Option<ModPath> {
    let parsed = SourceFile::parse(&format!("use {};", path), edition).ok().ok()?;
    let path = parsed.items().find_map(|item| match item {
        ast::Item::Use(use_item) => use_item.use_tree()?.path(),
        _ => None,
    })?;
    ModPath::from_src(sema.db as &dyn ExpandDatabase, path, &mut |_| {
        span::SyntaxContext::root(edition)
    })
}

#[cfg(test)]
mod tests {
    use hir::{Semantics, db::ExpandDatabase};
    use ide_db::{RootDatabase, text_edit::TextEdit};
    use span::Edition;
    use stdx::trim_indent;
    use test_utils::assert_eq_text;

    use crate::{
        fixture,
        rename_move::{parse_mod_path, parse_move_target},
    };

    #[track_caller]
    fn check(
        new_name: &str,
        #[rust_analyzer::rust_fixture] ra_fixture_before: &str,
        #[rust_analyzer::rust_fixture] ra_fixture_after: &str,
    ) {
        let ra_fixture_after = &trim_indent(ra_fixture_after);
        let (analysis, position) = fixture::position(ra_fixture_before);
        if !ra_fixture_after.starts_with("error: ")
            && let Err(err) = analysis.prepare_rename(position).unwrap()
        {
            panic!("Prepare rename to '{new_name}' was failed: {err}")
        }
        let rename_result = analysis
            .rename(position, new_name)
            .unwrap_or_else(|err| panic!("Rename to '{new_name}' was cancelled: {err}"));
        match rename_result {
            Ok(source_change) => {
                let mut text_edit_builder = TextEdit::builder();
                let (&file_id, edit) = match source_change.source_file_edits.len() {
                    0 => return,
                    1 => source_change.source_file_edits.iter().next().unwrap(),
                    _ => panic!(),
                };
                for indel in edit.0.iter() {
                    text_edit_builder.replace(indel.delete, indel.insert.clone());
                }
                let mut result = analysis.file_text(file_id).unwrap().to_string();
                text_edit_builder.finish().apply(&mut result);
                assert_eq_text!(ra_fixture_after, &*result);
            }
            Err(err) => {
                if ra_fixture_after.starts_with("error:") {
                    let error_message =
                        ra_fixture_after.chars().skip("error:".len()).collect::<String>();
                    assert_eq!(error_message.trim(), err.to_string());
                } else {
                    panic!("Rename to '{new_name}' failed unexpectedly: {err}")
                }
            }
        };
    }

    #[test]
    fn test_move_to_existing_module() {
        check(
            "crate::bar::FooStruct",
            r#"
//- /main.rs
mod foo;
mod bar;
//- /foo.rs
struct $0FooStruct;
struct OtherFooStruct;
//- /bar.rs
struct BarStruct;
            "#,
            r#"
//- /main.rs
mod foo;
mod bar;
//- /foo.rs
struct OtherFooStruct;
//- /bar.rs
struct BarStruct;
struct FooStruct;
            "#,
        );
    }

    #[test]
    fn test_basic_nested_rename_move() {
        check(
            "crate::foo::bar",
            r#"
//- /main.rs
mod foo;
//- /foo.rs
mod bar;
//- /foo/bar.rs
struct $0OldName;
            "#,
            r#"
//- /main.rs
mod foo;
//- /foo.rs
mod bar;
//- /foo/bar.rs
struct NewName;
            "#,
        );
    }

    #[test]
    fn test_basic_rename_move() {
        check(
            "crate::alpha::beta::bar_mod::Bar",
            r#"
//- /main.rs
use crate::foo_mod::{Foo, Other};

mod foo_mod;

pub fn use_foo(x: Foo) -> i32 {
    x.a
}

pub fn use_other(y: Other) -> Vec<i32> {
    y.0
}
//- /foo_mod.rs
use std::str::String;
use std::vec::Vec;

#[derive(Default)]
struct $0Foo {
    a: i32,
    b: String,
};

struct Other(Vec<i32>);
            "#,
            r#"
//- /main.rs
use crate::foo_mod::Other;
use crate::bar_mod::Bar;

mod foo_mod;
mod bar_mod;

pub fn use_foo(x: Foo) -> i32 {
    x.a
}

pub fn use_other(y: Other) -> i32 {
    y.0
}
//- /foo_mod.rs
use std::vec::Vec;

struct Other(Vec<i32>);
//- /bar_mod.rs
use std::str::String;

#[derive(Default)]
struct $0Bar {
    a: i32,
    b: String,
};
            "#,
        );
    }

    #[test]
    fn test_parse_mod_path() {
        let db = RootDatabase::default();
        let sema = Semantics::new(&db);
        let edition = Edition::LATEST;

        let cases = [
            "crate::foo::bar",
            "crate::r#fn::bar",
            "crate::super::bar",
            "crate::foo::bar::MyStruct",
        ];

        for path in cases {
            let parsed = parse_mod_path(path, &sema, edition);
            dbg!(parsed);
        }
    }

    #[test]
    fn test_parse_move_target() {
        let db = RootDatabase::default();
        let sema = Semantics::new(&db);

        let cases = [
            ("crate::foo::bar", true),
            ("crate::r#fn::bar", true),
            ("super::foo::bar", false),
            ("::foo::bar", false),
            ("foo", false),
        ];

        for (path, should_parse) in cases {
            let parsed = parse_move_target(path, &sema, Edition::LATEST);
            let act =
                parsed.map(|mp| format!("{}", mp.display_verbatim(sema.db as &dyn ExpandDatabase)));
            let exp = should_parse.then_some(path);
            // assert_eq!(exp, act.as_deref());
        }
    }
}
