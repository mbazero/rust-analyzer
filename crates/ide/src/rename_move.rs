use hir::ModPath;
use hir::Module;
use hir::ModuleDef;
use hir::Semantics;
use hir::db::ExpandDatabase;
use ide_db::defs::Definition;
use ide_db::rename::RenameError;
use ide_db::rename::bail;
use ide_db::rename::format_err;
use ide_db::rename_move::RenameMoveAdt;
use ide_db::{FilePosition, RootDatabase, source_change::SourceChange};
use itertools::Itertools;
use span::Edition;
use syntax::AstNode;
use syntax::SourceFile;
use syntax::ast;
use syntax::ast::HasModuleItem;

use crate::rename::find_definitions;

pub(crate) type Result<T> = crate::rename::RenameResult<T>;

// TODO: Implement rename-move
//
// Rename-move (RM) is triggered by normal rename code action with a fully qualified rename path--e.g.
// crate::foo::bar::NewStructName.
//
// Everything below is a tentative sketch for the implementation and is subject to change.
//
// Tentative rename-move steps:
// - Parse new_path into a ModPath
// - Find definition and ensure it can be rename-moved
//   - To start, only support Module, Function, Adt, Const, and Static definitions
// - Find associated impls in file
// - Construct RenameMoveComponents struct with definitions and associated impl
// - Get or create the destination module
//   - If destination module doesn't exist, we need to add source edits to create it
//   - Also need to support creating intermediate modules for nested paths
// - Move all RenameMoveComponents to target module
//   - See extract_module.rs and move_mod_to_file.rs for some guidance here
// - Pull in required imports
//   - Update internal use statements
//   - Find any unresolved internal refs
//      - Refs can be resolved by internal use statements or use statements already in the module
// - Update internal visibility
//      - Struct fields
//      - Impl constants
//      - Function defs
// - Update all external usages (rename)
//   - This can probably be done with existing rename.rs infrastructure with some slight
//   modifications
//
// Below are some random notes on the implementation:
//
// Possible pre-compilation approach:
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
// Edge cases:
// - Support raw identifiers
// - Derive macros, etc must be moved as well
// - Adt defined within function etc
//   - Do we want to support this?
// - Adt defined within inline module
// - Delete src module if it becomes empty
// - Handle inline module targets
//   - Need to indent appropriately
// - Handle inline module origins
//   - Need to unindent
// - Actually, do need to handle multiple defintions since the def could be annotated by a proc macro which creates some additional defs, maybe
//
// Misc:
// - Once a functional implementation is complete, see how much functionality can be merged with:
//   - rename.rs
//   - extract_module.rs
//   - move_mod_to_file.rs
// - extract_module and move_mod_to_file are both special cases of rename_move
//   - These should be updated to call into rename_move utils

pub(crate) fn rename_move(
    db: &RootDatabase,
    position: FilePosition,
    new_path: &str,
) -> Result<SourceChange> {
    let sema = Semantics::new(db);
    let file_id = sema
        .attach_first_edition(position.file_id)
        .ok_or_else(|| format_err!("No references found at position"))?;
    let source_file = sema.parse(file_id);
    let source_mod = sema
        .file_to_module_def(position.file_id)
        .ok_or_else(|| format_err!("Invalid source module"))?;
    let syntax = source_file.syntax();
    let edition = file_id.edition(db);

    let mut new_path = parse_move_target(new_path, &sema, edition)?;
    let new_name = new_path.segments().last().ok_or_else(|| format_err!("Invalid new path"))?;

    // TODO: Bail if definition target is defined within a macro
    // TODO: Probably do want to support multiple definitions if they are define in a proc macro, which will get moved alongside the main def
    let (_file_range, _syntax_kind, def, _new_name, _rename_def) =
        find_definitions(&sema, syntax, position, new_name)?.exactly_one().map_err(|_| {
            // Multiple found definitions indicates macro involvement, which is currently unsupported
            format_err!("Rename-move unsupported")
        })?;

    let source_change = match def {
        Definition::Module(_module) => {
            // TODO: Handle module moves
            bail!("Module moves not yet supported");
        }
        Definition::Adt(adt) => {
            let (new_name, dst_mod) = {
                let new_name = new_path
                    .pop_segment()
                    .ok_or_else(|| format_err!("Invalid ADT rename-move target"))?;
                let dst_mod = get_or_create_mod(&sema, &new_path, source_mod)
                    .ok_or_else(|| format_err!("Invalid destination module"))?;
                (new_name, dst_mod)
            };

            RenameMoveAdt::new(&sema, adt, new_name, source_mod, dst_mod)
                .into_source_change(&sema)
                .ok_or_else(|| format_err!("Failed to rename-move ADT"))?
        }
        _ => bail!("Rename-move unsupported"),
    };

    Ok(source_change)
}

fn get_or_create_mod(
    sema: &Semantics<'_, RootDatabase>,
    mod_path: &ModPath,
    anchor_mod: Module,
) -> Option<Module> {
    let root_mod = match mod_path.kind {
        hir::PathKind::Crate => anchor_mod.crate_root(sema.db),
        hir::PathKind::Super(n) => (0..n).fold(anchor_mod, |m, _| m.parent(sema.db).unwrap()),
        _ => {
            // TODO: Add error messages for unsupported path types
            return None;
        }
    };

    // TODO: Support creating non-existent module

    root_mod
        .resolve_mod_path(sema.db, mod_path.segments().iter().cloned())?
        .filter_map(|item| match item.into_module_def() {
            ModuleDef::Module(module) => Some(module),
            _ => None,
        })
        .last()
}

// TODO: Use alog::least_common_ancestor
fn get_mod_lca(sema: &Semantics<'_, RootDatabase>, mod_a: Module, mod_b: Module) -> Option<Module> {
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
    use ide_db::{
        RootDatabase,
        base_db::SourceDatabase,
        source_change::{FileSystemEdit, SourceChange},
    };
    use stdx::{format_to, trim_indent};
    use test_utils::assert_eq_text;

    use crate::{RenameConfig, fixture};

    const TEST_CONFIG: RenameConfig =
        RenameConfig { prefer_no_std: false, prefer_prelude: true, prefer_absolute: false };

    #[track_caller]
    pub(crate) fn check(
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
            .rename(position, new_name, &TEST_CONFIG)
            .unwrap_or_else(|err| panic!("Rename to '{new_name}' was cancelled: {err}"));

        // TODO: Try using check_source_change with file edit limit check
        match rename_result {
            Ok(source_change) => {
                check_source_change(analysis.db, source_change, ra_fixture_after);
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

    // TODO: Extract to ide-test-utils module
    fn check_source_change(db: RootDatabase, source_change: SourceChange, after: &str) {
        let skip_header = source_change.source_file_edits.len() == 1
            && source_change.file_system_edits.is_empty();

        let mut buf = String::new();
        for (file_id, (edit, snippet_edit)) in source_change.source_file_edits {
            let mut text = db.file_text(file_id).text(&db).as_ref().to_owned();
            edit.apply(&mut text);
            if let Some(snippet_edit) = snippet_edit {
                snippet_edit.apply(&mut text);
            }
            if !skip_header {
                let source_root_id = db.file_source_root(file_id).source_root_id(&db);
                let sr = db.source_root(source_root_id).source_root(&db);
                let path = sr.path_for_file(&file_id).unwrap();
                format_to!(buf, "//- {}\n", path)
            }
            dbg!(&text);
            buf.push_str(&text);
        }

        for file_system_edit in source_change.file_system_edits {
            let (dst, contents) = match file_system_edit {
                FileSystemEdit::CreateFile { dst, initial_contents } => (dst, initial_contents),
                FileSystemEdit::MoveFile { src, dst } => {
                    (dst, db.file_text(src).text(&db).as_ref().to_owned())
                }
                FileSystemEdit::MoveDir { src, src_id, dst } => {
                    // temporary placeholder for MoveDir since we are not using MoveDir in ide assists yet.
                    (dst, format!("{src_id:?}\n{src:?}"))
                }
            };

            let source_root_id = db.file_source_root(dst.anchor).source_root_id(&db);
            let sr = db.source_root(source_root_id).source_root(&db);
            let mut base = sr.path_for_file(&dst.anchor).unwrap().clone();
            base.pop();
            let created_file_path = base.join(&dst.path).unwrap();
            format_to!(buf, "//- {}\n", created_file_path);
            buf.push_str(&contents);
        }

        assert_eq_text!(after, &buf);
    }

    // TODO: Test coverage to add
    // - Regular & trait impl block tests
    //   - Trait impls in same file are moved
    //   - Multiple trait impls in same file are moved
    //   - Trait impls in same file but inline module aren't moved

    #[test]
    fn test_rename_move_to_existing_module() {
        check(
            "crate::bar::FooStruct",
            r#"
//- /main.rs
mod foo;
mod bar;
//- /foo.rs
struct $0FooStruct;
//- /bar.rs
struct BarStruct;
            "#,
            r#"
//- /foo.rs
//- /bar.rs
struct FooStruct;

struct BarStruct;
            "#,
        );
    }

    #[test]
    fn test_rename_move_to_existing_inline_module() {
        check(
            "crate::bar::FooStruct",
            r#"
//- /main.rs
mod foo;

mod bar {
    struct BarStruct;
}
//- /foo.rs
struct $0FooStruct;
            "#,
            r#"
//- /main.rs
mod foo;

mod bar {
    struct FooStruct;

    struct BarStruct;
}
//- /foo.rs
            "#,
        );
    }

    #[test]
    fn test_rename_move_to_existing_nested_inline_module() {
        todo!()
    }

    #[test]
    fn test_rename_move_from_inline_module() {
        todo!()
    }

    #[test]
    fn test_rename_move_from_nested_inline_module() {
        todo!()
    }

    #[test]
    fn test_simple_move_to_existing_module() {
        check(
            "crate::bar::FooStruct",
            r#"
//- /main.rs
mod foo;
mod bar;

pub trait MainTrait {
    fn main_trait_fn(&self) -> bool;
}
//- /foo.rs
use crate::MainTrate;

/// FooStruct doc
struct $0FooStruct;

impl FooStruct {
    fn method_in_first_impl(&self) -> i32 {
        0
    }
}

struct OtherFooStruct;

impl MainTrait for FooStruct {
    fn main_trait_fn(&self) -> bool {
        false
    }
}

impl MainTrait for OtherFooStruct {
    fn main_trait_fn(&self) -> bool {
        false
    }
}
//- /bar.rs
use std::string::String;
use std::vec::Vec;

struct BarStruct(Vec<String>);
            "#,
            r#"
//- /foo.rs
use crate::MainTrate;

struct OtherFooStruct;

impl MainTrait for OtherFooStruct {
    fn main_trait_fn(&self) -> bool {
        false
    }
}
//- /bar.rs
use std::string::String;
use std::vec::Vec;

/// FooStruct doc
struct FooStruct;

impl FooStruct {
    fn method_in_first_impl(&self) -> i32 {
        0
    }
}

impl MainTrait for FooStruct {
    fn main_trait_fn(&self) -> bool {
        false
    }
}

struct BarStruct(Vec<String>);
            "#,
        );
    }

    #[test]
    fn test_complex_move_to_new_module() {
        check(
            "crate::bar::BarStruct",
            r#"
//- /main.rs
use std::str::String;
use std::vec::Vec;

use crate::foo::{FooStruct, Other};

mod foo;

pub fn use_foo_struct(x: FooStruct) -> String {
    x.b
}

pub fn use_other_struct(y: OtherStruct) -> Vec<i32> {
    y.0
}
//- /foo.rs
use std::str::String;
use std::vec::Vec;

#[derive(Default)]
struct $0FooStruct {
    a: i32,
    b: String,
};

struct OtherStruct(Vec<i32>);
            "#,
            r#"
//- /main.rs
use std::str::String;
use std::vec::Vec;

use crate::bar::BarStruct;
use crate::foo::OtherStruct;

mod foo;
mod bar;

pub fn use_foo_struct(x: BarStruct) -> String {
    x.b
}

pub fn use_other_struct(y: OtherStruct) -> Vec<i32> {
    y.0
}
//- /foo.rs
use std::vec::Vec;

struct OtherStruct(Vec<i32>);
//- /bar.rs
use std::str::String;

#[derive(Default)]
struct BarStruct {
    a: i32,
    b: String,
};
            "#,
        );
    }
}
