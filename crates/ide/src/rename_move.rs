use hir::HasSource;
use hir::ModPath;
use hir::Module;
use hir::Semantics;
use hir::db::ExpandDatabase;
use ide_db::defs::Definition;
use ide_db::rename::RenameError;
use ide_db::rename::bail;
use ide_db::rename::format_err;
use ide_db::rename_move::RenameMoveAdt;
use ide_db::rename_move::RenameMoveConfig;
use ide_db::rename_move::RenameMoveDefinition;
use ide_db::{FilePosition, RootDatabase, source_change::SourceChange};
use itertools::Itertools;
use span::Edition;
use syntax::AstNode;
use syntax::SourceFile;
use syntax::ast;
use syntax::ast::HasModuleItem;
use syntax::ast::Visibility;
use syntax::ast::make;

use crate::rename::find_definitions;

pub(crate) type Result<T> = crate::rename::RenameResult<T>;

pub(crate) fn rename_move(
    db: &RootDatabase,
    position: FilePosition,
    new_path: &str,
) -> Result<SourceChange> {
    let sema = Semantics::new(db);
    let file_id = sema
        .attach_first_edition(position.file_id)
        .ok_or_else(|| format_err!("No references found at position"))?;
    let origin_file = sema.parse(file_id);
    let syntax = origin_file.syntax();
    let edition = file_id.edition(db);

    let new_path_str = new_path;
    let mut new_path = parse_move_target(new_path_str, &sema, edition)?;
    let new_name = new_path.segments().last().ok_or_else(|| format_err!("Invalid new path"))?;

    // TODO: Bail if definition target is defined within a macro
    // TODO: Probably do want to support multiple definitions if they are define in a proc macro, which will get moved alongside the main def
    let (_file_range, _syntax_kind, def, _new_name, _rename_def) =
        find_definitions(&sema, syntax, position, new_name)?.exactly_one().map_err(|_| {
            // Multiple found definitions indicates macro involvement, which is currently unsupported
            format_err!("Rename-move unsupported")
        })?;

    let (new_name, origin_mod, target_mod) = match def {
        Definition::Module(_module) => {
            // TODO: Handle module moves
            bail!("Module moves not yet supported");
        }
        Definition::Adt(adt) => {
            let origin_mod = adt.module(sema.db);
            let new_name = new_path
                .pop_segment()
                .ok_or_else(|| format_err!("Invalid ADT rename-move target"))?;
            let target_mod = get_or_create_mod(&sema, origin_mod, new_path)
                .ok_or_else(|| format_err!("Invalid destination module"))?;
            (new_name, origin_mod, target_mod)
        }
        _ => bail!("Rename-move unsupported"),
    };

    let config = RenameMoveConfig {
        sema: &sema,
        new_name,
        origin_mod,
        target_mod,
        target_path: new_path_str,
        required_vis: get_required_vis(&sema, origin_mod, target_mod),
        include_impls: true,
    };

    RenameMoveDefinition::new(def)
        .ok_or_else(|| format_err!("Failed to compile"))?
        .rename_move(config)
        .ok_or_else(|| format_err!("Rename move failed"))
}

fn get_or_create_mod(
    sema: &Semantics<'_, RootDatabase>,
    anchor_mod: Module,
    mod_path: ModPath,
) -> Option<Module> {
    let ResolvedModPath { mut resolved, unresolved } =
        ResolvedModPath::new(sema.db, anchor_mod, mod_path)?;

    if unresolved.is_empty() {
        return resolved.pop();
    }

    todo!("Support non-existing modules");
}

fn get_required_vis(
    sema: &Semantics<'_, RootDatabase>,
    origin_mod: Module,
    target_mod: Module,
) -> Option<Visibility> {
    if origin_mod.ancestors(sema.db).find(|ancestor| ancestor == &target_mod).is_some() {
        // Target is ancestor
        return None;
    }

    if origin_mod
        .parent(sema.db)
        .into_iter()
        .flat_map(|parent| parent.children(sema.db))
        .find(|sibling| sibling == &target_mod)
        .is_some()
    {
        // Target is sibling
        return None;
    }

    Some(make::visibility_pub_crate().clone_for_update())
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

// TODO: Add visibility
#[derive(Default, Debug)]
struct ResolvedModPath {
    resolved: Vec<Module>,
    unresolved: Vec<hir::Name>,
}

impl ResolvedModPath {
    fn new(db: &RootDatabase, anchor_mod: Module, mod_path: ModPath) -> Option<Self> {
        let mut cur_mod = match mod_path.kind {
            hir::PathKind::Crate => anchor_mod.crate_root(db),
            hir::PathKind::Super(n) => (0..n).fold(anchor_mod, |m, _| m.parent(db).unwrap()),
            _ => {
                // TODO: Add error messages for unsupported path types
                return None;
            }
        };

        let mut segments = mod_path.into_segments();
        let mut resolved = vec![cur_mod];
        let mut unresolved = vec![];
        while let Some(segment) = segments.next() {
            if let Some(next_mod) = cur_mod.child(db, &segment) {
                resolved.push(next_mod);
                cur_mod = next_mod;
            } else {
                unresolved.push(segment);
                unresolved.extend(segments);
                break;
            }
        }

        Some(Self { resolved, unresolved })
    }
}

#[cfg(test)]
mod tests {
    use ide_db::{
        RootDatabase,
        base_db::SourceDatabase,
        source_change::{FileSystemEdit, SourceChange},
    };
    use span::Edition;
    use stdx::{format_to, trim_indent};
    use syntax::SourceFile;
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

    // TODO: Remove
    #[test]
    #[allow(clippy::dbg_macro)]
    fn test_print() {
        let sf = SourceFile::parse(
            r#"
use crate::foo::{Foo, Bar};
"#,
            Edition::LATEST,
        )
        .tree();

        dbg!(sf);
    }

    // TODO: Remove
    #[test]
    fn test_sandbox() {
        check(
            "crate::fizz::FizzStruct",
            r#"
mod foo {
    pub(crate) struct $0FooStruct;
    pub(crate) struct FooOtherStruct;
}

mod fizz {}

mod bar {
    use crate::foo::FooStruct;
    fn use_foo(f: FooStruct) -> FooStruct { f }
}

mod beta {
    use crate::foo::{FooStruct, FooOtherStruct};
    fn use_foo(f: (FooStruct, FooOtherStruct)) -> (FooStruct, FooOtherStruct) { f }
}
"#,
            r#"
mod foo {
    pub(crate) struct FooOtherStruct;
}

mod fizz {
    pub(crate) struct FizzStruct;
}

mod bar {
    use crate::fizz::FizzStruct;
    fn use_foo(f: FizzStruct) -> FizzStruct { f }
}

mod beta {
    use crate::fizz::FizzStruct;
    use crate::foo::FooOtherStruct;
    fn use_foo(f: (FizzStruct, FooOtherStruct)) -> (FizzStruct, FooOtherStruct) { f }
}
"#,
        );
    }

    // TODO: Alias checks
    // TODO: Nested import
    // TODO: Variant glob import
    // TODO: Variant normal import
    #[test]
    fn test_rename_move_external_refs_updated_properly() {
        check(
            "crate::target::TargetStruct",
            r#"
//- /main.rs
mod origin;
mod target;
mod crate_import;
mod super_import;
mod glob_import;
mod mod_qualified_ref;
mod mod_qualified_ref_from_glob;
mod crate_qualified_ref;
mod super_qualified_ref;
mod nested_super_qualified_ref;
//- /origin.rs
pub(crate) mod inner {
    pub(crate) struct $0OriginStruct;
}
//- /target.rs
//- /crate_import.rs
use crate::origin::inner::OriginStruct;
struct Inner(OriginStruct);
//- /super_import.rs
use super::origin::inner::OriginStruct;
struct Inner(OriginStruct);
//- /glob_import.rs
use crate::origin::inner::*;
struct Inner(OriginStruct);
//- /mod_qualified_ref.rs
use super::origin::inner;
struct Inner(inner::OriginStruct);
//- /mod_qualified_ref_from_glob.rs
use super::origin::*;
struct Inner(inner::OriginStruct);
//- /crate_qualified_ref.rs
struct Inner(crate::origin::inner::OriginStruct);
//- /super_qualified_ref.rs
struct Inner(super::origin::inner::OriginStruct);
//- /nested_super_qualified_ref.rs
use crate::origin::inner::OriginStruct;
mod inner {
    struct Inner(super::OriginStruct);
}
"#,
            r#"
//- /origin.rs
pub(crate) mod inner {}
//- /target.rs
pub(crate) struct TargetStruct;
//- /crate_import.rs
use crate::target::TargetStruct;
struct Inner(TargetStruct);
//- /super_import.rs
use crate::target::TargetStruct;

struct Inner(TargetStruct);
//- /glob_import.rs
use crate::origin::inner::*;
use crate::target::TargetStruct;
struct Inner(TargetStruct);
//- /mod_qualified_ref.rs
use super::origin::inner;
struct Inner(crate::target::TargetStruct);
//- /mod_qualified_ref_from_glob.rs
use super::origin::*;
struct Inner(crate::target::TargetStruct);
//- /crate_qualified_ref.rs
struct Inner(crate::target::TargetStruct);
//- /super_qualified_ref.rs
struct Inner(crate::target::TargetStruct);
//- /nested_super_qualified_ref.rs
use crate::target::TargetStruct;
mod inner {
    struct Inner(crate::target::TargetStruct);
}
"#,
        )
    }

    // TODO: Fix spacing on use statements
    #[test]
    fn test_rename_move_simple_ref_update() {
        check(
            "crate::fizz::FizzStruct",
            r#"
mod foo {
    pub(crate) struct $0FooStruct;
}

mod fizz {}

mod bar {
    use crate::foo::FooStruct;

    fn use_foo(f: FooStruct) -> FooStruct { f }
}
"#,
            r#"
mod foo {}

mod fizz {
    pub(crate) struct FizzStruct;
}

mod bar {
    use crate::fizz::FizzStruct;

    fn use_foo(f: FizzStruct) -> FizzStruct { f }
}
"#,
        );
    }

    // TODO: Conflict detection in usage file
    // TODO: Fix spacing on use statements
    #[test]
    fn test_rename_move_simple_usage_updates() {
        check(
            "crate::fizz::FizzStruct",
            r#"
mod foo {
    pub(crate) struct $0FooStruct;
    pub(crate) struct FooOtherStruct;
}

mod fizz {}

mod bar {
    use crate::foo::FooStruct;
    fn use_foo(f: FooStruct) -> FooStruct { f }
}

mod beta {
    use crate::foo::{FooStruct, FooOtherStruct};
    fn use_foo(f: (FooStruct, FooOtherStruct)) -> (FooStruct, FooOtherStruct) { f }
}

mod gamma {
    use super::foo::FooStruct;
    fn use_foo(f: FooStruct) -> FooStruct { f }
}

mod delta {
    use super::foo::{FooStruct, FooOtherStruct};
    fn use_foo(f: (FooStruct, FooOtherStruct)) -> (FooStruct, FooOtherStruct) { f }
}

mod episilon {
    fn use_foo(f: crate::foo::FooStruct) -> super::foo::FooStruct { f }
}
"#,
            r#"
mod foo {
    pub(crate) struct FooOtherStruct;
}

mod fizz {
    pub(crate) struct FizzStruct;
}

mod bar {
    use crate::fizz::FizzStruct;
    fn use_foo(f: FizzStruct) -> FizzStruct { f }
}

mod beta {
    use crate::fizz::FizzStruct;
    use crate::foo::FooOtherStruct;
    fn use_foo(f: (FizzStruct, FooOtherStruct)) -> (FizzStruct, FooOtherStruct) { f }
}

mod gamma {
    use crate::fizz::FizzStruct;

    fn use_foo(f: FizzStruct) -> FizzStruct { f }
}

mod delta {
    use crate::fizz::FizzStruct;

    use super::foo::FooOtherStruct;
    fn use_foo(f: (FizzStruct, FooOtherStruct)) -> (FizzStruct, FooOtherStruct) { f }
}

mod episilon {
    fn use_foo(f: crate::fizz::FizzStruct) -> crate::fizz::FizzStruct { f }
}
"#,
        );
    }

    #[test]
    #[ignore]
    fn test_rename_move_struct_update_refs() {
        check(
            "crate::fizz::FizzStruct",
            r#"
//- /main.rs
mod foo;
mod fizz;
mod bar;
mod baz;
//- /foo.rs
pub(crate) const FIZZ_CONST: usize = 0;

pub(crate) struct $0FooStruct;

fn use_foo(f: FooStruct) -> FooStruct {
    f
}
//- /fizz.rs
use crate::foo::FooStruct;

pub(crate) enum FizzEnum {
    A(FooStruct)
}
//- /bar.rs
use crate::foo::FooStruct;

fn use_foo(f: FooStruct) -> FooStruct {
    f
}

mod inner_rel {
    use super::FooStruct;
    use crate::foo::FOO_CONST;

    fn use_foo_inner(f: FooStruct) -> (FooStruct, usize) {
        (f, FOO_CONST)
    }
}

mod inner_abs {
    use crate::foo::{FooStruct, FOO_CONST};

    fn use_foo_inner(f: FooStruct) -> (FooStruct, usize) {
        (f, FOO_CONST)
    }
}
//- /baz.rs
fn use_foo_qualfied(f: crate::foo::FooStruct) -> crate::foo::FooStruct {
    f
}
"#,
            r#"
//- /foo.rs
use crate::fizz::FizzStruct;

pub(crate) const FIZZ_CONST: usize = 0;

fn use_foo(f: FizzStruct) -> FizzStruct {
    f
}
//- /fizz.rs
pub(crate) struct FizzStruct;

pub(crate) enum FizzEnum {
    A(FizzStruct)
}
//- /bar.rs
use crate::fizz::FizzStruct;

fn use_foo(f: FizzStruct) -> FizzStruct {
    f
}

mod inner_rel {
    use super::FizzStruct;
    use crate::foo::FOO_CONST;

    fn use_foo_inner(f: FizzStruct) -> (FizzStruct, usize) {
        (f, FOO_CONST)
    }
}

mod inner_abs {
    use crate::foo::FOO_CONST;
    use crate::fizz::FizzStruct;

    fn use_foo_inner(f: FizzStruct) -> (FizzStruct, usize) {
        (f, FOO_CONST)
    }
}
//- /baz.rs
fn use_foo_qualfied(f: crate::fizz::FizzStruct) -> crate::fizz::FizzStruct {
    f
}
"#,
        );
    }

    // TODO: Test coverage to add
    // - Regular & trait impl block tests
    //   - Trait impls in same file are moved
    //   - Multiple trait impls in same file are moved
    //   - Trait impls in same file but inline module aren't moved

    // TODO: Support multiple inputs defining the same rename
    // - E.g. super based stuff
    #[test]
    fn test_rename_move_to_existing_module() {
        check(
            "crate::bar::FooStruct",
            r#"
//- /main.rs
mod foo;
mod bar;
//- /foo.rs
struct $0FooStruct {
    x: i32,
}
//- /bar.rs
use std::string::String;

pub const BAR_CONST: usize = 0;

struct BarStruct(String);
"#,
            r#"
//- /foo.rs
//- /bar.rs
use std::string::String;

pub const BAR_CONST: usize = 0;

struct FooStruct {
    x: i32,
}

struct BarStruct(String);
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
    use std::string::String;

    pub const BAR_CONST: usize = 0;

    struct BarStruct(String);
}
//- /foo.rs
struct $0FooStruct {
    x: i32,
}
"#,
            r#"
//- /main.rs
mod foo;

mod bar {
    use std::string::String;

    pub const BAR_CONST: usize = 0;

    struct FooStruct {
        x: i32,
    }

    struct BarStruct(String);
}
//- /foo.rs
"#,
        );
    }

    #[test]
    fn test_rename_move_to_existing_nested_inline_module() {
        check(
            "crate::bar::baz::FooStruct",
            r#"
//- /main.rs
mod foo;

mod bar {
    mod baz {
        use std::string::String;

        pub const BAR_CONST: usize = 0;

        struct BarStruct(String);
    }
}
//- /foo.rs
struct $0FooStruct {
    x: i32,
}
"#,
            r#"
//- /main.rs
mod foo;

mod bar {
    mod baz {
        use std::string::String;

        pub const BAR_CONST: usize = 0;

        pub(crate) struct FooStruct {
            x: i32,
        }

        struct BarStruct(String);
    }
}
//- /foo.rs
"#,
        );
    }

    #[test]
    // TODO: Fix whitespace issue after deletion
    // TODO: Maybe auto-delete empty modules after move?
    // - Or maybe only auto-delete empty inline modules?
    fn test_rename_move_from_inline_module() {
        check(
            "crate::bar::FooStruct",
            r#"
//- /main.rs
mod bar;

mod foo {
    struct $0FooStruct {
        x: i32,
    }
}
//- /bar.rs
use std::string::String;

pub const BAR_CONST: usize = 0;

struct BarStruct(String);
"#,
            r#"
//- /main.rs
mod bar;

mod foo {}
//- /bar.rs
use std::string::String;

pub const BAR_CONST: usize = 0;

struct FooStruct {
    x: i32,
}

struct BarStruct(String);
"#,
        );
    }

    #[test]
    // TODO: Fix whitespace issue
    fn test_rename_move_from_nested_inline_module() {
        check(
            "crate::bar::FooStruct",
            r#"
//- /main.rs
mod bar;

mod foo {
    mod fizz {
        struct $0FooStruct {
            x: i32,
        }
    }
}
//- /bar.rs
use std::string::String;

pub const BAR_CONST: usize = 0;

struct BarStruct(String);
"#,
            r#"
//- /main.rs
mod bar;

mod foo {
    mod fizz {}
}
//- /bar.rs
use std::string::String;

pub const BAR_CONST: usize = 0;

pub(crate) struct FooStruct {
    x: i32,
}

struct BarStruct(String);
"#,
        );
    }

    #[test]
    fn test_rename_move_to_existing_module_with_impls() {
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
    #[ignore]
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
