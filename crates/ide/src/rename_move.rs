use hir::ModPath;
use hir::Name;
use hir::Semantics;
use hir::db::ExpandDatabase;
use ide_db::FileRange;
use ide_db::defs::Definition;
use ide_db::rename::RenameDefinition;
use ide_db::rename::RenameError;
use ide_db::rename::bail;
use ide_db::rename::format_err;
use ide_db::{FilePosition, RootDatabase, source_change::SourceChange};
use itertools::Itertools;
use span::Edition;
use syntax::AstNode;
use syntax::SourceFile;
use syntax::SyntaxKind;
use syntax::SyntaxNode;
use syntax::ast;
use syntax::ast::HasModuleItem;

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
    let syntax = source_file.syntax();
    let edition = file_id.edition(db);

    let new_path = parse_move_target(new_path, &sema, edition)?;
    let def = find_definitions(&sema, syntax, position, new_path.segments().last().unwrap())?
        .exactly_one()
        .map_err(|_| {
            // Multiple found definitions indicates macro involvement, which is currently unsupported
            format_err!("Rename-move unsupported")
        })?;

    // RETURN

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
    use ide_db::RootDatabase;
    use span::Edition;

    use crate::rename_move::{parse_mod_path, parse_move_target};

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
