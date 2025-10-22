//! Rename infrastructure for rust-analyzer. It is used primarily for the
//! literal "rename" in the ide (look for tests there), but it is also available
//! as a general-purpose service. For example, it is used by the fix for the
//! "incorrect case" diagnostic.
//!
//! It leverages the [`crate::search`] functionality to find what needs to be
//! renamed. The actual renames are tricky -- field shorthands need special
//! attention, and, when renaming modules, you also want to rename files on the
//! file system.
//!
//! Another can of worms are macros:
//!
//! ```ignore
//! macro_rules! m { () => { fn f() {} } }
//! m!();
//! fn main() {
//!     f() // <- rename me
//! }
//! ```
//!
//! The correct behavior in such cases is probably to show a dialog to the user.
//! Our current behavior is ¯\_(ツ)_/¯.
use std::fmt::{self, Display};

use crate::{
    source_change::ChangeAnnotation,
    text_edit::{TextEdit, TextEditBuilder},
};
use base_db::AnchoredPathBuf;
use either::Either;
use hir::{FieldSource, FileRange, InFile, Module, ModuleSource, Name, Semantics, sym, ModPath, PathKind, db::ExpandDatabase};
use span::{Edition, FileId, SyntaxContext};
use stdx::{TupleExt, never};
use syntax::{
    AstNode, SyntaxKind, T, TextRange,
    ast::{self, HasName, HasModuleItem},
};

use crate::{
    RootDatabase,
    defs::Definition,
    search::{FileReference, FileReferenceNode},
    source_change::{FileSystemEdit, SourceChange},
    syntax_helpers::node_ext::expr_as_name_ref,
    traits::convert_to_def_in_trait,
};

pub type Result<T, E = RenameError> = std::result::Result<T, E>;

#[derive(Debug)]
pub struct RenameError(pub String);

/// Context for rename operations, including move operations
#[derive(Debug)]
pub struct RenameContext {
    pub original_def: Definition,
    pub original_name: Name,
    pub target_module_path: ModPath,
    pub new_item_name: Name,
    pub operation_type: RenameOperationType,
    pub source_module: Module,
}

/// Types of rename operations supported
#[derive(Debug, PartialEq, Eq)]
pub enum RenameOperationType {
    /// Same module, name change only (Requirement 5.1)
    SimpleRename,
    /// Different module, name change (Requirement 1.2)
    MoveAndRename,
    /// Different module, same name (Requirement 1.2)
    MoveOnly,
    /// Relative path within current module (Requirement 5.2)
    RelativeMove,
}

/// Result of comparing current item path with target path
#[derive(Debug, PartialEq, Eq)]
pub enum PathComparison {
    /// Only name change (Requirement 5.1)
    SameLocation,
    /// Move required (Requirement 1.2)
    DifferentModule,
    /// Invalid path syntax (Requirement 1.3)
    Invalid,
}

impl fmt::Display for RenameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[macro_export]
macro_rules! _format_err {
    ($fmt:expr) => { RenameError(format!($fmt)) };
    ($fmt:expr, $($arg:tt)+) => { RenameError(format!($fmt, $($arg)+)) }
}
pub use _format_err as format_err;

#[macro_export]
macro_rules! _bail {
    ($($tokens:tt)*) => { return Err(format_err!($($tokens)*)) }
}
pub use _bail as bail;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RenameDefinition {
    Yes,
    No,
}

impl Definition {
    pub fn rename(
        &self,
        sema: &Semantics<'_, RootDatabase>,
        new_name: &str,
        rename_definition: RenameDefinition,
    ) -> Result<SourceChange> {
        // self.krate() returns None if
        // self is a built-in attr, built-in type or tool module.
        // it is not allowed for these defs to be renamed.
        // cases where self.krate() is None is handled below.
        let edition = if let Some(krate) = self.krate(sema.db) {
            // Can we not rename non-local items?
            // Then bail if non-local
            if !krate.origin(sema.db).is_local() {
                bail!("Cannot rename a non-local definition")
            }
            krate.edition(sema.db)
        } else {
            Edition::LATEST
        };

        match *self {
            Definition::Module(module) => rename_mod(sema, module, new_name),
            Definition::ToolModule(_) => {
                bail!("Cannot rename a tool module")
            }
            Definition::BuiltinType(_) => {
                bail!("Cannot rename builtin type")
            }
            Definition::BuiltinAttr(_) => {
                bail!("Cannot rename a builtin attr.")
            }
            Definition::SelfType(_) => bail!("Cannot rename `Self`"),
            Definition::Macro(mac) => {
                rename_reference(sema, Definition::Macro(mac), new_name, rename_definition, edition)
            }
            def => rename_reference(sema, def, new_name, rename_definition, edition),
        }
    }

    /// Textual range of the identifier which will change when renaming this
    /// `Definition`. Note that builtin types can't be
    /// renamed and extern crate names will report its range, though a rename will introduce
    /// an alias instead.
    pub fn range_for_rename(self, sema: &Semantics<'_, RootDatabase>) -> Option<FileRange> {
        let syn_ctx_is_root = |(range, ctx): (_, SyntaxContext)| ctx.is_root().then_some(range);
        let res = match self {
            Definition::Macro(mac) => {
                let src = sema.source(mac)?;
                let name = match &src.value {
                    Either::Left(it) => it.name()?,
                    Either::Right(it) => it.name()?,
                };
                src.with_value(name.syntax())
                    .original_file_range_opt(sema.db)
                    .and_then(syn_ctx_is_root)
            }
            Definition::Field(field) => {
                let src = sema.source(field)?;
                match &src.value {
                    FieldSource::Named(record_field) => {
                        let name = record_field.name()?;
                        src.with_value(name.syntax())
                            .original_file_range_opt(sema.db)
                            .and_then(syn_ctx_is_root)
                    }
                    FieldSource::Pos(_) => None,
                }
            }
            Definition::Crate(_) => None,
            Definition::Module(module) => {
                let src = module.declaration_source(sema.db)?;
                let name = src.value.name()?;
                src.with_value(name.syntax())
                    .original_file_range_opt(sema.db)
                    .and_then(syn_ctx_is_root)
            }
            Definition::Function(it) => name_range(it, sema).and_then(syn_ctx_is_root),
            Definition::Adt(adt) => match adt {
                hir::Adt::Struct(it) => name_range(it, sema).and_then(syn_ctx_is_root),
                hir::Adt::Union(it) => name_range(it, sema).and_then(syn_ctx_is_root),
                hir::Adt::Enum(it) => name_range(it, sema).and_then(syn_ctx_is_root),
            },
            Definition::Variant(it) => name_range(it, sema).and_then(syn_ctx_is_root),
            Definition::Const(it) => name_range(it, sema).and_then(syn_ctx_is_root),
            Definition::Static(it) => name_range(it, sema).and_then(syn_ctx_is_root),
            Definition::Trait(it) => name_range(it, sema).and_then(syn_ctx_is_root),
            Definition::TypeAlias(it) => name_range(it, sema).and_then(syn_ctx_is_root),
            Definition::Local(it) => {
                name_range(it.primary_source(sema.db), sema).and_then(syn_ctx_is_root)
            }
            Definition::GenericParam(generic_param) => match generic_param {
                hir::GenericParam::LifetimeParam(lifetime_param) => {
                    let src = sema.source(lifetime_param)?;
                    src.with_value(src.value.lifetime()?.syntax())
                        .original_file_range_opt(sema.db)
                        .and_then(syn_ctx_is_root)
                }
                _ => {
                    let param = match generic_param {
                        hir::GenericParam::TypeParam(it) => it.merge(),
                        hir::GenericParam::ConstParam(it) => it.merge(),
                        hir::GenericParam::LifetimeParam(_) => return None,
                    };
                    let src = sema.source(param)?;
                    let name = match &src.value {
                        Either::Left(x) => x.name()?,
                        Either::Right(_) => return None,
                    };
                    src.with_value(name.syntax())
                        .original_file_range_opt(sema.db)
                        .and_then(syn_ctx_is_root)
                }
            },
            Definition::Label(label) => {
                let src = sema.source(label)?;
                let lifetime = src.value.lifetime()?;
                src.with_value(lifetime.syntax())
                    .original_file_range_opt(sema.db)
                    .and_then(syn_ctx_is_root)
            }
            Definition::ExternCrateDecl(it) => {
                let src = sema.source(it)?;
                if let Some(rename) = src.value.rename() {
                    let name = rename.name()?;
                    src.with_value(name.syntax())
                        .original_file_range_opt(sema.db)
                        .and_then(syn_ctx_is_root)
                } else {
                    let name = src.value.name_ref()?;
                    src.with_value(name.syntax())
                        .original_file_range_opt(sema.db)
                        .and_then(syn_ctx_is_root)
                }
            }
            Definition::InlineAsmOperand(it) => name_range(it, sema).and_then(syn_ctx_is_root),
            Definition::BuiltinType(_)
            | Definition::BuiltinLifetime(_)
            | Definition::BuiltinAttr(_)
            | Definition::SelfType(_)
            | Definition::ToolModule(_)
            | Definition::TupleField(_)
            | Definition::InlineAsmRegOrRegClass(_) => return None,
            // FIXME: This should be doable in theory
            Definition::DeriveHelper(_) => return None,
        };
        return res;

        fn name_range<D>(
            def: D,
            sema: &Semantics<'_, RootDatabase>,
        ) -> Option<(FileRange, SyntaxContext)>
        where
            D: hir::HasSource,
            D::Ast: ast::HasName,
        {
            let src = sema.source(def)?;
            let name = src.value.name()?;
            src.with_value(name.syntax()).original_file_range_opt(sema.db)
        }
    }
}

fn rename_mod(
    sema: &Semantics<'_, RootDatabase>,
    module: hir::Module,
    new_name: &str,
) -> Result<SourceChange> {
    let mut source_change = SourceChange::default();

    if module.is_crate_root() {
        return Ok(source_change);
    }

    let InFile { file_id, value: def_source } = module.definition_source(sema.db);
    let edition = file_id.edition(sema.db);
    let (new_name, kind) = IdentifierKind::classify(edition, new_name)?;
    if kind != IdentifierKind::Ident {
        bail!(
            "Invalid name `{0}`: cannot rename module to {0}",
            new_name.display(sema.db, edition)
        );
    }
    if let ModuleSource::SourceFile(..) = def_source {
        let anchor = file_id.original_file(sema.db).file_id(sema.db);

        let is_mod_rs = module.is_mod_rs(sema.db);
        let has_detached_child = module.children(sema.db).any(|child| !child.is_inline(sema.db));

        // Module exists in a named file
        if !is_mod_rs {
            let path = format!("{}.rs", new_name.as_str());
            let dst = AnchoredPathBuf { anchor, path };
            source_change.push_file_system_edit(FileSystemEdit::MoveFile { src: anchor, dst })
        }

        // Rename the dir if:
        //  - Module source is in mod.rs
        //  - Module has submodules defined in separate files
        let dir_paths = match (is_mod_rs, has_detached_child, module.name(sema.db)) {
            // Go up one level since the anchor is inside the dir we're trying to rename
            (true, _, Some(mod_name)) => {
                Some((format!("../{}", mod_name.as_str()), format!("../{}", new_name.as_str())))
            }
            // The anchor is on the same level as target dir
            (false, true, Some(mod_name)) => {
                Some((mod_name.as_str().to_owned(), new_name.as_str().to_owned()))
            }
            _ => None,
        };

        if let Some((src, dst)) = dir_paths {
            let src = AnchoredPathBuf { anchor, path: src };
            let dst = AnchoredPathBuf { anchor, path: dst };
            source_change.push_file_system_edit(FileSystemEdit::MoveDir {
                src,
                src_id: anchor,
                dst,
            })
        }
    }

    if let Some(src) = module.declaration_source(sema.db) {
        let file_id = src.file_id.original_file(sema.db);
        match src.value.name() {
            Some(name) => {
                if let Some(file_range) = src
                    .with_value(name.syntax())
                    .original_file_range_opt(sema.db)
                    .map(TupleExt::head)
                {
                    let new_name = new_name.display(sema.db, edition).to_string();
                    source_change.insert_source_edit(
                        file_id.file_id(sema.db),
                        TextEdit::replace(file_range.range, new_name),
                    )
                };
            }
            _ => never!("Module source node is missing a name"),
        }
    }

    let def = Definition::Module(module);
    let usages = def.usages(sema).all();
    let ref_edits = usages.iter().map(|(file_id, references)| {
        let edition = file_id.edition(sema.db);
        (
            file_id.file_id(sema.db),
            source_edit_from_references(sema.db, references, def, &new_name, edition),
        )
    });
    source_change.extend(ref_edits);

    Ok(source_change)
}

fn rename_reference(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    new_name: &str,
    rename_definition: RenameDefinition,
    edition: Edition,
) -> Result<SourceChange> {
    let (mut new_name, ident_kind) = IdentifierKind::classify(edition, new_name)?;

    if matches!(
        def,
        Definition::GenericParam(hir::GenericParam::LifetimeParam(_)) | Definition::Label(_)
    ) {
        match ident_kind {
            IdentifierKind::Underscore => {
                bail!(
                    "Invalid name `{}`: not a lifetime identifier",
                    new_name.display(sema.db, edition)
                );
            }
            IdentifierKind::Ident => {
                new_name = Name::new_lifetime(&format!("'{}", new_name.as_str()))
            }
            IdentifierKind::Lifetime => (),
            IdentifierKind::LowercaseSelf => bail!(
                "Invalid name `{}`: not a lifetime identifier",
                new_name.display(sema.db, edition)
            ),
        }
    } else {
        match ident_kind {
            IdentifierKind::Lifetime => {
                cov_mark::hit!(rename_not_an_ident_ref);
                bail!("Invalid name `{}`: not an identifier", new_name.display(sema.db, edition));
            }
            IdentifierKind::Ident => cov_mark::hit!(rename_non_local),
            IdentifierKind::Underscore => (),
            IdentifierKind::LowercaseSelf => {
                bail!(
                    "Invalid name `{}`: cannot rename to `self`",
                    new_name.display(sema.db, edition)
                );
            }
        }
    }

    let def = convert_to_def_in_trait(sema.db, def);
    let usages = def.usages(sema).all();

    if !usages.is_empty() && ident_kind == IdentifierKind::Underscore {
        cov_mark::hit!(rename_underscore_multiple);
        bail!("Cannot rename reference to `_` as it is being referenced multiple times");
    }
    let mut source_change = SourceChange::default();
    source_change.extend(usages.iter().map(|(file_id, references)| {
        let edition = file_id.edition(sema.db);
        (
            file_id.file_id(sema.db),
            source_edit_from_references(sema.db, references, def, &new_name, edition),
        )
    }));
    if rename_definition == RenameDefinition::Yes {
        // This needs to come after the references edits, because we change the annotation of existing edits
        // if a conflict is detected.
        let (file_id, edit) = source_edit_from_def(sema, def, &new_name, &mut source_change)?;
        source_change.insert_source_edit(file_id, edit);
    }
    Ok(source_change)
}

pub fn source_edit_from_references(
    db: &RootDatabase,
    references: &[FileReference],
    def: Definition,
    new_name: &Name,
    edition: Edition,
) -> TextEdit {
    let name_display = new_name.display(db, edition);
    let mut edit = TextEdit::builder();
    // macros can cause multiple refs to occur for the same text range, so keep track of what we have edited so far
    let mut edited_ranges = Vec::new();
    for &FileReference { range, ref name, .. } in references {
        let name_range = name.text_range();
        let has_emitted_edit = match name {
            // if the ranges differ then the node is inside a macro call, we can't really attempt
            // to make special rewrites like shorthand syntax and such, so just rename the node in
            // the macro input
            FileReferenceNode::NameRef(name_ref) if name_range == range => {
                source_edit_from_name_ref(&mut edit, name_ref, &name_display, def)
            }
            FileReferenceNode::Name(name) if name_range == range => {
                source_edit_from_name(&mut edit, name, &name_display)
            }
            _ => false,
        };
        if !has_emitted_edit && !edited_ranges.contains(&range.start()) {
            edit.replace(range, name_display.to_string());
            edited_ranges.push(range.start());
        }
    }

    edit.finish()
}

fn source_edit_from_name(
    edit: &mut TextEditBuilder,
    name: &ast::Name,
    new_name: &dyn Display,
) -> bool {
    if ast::RecordPatField::for_field_name(name).is_some()
        && let Some(ident_pat) = name.syntax().parent().and_then(ast::IdentPat::cast)
    {
        cov_mark::hit!(rename_record_pat_field_name_split);
        // Foo { ref mut field } -> Foo { new_name: ref mut field }
        //      ^ insert `new_name: `

        // FIXME: instead of splitting the shorthand, recursively trigger a rename of the
        // other name https://github.com/rust-lang/rust-analyzer/issues/6547
        edit.insert(ident_pat.syntax().text_range().start(), format!("{new_name}: "));
        return true;
    }

    false
}

fn source_edit_from_name_ref(
    edit: &mut TextEditBuilder,
    name_ref: &ast::NameRef,
    new_name: &dyn Display,
    def: Definition,
) -> bool {
    if name_ref.super_token().is_some() {
        return true;
    }

    if let Some(record_field) = ast::RecordExprField::for_name_ref(name_ref) {
        let rcf_name_ref = record_field.name_ref();
        let rcf_expr = record_field.expr();
        match &(rcf_name_ref, rcf_expr.and_then(|it| expr_as_name_ref(&it))) {
            // field: init-expr, check if we can use a field init shorthand
            (Some(field_name), Some(init)) => {
                let new_name = new_name.to_string();
                if field_name == name_ref {
                    if init.text() == new_name {
                        cov_mark::hit!(test_rename_field_put_init_shorthand);
                        // Foo { field: local } -> Foo { local }
                        //       ^^^^^^^ delete this

                        // same names, we can use a shorthand here instead.
                        // we do not want to erase attributes hence this range start
                        let s = field_name.syntax().text_range().start();
                        let e = init.syntax().text_range().start();
                        edit.delete(TextRange::new(s, e));
                        return true;
                    }
                } else if init == name_ref && field_name.text() == new_name {
                    cov_mark::hit!(test_rename_local_put_init_shorthand);
                    // Foo { field: local } -> Foo { field }
                    //            ^^^^^^^ delete this

                    // same names, we can use a shorthand here instead.
                    // we do not want to erase attributes hence this range start
                    let s = field_name.syntax().text_range().end();
                    let e = init.syntax().text_range().end();
                    edit.delete(TextRange::new(s, e));
                    return true;
                }
            }
            // init shorthand
            (None, Some(_)) if matches!(def, Definition::Field(_)) => {
                cov_mark::hit!(test_rename_field_in_field_shorthand);
                // Foo { field } -> Foo { new_name: field }
                //       ^ insert `new_name: `
                let offset = name_ref.syntax().text_range().start();
                edit.insert(offset, format!("{new_name}: "));
                return true;
            }
            (None, Some(_)) if matches!(def, Definition::Local(_)) => {
                cov_mark::hit!(test_rename_local_in_field_shorthand);
                // Foo { field } -> Foo { field: new_name }
                //            ^ insert `: new_name`
                let offset = name_ref.syntax().text_range().end();
                edit.insert(offset, format!(": {new_name}"));
                return true;
            }
            _ => (),
        }
    } else if let Some(record_field) = ast::RecordPatField::for_field_name_ref(name_ref) {
        let rcf_name_ref = record_field.name_ref();
        let rcf_pat = record_field.pat();
        match (rcf_name_ref, rcf_pat) {
            // field: rename
            (Some(field_name), Some(ast::Pat::IdentPat(pat)))
                if field_name == *name_ref && pat.at_token().is_none() =>
            {
                // field name is being renamed
                if let Some(name) = pat.name() {
                    let new_name = new_name.to_string();
                    if name.text() == new_name {
                        cov_mark::hit!(test_rename_field_put_init_shorthand_pat);
                        // Foo { field: ref mut local } -> Foo { ref mut field }
                        //       ^^^^^^^ delete this
                        //                      ^^^^^ replace this with `field`

                        // same names, we can use a shorthand here instead/
                        // we do not want to erase attributes hence this range start
                        let s = field_name.syntax().text_range().start();
                        let e = pat.syntax().text_range().start();
                        edit.delete(TextRange::new(s, e));
                        edit.replace(name.syntax().text_range(), new_name);
                        return true;
                    }
                }
            }
            _ => (),
        }
    }
    false
}

fn source_edit_from_def(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    new_name: &Name,
    source_change: &mut SourceChange,
) -> Result<(FileId, TextEdit)> {
    let mut edit = TextEdit::builder();
    if let Definition::Local(local) = def {
        let mut file_id = None;

        let conflict_annotation = if !sema.rename_conflicts(&local, new_name).is_empty() {
            Some(
                source_change.insert_annotation(ChangeAnnotation {
                    label: "This rename will change the program's meaning".to_owned(),
                    needs_confirmation: true,
                    description: Some(
                        "Some variable(s) will shadow the renamed variable \
                        or be shadowed by it if the rename is performed"
                            .to_owned(),
                    ),
                }),
            )
        } else {
            None
        };

        for source in local.sources(sema.db) {
            let source = match source.source.clone().original_ast_node_rooted(sema.db) {
                Some(source) => source,
                None => match source
                    .source
                    .syntax()
                    .original_file_range_opt(sema.db)
                    .map(TupleExt::head)
                {
                    Some(FileRange { file_id: file_id2, range }) => {
                        file_id = Some(file_id2);
                        edit.replace(
                            range,
                            new_name.display(sema.db, file_id2.edition(sema.db)).to_string(),
                        );
                        continue;
                    }
                    None => {
                        bail!("Can't rename local that is defined in a macro declaration")
                    }
                },
            };
            file_id = Some(source.file_id);
            if let Either::Left(pat) = source.value {
                let name_range = pat.name().unwrap().syntax().text_range();
                // special cases required for renaming fields/locals in Record patterns
                if let Some(pat_field) = pat.syntax().parent().and_then(ast::RecordPatField::cast) {
                    if let Some(name_ref) = pat_field.name_ref() {
                        if new_name.as_str() == name_ref.text().as_str().trim_start_matches("r#")
                            && pat.at_token().is_none()
                        {
                            // Foo { field: ref mut local } -> Foo { ref mut field }
                            //       ^^^^^^ delete this
                            //                      ^^^^^ replace this with `field`
                            cov_mark::hit!(test_rename_local_put_init_shorthand_pat);
                            edit.delete(
                                name_ref
                                    .syntax()
                                    .text_range()
                                    .cover_offset(pat.syntax().text_range().start()),
                            );
                            edit.replace(name_range, name_ref.text().to_string());
                        } else {
                            // Foo { field: ref mut local @ local 2} -> Foo { field: ref mut new_name @ local2 }
                            // Foo { field: ref mut local } -> Foo { field: ref mut new_name }
                            //                      ^^^^^ replace this with `new_name`
                            edit.replace(
                                name_range,
                                new_name
                                    .display(sema.db, source.file_id.edition(sema.db))
                                    .to_string(),
                            );
                        }
                    } else {
                        // Foo { ref mut field } -> Foo { field: ref mut new_name }
                        //   original_ast_node_rootedd: `
                        //               ^^^^^ replace this with `new_name`
                        edit.insert(
                            pat.syntax().text_range().start(),
                            format!("{}: ", pat_field.field_name().unwrap()),
                        );
                        edit.replace(
                            name_range,
                            new_name.display(sema.db, source.file_id.edition(sema.db)).to_string(),
                        );
                    }
                } else {
                    edit.replace(
                        name_range,
                        new_name.display(sema.db, source.file_id.edition(sema.db)).to_string(),
                    );
                }
            }
        }
        let mut edit = edit.finish();

        for (edit, _) in source_change.source_file_edits.values_mut() {
            edit.set_annotation(conflict_annotation);
        }
        edit.set_annotation(conflict_annotation);

        let Some(file_id) = file_id else { bail!("No file available to rename") };
        return Ok((file_id.file_id(sema.db), edit));
    }
    let FileRange { file_id, range } = def
        .range_for_rename(sema)
        .ok_or_else(|| format_err!("No identifier available to rename"))?;
    let (range, new_name) = match def {
        Definition::ExternCrateDecl(decl) if decl.alias(sema.db).is_none() => (
            TextRange::empty(range.end()),
            format!(" as {}", new_name.display(sema.db, file_id.edition(sema.db)),),
        ),
        _ => (range, new_name.display(sema.db, file_id.edition(sema.db)).to_string()),
    };
    edit.replace(range, new_name);
    Ok((file_id.file_id(sema.db), edit.finish()))
}

/// Parse a rename target that may be a fully-qualified path
/// Leverages existing ModPath::from_src infrastructure (Requirement 1.1, 1.3)
pub fn parse_rename_target(
    input: &str,
    db: &dyn ExpandDatabase,
) -> Result<ModPath> {
    // Parse as AST path first to validate syntax (Requirement 1.3)
    let parsed = syntax::ast::SourceFile::parse(&format!("use {};", input), Edition::CURRENT);
    if !parsed.errors().is_empty() {
        return Err(RenameError(format!("Invalid path syntax: {}", input)));
    }
    
    // Extract path and convert using existing ModPath::from_src (Requirement 1.1)
    let use_stmt = parsed.tree().items().find_map(|item| match item {
        syntax::ast::Item::Use(use_stmt) => use_stmt.use_tree()?.path(),
        _ => None,
    }).ok_or_else(|| RenameError(format!("Could not extract path from: {}", input)))?;
    
    ModPath::from_src(db, use_stmt, &mut |_| SyntaxContext::root(Edition::CURRENT))
        .ok_or_else(|| RenameError(format!("Could not parse path: {}", input)))
}

/// Extract module path and item name from a fully-qualified path
/// (Requirements 1.4, 1.5)
pub fn parse_fully_qualified_path(
    path: &str,
    db: &dyn ExpandDatabase,
) -> Result<(ModPath, Name)> {
    // Parse using existing rust-analyzer infrastructure
    let mod_path = parse_rename_target(path, db)?;
    
    // Extract item name from final segment (Requirement 1.4)
    let item_name = mod_path.segments().last()
        .ok_or_else(|| RenameError("Path must contain at least one segment".to_string()))?
        .clone();
    
    // Create module path by removing final segment (Requirement 1.5)
    let mut module_segments = mod_path.segments().to_vec();
    module_segments.pop(); // Remove item name
    
    let module_path = if module_segments.is_empty() {
        // Item in crate root
        ModPath::from_kind(mod_path.kind)
    } else {
        ModPath::from_segments(mod_path.kind, module_segments)
    };
    
    Ok((module_path, item_name))
}

/// Compare current item's ModPath with target ModPath (Requirements 1.2, 5.1, 5.2, 5.3)
pub fn compare_paths(
    current_module: &ModPath,
    target_module: &ModPath,
) -> PathComparison {
    // Check if paths are identical (same module location)
    if current_module.kind == target_module.kind 
        && current_module.segments() == target_module.segments() {
        return PathComparison::SameLocation;
    }
    
    // Different module paths indicate a move operation is required
    PathComparison::DifferentModule
}

/// Determine the type of rename operation based on current and target paths
/// (Requirements 1.2, 5.1, 5.2, 5.3)
pub fn determine_operation_type(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: &ModPath,
    new_item_name: &Name,
) -> Result<RenameOperationType> {
    // Get current module of the definition
    let current_module = match def.module(sema.db) {
        Some(module) => module,
        None => return Err(RenameError("Cannot determine current module for definition".to_string())),
    };
    
    // Convert current module to ModPath for comparison
    let current_module_path = module_to_mod_path(sema, current_module)?;
    
    // Get current item name
    let current_name = def.name(sema.db)
        .ok_or_else(|| RenameError("Cannot determine current name for definition".to_string()))?;
    
    // Compare paths to determine operation type
    let path_comparison = compare_paths(&current_module_path, target_module_path);
    
    match path_comparison {
        PathComparison::SameLocation => {
            // Same module - check if name is changing (Requirement 5.1)
            if current_name == *new_item_name {
                // Same location, same name - no operation needed
                Ok(RenameOperationType::SimpleRename)
            } else {
                // Same location, different name - simple rename (Requirement 5.1)
                Ok(RenameOperationType::SimpleRename)
            }
        }
        PathComparison::DifferentModule => {
            // Different module - determine if name is also changing (Requirement 1.2)
            if current_name == *new_item_name {
                // Different module, same name - move only (Requirement 1.2)
                Ok(RenameOperationType::MoveOnly)
            } else {
                // Different module, different name - move and rename (Requirement 1.2)
                Ok(RenameOperationType::MoveAndRename)
            }
        }
        PathComparison::Invalid => {
            Err(RenameError("Invalid target path".to_string()))
        }
    }
}

/// Convert a Module to ModPath for comparison purposes
fn module_to_mod_path(
    sema: &Semantics<'_, RootDatabase>,
    module: Module,
) -> Result<ModPath> {
    let mut segments = Vec::new();
    let mut current = Some(module);
    
    // Walk up the module hierarchy to build the path
    while let Some(mod_) = current {
        if let Some(parent) = mod_.parent(sema.db) {
            if let Some(name) = mod_.name(sema.db) {
                segments.insert(0, name);
            }
            current = Some(parent);
        } else {
            // Reached crate root
            break;
        }
    }
    
    // Create ModPath with crate root kind
    if segments.is_empty() {
        Ok(ModPath::from_kind(PathKind::Crate))
    } else {
        Ok(ModPath::from_segments(PathKind::Crate, segments))
    }
}

/// Detect if a path specification is relative within the current module (Requirement 5.2)
pub fn is_relative_path_in_same_module(
    current_module_path: &ModPath,
    target_module_path: &ModPath,
) -> bool {
    // Check if target is a relative path within the same module
    // This handles cases like renaming "Item" to "submodule::Item" within the same parent module
    if target_module_path.kind == PathKind::Plain {
        // Plain paths are relative - check if they extend the current module
        let current_segments = current_module_path.segments();
        let target_segments = target_module_path.segments();
        
        if target_segments.len() > current_segments.len() {
            // Check if target starts with current module path
            return target_segments[..current_segments.len()] == *current_segments;
        }
    }
    
    false
}

/// Support both absolute (crate::) and relative path specifications (Requirements 5.2, 5.3)
pub fn normalize_target_path(
    current_module_path: &ModPath,
    target_path_str: &str,
    db: &dyn ExpandDatabase,
) -> Result<ModPath> {
    let parsed_path = parse_rename_target(target_path_str, db)?;
    
    match parsed_path.kind {
        PathKind::Crate | PathKind::Super(_) | PathKind::Abs => {
            // Absolute path - use as-is (Requirement 5.3)
            Ok(parsed_path)
        }
        PathKind::Plain => {
            // Relative path - resolve relative to current module (Requirement 5.2)
            let mut resolved_segments = current_module_path.segments().to_vec();
            resolved_segments.extend(parsed_path.segments().iter().cloned());
            
            Ok(ModPath::from_segments(current_module_path.kind, resolved_segments))
        }
        PathKind::DollarCrate(_) => {
            // Dollar crate paths are treated as absolute
            Ok(parsed_path)
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IdentifierKind {
    Ident,
    Lifetime,
    Underscore,
    LowercaseSelf,
}

impl IdentifierKind {
    pub fn classify(edition: Edition, new_name: &str) -> Result<(Name, IdentifierKind)> {
        match parser::LexedStr::single_token(edition, new_name) {
            Some(res) => match res {
                (SyntaxKind::IDENT, _) => Ok((Name::new_root(new_name), IdentifierKind::Ident)),
                (T![_], _) => {
                    Ok((Name::new_symbol_root(sym::underscore), IdentifierKind::Underscore))
                }
                (SyntaxKind::LIFETIME_IDENT, _) if new_name != "'static" && new_name != "'_" => {
                    Ok((Name::new_lifetime(new_name), IdentifierKind::Lifetime))
                }
                _ if SyntaxKind::from_keyword(new_name, edition).is_some() => match new_name {
                    "self" => Ok((Name::new_root(new_name), IdentifierKind::LowercaseSelf)),
                    "crate" | "super" | "Self" => {
                        bail!("Invalid name `{}`: cannot rename to a keyword", new_name)
                    }
                    _ => Ok((Name::new_root(new_name), IdentifierKind::Ident)),
                },
                (_, Some(syntax_error)) => bail!("Invalid name `{}`: {}", new_name, syntax_error),
                (_, None) => bail!("Invalid name `{}`: not an identifier", new_name),
            },
            None => bail!("Invalid name `{}`: not an identifier", new_name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hir::PathKind;
    use crate::RootDatabase;
    use test_fixture::WithFixture;

    #[test]
    fn test_parse_rename_target_simple() {
        let db = RootDatabase::with_files("");
        
        // Test simple identifier
        let result = parse_rename_target("foo", &db);
        assert!(result.is_ok());
        let path = result.unwrap();
        assert_eq!(path.kind, PathKind::Plain);
        assert_eq!(path.segments().len(), 1);
        assert_eq!(path.segments()[0].as_str(), "foo");
    }

    #[test]
    fn test_parse_rename_target_qualified() {
        let db = RootDatabase::with_files("");
        
        // Test fully-qualified path
        let result = parse_rename_target("crate::module::Item", &db);
        assert!(result.is_ok());
        let path = result.unwrap();
        assert_eq!(path.kind, PathKind::Crate);
        assert_eq!(path.segments().len(), 2);
        assert_eq!(path.segments()[0].as_str(), "module");
        assert_eq!(path.segments()[1].as_str(), "Item");
    }

    #[test]
    fn test_parse_fully_qualified_path() {
        let db = RootDatabase::with_files("");
        
        // Test extracting module path and item name
        let result = parse_fully_qualified_path("crate::module::submodule::Item", &db);
        assert!(result.is_ok());
        let (module_path, item_name) = result.unwrap();
        
        assert_eq!(module_path.kind, PathKind::Crate);
        assert_eq!(module_path.segments().len(), 2);
        assert_eq!(module_path.segments()[0].as_str(), "module");
        assert_eq!(module_path.segments()[1].as_str(), "submodule");
        assert_eq!(item_name.as_str(), "Item");
    }

    #[test]
    fn test_parse_fully_qualified_path_crate_root() {
        let db = RootDatabase::with_files("");
        
        // Test item in crate root
        let result = parse_fully_qualified_path("crate::Item", &db);
        assert!(result.is_ok());
        let (module_path, item_name) = result.unwrap();
        
        assert_eq!(module_path.kind, PathKind::Crate);
        assert_eq!(module_path.segments().len(), 0);
        assert_eq!(item_name.as_str(), "Item");
    }

    #[test]
    fn test_parse_invalid_path() {
        let db = RootDatabase::with_files("");
        
        // Test invalid syntax
        let result = parse_rename_target("invalid!@#", &db);
        assert!(result.is_err());
        assert!(result.unwrap_err().0.contains("Invalid path syntax"));
    }

    #[test]
    fn test_compare_paths_same_location() {
        // Test same module paths (Requirement 5.1)
        let path1 = ModPath::from_segments(PathKind::Crate, vec![Name::new_root("module")]);
        let path2 = ModPath::from_segments(PathKind::Crate, vec![Name::new_root("module")]);
        
        let result = compare_paths(&path1, &path2);
        assert_eq!(result, PathComparison::SameLocation);
    }

    #[test]
    fn test_compare_paths_different_module() {
        // Test different module paths (Requirement 1.2)
        let path1 = ModPath::from_segments(PathKind::Crate, vec![Name::new_root("module1")]);
        let path2 = ModPath::from_segments(PathKind::Crate, vec![Name::new_root("module2")]);
        
        let result = compare_paths(&path1, &path2);
        assert_eq!(result, PathComparison::DifferentModule);
    }

    #[test]
    fn test_compare_paths_crate_root() {
        // Test crate root paths
        let path1 = ModPath::from_kind(PathKind::Crate);
        let path2 = ModPath::from_kind(PathKind::Crate);
        
        let result = compare_paths(&path1, &path2);
        assert_eq!(result, PathComparison::SameLocation);
    }

    #[test]
    fn test_compare_paths_nested_modules() {
        // Test nested module paths
        let path1 = ModPath::from_segments(PathKind::Crate, vec![
            Name::new_root("module"), 
            Name::new_root("submodule")
        ]);
        let path2 = ModPath::from_segments(PathKind::Crate, vec![
            Name::new_root("module"), 
            Name::new_root("other")
        ]);
        
        let result = compare_paths(&path1, &path2);
        assert_eq!(result, PathComparison::DifferentModule);
    }

    #[test]
    fn test_is_relative_path_in_same_module() {
        // Test relative path detection (Requirement 5.2)
        let current = ModPath::from_segments(PathKind::Crate, vec![Name::new_root("module")]);
        let target = ModPath::from_segments(PathKind::Plain, vec![
            Name::new_root("module"), 
            Name::new_root("submodule")
        ]);
        
        let result = is_relative_path_in_same_module(&current, &target);
        assert!(result);
    }

    #[test]
    fn test_is_not_relative_path_different_module() {
        // Test non-relative path detection
        let current = ModPath::from_segments(PathKind::Crate, vec![Name::new_root("module1")]);
        let target = ModPath::from_segments(PathKind::Plain, vec![Name::new_root("module2")]);
        
        let result = is_relative_path_in_same_module(&current, &target);
        assert!(!result);
    }

    #[test]
    fn test_normalize_target_path_absolute() {
        let db = RootDatabase::with_files("");
        let current = ModPath::from_segments(PathKind::Crate, vec![Name::new_root("current")]);
        
        // Test absolute path normalization (Requirement 5.3)
        let result = normalize_target_path(&current, "crate::target::Item", &db);
        assert!(result.is_ok());
        let normalized = result.unwrap();
        assert_eq!(normalized.kind, PathKind::Crate);
        assert_eq!(normalized.segments().len(), 2);
        assert_eq!(normalized.segments()[0].as_str(), "target");
        assert_eq!(normalized.segments()[1].as_str(), "Item");
    }

    #[test]
    fn test_normalize_target_path_relative() {
        let db = RootDatabase::with_files("");
        let current = ModPath::from_segments(PathKind::Crate, vec![Name::new_root("current")]);
        
        // Test relative path normalization (Requirement 5.2)
        let result = normalize_target_path(&current, "submodule::Item", &db);
        assert!(result.is_ok());
        let normalized = result.unwrap();
        assert_eq!(normalized.kind, PathKind::Crate);
        assert_eq!(normalized.segments().len(), 3);
        assert_eq!(normalized.segments()[0].as_str(), "current");
        assert_eq!(normalized.segments()[1].as_str(), "submodule");
        assert_eq!(normalized.segments()[2].as_str(), "Item");
    }
}
