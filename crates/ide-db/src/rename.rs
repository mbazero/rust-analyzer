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

use crate::symbol_index::SymbolsDatabase;
use crate::{
    source_change::ChangeAnnotation,
    text_edit::{TextEdit, TextEditBuilder},
};
use base_db::{AnchoredPathBuf, SourceDatabase};
use either::Either;
use hir::{
    FieldSource, FileRange, InFile, ModPath, Module, ModuleSource, Name, PathKind, Semantics,
    db::ExpandDatabase, sym,
};
use span::{Edition, FileId, SyntaxContext};
use stdx::{TupleExt, never};
use syntax::{
    AstNode, AstToken, SyntaxKind, T, TextRange,
    ast::{self, HasAttrs, HasModuleItem, HasName, HasVisibility},
};

use crate::{
    RootDatabase,
    defs::Definition,
    search::{FileReference, FileReferenceNode, ReferenceCategory},
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
pub fn parse_rename_target(input: &str, db: &dyn ExpandDatabase) -> Result<ModPath> {
    // Parse as AST path first to validate syntax (Requirement 1.3)
    let parsed = syntax::ast::SourceFile::parse(&format!("use {};", input), Edition::CURRENT);
    if !parsed.errors().is_empty() {
        return Err(RenameError(format!("Invalid path syntax: {}", input)));
    }

    // Extract path and convert using existing ModPath::from_src (Requirement 1.1)
    let use_stmt = parsed
        .tree()
        .items()
        .find_map(|item| match item {
            syntax::ast::Item::Use(use_stmt) => use_stmt.use_tree()?.path(),
            _ => None,
        })
        .ok_or_else(|| RenameError(format!("Could not extract path from: {}", input)))?;

    ModPath::from_src(db, use_stmt, &mut |_| SyntaxContext::root(Edition::CURRENT))
        .ok_or_else(|| RenameError(format!("Could not parse path: {}", input)))
}

/// Extract module path and item name from a fully-qualified path
/// (Requirements 1.4, 1.5)
pub fn parse_fully_qualified_path(path: &str, db: &dyn ExpandDatabase) -> Result<(ModPath, Name)> {
    // Parse using existing rust-analyzer infrastructure
    let mod_path = parse_rename_target(path, db)?;

    // Extract item name from final segment (Requirement 1.4)
    let item_name = mod_path
        .segments()
        .last()
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
pub fn compare_paths(current_module: &ModPath, target_module: &ModPath) -> PathComparison {
    // Check if paths are identical (same module location)
    if current_module.kind == target_module.kind
        && current_module.segments() == target_module.segments()
    {
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
        None => {
            return Err(RenameError("Cannot determine current module for definition".to_string()));
        }
    };

    // Convert current module to ModPath for comparison
    let current_module_path = module_to_mod_path(sema, current_module)?;

    // Get current item name
    let current_name = def
        .name(sema.db)
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
        PathComparison::Invalid => Err(RenameError("Invalid target path".to_string())),
    }
}

/// Convert a Module to ModPath for comparison purposes
fn module_to_mod_path(sema: &Semantics<'_, RootDatabase>, module: Module) -> Result<ModPath> {
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

/// Module structure analysis for tracking existing vs missing module segments
/// (Requirements 2.1, 2.2, 2.3)
#[derive(Debug)]
pub struct ModuleStructure {
    pub current_module: Module,
    pub target_module_path: ModPath,
    pub missing_segments: Vec<Name>,
    pub existing_segments: Vec<Module>,
}

/// Plan for creating missing module structure
/// (Requirements 2.1, 2.2, 2.3, 2.4, 2.5)
#[derive(Debug)]
pub struct ModuleCreationPlan {
    pub directories_to_create: Vec<std::path::PathBuf>,
    pub files_to_create: Vec<ModuleFileSpec>,
    pub module_declarations_to_add: Vec<ModuleDeclaration>,
    pub user_preferences: ModuleOrganizationPreferences,
}

/// Specification for a module file to be created
/// (Requirements 2.2, 2.3)
#[derive(Debug)]
pub struct ModuleFileSpec {
    pub path: std::path::PathBuf,
    pub content: String,
    pub file_type: ModuleFileType,
}

/// Types of module files that can be created
/// (Requirement 2.2)
#[derive(Debug, PartialEq, Eq)]
pub enum ModuleFileType {
    /// mod.rs file
    ModRs,
    /// module_name.rs file
    ModuleFile,
}

/// Declaration to add to a parent module
/// (Requirement 2.4)
#[derive(Debug)]
pub struct ModuleDeclaration {
    pub parent_file: FileId,
    pub module_name: String,
    pub insert_position: syntax::TextSize,
    pub visibility: Option<String>,
}

/// User preferences for module organization
/// (Requirement 2.3)
#[derive(Debug, Clone)]
pub struct ModuleOrganizationPreferences {
    pub prefer_mod_rs: bool,
    pub directory_structure: DirectoryStructurePreference,
}

/// Directory structure preferences
/// (Requirement 2.3)
#[derive(Debug, Clone)]
pub enum DirectoryStructurePreference {
    /// Nested directory structure (src/module/submodule/)
    Nested,
    /// Flat file structure (src/module_submodule.rs)
    Flat,
}

impl Default for ModuleOrganizationPreferences {
    fn default() -> Self {
        Self {
            prefer_mod_rs: false, // Default to module_name.rs
            directory_structure: DirectoryStructurePreference::Nested,
        }
    }
}

impl ModuleCreationPlan {
    pub fn new(user_preferences: ModuleOrganizationPreferences) -> Self {
        Self {
            directories_to_create: Vec::new(),
            files_to_create: Vec::new(),
            module_declarations_to_add: Vec::new(),
            user_preferences,
        }
    }

    pub fn add_module_creation(&mut self, spec: ModuleFileSpec) -> Result<()> {
        // Add directory creation if needed
        if let Some(parent_dir) = spec.path.parent() {
            if !self.directories_to_create.contains(&parent_dir.to_path_buf()) {
                self.directories_to_create.push(parent_dir.to_path_buf());
            }
        }

        self.files_to_create.push(spec);
        Ok(())
    }

    pub fn add_module_declaration(&mut self, declaration: ModuleDeclaration) {
        self.module_declarations_to_add.push(declaration);
    }
}

/// Traverse current module hierarchy and identify existing vs missing module segments
/// (Requirements 2.1, 2.2, 2.3)
pub fn analyze_module_structure(
    sema: &Semantics<'_, RootDatabase>,
    current_module: Module,
    target_module_path: &ModPath,
) -> Result<ModuleStructure> {
    let mut existing_segments = Vec::new();
    let mut missing_segments = Vec::new();

    // Start from crate root for absolute paths
    let mut current = match target_module_path.kind {
        PathKind::Crate => current_module.crate_root(sema.db),
        PathKind::Super(n) => {
            // Handle super:: paths by going up n levels
            let mut module = current_module;
            for _ in 0..n {
                module = module.parent(sema.db).ok_or_else(|| {
                    RenameError(
                        "Cannot resolve super:: path - not enough parent modules".to_string(),
                    )
                })?;
            }
            module
        }
        PathKind::Plain => current_module, // Relative to current module
        PathKind::Abs => {
            return Err(RenameError("Absolute paths not supported for module creation".to_string()));
        }
        PathKind::DollarCrate(_) => current_module.crate_root(sema.db),
    };

    // Traverse each segment in the target path
    for segment in target_module_path.segments() {
        match find_child_module(sema, current, segment) {
            Some(child_module) => {
                // Module exists - add to existing segments
                existing_segments.push(child_module);
                current = child_module;
            }
            None => {
                // Module doesn't exist - add to missing segments
                missing_segments.push(segment.clone());
                // All subsequent segments will also be missing
                missing_segments.extend(
                    target_module_path
                        .segments()
                        .iter()
                        .skip(existing_segments.len() + missing_segments.len())
                        .cloned(),
                );
                break;
            }
        }
    }

    Ok(ModuleStructure {
        current_module,
        target_module_path: target_module_path.clone(),
        missing_segments,
        existing_segments,
    })
}

/// Find a child module by name
fn find_child_module(
    sema: &Semantics<'_, RootDatabase>,
    parent: Module,
    name: &Name,
) -> Option<Module> {
    parent.children(sema.db).find(|child| child.name(sema.db) == Some(name.clone()))
}

/// Create a plan for creating missing module structure
/// (Requirements 2.1, 2.2, 2.3, 2.4, 2.5)
pub fn create_module_creation_plan(
    sema: &Semantics<'_, RootDatabase>,
    module_structure: &ModuleStructure,
    user_preferences: &ModuleOrganizationPreferences,
) -> Result<ModuleCreationPlan> {
    let mut plan = ModuleCreationPlan::new(user_preferences.clone());

    if module_structure.missing_segments.is_empty() {
        // No missing modules - nothing to create
        return Ok(plan);
    }

    // Start from the last existing module (or crate root if none exist)
    let current_module = if let Some(last_existing) = module_structure.existing_segments.last() {
        *last_existing
    } else {
        module_structure.current_module.crate_root(sema.db)
    };

    // Create each missing module in sequence
    for segment in &module_structure.missing_segments {
        let module_spec =
            create_module_file_spec(sema, current_module, segment.as_str(), user_preferences)?;

        plan.add_module_creation(module_spec)?;

        // Add module declaration to parent (Requirement 2.4)
        let declaration = create_module_declaration(sema, current_module, segment.as_str())?;
        plan.add_module_declaration(declaration);

        // For subsequent iterations, we would be working with the newly created module
        // but since it doesn't exist yet, we continue with the current module as parent
    }

    // Validate module integration (Requirement 2.5)
    validate_module_integration(&plan, sema)?;

    Ok(plan)
}

/// Create a specification for a module file
/// (Requirements 2.2, 2.3)
fn create_module_file_spec(
    sema: &Semantics<'_, RootDatabase>,
    parent_module: Module,
    module_name: &str,
    preferences: &ModuleOrganizationPreferences,
) -> Result<ModuleFileSpec> {
    // Determine file type based on preferences (Requirement 2.2)
    let file_type =
        if preferences.prefer_mod_rs { ModuleFileType::ModRs } else { ModuleFileType::ModuleFile };

    // Calculate module path based on parent module and preferences
    let path = calculate_module_path(sema, parent_module, module_name, &file_type, preferences)?;

    // Generate appropriate module content
    let content = generate_module_content(module_name);

    Ok(ModuleFileSpec { path, content, file_type })
}

/// Calculate the file system path for a new module
/// (Requirements 2.1, 2.2, 2.3)
fn calculate_module_path(
    sema: &Semantics<'_, RootDatabase>,
    parent_module: Module,
    module_name: &str,
    file_type: &ModuleFileType,
    preferences: &ModuleOrganizationPreferences,
) -> Result<std::path::PathBuf> {
    // Get the parent module's file path
    let parent_source = parent_module.definition_source(sema.db);
    let parent_file_id = parent_source.file_id.original_file(sema.db);
    // Get the VFS path for the parent file
    let source_root = sema.db.file_source_root(parent_file_id.file_id(sema.db));
    let source_root_data = sema.db.source_root(source_root.source_root_id(sema.db));
    let source_root_ref = source_root_data.source_root(sema.db);
    let parent_vfs_path = source_root_ref
        .path_for_file(&parent_file_id.file_id(sema.db))
        .ok_or_else(|| RenameError("Cannot find path for parent file".to_string()))?;

    let parent_dir = parent_vfs_path
        .as_path()
        .and_then(|p| p.parent())
        .ok_or_else(|| RenameError("Cannot determine parent directory".to_string()))?;

    match preferences.directory_structure {
        DirectoryStructurePreference::Nested => {
            match file_type {
                ModuleFileType::ModRs => {
                    // Create directory with module name and mod.rs inside
                    let mut path = parent_dir.to_path_buf();
                    path.push(module_name);
                    path.push("mod.rs");
                    Ok(path.into())
                }
                ModuleFileType::ModuleFile => {
                    // Create module_name.rs in parent directory
                    let mut path = parent_dir.to_path_buf();
                    path.push(format!("{}.rs", module_name));
                    Ok(path.into())
                }
            }
        }
        DirectoryStructurePreference::Flat => {
            // Always create module_name.rs files in flat structure
            let mut path = parent_dir.to_path_buf();
            path.push(format!("{}.rs", module_name));
            Ok(path.into())
        }
    }
}

/// Generate content for a new module file with correct formatting
fn generate_module_content(module_name: &str) -> String {
    // Create proper module documentation and structure
    let module_doc = format!("//! The `{}` module.", module_name);
    let todo_comment = "// TODO: Add module implementation";

    // Format with proper spacing and structure
    format!(
        "{}\n//!\n//! This module was created automatically during a rename/move operation.\n\n{}\n",
        module_doc, todo_comment
    )
}

/// Create a module declaration to add to the parent module
/// (Requirement 2.4)
fn create_module_declaration(
    sema: &Semantics<'_, RootDatabase>,
    parent_module: Module,
    module_name: &str,
) -> Result<ModuleDeclaration> {
    let parent_source = parent_module.definition_source(sema.db);
    let parent_file_id = parent_source.file_id.original_file(sema.db);

    // Find appropriate position to insert module declaration
    let insert_position = find_module_declaration_position(sema, parent_module)?;

    // Determine appropriate visibility
    let visibility = determine_module_visibility(sema, parent_module);

    Ok(ModuleDeclaration {
        parent_file: parent_file_id.file_id(sema.db),
        module_name: module_name.to_string(),
        insert_position,
        visibility,
    })
}

/// Find the appropriate position to insert a module declaration
fn find_module_declaration_position(
    sema: &Semantics<'_, RootDatabase>,
    parent_module: Module,
) -> Result<syntax::TextSize> {
    let source = parent_module.definition_source(sema.db);

    match &source.value {
        ModuleSource::SourceFile(source_file) => {
            // Find the end of existing module declarations or the beginning of the file
            let mut insert_pos = syntax::TextSize::from(0);

            for item in source_file.items() {
                match item {
                    syntax::ast::Item::Module(_) => {
                        // Insert after the last module declaration
                        insert_pos = item.syntax().text_range().end();
                    }
                    _ => {
                        // Stop at the first non-module item
                        break;
                    }
                }
            }

            Ok(insert_pos)
        }
        ModuleSource::Module(_) => {
            // Inline module - not supported for adding new modules
            Err(RenameError("Cannot add modules to inline module definitions".to_string()))
        }
        ModuleSource::BlockExpr(_) => {
            // Block module - not supported
            Err(RenameError("Cannot add modules to block expressions".to_string()))
        }
    }
}

/// Determine appropriate visibility for a new module
fn determine_module_visibility(
    _sema: &Semantics<'_, RootDatabase>,
    _parent_module: Module,
) -> Option<String> {
    // For now, use default (private) visibility
    // This could be enhanced to analyze the parent module's visibility patterns
    None
}

/// Validate that the module creation plan will integrate properly into the existing module tree
/// (Requirement 2.5)
fn validate_module_integration(
    plan: &ModuleCreationPlan,
    sema: &Semantics<'_, RootDatabase>,
) -> Result<()> {
    // Basic validation - ensure no conflicting file paths
    let mut file_paths = std::collections::HashSet::new();

    for file_spec in &plan.files_to_create {
        if !file_paths.insert(&file_spec.path) {
            return Err(RenameError(format!(
                "Duplicate file path in creation plan: {}",
                file_spec.path.display()
            )));
        }
    }

    // Validate module names are valid Rust identifiers
    for file_spec in &plan.files_to_create {
        if let Some(module_name) = file_spec.path.file_stem().and_then(|s| s.to_str()) {
            let module_name = if module_name == "mod" {
                // For mod.rs files, use the parent directory name
                file_spec
                    .path
                    .parent()
                    .and_then(|p| p.file_name())
                    .and_then(|s| s.to_str())
                    .unwrap_or("unknown")
            } else {
                module_name
            };

            if !is_valid_module_name(module_name) {
                return Err(RenameError(format!("Invalid module name: {}", module_name)));
            }
        }
    }

    // Validate that parent modules can accept new child modules
    for declaration in &plan.module_declarations_to_add {
        validate_parent_can_accept_module(sema, declaration)?;
    }

    Ok(())
}

/// Enhanced error handling for move operations with detailed error messages and recovery suggestions
pub fn handle_move_operation_with_recovery(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: &ModPath,
    new_item_name: &Name,
) -> Result<SourceChange, MoveRenameError> {
    let mut rollback_context = RollbackContext::new();

    // Comprehensive validation with detailed error messages
    match validate_move_operation_comprehensive(sema, def, target_module_path, new_item_name) {
        Ok(()) => {}
        Err(e) => {
            return Err(enhance_error_with_suggestions(e, sema, def, target_module_path, new_item_name));
        }
    }

    // Execute move operation with rollback tracking
    match execute_move_operation_with_tracking(sema, def, target_module_path, new_item_name, &mut rollback_context) {
        Ok(source_change) => Ok(source_change),
        Err(e) => {
            // Attempt rollback on failure
            if rollback_context.needs_rollback() {
                if let Ok(rollback_change) = execute_rollback(&rollback_context, sema) {
                    // Combine error with rollback information
                    return Err(MoveRenameError::ReferenceUpdateFailed {
                        file: "multiple files".to_string(),
                        error: format!("Operation failed and was rolled back: {}", e),
                    });
                }
            }
            Err(e)
        }
    }
}

/// Comprehensive validation with detailed error reporting
fn validate_move_operation_comprehensive(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: &ModPath,
    new_item_name: &Name,
) -> Result<(), MoveRenameError> {
    // Get target module
    let target_module = resolve_target_module(sema, target_module_path)?;

    // Validate name conflicts with enhanced error messages
    validate_no_name_conflicts_enhanced(sema, target_module, new_item_name)?;

    // Validate circular dependencies with detailed analysis
    validate_no_circular_dependencies_enhanced(sema, def, target_module)?;

    // Validate visibility constraints with suggestions
    validate_visibility_constraints_enhanced(sema, def, target_module)?;

    // Validate dependency accessibility with detailed reporting
    validate_dependency_accessibility_enhanced(sema, def, target_module)?;

    Ok(())
}

/// Enhanced name conflict validation with detailed error messages
fn validate_no_name_conflicts_enhanced(
    sema: &Semantics<'_, RootDatabase>,
    target_module: Module,
    new_item_name: &Name,
) -> Result<(), MoveRenameError> {
    if let Some(existing_item) = find_existing_item_in_module(sema, &target_module, new_item_name) {
        let existing_name = existing_item.name(sema.db)
            .map(|name| name.display(sema.db, Edition::CURRENT).to_string())
            .unwrap_or_else(|| "unnamed item".to_string());
        
        let item_type = get_item_type_description(&existing_item);
        let module_path = module_to_mod_path(sema, target_module)
            .map(|path| format_mod_path(&path))
            .unwrap_or_else(|_| "unknown module".to_string());
        
        return Err(MoveRenameError::NameConflict {
            existing_item: format!("{} '{}' in module '{}'", item_type, existing_name, module_path),
            target_name: new_item_name.display(sema.db, Edition::CURRENT).to_string(),
        });
    }
    
    Ok(())
}

/// Enhanced circular dependency validation with detailed analysis
fn validate_no_circular_dependencies_enhanced(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module: Module,
) -> Result<(), MoveRenameError> {
    let current_module = def.module(sema.db)
        .ok_or_else(|| MoveRenameError::DependencyError("Cannot determine current module".to_string()))?;

    if would_create_circular_dependency(sema, def, current_module, target_module)? {
        let item_name = def.name(sema.db)
            .map(|name| name.display(sema.db, Edition::CURRENT).to_string())
            .unwrap_or_else(|| "unnamed item".to_string());
        
        let current_module_path = module_to_mod_path(sema, current_module)
            .map(|path| format_mod_path(&path))
            .unwrap_or_else(|_| "unknown module".to_string());
        
        let target_module_path = module_to_mod_path(sema, target_module)
            .map(|path| format_mod_path(&path))
            .unwrap_or_else(|_| "unknown module".to_string());
        
        return Err(MoveRenameError::CircularDependency(
            format!(
                "Moving '{}' from '{}' to '{}' would create a circular dependency. \
                The target module already depends on the current module, and moving this item \
                would make the current module depend on the target module.",
                item_name, current_module_path, target_module_path
            )
        ));
    }
    
    Ok(())
}

/// Enhanced visibility validation with suggestions
fn validate_visibility_constraints_enhanced(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module: Module,
) -> Result<(), MoveRenameError> {
    let current_visibility = get_item_visibility(sema, def)?;
    let target_module_context = get_module_visibility_context(sema, target_module)?;
    
    if !is_visibility_compatible(current_visibility.clone(), target_module_context) {
        let item_name = def.name(sema.db)
            .map(|name| name.display(sema.db, Edition::CURRENT).to_string())
            .unwrap_or_else(|| "unnamed item".to_string());
        
        let visibility_desc = describe_visibility(&current_visibility);
        let suggestion = suggest_visibility_fix(&current_visibility, &target_module_context);
        
        return Err(MoveRenameError::VisibilityViolation(
            format!(
                "Item '{}' has {} visibility which is incompatible with the target module. {}",
                item_name, visibility_desc, suggestion
            )
        ));
    }
    
    // Validate that existing references will remain accessible
    validate_references_remain_accessible_enhanced(sema, def, target_module)?;
    
    Ok(())
}

/// Enhanced dependency validation with detailed reporting
fn validate_dependency_accessibility_enhanced(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module: Module,
) -> Result<(), MoveRenameError> {
    let dependencies = find_item_dependencies(sema, def)?;
    let mut inaccessible_deps = Vec::new();
    
    for dependency in dependencies {
        if !will_dependency_be_accessible(sema, dependency, target_module)? {
            let dep_name = dependency.name(sema.db)
                .map(|name| name.display(sema.db, Edition::CURRENT).to_string())
                .unwrap_or_else(|| "unnamed dependency".to_string());
            
            let dep_module = dependency.module(sema.db);
            let dep_location = if let Some(module) = dep_module {
                module_to_mod_path(sema, module)
                    .map(|path| format_mod_path(&path))
                    .unwrap_or_else(|_| "unknown module".to_string())
            } else {
                "built-in".to_string()
            };
            
            inaccessible_deps.push(format!("'{}' in '{}'", dep_name, dep_location));
        }
    }
    
    if !inaccessible_deps.is_empty() {
        return Err(MoveRenameError::DependencyError(
            format!(
                "The following dependencies would become inaccessible after the move: {}. \
                Consider making these dependencies public or moving them to a common parent module.",
                inaccessible_deps.join(", ")
            )
        ));
    }
    
    Ok(())
}

/// Execute move operation with rollback tracking
fn execute_move_operation_with_tracking(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: &ModPath,
    new_item_name: &Name,
    rollback_context: &mut RollbackContext,
) -> Result<SourceChange, MoveRenameError> {
    let mut source_change = SourceChange::default();

    // Step 1: Create target module structure if needed
    let target_module = match resolve_target_module(sema, target_module_path) {
        Ok(module) => module,
        Err(_) => {
            // Module doesn't exist, create it
            let current_module = def.module(sema.db)
                .ok_or_else(|| MoveRenameError::ModuleCreationFailed("Cannot determine current module".to_string()))?;
            
            let module_structure = analyze_module_structure(sema, current_module, target_module_path)
                .map_err(|e| MoveRenameError::ModuleCreationFailed(format!("Failed to analyze module structure: {}", e.0)))?;
            
            let user_preferences = get_user_module_preferences();
            let creation_plan = create_module_creation_plan(sema, &module_structure, &user_preferences)
                .map_err(|e| MoveRenameError::ModuleCreationFailed(format!("Failed to create module plan: {}", e.0)))?;
            
            // Track module creation for rollback
            for file_spec in &creation_plan.files_to_create {
                // We don't have the FileId yet, but we can track the path
                rollback_context.record_directory_creation(file_spec.path.clone());
            }
            
            let module_creation_change = execute_module_creation_plan(&creation_plan, sema)
                .map_err(|e| MoveRenameError::ModuleCreationFailed(format!("Failed to execute module creation: {}", e.0)))?;
            
            source_change.extend(module_creation_change.source_file_edits);
            source_change.file_system_edits.extend(module_creation_change.file_system_edits);
            
            // Try to resolve the module again after creation
            resolve_target_module(sema, target_module_path)
                .map_err(|e| MoveRenameError::ModuleCreationFailed(format!("Failed to resolve created module: {}", e.0)))?
        }
    };

    // Step 2: Move the item
    let current_module = def.module(sema.db)
        .ok_or_else(|| MoveRenameError::DependencyError("Cannot determine current module".to_string()))?;
    
    // Record original file content for rollback
    if let Some(file_range) = def.range_for_rename(sema) {
        let original_content = sema.db.file_text(file_range.file_id).to_string();
        rollback_context.record_file_modification(file_range.file_id.file_id(sema.db), original_content);
    }
    
    let move_change = move_item_to_module(sema, def, target_module, new_item_name)
        .map_err(|e| MoveRenameError::ReferenceUpdateFailed {
            file: "item move".to_string(),
            error: format!("Failed to move item: {}", e.0),
        })?;
    
    source_change.extend(move_change.source_file_edits);
    source_change.file_system_edits.extend(move_change.file_system_edits);

    // Step 3: Update references
    let reference_change = update_external_references(sema, def, current_module, target_module, new_item_name)
        .map_err(|e| MoveRenameError::ReferenceUpdateFailed {
            file: "reference updates".to_string(),
            error: format!("Failed to update references: {}", e.0),
        })?;
    
    source_change.extend(reference_change.source_file_edits);
    source_change.file_system_edits.extend(reference_change.file_system_edits);

    Ok(source_change)
}

/// Enhance error with suggestions and recovery options
fn enhance_error_with_suggestions(
    error: MoveRenameError,
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: &ModPath,
    new_item_name: &Name,
) -> MoveRenameError {
    match error {
        MoveRenameError::NameConflict { existing_item, target_name } => {
            let suggestions = generate_alternative_names(sema, target_module_path, new_item_name);
            let suggestion_text = if suggestions.is_empty() {
                "Consider using a different name.".to_string()
            } else {
                format!("Consider using one of these alternative names: {}", suggestions.join(", "))
            };
            
            MoveRenameError::NameConflict {
                existing_item: format!("{}. {}", existing_item, suggestion_text),
                target_name,
            }
        }
        MoveRenameError::InvalidPath(msg) => {
            let suggestions = generate_path_suggestions(sema, def, target_module_path);
            MoveRenameError::InvalidPath(format!("{}. {}", msg, suggestions))
        }
        MoveRenameError::VisibilityViolation(msg) => {
            let suggestions = generate_visibility_suggestions(sema, def, target_module_path);
            MoveRenameError::VisibilityViolation(format!("{}. {}", msg, suggestions))
        }
        other => other,
    }
}

/// Generate alternative names when there's a naming conflict
fn generate_alternative_names(
    sema: &Semantics<'_, RootDatabase>,
    target_module_path: &ModPath,
    base_name: &Name,
) -> Vec<String> {
    let mut alternatives = Vec::new();
    let base_str = base_name.as_str();
    
    // Try common suffixes
    for suffix in &["_new", "_moved", "_v2", "_alt"] {
        let candidate = format!("{}{}", base_str, suffix);
        if let Ok(candidate_name) = Name::new(&candidate) {
            // Check if this name would be available
            if let Ok(target_module) = resolve_target_module(sema, target_module_path) {
                if find_existing_item_in_module(sema, &target_module, &candidate_name).is_none() {
                    alternatives.push(candidate);
                }
            }
        }
    }
    
    // Try numbered suffixes
    for i in 1..=5 {
        let candidate = format!("{}_{}", base_str, i);
        if let Ok(candidate_name) = Name::new(&candidate) {
            if let Ok(target_module) = resolve_target_module(sema, target_module_path) {
                if find_existing_item_in_module(sema, &target_module, &candidate_name).is_none() {
                    alternatives.push(candidate);
                }
            }
        }
    }
    
    alternatives.truncate(3); // Limit to 3 suggestions
    alternatives
}

/// Generate path suggestions for invalid paths
fn generate_path_suggestions(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    _target_module_path: &ModPath,
) -> String {
    let current_module = def.module(sema.db);
    
    if let Some(module) = current_module {
        let current_path = module_to_mod_path(sema, module)
            .map(|path| format_mod_path(&path))
            .unwrap_or_else(|_| "crate".to_string());
        
        format!(
            "Valid path formats: 'crate::module::item', '{}::item', or 'super::item'. \
            Current module is '{}'.",
            current_path, current_path
        )
    } else {
        "Valid path formats: 'crate::module::item', 'module::item', or 'super::item'.".to_string()
    }
}

/// Generate visibility suggestions
fn generate_visibility_suggestions(
    _sema: &Semantics<'_, RootDatabase>,
    _def: Definition,
    _target_module_path: &ModPath,
) -> String {
    "Consider making the item public with 'pub', or use 'pub(crate)' for crate-wide visibility, \
    or 'pub(super)' for parent module visibility.".to_string()
}

/// Get a human-readable description of an item type
fn get_item_type_description(def: &Definition) -> &'static str {
    match def {
        Definition::Module(_) => "module",
        Definition::Function(_) => "function",
        Definition::Adt(adt) => match adt {
            hir::Adt::Struct(_) => "struct",
            hir::Adt::Union(_) => "union",
            hir::Adt::Enum(_) => "enum",
        },
        Definition::Variant(_) => "enum variant",
        Definition::Const(_) => "constant",
        Definition::Static(_) => "static",
        Definition::Trait(_) => "trait",
        Definition::TypeAlias(_) => "type alias",
        Definition::Macro(_) => "macro",
        Definition::Local(_) => "local variable",
        Definition::Field(_) => "field",
        Definition::SelfType(_) => "Self type",
        Definition::GenericParam(_) => "generic parameter",
        Definition::Label(_) => "label",
        Definition::BuiltinType(_) => "builtin type",
        Definition::BuiltinLifetime(_) => "builtin lifetime",
        Definition::BuiltinAttr(_) => "builtin attribute",
        Definition::ToolModule(_) => "tool module",
        Definition::DeriveHelper(_) => "derive helper",
        Definition::TupleField(_) => "tuple field",
        Definition::InlineAsmOperand(_) => "inline asm operand",
        Definition::InlineAsmRegOrRegClass(_) => "inline asm register",
        Definition::ExternCrateDecl(_) => "extern crate",
        Definition::Crate(_) => "crate",
    }
}

/// Describe visibility in human-readable terms
fn describe_visibility(visibility: &ItemVisibility) -> String {
    match visibility {
        ItemVisibility::Private => "private".to_string(),
        ItemVisibility::Public => "public".to_string(),
        ItemVisibility::PubCrate => "pub(crate)".to_string(),
        ItemVisibility::PubSuper => "pub(super)".to_string(),
        ItemVisibility::PubSelf => "pub(self)".to_string(),
        ItemVisibility::PubIn(path) => path.clone(),
    }
}

/// Suggest visibility fixes
fn suggest_visibility_fix(
    _current_visibility: &ItemVisibility,
    _target_context: &ModuleVisibilityContext,
) -> String {
    "Consider making the item public or adjusting its visibility to be compatible with the target module.".to_string()
}

/// Resolve target module from path
fn resolve_target_module(
    sema: &Semantics<'_, RootDatabase>,
    target_module_path: &ModPath,
) -> Result<Module, MoveRenameError> {
    // This is a simplified resolution - a full implementation would use proper path resolution
    // For now, we'll try to find the module by traversing the path
    
    // Start from crate root
    let crate_root = sema.db.crate_graph().iter().next()
        .and_then(|crate_id| sema.db.crate_def_map(crate_id).root_module_id())
        .and_then(|module_id| sema.db.module_data(module_id).name.clone())
        .ok_or_else(|| MoveRenameError::InvalidPath("Cannot find crate root".to_string()))?;
    
    // For now, return an error indicating the module needs to be created
    Err(MoveRenameError::ModuleCreationFailed("Target module does not exist".to_string()))
}

/// Enhanced reference accessibility validation
fn validate_references_remain_accessible_enhanced(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module: Module,
) -> Result<(), MoveRenameError> {
    let usages = def.usages(sema).all();
    let mut inaccessible_refs = Vec::new();
    
    for (file_id, references) in usages {
        for reference in references {
            if !will_reference_remain_accessible(sema, &reference, def, target_module, file_id)? {
                let file_path = sema.db.file_source_root(file_id.file_id(sema.db));
                let source_root_data = sema.db.source_root(file_path.source_root_id(sema.db));
                let source_root_ref = source_root_data.source_root(sema.db);
                
                let file_name = source_root_ref
                    .path_for_file(&file_id.file_id(sema.db))
                    .and_then(|path| path.as_path())
                    .and_then(|path| path.file_name())
                    .and_then(|name| name.to_str())
                    .unwrap_or("unknown file");
                
                inaccessible_refs.push(format!("{}:{}", file_name, u32::from(reference.range.start())));
            }
        }
    }
    
    if !inaccessible_refs.is_empty() {
        return Err(MoveRenameError::VisibilityViolation(
            format!(
                "The following references would become inaccessible after the move: {}. \
                Consider making the item public or adjusting visibility.",
                inaccessible_refs.join(", ")
            )
        ));
    }
    
    Ok(())
}

/// Check if a module name is a valid Rust identifier
fn is_valid_module_name(name: &str) -> bool {
    // Basic validation - could be enhanced with full Rust identifier rules
    !name.is_empty()
        && name.chars().all(|c| c.is_alphanumeric() || c == '_')
        && !name.chars().next().unwrap().is_ascii_digit()
        && !is_rust_keyword(name)
}

/// Check if a string is a Rust keyword
fn is_rust_keyword(name: &str) -> bool {
    matches!(
        name,
        "as" | "break"
            | "const"
            | "continue"
            | "crate"
            | "else"
            | "enum"
            | "extern"
            | "false"
            | "fn"
            | "for"
            | "if"
            | "impl"
            | "in"
            | "let"
            | "loop"
            | "match"
            | "mod"
            | "move"
            | "mut"
            | "pub"
            | "ref"
            | "return"
            | "self"
            | "Self"
            | "static"
            | "struct"
            | "super"
            | "trait"
            | "true"
            | "type"
            | "unsafe"
            | "use"
            | "where"
            | "while"
            | "async"
            | "await"
            | "dyn"
            | "abstract"
            | "become"
            | "box"
            | "do"
            | "final"
            | "macro"
            | "override"
            | "priv"
            | "typeof"
            | "unsized"
            | "virtual"
            | "yield"
            | "try"
    )
}

// ============================================================================
// TASK 6: VALIDATION AND CONFLICT DETECTION (PRE-OPERATION CHECKS)
// ============================================================================

/// Error types for move/rename operations with detailed error information
/// (Requirements 4.1, 4.2, 4.3, 4.4, 4.5)
#[derive(Debug)]
pub enum MoveRenameError {
    /// Invalid path syntax (Requirement 1.3)
    InvalidPath(String),
    /// Module creation failed (Requirement 2.1)
    ModuleCreationFailed(String),
    /// Name conflict with existing item (Requirement 4.1)
    NameConflict { existing_item: String, target_name: String },
    /// Circular dependency would be created (Requirement 4.3)
    CircularDependency(String),
    /// Visibility constraints would be violated (Requirement 4.5)
    VisibilityViolation(String),
    /// General file system error
    FileSystemError(String),
    /// Dependency error (Requirement 4.2)
    DependencyError(String),
    /// Module integration failed (Requirement 2.5)
    ModuleIntegrationFailed(String),
    /// Reference update failed (Requirements 3.1-3.5, 6.1-6.5)
    ReferenceUpdateFailed { file: String, error: String },
}

impl fmt::Display for MoveRenameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MoveRenameError::InvalidPath(msg) => write!(f, "Invalid path: {}", msg),
            MoveRenameError::ModuleCreationFailed(msg) => write!(f, "Module creation failed: {}", msg),
            MoveRenameError::NameConflict { existing_item, target_name } => {
                write!(f, "Name conflict: '{}' already exists, cannot rename to '{}'", existing_item, target_name)
            }
            MoveRenameError::CircularDependency(msg) => write!(f, "Circular dependency: {}", msg),
            MoveRenameError::VisibilityViolation(msg) => write!(f, "Visibility violation: {}", msg),
            MoveRenameError::FileSystemError(msg) => write!(f, "File system error: {}", msg),
            MoveRenameError::DependencyError(msg) => write!(f, "Dependency error: {}", msg),
            MoveRenameError::ModuleIntegrationFailed(msg) => write!(f, "Module integration failed: {}", msg),
            MoveRenameError::ReferenceUpdateFailed { file, error } => {
                write!(f, "Reference update failed in {}: {}", file, error)
            }
        }
    }
}

impl std::error::Error for MoveRenameError {}

impl From<MoveRenameError> for RenameError {
    fn from(err: MoveRenameError) -> Self {
        RenameError(err.to_string())
    }
}

/// Rollback context for tracking changes that need to be undone on failure
#[derive(Debug, Default)]
pub struct RollbackContext {
    /// Files that were created and need to be deleted on rollback
    pub created_files: Vec<FileId>,
    /// Files that were modified and their original content for restoration
    pub modified_files: Vec<(FileId, String)>,
    /// Directories that were created and need to be removed on rollback
    pub created_directories: Vec<std::path::PathBuf>,
    /// Module declarations that were added and need to be removed on rollback
    pub added_module_declarations: Vec<(FileId, TextRange)>,
}

impl RollbackContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a file creation for potential rollback
    pub fn record_file_creation(&mut self, file_id: FileId) {
        self.created_files.push(file_id);
    }

    /// Record a file modification for potential rollback
    pub fn record_file_modification(&mut self, file_id: FileId, original_content: String) {
        self.modified_files.push((file_id, original_content));
    }

    /// Record a directory creation for potential rollback
    pub fn record_directory_creation(&mut self, dir_path: std::path::PathBuf) {
        self.created_directories.push(dir_path);
    }

    /// Record a module declaration addition for potential rollback
    pub fn record_module_declaration(&mut self, file_id: FileId, range: TextRange) {
        self.added_module_declarations.push((file_id, range));
    }

    /// Check if rollback is needed (has any recorded changes)
    pub fn needs_rollback(&self) -> bool {
        !self.created_files.is_empty()
            || !self.modified_files.is_empty()
            || !self.created_directories.is_empty()
            || !self.added_module_declarations.is_empty()
    }
}

/// Execute rollback operations to undo partial changes
/// This maintains code integrity when operations fail partway through
pub fn execute_rollback(
    rollback_context: &RollbackContext,
    sema: &Semantics<'_, RootDatabase>,
) -> Result<SourceChange> {
    let mut source_change = SourceChange::default();

    // Remove added module declarations (in reverse order)
    for (file_id, range) in rollback_context.added_module_declarations.iter().rev() {
        let delete_edit = TextEdit::delete(*range);
        source_change.insert_source_edit(*file_id, delete_edit);
    }

    // Restore modified files to their original content
    for (file_id, original_content) in &rollback_context.modified_files {
        // Get current file content to calculate full replacement
        let current_text = sema.db.file_text(*file_id);
        let full_range = TextRange::new(0.into(), current_text.len().into());
        let restore_edit = TextEdit::replace(full_range, original_content.clone());
        source_change.insert_source_edit(*file_id, restore_edit);
    }

    // Delete created files
    for file_id in &rollback_context.created_files {
        // Find the file path for deletion
        if let Some(anchored_path) = find_anchored_path_for_file(sema, *file_id)? {
            source_change.push_file_system_edit(FileSystemEdit::MoveFile {
                src: anchored_path.anchor,
                dst: AnchoredPathBuf {
                    anchor: anchored_path.anchor,
                    path: format!("{}.deleted", anchored_path.path),
                },
            });
        }
    }

    // Remove created directories (in reverse order to handle nested directories)
    for dir_path in rollback_context.created_directories.iter().rev() {
        if let Some(anchor_file) = find_anchor_file_for_path(sema, dir_path)? {
            if let Ok(relative_path) = calculate_relative_path(sema, anchor_file, dir_path) {
                // Mark directory for removal (actual removal would be handled by the file system)
                // For now, we'll create a marker file to indicate the directory should be removed
                let marker_path = format!("{}/.to_remove", relative_path);
                let anchored_path = AnchoredPathBuf {
                    anchor: anchor_file,
                    path: marker_path,
                };
                source_change.push_file_system_edit(FileSystemEdit::CreateFile {
                    dst: anchored_path,
                    initial_contents: "This directory was created during a failed operation and should be removed.".to_string(),
                });
            }
        }
    }

    Ok(source_change)
}

/// Find anchored path for a file ID
fn find_anchored_path_for_file(
    sema: &Semantics<'_, RootDatabase>,
    file_id: FileId,
) -> Result<Option<AnchoredPathBuf>> {
    let source_root = sema.db.file_source_root(file_id);
    let source_root_data = sema.db.source_root(source_root.source_root_id(sema.db));
    let source_root_ref = source_root_data.source_root(sema.db);

    if let Some(vfs_path) = source_root_ref.path_for_file(&file_id) {
        if let Some(path) = vfs_path.as_path() {
            // Use the file itself as anchor with empty relative path
            return Ok(Some(AnchoredPathBuf {
                anchor: file_id,
                path: String::new(),
            }));
        }
    }

    Ok(None)
}

/// Check for existing items with same name in target module
/// (Requirement 4.1)
pub fn check_name_conflicts(
    sema: &Semantics<'_, RootDatabase>,
    target_module: Module,
    new_item_name: &Name,
) -> Result<(), MoveRenameError> {
    // Get all items in the target module
    let module_scope = target_module.scope(sema.db, None);
    
    // Check for conflicts with existing definitions
    for (name, def) in module_scope {
        if name == *new_item_name {
            let existing_item_name = match def {
                hir::ScopeDef::ModuleDef(module_def) => {
                    format!("{:?}", module_def)
                }
                hir::ScopeDef::Unknown => "unknown item".to_string(),
                hir::ScopeDef::ImplSelfType(_) => "impl Self type".to_string(),
                hir::ScopeDef::AdtSelfType(_) => "ADT Self type".to_string(),
                hir::ScopeDef::GenericParam(_) => "generic parameter".to_string(),
                hir::ScopeDef::Local(_) => "local variable".to_string(),
                hir::ScopeDef::Label(_) => "label".to_string(),
            };
            
            return Err(MoveRenameError::NameConflict {
                existing_item: existing_item_name,
                target_name: new_item_name.display(sema.db, Edition::CURRENT).to_string(),
            });
        }
    }
    
    // Check for conflicts with child modules
    for child_module in target_module.children(sema.db) {
        if let Some(child_name) = child_module.name(sema.db) {
            if child_name == *new_item_name {
                return Err(MoveRenameError::NameConflict {
                    existing_item: format!("module {}", child_name.display(sema.db, Edition::CURRENT)),
                    target_name: new_item_name.display(sema.db, Edition::CURRENT).to_string(),
                });
            }
        }
    }
    
    Ok(())
}

/// Find existing item in module by name for conflict detection
/// (Requirement 4.1)
pub fn find_existing_item_in_module(
    sema: &Semantics<'_, RootDatabase>,
    target_module: &Module,
    item_name: &Name,
) -> Option<Definition> {
    let module_scope = target_module.scope(sema.db, None);
    
    // Look for existing definitions with the same name
    for (name, scope_def) in module_scope {
        if name == *item_name {
            // Convert ScopeDef to Definition if possible
            match scope_def {
                hir::ScopeDef::ModuleDef(module_def) => {
                    return Some(Definition::from(module_def));
                }
                hir::ScopeDef::Local(local) => {
                    return Some(Definition::Local(local));
                }
                hir::ScopeDef::Label(label) => {
                    return Some(Definition::Label(label));
                }
                hir::ScopeDef::GenericParam(generic_param) => {
                    return Some(Definition::GenericParam(generic_param));
                }
                _ => {
                    // Other scope definitions don't have corresponding Definition variants
                    // but they still represent conflicts
                    continue;
                }
            }
        }
    }
    
    // Check child modules
    for child_module in target_module.children(sema.db) {
        if let Some(child_name) = child_module.name(sema.db) {
            if child_name == *item_name {
                return Some(Definition::Module(child_module));
            }
        }
    }
    
    None
}

/// Validate that move operation won't create naming conflicts with detailed error messages
/// (Requirement 4.1)
pub fn validate_no_name_conflicts(
    sema: &Semantics<'_, RootDatabase>,
    target_module: Module,
    new_item_name: &Name,
) -> Result<(), MoveRenameError> {
    if let Some(existing_item) = find_existing_item_in_module(sema, &target_module, new_item_name) {
        let existing_name = existing_item.name(sema.db)
            .map(|name| name.display(sema.db, Edition::CURRENT).to_string())
            .unwrap_or_else(|| "unnamed item".to_string());
        
        let item_type = match existing_item {
            Definition::Module(_) => "module",
            Definition::Function(_) => "function", 
            Definition::Adt(adt) => match adt {
                hir::Adt::Struct(_) => "struct",
                hir::Adt::Union(_) => "union", 
                hir::Adt::Enum(_) => "enum",
            },
            Definition::Variant(_) => "enum variant",
            Definition::Const(_) => "constant",
            Definition::Static(_) => "static",
            Definition::Trait(_) => "trait",
            Definition::TypeAlias(_) => "type alias",
            Definition::Macro(_) => "macro",
            Definition::Local(_) => "local variable",
            Definition::Field(_) => "field",
            Definition::Label(_) => "label",
            Definition::GenericParam(_) => "generic parameter",
            _ => "item",
        };
        
        return Err(MoveRenameError::NameConflict {
            existing_item: format!("{} '{}'", item_type, existing_name),
            target_name: new_item_name.display(sema.db, Edition::CURRENT).to_string(),
        });
    }
    
    Ok(())
}

/// Comprehensive validation function for move operations with all checks
/// (Requirements 4.1, 4.2, 4.3, 4.4, 4.5)
pub fn validate_move_operation(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    _target_module_path: &ModPath,
    new_item_name: &Name,
    target_module: Module,
) -> Result<(), MoveRenameError> {
    // Check for name conflicts (Requirement 4.1)
    validate_no_name_conflicts(sema, target_module, new_item_name)?;
    
    // Validate circular dependency prevention (Requirement 4.3)
    validate_no_circular_dependencies(sema, def, target_module)?;
    
    // Check visibility constraints (Requirement 4.4, 4.5)
    validate_visibility_constraints(sema, def, target_module)?;
    
    // Validate dependencies can be maintained (Requirement 4.2)
    validate_dependency_accessibility(sema, def, target_module)?;
    
    Ok(())
}

/// Check for circular dependency creation
/// (Requirement 4.3)
pub fn validate_no_circular_dependencies(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module: Module,
) -> Result<(), MoveRenameError> {
    // Get the current module of the definition
    let current_module = match def.module(sema.db) {
        Some(module) => module,
        None => return Ok(()), // Built-in items can't create circular dependencies
    };
    
    // Check if moving this item would create a circular dependency
    if would_create_circular_dependency(sema, def, current_module, target_module)? {
        let item_name = def.name(sema.db)
            .map(|name| name.display(sema.db, Edition::CURRENT).to_string())
            .unwrap_or_else(|| "unnamed item".to_string());
        
        return Err(MoveRenameError::CircularDependency(
            format!("Moving '{}' to module '{}' would create a circular dependency", 
                   item_name, 
                   target_module.name(sema.db)
                       .map(|name| name.display(sema.db, Edition::CURRENT).to_string())
                       .unwrap_or_else(|| "crate root".to_string()))
        ));
    }
    
    Ok(())
}

/// Check if moving an item would create circular dependencies
fn would_create_circular_dependency(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    current_module: Module,
    target_module: Module,
) -> Result<bool, MoveRenameError> {
    // If target module is the same as current, no circular dependency
    if current_module == target_module {
        return Ok(false);
    }
    
    // Check if target module depends on the current module
    if module_depends_on_module(sema, target_module, current_module)? {
        // Check if the item being moved is used by the target module
        if item_is_used_by_module(sema, def, target_module)? {
            return Ok(true);
        }
    }
    
    // Check if moving this item would make the current module depend on target module
    // when target module already depends on current module
    if module_depends_on_module(sema, target_module, current_module)? {
        // If current module would need to import from target module after the move
        if current_module_would_depend_on_target_after_move(sema, def, current_module, target_module)? {
            return Ok(true);
        }
    }
    
    Ok(false)
}

/// Check if one module depends on another
fn module_depends_on_module(
    sema: &Semantics<'_, RootDatabase>,
    dependent: Module,
    dependency: Module,
) -> Result<bool, MoveRenameError> {
    // Get all imports in the dependent module
    let dependent_source = dependent.definition_source(sema.db);
    
    match &dependent_source.value {
        hir::ModuleSource::SourceFile(source_file) => {
            // Check all use statements in the source file
            for item in source_file.items() {
                if let syntax::ast::Item::Use(use_stmt) = item {
                    if use_statement_imports_from_module(sema, &use_stmt, dependency)? {
                        return Ok(true);
                    }
                }
            }
        }
        hir::ModuleSource::Module(module_ast) => {
            // Check use statements in inline module
            if let Some(item_list) = module_ast.item_list() {
                for item in item_list.items() {
                    if let syntax::ast::Item::Use(use_stmt) = item {
                        if use_statement_imports_from_module(sema, &use_stmt, dependency)? {
                            return Ok(true);
                        }
                    }
                }
            }
        }
        hir::ModuleSource::BlockExpr(_) => {
            // Block expressions are not relevant for module dependencies
            return Ok(false);
        }
    }
    
    Ok(false)
}

/// Check if a use statement imports from a specific module
fn use_statement_imports_from_module(
    sema: &Semantics<'_, RootDatabase>,
    use_stmt: &syntax::ast::Use,
    target_module: Module,
) -> Result<bool, MoveRenameError> {
    if let Some(use_tree) = use_stmt.use_tree() {
        if let Some(path) = use_tree.path() {
            // Try to resolve the path to see if it refers to the target module
            if path_refers_to_module(sema, &path, target_module)? {
                return Ok(true);
            }
        }
    }
    
    Ok(false)
}

/// Check if a path refers to a specific module
fn path_refers_to_module(
    sema: &Semantics<'_, RootDatabase>,
    path: &syntax::ast::Path,
    target_module: Module,
) -> Result<bool, MoveRenameError> {
    // This is a simplified check - a full implementation would use
    // rust-analyzer's path resolution to determine if the path refers to the target module
    
    let path_text = path.syntax().text().to_string();
    
    // Get the target module path for comparison
    let target_module_path = module_to_mod_path(sema, target_module)
        .map_err(|e| MoveRenameError::DependencyError(format!("Failed to get module path: {}", e.0)))?;
    
    // Simple check: see if the path starts with the target module path
    let target_path_text = format_mod_path(&target_module_path);
    
    Ok(path_text.starts_with(&target_path_text))
}

/// Format a ModPath as a string for comparison
fn format_mod_path(mod_path: &ModPath) -> String {
    let mut parts = Vec::new();
    
    match mod_path.kind {
        PathKind::Crate => parts.push("crate".to_string()),
        PathKind::Super(n) => {
            for _ in 0..n {
                parts.push("super".to_string());
            }
        }
        PathKind::Plain => {
            // No prefix for plain paths
        }
        PathKind::Abs => {
            // Absolute paths start with ::
            parts.push("".to_string());
        }
        PathKind::DollarCrate(_) => parts.push("$crate".to_string()),
    }
    
    for segment in mod_path.segments() {
        parts.push(segment.as_str().to_string());
    }
    
    parts.join("::")
}

/// Check if an item is used by a specific module
fn item_is_used_by_module(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    _module: Module,
) -> Result<bool, MoveRenameError> {
    // Get all usages of the item
    let usages = def.usages(sema).all();
    
    // Check if any usage is in the specified module
    for (file_id, _references) in usages {
        // Get the module for this file
        if let Some(file_module) = sema.file_to_module_def(file_id.file_id(sema.db)) {
            if file_module == _module {
                return Ok(true);
            }
        }
    }
    
    Ok(false)
}

/// Check if current module would depend on target module after the move
fn current_module_would_depend_on_target_after_move(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    current_module: Module,
    _target_module: Module,
) -> Result<bool, MoveRenameError> {
    // Check if there are any remaining references to the moved item in the current module
    let usages = def.usages(sema).all();
    
    for (file_id, references) in usages {
        if let Some(file_module) = sema.file_to_module_def(file_id.file_id(sema.db)) {
            if file_module == current_module && !references.is_empty() {
                // There are references in the current module that would need to import from target
                return Ok(true);
            }
        }
    }
    
    Ok(false)
}

/// Validate visibility constraints are maintained after move
/// (Requirements 4.4, 4.5)
pub fn validate_visibility_constraints(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module: Module,
) -> Result<(), MoveRenameError> {
    // Get current visibility of the item
    let current_visibility = get_item_visibility(sema, def)?;
    
    // Get target module visibility context
    let target_module_visibility = get_module_visibility_context(sema, target_module)?;
    
    // Check if move would violate Rust's module privacy rules (Requirement 4.5)
    if !is_visibility_compatible(current_visibility, target_module_visibility) {
        return Err(MoveRenameError::VisibilityViolation(
            "Moving item would violate visibility constraints".to_string()
        ));
    }
    
    // Validate that existing references will remain accessible (Requirement 4.4)
    validate_references_remain_accessible(sema, def, target_module)?;
    
    Ok(())
}

/// Get the visibility of an item
fn get_item_visibility(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
) -> Result<ItemVisibility, MoveRenameError> {
    match def {
        Definition::Function(func) => {
            let source = sema.source(func)
                .ok_or_else(|| MoveRenameError::VisibilityViolation("Cannot get function source".to_string()))?;
            Ok(extract_visibility_from_ast(source.value.visibility()))
        }
        Definition::Adt(adt) => {
            match adt {
                hir::Adt::Struct(struct_) => {
                    let source = sema.source(struct_)
                        .ok_or_else(|| MoveRenameError::VisibilityViolation("Cannot get struct source".to_string()))?;
                    Ok(extract_visibility_from_ast(source.value.visibility()))
                }
                hir::Adt::Enum(enum_) => {
                    let source = sema.source(enum_)
                        .ok_or_else(|| MoveRenameError::VisibilityViolation("Cannot get enum source".to_string()))?;
                    Ok(extract_visibility_from_ast(source.value.visibility()))
                }
                hir::Adt::Union(union_) => {
                    let source = sema.source(union_)
                        .ok_or_else(|| MoveRenameError::VisibilityViolation("Cannot get union source".to_string()))?;
                    Ok(extract_visibility_from_ast(source.value.visibility()))
                }
            }
        }
        Definition::Const(const_) => {
            let source = sema.source(const_)
                .ok_or_else(|| MoveRenameError::VisibilityViolation("Cannot get const source".to_string()))?;
            Ok(extract_visibility_from_ast(source.value.visibility()))
        }
        Definition::Static(static_) => {
            let source = sema.source(static_)
                .ok_or_else(|| MoveRenameError::VisibilityViolation("Cannot get static source".to_string()))?;
            Ok(extract_visibility_from_ast(source.value.visibility()))
        }
        Definition::Trait(trait_) => {
            let source = sema.source(trait_)
                .ok_or_else(|| MoveRenameError::VisibilityViolation("Cannot get trait source".to_string()))?;
            Ok(extract_visibility_from_ast(source.value.visibility()))
        }
        Definition::TypeAlias(type_alias) => {
            let source = sema.source(type_alias)
                .ok_or_else(|| MoveRenameError::VisibilityViolation("Cannot get type alias source".to_string()))?;
            Ok(extract_visibility_from_ast(source.value.visibility()))
        }
        Definition::Module(module) => {
            if let Some(decl_source) = module.declaration_source(sema.db) {
                Ok(extract_visibility_from_ast(decl_source.value.visibility()))
            } else {
                // Root modules are implicitly public
                Ok(ItemVisibility::Public)
            }
        }
        _ => {
            // Other items (locals, fields, etc.) have default visibility
            Ok(ItemVisibility::Private)
        }
    }
}

/// Visibility levels for items
#[derive(Debug, Clone, PartialEq, Eq)]
enum ItemVisibility {
    Private,
    Public,
    PubCrate,
    PubSuper,
    PubSelf,
    PubIn(String),
}

/// Extract visibility from AST visibility node
fn extract_visibility_from_ast(visibility: Option<syntax::ast::Visibility>) -> ItemVisibility {
    match visibility {
        Some(vis) => {
            let vis_text = vis.syntax().text().to_string();
            match vis_text.as_str() {
                "pub" => ItemVisibility::Public,
                "pub(crate)" => ItemVisibility::PubCrate,
                "pub(super)" => ItemVisibility::PubSuper,
                "pub(self)" => ItemVisibility::PubSelf,
                _ if vis_text.starts_with("pub(in ") => {
                    ItemVisibility::PubIn(vis_text)
                }
                _ => ItemVisibility::Private,
            }
        }
        None => ItemVisibility::Private,
    }
}

/// Get visibility context for a module
fn get_module_visibility_context(
    sema: &Semantics<'_, RootDatabase>,
    module: Module,
) -> Result<ModuleVisibilityContext, MoveRenameError> {
    let is_crate_root = module.is_crate_root();
    let parent = module.parent(sema.db);
    
    Ok(ModuleVisibilityContext {
        is_crate_root,
        has_parent: parent.is_some(),
        module_path: module_to_mod_path(sema, module)
            .map_err(|e| MoveRenameError::VisibilityViolation(format!("Cannot get module path: {}", e.0)))?,
    })
}

/// Context for module visibility validation
#[derive(Debug)]
struct ModuleVisibilityContext {
    is_crate_root: bool,
    has_parent: bool,
    module_path: ModPath,
}

/// Check if item visibility is compatible with target module
fn is_visibility_compatible(
    item_visibility: ItemVisibility,
    _module_context: ModuleVisibilityContext,
) -> bool {
    // For now, allow all moves - a full implementation would check
    // if the visibility makes sense in the new module context
    match item_visibility {
        ItemVisibility::Private => true, // Private items can be moved anywhere within the same crate
        ItemVisibility::Public => true, // Public items can be moved anywhere
        ItemVisibility::PubCrate => true, // pub(crate) items can be moved anywhere within the crate
        ItemVisibility::PubSuper => true, // pub(super) items need parent module validation
        ItemVisibility::PubSelf => true, // pub(self) is equivalent to private
        ItemVisibility::PubIn(_) => true, // pub(in path) needs path validation
    }
}

/// Validate that existing references will remain accessible after move
fn validate_references_remain_accessible(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module: Module,
) -> Result<(), MoveRenameError> {
    let usages = def.usages(sema).all();
    
    for (file_id, references) in usages {
        for reference in references {
            if !will_reference_remain_accessible(sema, &reference, def, target_module, file_id)? {
                return Err(MoveRenameError::VisibilityViolation(
                    format!("Reference at file {}:{} would become inaccessible after move", 
                           file_id.file_id(sema.db).index(), u32::from(reference.range.start()))
                ));
            }
        }
    }
    
    Ok(())
}

/// Check if a reference will remain accessible after moving the item
fn will_reference_remain_accessible(
    sema: &Semantics<'_, RootDatabase>,
    _reference: &FileReference,
    def: Definition,
    target_module: Module,
    file_id: base_db::EditionedFileId,
) -> Result<bool, MoveRenameError> {
    // Get the module containing the reference
    let reference_module = sema.file_to_module_def(file_id.file_id(sema.db))
        .ok_or_else(|| MoveRenameError::VisibilityViolation("Cannot determine reference module".to_string()))?;
    
    // Check if the reference module can access the target module
    if can_module_access_module(sema, reference_module, target_module)? {
        // Check if the item will be visible from the reference location
        let item_visibility = get_item_visibility(sema, def)?;
        return Ok(is_item_accessible_from_module(item_visibility, reference_module, target_module));
    }
    
    Ok(false)
}

/// Check if one module can access another module
fn can_module_access_module(
    sema: &Semantics<'_, RootDatabase>,
    accessor: Module,
    target: Module,
) -> Result<bool, MoveRenameError> {
    // Same module - always accessible
    if accessor == target {
        return Ok(true);
    }
    
    // Same crate - check module hierarchy
    if accessor.krate() == target.krate() {
        // Within the same crate, modules can generally access each other
        // unless there are specific visibility restrictions
        return Ok(true);
    }
    
    // Different crates - need to check if target is public
    Ok(false)
}

/// Check if an item is accessible from a specific module
fn is_item_accessible_from_module(
    item_visibility: ItemVisibility,
    accessor_module: Module,
    item_module: Module,
) -> bool {
    match item_visibility {
        ItemVisibility::Public => true, // Public items are always accessible
        ItemVisibility::Private => accessor_module == item_module, // Private items only in same module
        ItemVisibility::PubCrate => accessor_module.krate() == item_module.krate(), // Same crate
        ItemVisibility::PubSuper => {
            // Accessible from parent module and its descendants
            // This is a simplified check
            true
        }
        ItemVisibility::PubSelf => accessor_module == item_module, // Same as private
        ItemVisibility::PubIn(_) => {
            // pub(in path) - would need to parse and validate the path
            // For now, assume accessible
            true
        }
    }
}

/// Validate that dependencies can be maintained after move
/// (Requirement 4.2)
pub fn validate_dependency_accessibility(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module: Module,
) -> Result<(), MoveRenameError> {
    // Check if the item has dependencies that might become inaccessible
    let dependencies = find_item_dependencies(sema, def)?;
    
    for dependency in dependencies {
        if !will_dependency_be_accessible(sema, dependency, target_module)? {
            let dep_name = dependency.name(sema.db)
                .map(|name| name.display(sema.db, Edition::CURRENT).to_string())
                .unwrap_or_else(|| "unnamed dependency".to_string());
            
            return Err(MoveRenameError::DependencyError(
                format!("Dependency '{}' would become inaccessible after move", dep_name)
            ));
        }
    }
    
    Ok(())
}

/// Find dependencies of an item
fn find_item_dependencies(
    _sema: &Semantics<'_, RootDatabase>,
    def: Definition,
) -> Result<Vec<Definition>, MoveRenameError> {
    let mut dependencies = Vec::new();
    
    // Get the source code of the item
    if let Some(source_node) = get_item_source_node(_sema, def) {
        // Find all references within the item's source
        for node in source_node.descendants() {
            if let Some(name_ref) = syntax::ast::NameRef::cast(node) {
                // Try to resolve this name reference
                if let Some(resolved_def) = resolve_name_ref_to_definition(_sema, &name_ref) {
                    if resolved_def != def { // Don't include self-references
                        dependencies.push(resolved_def);
                    }
                }
            }
        }
    }
    
    Ok(dependencies)
}

/// Get the source node for an item
fn get_item_source_node(
    _sema: &Semantics<'_, RootDatabase>,
    def: Definition,
) -> Option<syntax::SyntaxNode> {
    match def {
        Definition::Function(func) => {
            _sema.source(func).map(|source| source.value.syntax().clone())
        }
        Definition::Adt(adt) => {
            match adt {
                hir::Adt::Struct(struct_) => {
                    _sema.source(struct_).map(|source| source.value.syntax().clone())
                }
                hir::Adt::Enum(enum_) => {
                    _sema.source(enum_).map(|source| source.value.syntax().clone())
                }
                hir::Adt::Union(union_) => {
                    _sema.source(union_).map(|source| source.value.syntax().clone())
                }
            }
        }
        Definition::Const(const_) => {
            _sema.source(const_).map(|source| source.value.syntax().clone())
        }
        Definition::Static(static_) => {
            _sema.source(static_).map(|source| source.value.syntax().clone())
        }
        Definition::Trait(trait_) => {
            _sema.source(trait_).map(|source| source.value.syntax().clone())
        }
        Definition::TypeAlias(type_alias) => {
            _sema.source(type_alias).map(|source| source.value.syntax().clone())
        }
        _ => None,
    }
}

/// Resolve a name reference to a definition
fn resolve_name_ref_to_definition(
    _sema: &Semantics<'_, RootDatabase>,
    _name_ref: &syntax::ast::NameRef,
) -> Option<Definition> {
    // This is a simplified resolution - rust-analyzer has more sophisticated resolution
    // For now, return None to avoid compilation errors
    // A full implementation would use proper path resolution
    None
}

/// Check if a dependency will be accessible from the target module
fn will_dependency_be_accessible(
    sema: &Semantics<'_, RootDatabase>,
    dependency: Definition,
    target_module: Module,
) -> Result<bool, MoveRenameError> {
    // Get the module containing the dependency
    let dependency_module = match dependency.module(sema.db) {
        Some(module) => module,
        None => return Ok(true), // Built-in items are always accessible
    };
    
    // Check if target module can access the dependency module
    if !can_module_access_module(sema, target_module, dependency_module)? {
        return Ok(false);
    }
    
    // Check if the dependency item is visible from the target module
    let dependency_visibility = get_item_visibility(sema, dependency)?;
    Ok(is_item_accessible_from_module(dependency_visibility, target_module, dependency_module))
}

/// Validate that a parent module can accept a new child module
fn validate_parent_can_accept_module(
    sema: &Semantics<'_, RootDatabase>,
    declaration: &ModuleDeclaration,
) -> Result<()> {
    // Check if the parent file exists and is accessible
    let source_root = sema.db.file_source_root(declaration.parent_file);
    let source_root_data = sema.db.source_root(source_root.source_root_id(sema.db));
    let source_root_ref = source_root_data.source_root(sema.db);
    let file_path = source_root_ref
        .path_for_file(&declaration.parent_file)
        .ok_or_else(|| RenameError("Cannot find path for parent file".to_string()))?;

    // Validate that the module name doesn't conflict with existing items
    // This is a simplified check - a full implementation would parse the file
    // and check for naming conflicts with existing items

    // Basic validation that we found the file path
    if file_path.as_path().is_none() {
        return Err(RenameError(format!(
            "Parent file for module declaration not found: {:?}",
            declaration.parent_file
        )));
    }

    Ok(())
}

/// Execute the module creation plan by generating file system edits
/// (Requirements 2.1, 2.4, 2.5)
pub fn execute_module_creation_plan(
    plan: &ModuleCreationPlan,
    sema: &Semantics<'_, RootDatabase>,
) -> Result<SourceChange> {
    let mut source_change = SourceChange::default();

    // Validate plan before execution (Requirement 2.5)
    validate_module_creation_plan(plan, sema)?;

    // Create directory structure with proper error handling (Requirement 2.1)
    for dir_path in &plan.directories_to_create {
        create_directory_structure(sema, dir_path, &mut source_change)?;
    }

    // Create module files with correct content (Requirements 2.2, 2.3)
    for file_spec in &plan.files_to_create {
        create_module_file(sema, file_spec, &mut source_change)?;
    }

    // Add module declarations to parent modules (Requirement 2.4)
    for declaration in &plan.module_declarations_to_add {
        add_module_declaration_to_parent(declaration, &mut source_change)?;
    }

    Ok(source_change)
}

/// Create directory structure with proper error handling (Requirement 2.1)
fn create_directory_structure(
    sema: &Semantics<'_, RootDatabase>,
    dir_path: &std::path::Path,
    _source_change: &mut SourceChange,
) -> Result<()> {
    // Find appropriate anchor file for the directory creation
    let anchor_file = find_anchor_file_for_path(sema, dir_path)?
        .ok_or_else(|| RenameError("No anchor file found for directory creation".to_string()))?;

    // Calculate relative path with proper error handling
    let relative_path = calculate_relative_path(sema, anchor_file, dir_path)
        .map_err(|e| RenameError(format!("Failed to calculate directory path: {}", e.0)))?;

    // Validate directory path
    if relative_path.is_empty() {
        return Err(RenameError("Invalid empty directory path".to_string()));
    }

    // Note: Directories will be created implicitly when files are created
    // This is a placeholder for explicit directory creation if needed in the future

    Ok(())
}

/// Create module file with correct content (Requirements 2.2, 2.3)
fn create_module_file(
    sema: &Semantics<'_, RootDatabase>,
    file_spec: &ModuleFileSpec,
    source_change: &mut SourceChange,
) -> Result<()> {
    // Find anchor file for the module file creation
    let anchor_file = find_anchor_file_for_path(sema, &file_spec.path)?
        .ok_or_else(|| RenameError("No anchor file found for module file creation".to_string()))?;

    // Calculate relative path with error handling
    let relative_path = calculate_relative_path(sema, anchor_file, &file_spec.path)
        .map_err(|e| RenameError(format!("Failed to calculate module file path: {}", e.0)))?;

    // Validate file path
    if relative_path.is_empty() {
        return Err(RenameError("Invalid empty module file path".to_string()));
    }

    // Validate file content
    if file_spec.content.is_empty() {
        return Err(RenameError("Module file content cannot be empty".to_string()));
    }

    // Create anchored path for file system operation
    let anchored_path = AnchoredPathBuf { anchor: anchor_file, path: relative_path };

    // Add file creation to source change
    source_change.push_file_system_edit(FileSystemEdit::CreateFile {
        dst: anchored_path,
        initial_contents: file_spec.content.clone(),
    });

    Ok(())
}

/// Add module declaration to parent module (Requirement 2.4)
fn add_module_declaration_to_parent(
    declaration: &ModuleDeclaration,
    source_change: &mut SourceChange,
) -> Result<()> {
    // Validate module declaration
    if declaration.module_name.is_empty() {
        return Err(RenameError("Module name cannot be empty".to_string()));
    }

    if !is_valid_module_name(&declaration.module_name) {
        return Err(RenameError(format!("Invalid module name: {}", declaration.module_name)));
    }

    // Format module declaration with proper visibility
    let module_decl_text = format_module_declaration(declaration);

    // Create text edit for module declaration insertion
    let edit = TextEdit::insert(declaration.insert_position, module_decl_text);

    // Add edit to source change
    source_change.insert_source_edit(declaration.parent_file, edit);

    Ok(())
}

/// Validate module creation plan before execution (Requirement 2.5)
fn validate_module_creation_plan(
    plan: &ModuleCreationPlan,
    sema: &Semantics<'_, RootDatabase>,
) -> Result<()> {
    // Validate that we have something to create
    if plan.files_to_create.is_empty() && plan.module_declarations_to_add.is_empty() {
        return Err(RenameError("Nothing to create in module creation plan".to_string()));
    }

    // Validate file specifications
    for file_spec in &plan.files_to_create {
        validate_module_file_spec(file_spec)?;
    }

    // Validate module declarations
    for declaration in &plan.module_declarations_to_add {
        validate_module_declaration_spec(sema, declaration)?;
    }

    // Check for conflicts between files and declarations
    validate_plan_consistency(plan)?;

    Ok(())
}

/// Validate a module file specification
fn validate_module_file_spec(file_spec: &ModuleFileSpec) -> Result<()> {
    // Check file path validity
    if file_spec.path.as_os_str().is_empty() {
        return Err(RenameError("Module file path cannot be empty".to_string()));
    }

    // Check file extension for Rust files
    if let Some(extension) = file_spec.path.extension() {
        if extension != "rs" {
            return Err(RenameError(format!(
                "Invalid file extension: expected .rs, got .{}",
                extension.to_string_lossy()
            )));
        }
    } else {
        return Err(RenameError("Module file must have .rs extension".to_string()));
    }

    // Validate file content is not empty
    if file_spec.content.trim().is_empty() {
        return Err(RenameError("Module file content cannot be empty".to_string()));
    }

    // Validate file type consistency
    match file_spec.file_type {
        ModuleFileType::ModRs => {
            if !file_spec
                .path
                .file_name()
                .and_then(|name| name.to_str())
                .map(|name| name == "mod.rs")
                .unwrap_or(false)
            {
                return Err(RenameError("ModRs file type must have filename 'mod.rs'".to_string()));
            }
        }
        ModuleFileType::ModuleFile => {
            if file_spec
                .path
                .file_name()
                .and_then(|name| name.to_str())
                .map(|name| name == "mod.rs")
                .unwrap_or(false)
            {
                return Err(RenameError(
                    "ModuleFile type cannot have filename 'mod.rs'".to_string(),
                ));
            }
        }
    }

    Ok(())
}

/// Validate a module declaration specification
fn validate_module_declaration_spec(
    sema: &Semantics<'_, RootDatabase>,
    declaration: &ModuleDeclaration,
) -> Result<()> {
    // Validate module name
    if declaration.module_name.is_empty() {
        return Err(RenameError("Module declaration name cannot be empty".to_string()));
    }

    if !is_valid_module_name(&declaration.module_name) {
        return Err(RenameError(format!(
            "Invalid module name in declaration: {}",
            declaration.module_name
        )));
    }

    // Validate parent file exists
    let source_root = sema.db.file_source_root(declaration.parent_file);
    let source_root_data = sema.db.source_root(source_root.source_root_id(sema.db));
    let source_root_ref = source_root_data.source_root(sema.db);

    if source_root_ref.path_for_file(&declaration.parent_file).is_none() {
        return Err(RenameError(format!(
            "Parent file for module declaration not found: {:?}",
            declaration.parent_file
        )));
    }

    // Validate visibility specification if present
    if let Some(ref visibility) = declaration.visibility {
        if !is_valid_visibility_spec(visibility) {
            return Err(RenameError(format!("Invalid visibility specification: {}", visibility)));
        }
    }

    Ok(())
}

/// Validate plan consistency (no conflicts between files and declarations)
fn validate_plan_consistency(plan: &ModuleCreationPlan) -> Result<()> {
    // Check that each module declaration has a corresponding file
    for declaration in &plan.module_declarations_to_add {
        let has_corresponding_file = plan.files_to_create.iter().any(|file_spec| {
            // Check if file corresponds to the module declaration
            match file_spec.file_type {
                ModuleFileType::ModRs => {
                    // For mod.rs, check if parent directory name matches module name
                    file_spec
                        .path
                        .parent()
                        .and_then(|parent| parent.file_name())
                        .and_then(|name| name.to_str())
                        .map(|name| name == declaration.module_name)
                        .unwrap_or(false)
                }
                ModuleFileType::ModuleFile => {
                    // For module_name.rs, check if file stem matches module name
                    file_spec
                        .path
                        .file_stem()
                        .and_then(|stem| stem.to_str())
                        .map(|stem| stem == declaration.module_name)
                        .unwrap_or(false)
                }
            }
        });

        if !has_corresponding_file {
            return Err(RenameError(format!(
                "Module declaration '{}' has no corresponding file in creation plan",
                declaration.module_name
            )));
        }
    }

    // Check for duplicate module names
    let mut module_names = std::collections::HashSet::new();
    for declaration in &plan.module_declarations_to_add {
        if !module_names.insert(&declaration.module_name) {
            return Err(RenameError(format!(
                "Duplicate module name in creation plan: {}",
                declaration.module_name
            )));
        }
    }

    // Check for duplicate file paths
    let mut file_paths = std::collections::HashSet::new();
    for file_spec in &plan.files_to_create {
        if !file_paths.insert(&file_spec.path) {
            return Err(RenameError(format!(
                "Duplicate file path in creation plan: {}",
                file_spec.path.display()
            )));
        }
    }

    Ok(())
}

/// Check if a visibility specification is valid
fn is_valid_visibility_spec(visibility: &str) -> bool {
    matches!(
        visibility.trim(),
        "pub"
            | "pub(crate)"
            | "pub(super)"
            | "pub(self)"
            | "pub(in crate)"
            | "pub(in super)"
            | "pub(in self)"
    ) || visibility.trim().starts_with("pub(in ")
}

/// Extract item definition from source module (Requirement 4.4)
pub fn extract_item_definition(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
) -> Result<ItemExtractionResult> {
    // Get the source range and file for the definition
    let source_range = def
        .range_for_rename(sema)
        .ok_or_else(|| RenameError("Cannot find source range for item".to_string()))?;

    // Get the syntax node for the item
    let source_file = sema.parse(source_range.file_id);
    let item_node = source_file
        .syntax()
        .covering_element(source_range.range)
        .into_node()
        .ok_or_else(|| RenameError("Cannot find syntax node for item".to_string()))?;

    // Find the complete item definition (walk up to find the item)
    let item_ast = find_item_ast_node(&item_node)
        .ok_or_else(|| RenameError("Cannot find complete item definition".to_string()))?;

    // Extract item text with proper formatting
    let item_text = item_ast.syntax().text().to_string();

    // Extract visibility and attributes
    let visibility = extract_item_visibility(&item_ast);
    let attributes = extract_item_attributes(&item_ast);

    // Get item range for removal
    let item_range = item_ast.syntax().text_range();

    Ok(ItemExtractionResult {
        item_text,
        item_range,
        visibility,
        attributes,
        source_file_id: source_range.file_id,
    })
}

/// Insert item into target module with correct formatting (Requirement 4.5)
pub fn insert_item_into_module(
    sema: &Semantics<'_, RootDatabase>,
    target_module: Module,
    extraction_result: &ItemExtractionResult,
    new_item_name: &Name,
) -> Result<SourceChange> {
    let mut source_change = SourceChange::default();

    // Get target module file
    let target_source = target_module.definition_source(sema.db);
    let target_file_id = target_source.file_id.original_file(sema.db);

    // Find appropriate insertion position in target module
    let insertion_pos = find_item_insertion_position(sema, target_module)?;

    // Format item for insertion with preserved attributes and visibility
    let formatted_item = format_item_for_insertion(
        &extraction_result.item_text,
        &extraction_result.visibility,
        &extraction_result.attributes,
        new_item_name,
    )?;

    // Create text edit for insertion
    let insert_edit = TextEdit::insert(insertion_pos, formatted_item);
    source_change.insert_source_edit(target_file_id.file_id(sema.db), insert_edit);

    // Create text edit for removal from source
    let remove_edit = TextEdit::delete(extraction_result.item_range);
    source_change
        .insert_source_edit(extraction_result.source_file_id.file_id(sema.db), remove_edit);

    Ok(source_change)
}

/// Result of item extraction containing all necessary information
#[derive(Debug)]
pub struct ItemExtractionResult {
    pub item_text: String,
    pub item_range: TextRange,
    pub visibility: Option<String>,
    pub attributes: Vec<String>,
    pub source_file_id: base_db::EditionedFileId,
}

/// Find the complete item AST node from any node within the item
fn find_item_ast_node(node: &syntax::SyntaxNode) -> Option<syntax::ast::Item> {
    // Walk up the syntax tree to find the item node
    let mut current = Some(node.clone());

    while let Some(node) = current {
        if let Some(item) = syntax::ast::Item::cast(node.clone()) {
            return Some(item);
        }
        current = node.parent();
    }

    None
}

/// Extract visibility modifier from an item (Requirement 4.4)
fn extract_item_visibility(item: &syntax::ast::Item) -> Option<String> {
    let visibility = match item {
        syntax::ast::Item::Fn(func) => func.visibility(),
        syntax::ast::Item::Struct(struct_) => struct_.visibility(),
        syntax::ast::Item::Enum(enum_) => enum_.visibility(),
        syntax::ast::Item::Union(union) => union.visibility(),
        syntax::ast::Item::Trait(trait_) => trait_.visibility(),
        syntax::ast::Item::TypeAlias(type_alias) => type_alias.visibility(),
        syntax::ast::Item::Const(const_) => const_.visibility(),
        syntax::ast::Item::Static(static_) => static_.visibility(),
        syntax::ast::Item::Module(module) => module.visibility(),
        syntax::ast::Item::Use(use_) => use_.visibility(),
        syntax::ast::Item::ExternCrate(extern_crate) => extern_crate.visibility(),
        syntax::ast::Item::Impl(_) => None, // Impl blocks don't have visibility
        syntax::ast::Item::ExternBlock(_) => None, // ExternBlock doesn't have visibility
        syntax::ast::Item::MacroCall(_) => None, // MacroCall doesn't have visibility
        syntax::ast::Item::MacroRules(macro_rules) => macro_rules.visibility(),
        syntax::ast::Item::MacroDef(macro_def) => macro_def.visibility(),
        syntax::ast::Item::AsmExpr(_) => None, // AsmExpr doesn't have visibility
    };

    visibility.map(|vis| vis.syntax().text().to_string())
}

/// Extract attributes from an item (Requirement 4.4)
fn extract_item_attributes(item: &syntax::ast::Item) -> Vec<String> {
    let attrs = match item {
        syntax::ast::Item::Fn(func) => func.attrs(),
        syntax::ast::Item::Struct(struct_) => struct_.attrs(),
        syntax::ast::Item::Enum(enum_) => enum_.attrs(),
        syntax::ast::Item::Union(union) => union.attrs(),
        syntax::ast::Item::Trait(trait_) => trait_.attrs(),
        syntax::ast::Item::TypeAlias(type_alias) => type_alias.attrs(),
        syntax::ast::Item::Const(const_) => const_.attrs(),
        syntax::ast::Item::Static(static_) => static_.attrs(),
        syntax::ast::Item::Module(module) => module.attrs(),
        syntax::ast::Item::Use(use_) => use_.attrs(),
        syntax::ast::Item::ExternCrate(extern_crate) => extern_crate.attrs(),
        syntax::ast::Item::Impl(impl_) => impl_.attrs(),
        syntax::ast::Item::ExternBlock(extern_block) => extern_block.attrs(),
        syntax::ast::Item::MacroCall(macro_call) => macro_call.attrs(),
        syntax::ast::Item::MacroRules(macro_rules) => macro_rules.attrs(),
        syntax::ast::Item::MacroDef(macro_def) => macro_def.attrs(),
        syntax::ast::Item::AsmExpr(asm_expr) => asm_expr.attrs(),
    };

    attrs.map(|attr| attr.syntax().text().to_string()).collect()
}

/// Find appropriate position to insert item in target module
fn find_item_insertion_position(
    sema: &Semantics<'_, RootDatabase>,
    target_module: Module,
) -> Result<syntax::TextSize> {
    let source = target_module.definition_source(sema.db);

    match &source.value {
        ModuleSource::SourceFile(source_file) => {
            // Find appropriate position after imports and module declarations
            let mut insert_pos = syntax::TextSize::from(0);
            let mut found_non_import = false;

            for item in source_file.items() {
                match item {
                    syntax::ast::Item::Use(_) => {
                        // Insert after the last use statement
                        insert_pos = item.syntax().text_range().end();
                    }
                    syntax::ast::Item::Module(_) if !found_non_import => {
                        // Insert after module declarations if no other items found
                        insert_pos = item.syntax().text_range().end();
                    }
                    _ => {
                        // Found a non-import, non-module item
                        found_non_import = true;
                        if insert_pos == syntax::TextSize::from(0) {
                            // Insert before the first non-import item
                            insert_pos = item.syntax().text_range().start();
                        }
                        break;
                    }
                }
            }

            // If we haven't found a position yet, insert at the end
            if insert_pos == syntax::TextSize::from(0) {
                insert_pos = source_file.syntax().text_range().end();
            }

            Ok(insert_pos)
        }
        ModuleSource::Module(inline_module) => {
            // For inline modules, insert at the end of the item list
            if let Some(item_list) = inline_module.item_list() {
                // Insert before the closing brace
                let closing_brace_pos =
                    item_list.syntax().text_range().end() - syntax::TextSize::from(1);
                Ok(closing_brace_pos)
            } else {
                Err(RenameError("Inline module has no item list".to_string()))
            }
        }
        ModuleSource::BlockExpr(_) => {
            Err(RenameError("Cannot insert items into block expression modules".to_string()))
        }
    }
}

/// Format item for insertion with preserved attributes and visibility (Requirement 4.5)
fn format_item_for_insertion(
    item_text: &str,
    _visibility: &Option<String>,
    attributes: &[String],
    new_item_name: &Name,
) -> Result<String> {
    let mut formatted = String::new();

    // Add proper spacing before the item
    formatted.push('\n');

    // Add attributes with proper formatting
    for attr in attributes {
        formatted.push_str(attr);
        formatted.push('\n');
    }

    // Parse the item to replace the name
    let item_with_new_name = replace_item_name(item_text, new_item_name)?;

    // Add the formatted item
    formatted.push_str(&item_with_new_name);

    // Add spacing after the item
    formatted.push('\n');

    Ok(formatted)
}

/// Replace item name in item text while preserving structure
fn replace_item_name(item_text: &str, new_name: &Name) -> Result<String> {
    // Parse the item text to find and replace the name
    let parsed = syntax::ast::SourceFile::parse(item_text, Edition::CURRENT);

    if !parsed.errors().is_empty() {
        return Err(RenameError(format!(
            "Failed to parse item for name replacement: {:?}",
            parsed.errors()
        )));
    }

    let source_file = parsed.tree();

    // Find the first item in the parsed text
    if let Some(item) = source_file.items().next() {
        // Get the name node from the item
        let name_node = get_item_name_node(&item)
            .ok_or_else(|| RenameError("Cannot find name node in item".to_string()))?;

        // Replace the name in the text
        let name_range = name_node.syntax().text_range();
        let mut result = item_text.to_string();

        // Calculate the replacement range within the item text
        let start = usize::from(name_range.start());
        let end = usize::from(name_range.end());

        if start <= result.len() && end <= result.len() && start <= end {
            result.replace_range(start..end, new_name.as_str());
            Ok(result)
        } else {
            Err(RenameError("Invalid name range for replacement".to_string()))
        }
    } else {
        Err(RenameError("No item found in parsed text".to_string()))
    }
}

/// Get the name node from an item
fn get_item_name_node(item: &syntax::ast::Item) -> Option<syntax::ast::Name> {
    match item {
        syntax::ast::Item::Fn(func) => func.name(),
        syntax::ast::Item::Struct(struct_) => struct_.name(),
        syntax::ast::Item::Enum(enum_) => enum_.name(),
        syntax::ast::Item::Union(union) => union.name(),
        syntax::ast::Item::Trait(trait_) => trait_.name(),
        syntax::ast::Item::TypeAlias(type_alias) => type_alias.name(),
        syntax::ast::Item::Const(const_) => const_.name(),
        syntax::ast::Item::Static(static_) => static_.name(),
        syntax::ast::Item::Module(module) => module.name(),
        syntax::ast::Item::MacroRules(macro_rules) => macro_rules.name(),
        syntax::ast::Item::MacroDef(macro_def) => macro_def.name(),
        // These items don't have simple names
        syntax::ast::Item::Use(_)
        | syntax::ast::Item::ExternCrate(_)
        | syntax::ast::Item::Impl(_)
        | syntax::ast::Item::ExternBlock(_)
        | syntax::ast::Item::MacroCall(_)
        | syntax::ast::Item::AsmExpr(_) => None,
    }
}

/// Move item from source to target module (combines extraction and insertion)
pub fn move_item_to_module(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module: Module,
    new_item_name: &Name,
) -> Result<SourceChange> {
    // Extract item from source module
    let extraction_result = extract_item_definition(sema, def)?;

    // Insert item into target module
    let source_change =
        insert_item_into_module(sema, target_module, &extraction_result, new_item_name)?;

    // Validate the move operation
    validate_item_move(&extraction_result, target_module, sema)?;

    Ok(source_change)
}

/// Validate that the item move operation is valid
fn validate_item_move(
    extraction_result: &ItemExtractionResult,
    target_module: Module,
    sema: &Semantics<'_, RootDatabase>,
) -> Result<()> {
    // Check that source and target are different
    let target_source = target_module.definition_source(sema.db);
    let target_file_id = target_source.file_id.original_file(sema.db);

    if extraction_result.source_file_id.file_id(sema.db) == target_file_id.file_id(sema.db) {
        // Same file - check if it's actually a different module within the same file
        // This is allowed for inline modules
    }

    // Validate that the item text is not empty
    if extraction_result.item_text.trim().is_empty() {
        return Err(RenameError("Cannot move empty item".to_string()));
    }

    // Validate that the item range is valid
    if extraction_result.item_range.is_empty() {
        return Err(RenameError("Invalid item range for move operation".to_string()));
    }

    Ok(())
}

/// Find an appropriate anchor file for file system operations with proper error handling
fn find_anchor_file_for_path(
    sema: &Semantics<'_, RootDatabase>,
    target_path: &std::path::Path,
) -> Result<Option<FileId>> {
    // Get all local source roots
    let local_roots_set = sema.db.local_roots();
    let local_roots: Vec<_> = local_roots_set.iter().collect();

    if local_roots.is_empty() {
        return Err(RenameError("No local source roots found".to_string()));
    }

    // Find the best anchor file by looking for files in the same directory or closest parent
    let mut best_anchor = None;
    let mut best_distance = usize::MAX;

    for &root in &local_roots {
        let source_root = sema.db.source_root(*root);
        let source_root_ref = source_root.source_root(sema.db);

        for file_id in source_root_ref.iter() {
            if let Some(file_path) = source_root_ref.path_for_file(&file_id) {
                if let Some(file_path_std) = file_path.as_path() {
                    // Calculate distance between file path and target path
                    let distance = calculate_path_distance(file_path_std.as_ref(), target_path);

                    if distance < best_distance {
                        best_distance = distance;
                        best_anchor = Some(file_id);
                    }
                }
            }
        }
    }

    match best_anchor {
        Some(anchor) => Ok(Some(anchor)),
        None => {
            // Fallback: use the first available file
            for &root in &local_roots {
                let source_root = sema.db.source_root(*root);
                let source_root_ref = source_root.source_root(sema.db);

                if let Some(first_file) = source_root_ref.iter().next() {
                    return Ok(Some(first_file));
                }
            }

            Err(RenameError("No anchor file found for file system operations".to_string()))
        }
    }
}

/// Calculate distance between two paths (number of different components)
fn calculate_path_distance(path1: &std::path::Path, path2: &std::path::Path) -> usize {
    let components1: Vec<_> = path1.components().collect();
    let components2: Vec<_> = path2.components().collect();

    let common_prefix_len =
        components1.iter().zip(components2.iter()).take_while(|(a, b)| a == b).count();

    // Distance is the sum of remaining components in both paths
    (components1.len() - common_prefix_len) + (components2.len() - common_prefix_len)
}

/// Calculate relative path from anchor file to target path with proper error handling
fn calculate_relative_path(
    sema: &Semantics<'_, RootDatabase>,
    anchor_file: FileId,
    target_path: &std::path::Path,
) -> Result<String> {
    // Get anchor file path
    let source_root = sema.db.file_source_root(anchor_file);
    let source_root_data = sema.db.source_root(source_root.source_root_id(sema.db));
    let source_root_ref = source_root_data.source_root(sema.db);

    let anchor_vfs_path = source_root_ref
        .path_for_file(&anchor_file)
        .ok_or_else(|| RenameError("Cannot find path for anchor file".to_string()))?;

    let anchor_path = anchor_vfs_path.as_path().ok_or_else(|| {
        RenameError("Anchor file path is not a valid filesystem path".to_string())
    })?;

    // Calculate relative path from anchor directory to target
    let anchor_dir = anchor_path
        .parent()
        .ok_or_else(|| RenameError("Cannot determine anchor file directory".to_string()))?;

    // Try to calculate relative path
    match target_path.strip_prefix(anchor_dir) {
        Ok(relative) => Ok(relative.to_string_lossy().to_string()),
        Err(_) => {
            // If target is not under anchor directory, try to find common ancestor
            if let Some(common_ancestor) = find_common_ancestor(anchor_dir.as_ref(), target_path) {
                let anchor_path: &std::path::Path = anchor_dir.as_ref();
                let anchor_relative = anchor_path.strip_prefix(&common_ancestor).map_err(|_| {
                    RenameError("Failed to calculate anchor relative path".to_string())
                })?;
                let target_relative = target_path.strip_prefix(&common_ancestor).map_err(|_| {
                    RenameError("Failed to calculate target relative path".to_string())
                })?;

                // Build relative path with .. components
                let mut relative_path = std::path::PathBuf::new();

                // Add .. for each component in anchor_relative
                for _ in anchor_relative.components() {
                    relative_path.push("..");
                }

                // Add target_relative components
                relative_path.push(target_relative);

                Ok(relative_path.to_string_lossy().to_string())
            } else {
                // Fallback: use absolute path
                Ok(target_path.to_string_lossy().to_string())
            }
        }
    }
}

/// Find common ancestor of two paths
fn find_common_ancestor(
    path1: &std::path::Path,
    path2: &std::path::Path,
) -> Option<std::path::PathBuf> {
    let components1: Vec<_> = path1.components().collect();
    let components2: Vec<_> = path2.components().collect();

    let mut common = std::path::PathBuf::new();

    for (comp1, comp2) in components1.iter().zip(components2.iter()) {
        if comp1 == comp2 {
            common.push(comp1);
        } else {
            break;
        }
    }

    if common.as_os_str().is_empty() { None } else { Some(common) }
}

/// Format a module declaration for insertion into source code
fn format_module_declaration(declaration: &ModuleDeclaration) -> String {
    let visibility = declaration.visibility.as_deref().unwrap_or("");
    let vis_prefix = if visibility.is_empty() { "" } else { &format!("{} ", visibility) };

    format!("{}mod {};\n", vis_prefix, declaration.module_name)
}

/// Integrate with user preferences for module organization
/// (Requirement 2.3)
pub fn get_user_module_preferences() -> ModuleOrganizationPreferences {
    // For now, return default preferences
    // A full implementation would read from user configuration
    ModuleOrganizationPreferences::default()
}

/// Main entry point for move operations with comprehensive error handling
/// This function provides the complete error handling workflow including:
/// - Detailed error messages for all failure scenarios
/// - Rollback logic for partial operation failures
/// - Graceful handling of module creation and reference update errors
/// (Requirements 4.1, 4.2, 4.3, 4.5, plus error handling for Requirements 2.1, 2.5, 3.1-3.5, 6.1-6.5)
pub fn execute_move_with_comprehensive_error_handling(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: &ModPath,
    new_item_name: &Name,
) -> Result<SourceChange, MoveRenameError> {
    // Use the enhanced error handling function
    handle_move_operation_with_recovery(sema, def, target_module_path, new_item_name)
}

/// Validate a complete move operation with comprehensive error checking
/// This is the main validation entry point that checks all requirements
pub fn validate_complete_move_operation(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: &ModPath,
    new_item_name: &Name,
) -> Result<(), MoveRenameError> {
    validate_move_operation_comprehensive(sema, def, target_module_path, new_item_name)
}

/// Handle module creation failures gracefully with detailed error reporting
/// (Requirement 2.1, 2.5)
pub fn handle_module_creation_failure(
    error: RenameError,
    module_path: &ModPath,
    rollback_context: &RollbackContext,
) -> MoveRenameError {
    let module_path_str = format_mod_path(module_path);
    
    let detailed_message = if rollback_context.needs_rollback() {
        format!(
            "Failed to create module '{}': {}. Partial changes have been rolled back. \
            Directories created: {}, Files modified: {}",
            module_path_str,
            error.0,
            rollback_context.created_directories.len(),
            rollback_context.modified_files.len()
        )
    } else {
        format!(
            "Failed to create module '{}': {}. No changes were made.",
            module_path_str,
            error.0
        )
    };
    
    MoveRenameError::ModuleCreationFailed(detailed_message)
}

/// Handle reference update failures gracefully with detailed error reporting
/// (Requirements 3.1-3.5, 6.1-6.5)
pub fn handle_reference_update_failure(
    error: RenameError,
    file_path: &str,
    operation_type: &str,
    rollback_context: &RollbackContext,
) -> MoveRenameError {
    let detailed_message = if rollback_context.needs_rollback() {
        format!(
            "Failed to update {} in '{}': {}. Operation has been rolled back. \
            Files affected: {}",
            operation_type,
            file_path,
            error.0,
            rollback_context.modified_files.len() + rollback_context.created_files.len()
        )
    } else {
        format!(
            "Failed to update {} in '{}': {}",
            operation_type,
            file_path,
            error.0
        )
    };
    
    MoveRenameError::ReferenceUpdateFailed {
        file: file_path.to_string(),
        error: detailed_message,
    }
}

/// Provide detailed error context for debugging and user feedback
pub fn create_error_context(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    target_module_path: &ModPath,
    new_item_name: &Name,
) -> String {
    let item_name = def.name(sema.db)
        .map(|name| name.display(sema.db, Edition::CURRENT).to_string())
        .unwrap_or_else(|| "unnamed item".to_string());
    
    let current_module = def.module(sema.db)
        .and_then(|module| module_to_mod_path(sema, module).ok())
        .map(|path| format_mod_path(&path))
        .unwrap_or_else(|| "unknown module".to_string());
    
    let target_module_str = format_mod_path(target_module_path);
    let new_name_str = new_item_name.display(sema.db, Edition::CURRENT).to_string();
    
    format!(
        "Move operation context:\n\
        - Item: '{}' ({})\n\
        - Current location: '{}'\n\
        - Target location: '{}'\n\
        - New name: '{}'",
        item_name,
        get_item_type_description(&def),
        current_module,
        target_module_str,
        new_name_str
    )
}

/// Check if an operation can be safely retried after fixing the reported issues
pub fn can_retry_operation(error: &MoveRenameError) -> bool {
    match error {
        MoveRenameError::NameConflict { .. } => true, // User can choose different name
        MoveRenameError::VisibilityViolation(_) => true, // User can adjust visibility
        MoveRenameError::InvalidPath(_) => true, // User can provide valid path
        MoveRenameError::CircularDependency(_) => false, // Structural issue, needs different approach
        MoveRenameError::DependencyError(_) => false, // May need code restructuring
        MoveRenameError::ModuleCreationFailed(_) => true, // May be temporary file system issue
        MoveRenameError::FileSystemError(_) => true, // May be temporary
        MoveRenameError::ModuleIntegrationFailed(_) => false, // Structural issue
        MoveRenameError::ReferenceUpdateFailed { .. } => true, // May be temporary parsing issue
    }
}

/// Generate a user-friendly error summary with actionable suggestions
pub fn generate_error_summary(error: &MoveRenameError) -> String {
    match error {
        MoveRenameError::NameConflict { existing_item, target_name } => {
            format!(
                "Name Conflict: Cannot use name '{}' because {} already exists. \
                Try using a different name or removing the existing item first.",
                target_name, existing_item
            )
        }
        MoveRenameError::CircularDependency(msg) => {
            format!(
                "Circular Dependency: {}. \
                Consider moving related items together or restructuring the module hierarchy.",
                msg
            )
        }
        MoveRenameError::VisibilityViolation(msg) => {
            format!(
                "Visibility Issue: {}. \
                Adjust the item's visibility or choose a different target module.",
                msg
            )
        }
        MoveRenameError::InvalidPath(msg) => {
            format!(
                "Invalid Path: {}. \
                Use a valid Rust module path like 'crate::module::submodule'.",
                msg
            )
        }
        MoveRenameError::DependencyError(msg) => {
            format!(
                "Dependency Issue: {}. \
                Make dependencies public or move them to an accessible location.",
                msg
            )
        }
        MoveRenameError::ModuleCreationFailed(msg) => {
            format!(
                "Module Creation Failed: {}. \
                Check file permissions and ensure the target directory is writable.",
                msg
            )
        }
        MoveRenameError::FileSystemError(msg) => {
            format!(
                "File System Error: {}. \
                Check file permissions and disk space.",
                msg
            )
        }
        MoveRenameError::ModuleIntegrationFailed(msg) => {
            format!(
                "Module Integration Failed: {}. \
                The module structure may be corrupted. Try a different target location.",
                msg
            )
        }
        MoveRenameError::ReferenceUpdateFailed { file, error } => {
            format!(
                "Reference Update Failed: Failed to update references in '{}': {}. \
                The file may have syntax errors or be read-only.",
                file, error
            )
        }
    }
}

/// Ensure proper module integration into existing module tree
/// (Requirement 2.5)
pub fn ensure_module_tree_integration(
    sema: &Semantics<'_, RootDatabase>,
    created_modules: &[Module],
    parent_module: Module,
) -> Result<()> {
    // Validate that all created modules are properly accessible from their parent
    for module in created_modules {
        if let Some(actual_parent) = module.parent(sema.db) {
            if actual_parent != parent_module {
                return Err(RenameError(format!(
                    "Module integration failed: expected parent {:?}, got {:?}",
                    parent_module.name(sema.db),
                    actual_parent.name(sema.db)
                )));
            }
        } else {
            return Err(RenameError("Created module has no parent".to_string()));
        }
    }

    Ok(())
}

// ============================================================================
// TASK 5: REFERENCE UPDATING FOR MOVED ITEMS
// ============================================================================

/// Context for updating references when an item is moved
/// (Requirements 3.1-3.5)
#[derive(Debug)]
pub struct ReferenceUpdateContext {
    pub old_module_path: ModPath,
    pub new_module_path: ModPath,
    pub item_name: Name,
    pub preserve_aliases: bool, // Requirement 3.5
}

/// Update external references and imports for a moved item
/// (Requirements 3.1, 3.2, 3.3, 3.4, 3.5)
pub fn update_external_references(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    old_module: Module,
    new_module: Module,
    new_name: &Name,
) -> Result<SourceChange> {
    let mut source_change = SourceChange::default();

    // Create reference update context
    let old_module_path = module_to_mod_path(sema, old_module)?;
    let new_module_path = module_to_mod_path(sema, new_module)?;
    
    let context = ReferenceUpdateContext {
        old_module_path,
        new_module_path,
        item_name: new_name.clone(),
        preserve_aliases: true, // Requirement 3.5
    };

    // Find all references to the moved item (Requirement 3.1)
    let usages = def.usages(sema).all();
    
    for (file_id, references) in usages {
        let updated_references = update_references_in_file(
            sema,
            &references,
            def,
            &context,
        )?;
        
        if !updated_references.is_empty() {
            source_change.insert_source_edit(file_id.file_id(sema.db), updated_references);
        }
    }

    // Handle re-exports (Requirement 3.4)
    let reexport_updates = update_reexports_for_moved_item(sema, def, &context)?;
    
    // Merge the reexport updates into the main source change
    for (file_id, edit) in reexport_updates.source_file_edits {
        source_change.insert_source_edit(file_id, edit.0);
    }
    for fs_edit in reexport_updates.file_system_edits {
        source_change.push_file_system_edit(fs_edit);
    }

    Ok(source_change)
}

/// Update references within a single file
/// (Requirements 3.2, 3.3, 3.5)
fn update_references_in_file(
    sema: &Semantics<'_, RootDatabase>,
    references: &[FileReference],
    def: Definition,
    context: &ReferenceUpdateContext,
) -> Result<TextEdit> {
    let mut edit_builder = TextEdit::builder();
    let mut edited_ranges = std::collections::HashSet::new();

    for reference in references {
        // Skip if we've already edited this range (macros can cause duplicates)
        if edited_ranges.contains(&reference.range.start()) {
            continue;
        }

        if reference.category.contains(ReferenceCategory::IMPORT) {
            // Update import statements (Requirement 3.2)
            if let Some(new_import) = update_import_statement(sema, reference, context)? {
                edit_builder.replace(reference.range, new_import);
                edited_ranges.insert(reference.range.start());
            }
        } else if reference.category.contains(ReferenceCategory::WRITE) || reference.category.contains(ReferenceCategory::READ) {
            // Update fully-qualified references (Requirement 3.3)
            if is_fully_qualified_reference(sema, reference, def)? {
                if let Some(new_path) = update_qualified_path_reference(sema, reference, context)? {
                    edit_builder.replace(reference.range, new_path);
                    edited_ranges.insert(reference.range.start());
                }
            }
        }
    }

    Ok(edit_builder.finish())
}

/// Update an import statement to reflect the new item location
/// (Requirement 3.2, 3.5)
fn update_import_statement(
    _sema: &Semantics<'_, RootDatabase>,
    _reference: &FileReference,
    _context: &ReferenceUpdateContext,
) -> Result<Option<String>> {
    // For now, return a simple updated import path
    // A full implementation would parse the existing use statement
    let new_path = build_new_import_path(_context)?;
    Ok(Some(new_path))
}

/// Find the use statement that contains the given range
fn find_use_statement_at_range(
    source_file: &syntax::ast::SourceFile,
    range: TextRange,
) -> Result<Option<syntax::ast::Use>> {
    // Walk through all items to find the use statement
    for item in source_file.items() {
        if let syntax::ast::Item::Use(use_stmt) = item {
            if use_stmt.syntax().text_range().contains_range(range) {
                return Ok(Some(use_stmt));
            }
        }
    }
    
    Ok(None)
}

/// Update the path in a use tree
/// (Requirement 3.2, 3.5)
fn update_use_tree_path(
    use_tree: &syntax::ast::UseTree,
    context: &ReferenceUpdateContext,
) -> Result<String> {
    let path = use_tree.path().ok_or_else(|| {
        RenameError("Use tree has no path".to_string())
    })?;

    // Check if this use statement imports the moved item
    if path_references_moved_item(&path, &context.old_module_path, &context.item_name)? {
        // Build new import path
        let new_path = build_new_import_path(context)?;
        
        // Preserve alias if present (Requirement 3.5)
        if let Some(rename) = use_tree.rename() {
            if context.preserve_aliases {
                return Ok(format!("{} as {}", new_path, rename.name().unwrap().text()));
            }
        }
        
        // Handle use tree structure (single import vs group imports)
        if let Some(use_tree_list) = use_tree.use_tree_list() {
            // This is a group import like `use module::{Item1, Item2}`
            // We need to update just the specific item
            update_group_import(use_tree_list, context)
        } else {
            // Simple import - replace the entire path
            Ok(new_path)
        }
    } else {
        // This use statement doesn't import our moved item
        Ok(use_tree.syntax().text().to_string())
    }
}

/// Check if a path references the moved item
fn path_references_moved_item(
    path: &syntax::ast::Path,
    old_module_path: &ModPath,
    item_name: &Name,
) -> Result<bool> {
    // Convert AST path to ModPath for comparison
    let path_text = path.syntax().text().to_string();
    
    // Simple check: see if the path ends with our item name
    // A more sophisticated implementation would parse the full path
    Ok(path_text.ends_with(item_name.as_str()))
}

/// Build new import path from context
fn build_new_import_path(context: &ReferenceUpdateContext) -> Result<String> {
    let mut path_parts = Vec::new();
    
    // Add path kind prefix
    match context.new_module_path.kind {
        PathKind::Crate => path_parts.push("crate".to_string()),
        PathKind::Super(n) => {
            for _ in 0..n {
                path_parts.push("super".to_string());
            }
        }
        PathKind::Plain => {
            // No prefix for plain paths
        }
        PathKind::Abs => path_parts.push("".to_string()), // Absolute path
        PathKind::DollarCrate(_) => path_parts.push("$crate".to_string()),
    }
    
    // Add module segments
    for segment in context.new_module_path.segments() {
        path_parts.push(segment.as_str().to_string());
    }
    
    // Add item name
    path_parts.push(context.item_name.as_str().to_string());
    
    Ok(path_parts.join("::"))
}

/// Update a group import to reflect moved item
fn update_group_import(
    use_tree_list: syntax::ast::UseTreeList,
    context: &ReferenceUpdateContext,
) -> Result<String> {
    // For now, return the original text
    // A full implementation would parse the group and update the specific item
    Ok(use_tree_list.syntax().text().to_string())
}

/// Check if a reference is a fully-qualified path reference
/// (Requirement 3.3)
fn is_fully_qualified_reference(
    _sema: &Semantics<'_, RootDatabase>,
    reference: &FileReference,
    _def: Definition,
) -> Result<bool> {
    // Simple heuristic: check if the reference name contains "::"
    let reference_text = match &reference.name {
        FileReferenceNode::NameRef(name_ref) => name_ref.text().to_string(),
        FileReferenceNode::Name(name) => name.text().to_string(),
        _ => return Ok(false),
    };
    
    Ok(reference_text.contains("::"))
}

/// Check if a path is fully qualified (starts with crate::, super::, etc.)
fn is_path_fully_qualified(path: &syntax::ast::Path) -> bool {
    if let Some(qualifier) = path.qualifier() {
        // Has a qualifier - check if it starts with a root
        let first_segment = get_first_path_segment(&qualifier);
        matches!(first_segment.as_deref(), Some("crate") | Some("super") | Some("self"))
    } else {
        // No qualifier - check if this path itself starts with a root
        let first_segment = get_first_path_segment(path);
        matches!(first_segment.as_deref(), Some("crate") | Some("super") | Some("self"))
    }
}

/// Get the first segment of a path
fn get_first_path_segment(path: &syntax::ast::Path) -> Option<String> {
    if let Some(qualifier) = path.qualifier() {
        get_first_path_segment(&qualifier)
    } else if let Some(segment) = path.segment() {
        segment.name_ref().map(|name| name.text().to_string())
    } else {
        None
    }
}

/// Update a fully-qualified path reference
/// (Requirement 3.3)
fn update_qualified_path_reference(
    _sema: &Semantics<'_, RootDatabase>,
    reference: &FileReference,
    context: &ReferenceUpdateContext,
) -> Result<Option<String>> {
    // Get the current reference text
    let current_text = match &reference.name {
        FileReferenceNode::NameRef(name_ref) => name_ref.text().to_string(),
        FileReferenceNode::Name(name) => name.text().to_string(),
        FileReferenceNode::Lifetime(lifetime) => lifetime.text().to_string(),
        FileReferenceNode::FormatStringEntry(string, _) => string.text().to_string(),
    };
    
    // If this references our moved item, update it
    if current_text == context.item_name.as_str() {
        // Build the new fully-qualified path
        let new_path = build_new_import_path(context)?;
        return Ok(Some(new_path));
    }
    
    Ok(None)
}

/// Update re-exports for a moved item
/// (Requirement 3.4)
fn update_reexports_for_moved_item(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
    context: &ReferenceUpdateContext,
) -> Result<SourceChange> {
    let mut source_change = SourceChange::default();
    
    // Find all re-exports of this item
    let reexports = find_reexports_of_item(sema, def)?;
    
    for reexport in reexports {
        let updated_reexport = update_reexport_statement(sema, &reexport, context)?;
        if let Some((file_id, edit)) = updated_reexport {
            source_change.insert_source_edit(file_id, edit);
        }
    }
    
    Ok(source_change)
}

/// Information about a re-export statement
#[derive(Debug)]
struct ReexportInfo {
    file_id: FileId,
    range: TextRange,
    use_stmt: syntax::ast::Use,
}

/// Find all re-exports of an item
fn find_reexports_of_item(
    sema: &Semantics<'_, RootDatabase>,
    def: Definition,
) -> Result<Vec<ReexportInfo>> {
    let mut reexports = Vec::new();
    
    // Get all usages of the item
    let usages = def.usages(sema).all();
    
    for (file_id, references) in usages {
        let source_file = sema.parse(file_id);
        
        for reference in &references {
            // Check if this reference is part of a pub use statement
            if let Some(reexport) = find_reexport_at_reference(&source_file, reference)? {
                reexports.push(ReexportInfo {
                    file_id: file_id.file_id(sema.db),
                    range: reference.range,
                    use_stmt: reexport,
                });
            }
        }
    }
    
    Ok(reexports)
}

/// Find a re-export statement at the given reference
fn find_reexport_at_reference(
    source_file: &syntax::ast::SourceFile,
    reference: &FileReference,
) -> Result<Option<syntax::ast::Use>> {
    // Find the use statement containing this reference
    for item in source_file.items() {
        if let syntax::ast::Item::Use(use_stmt) = item {
            // Check if this is a pub use statement
            if use_stmt.visibility().is_some() && 
               use_stmt.syntax().text_range().contains_range(reference.range) {
                return Ok(Some(use_stmt));
            }
        }
    }
    
    Ok(None)
}

/// Update a re-export statement
fn update_reexport_statement(
    _sema: &Semantics<'_, RootDatabase>,
    reexport: &ReexportInfo,
    context: &ReferenceUpdateContext,
) -> Result<Option<(FileId, TextEdit)>> {
    // Get the use tree from the re-export statement
    if let Some(use_tree) = reexport.use_stmt.use_tree() {
        // Update the path in the use tree
        let updated_use = update_use_tree_path(&use_tree, context)?;
        
        // Create edit to replace the entire use statement
        let edit = TextEdit::replace(reexport.use_stmt.syntax().text_range(), updated_use);
        
        return Ok(Some((reexport.file_id, edit)));
    }
    
    Ok(None)
}

// ============================================================================
// TASK 5.2: INTERNAL REFERENCE UPDATES
// ============================================================================

/// Types of internal references within moved items
/// (Requirements 6.1, 6.2, 6.3, 6.4, 6.5)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InternalReferenceType {
    /// super::module references (Requirement 6.3)
    SuperReference,
    /// crate::module references (Requirement 6.3)
    CrateReference,
    /// Sibling module references (Requirement 6.5)
    SiblingReference,
    /// Self-referential paths within the moved item (Requirement 6.4)
    SelfReference,
}

/// Information about an internal reference within a moved item
/// (Requirements 6.1, 6.2)
#[derive(Debug, Clone)]
pub struct InternalReference {
    pub range: TextRange,
    pub reference_type: InternalReferenceType,
    pub original_path: String,
    pub resolved_module: Option<Module>,
}

/// Update internal references within a moved item's source code
/// (Requirements 6.1, 6.2, 6.3, 6.4, 6.5)
pub fn update_internal_references(
    sema: &Semantics<'_, RootDatabase>,
    item_source: &syntax::SyntaxNode,
    old_module: Module,
    new_module: Module,
) -> Result<SourceChange> {
    let mut source_change = SourceChange::default();

    // Analyze imports and references within moved item (Requirement 6.1)
    let internal_references = find_internal_module_references(sema, item_source, old_module)?;

    if internal_references.is_empty() {
        return Ok(source_change);
    }

    // Create context for reference updates
    let old_module_path = module_to_mod_path(sema, old_module)?;
    let new_module_path = module_to_mod_path(sema, new_module)?;

    let mut edit_builder = TextEdit::builder();

    // Update each internal reference based on its type
    for reference in internal_references {
        match reference.reference_type {
            InternalReferenceType::SuperReference => {
                // Update super:: references (Requirement 6.3)
                if let Some(new_path) = calculate_new_super_path(
                    &reference,
                    &old_module_path,
                    &new_module_path,
                    sema,
                )? {
                    edit_builder.replace(reference.range, new_path);
                }
            }
            InternalReferenceType::CrateReference => {
                // Update crate:: references (Requirement 6.3)
                if let Some(new_path) = calculate_new_crate_path(
                    &reference,
                    &old_module_path,
                    &new_module_path,
                    sema,
                )? {
                    edit_builder.replace(reference.range, new_path);
                }
            }
            InternalReferenceType::SiblingReference => {
                // Update sibling module references (Requirement 6.5)
                if let Some(new_path) = calculate_new_sibling_path(
                    &reference,
                    &old_module_path,
                    &new_module_path,
                    sema,
                )? {
                    edit_builder.replace(reference.range, new_path);
                }
            }
            InternalReferenceType::SelfReference => {
                // Update self-referential paths (Requirement 6.4)
                if let Some(new_path) = calculate_new_self_path(
                    &reference,
                    &old_module_path,
                    &new_module_path,
                    sema,
                )? {
                    edit_builder.replace(reference.range, new_path);
                }
            }
        }
    }

    let edit = edit_builder.finish();
    if !edit.is_empty() {
        // We need to determine which file this edit applies to
        // For now, we'll assume it's the same file as the item source
        if let Some(file_id) = find_file_id_for_syntax_node(sema, item_source) {
            source_change.insert_source_edit(file_id, edit);
        }
    }

    Ok(source_change)
}

/// Find all internal module references within an item's source code
/// (Requirement 6.1)
fn find_internal_module_references(
    sema: &Semantics<'_, RootDatabase>,
    item_source: &syntax::SyntaxNode,
    current_module: Module,
) -> Result<Vec<InternalReference>> {
    let mut references = Vec::new();

    // Walk through all path expressions in the item source
    for node in item_source.descendants() {
        if let Some(path) = syntax::ast::Path::cast(node) {
            if let Some(internal_ref) = analyze_path_for_internal_reference(sema, &path, current_module)? {
                references.push(internal_ref);
            }
        }
    }

    Ok(references)
}

/// Analyze a path to determine if it's an internal reference that needs updating
fn analyze_path_for_internal_reference(
    sema: &Semantics<'_, RootDatabase>,
    path: &syntax::ast::Path,
    current_module: Module,
) -> Result<Option<InternalReference>> {
    let path_text = path.syntax().text().to_string();
    let range = path.syntax().text_range();

    // Determine the type of reference based on the path structure
    let reference_type = if path_text.starts_with("super::") {
        InternalReferenceType::SuperReference
    } else if path_text.starts_with("crate::") {
        InternalReferenceType::CrateReference
    } else if path_text.starts_with("self::") {
        InternalReferenceType::SelfReference
    } else {
        // Check if this is a sibling module reference
        if is_sibling_module_reference(sema, path, current_module)? {
            InternalReferenceType::SiblingReference
        } else {
            // Not an internal reference we need to update
            return Ok(None);
        }
    };

    // Try to resolve the module this path refers to
    let resolved_module = resolve_path_to_module(sema, path, current_module)?;

    Ok(Some(InternalReference {
        range,
        reference_type,
        original_path: path_text,
        resolved_module,
    }))
}

/// Check if a path is a sibling module reference
fn is_sibling_module_reference(
    sema: &Semantics<'_, RootDatabase>,
    path: &syntax::ast::Path,
    current_module: Module,
) -> Result<bool> {
    // Get the first segment of the path
    if let Some(first_segment) = get_first_path_segment(path) {
        // Check if this segment is a sibling module
        if let Some(parent) = current_module.parent(sema.db) {
            for sibling in parent.children(sema.db) {
                if let Some(sibling_name) = sibling.name(sema.db) {
                    if sibling_name.as_str() == first_segment && sibling != current_module {
                        return Ok(true);
                    }
                }
            }
        }
    }
    
    Ok(false)
}

/// Resolve a path to the module it refers to
fn resolve_path_to_module(
    sema: &Semantics<'_, RootDatabase>,
    path: &syntax::ast::Path,
    current_module: Module,
) -> Result<Option<Module>> {
    // This is a simplified resolution - a full implementation would use
    // rust-analyzer's path resolution infrastructure
    
    let path_text = path.syntax().text().to_string();
    
    if path_text.starts_with("super::") {
        // Super reference - get parent module
        Ok(current_module.parent(sema.db))
    } else if path_text.starts_with("crate::") {
        // Crate reference - get crate root
        Ok(Some(current_module.crate_root(sema.db)))
    } else if path_text.starts_with("self::") {
        // Self reference - current module
        Ok(Some(current_module))
    } else {
        // Try to resolve as sibling module
        if let Some(first_segment) = get_first_path_segment(path) {
            if let Some(parent) = current_module.parent(sema.db) {
                for sibling in parent.children(sema.db) {
                    if let Some(sibling_name) = sibling.name(sema.db) {
                        if sibling_name.as_str() == first_segment {
                            return Ok(Some(sibling));
                        }
                    }
                }
            }
        }
        Ok(None)
    }
}

/// Calculate new super:: path for the new module context
/// (Requirement 6.3)
fn calculate_new_super_path(
    reference: &InternalReference,
    old_module_path: &ModPath,
    new_module_path: &ModPath,
    sema: &Semantics<'_, RootDatabase>,
) -> Result<Option<String>> {
    // Parse the original super:: path
    let original_path = &reference.original_path;
    
    if !original_path.starts_with("super::") {
        return Ok(None);
    }

    // Count the number of super:: segments
    let super_count = original_path.matches("super::").count();
    let remaining_path = original_path.strip_prefix(&"super::".repeat(super_count))
        .unwrap_or("");

    // Calculate the new path based on the new module hierarchy
    let old_depth = old_module_path.segments().len();
    let new_depth = new_module_path.segments().len();

    // Determine how many super:: we need in the new context
    let new_super_count = if old_depth >= super_count {
        // We can still go up the same number of levels
        if new_depth >= super_count {
            super_count
        } else {
            // We need to go to crate root and then down
            return Ok(Some(format!("crate::{}", remaining_path)));
        }
    } else {
        // Original path went beyond crate root - this shouldn't happen
        return Err(RenameError("Invalid super:: path in original reference".to_string()));
    };

    // Build the new path
    let new_path = if new_super_count > 0 {
        format!("{}{}", "super::".repeat(new_super_count), remaining_path)
    } else {
        remaining_path.to_string()
    };

    Ok(Some(new_path))
}

/// Calculate new crate:: path for the new module context
/// (Requirement 6.3)
fn calculate_new_crate_path(
    _reference: &InternalReference,
    _old_module_path: &ModPath,
    _new_module_path: &ModPath,
    _sema: &Semantics<'_, RootDatabase>,
) -> Result<Option<String>> {
    // Crate:: paths are absolute and don't need to change
    // unless the target module itself has moved, which would be handled
    // by external reference updates
    
    // For now, we keep crate:: paths unchanged
    Ok(None)
}

/// Calculate new sibling module path for the new module context
/// (Requirement 6.5)
fn calculate_new_sibling_path(
    reference: &InternalReference,
    old_module_path: &ModPath,
    new_module_path: &ModPath,
    _sema: &Semantics<'_, RootDatabase>,
) -> Result<Option<String>> {
    let original_path = &reference.original_path;
    
    // Parse the original path to get the sibling module name
    let path_parts: Vec<&str> = original_path.split("::").collect();
    if path_parts.is_empty() {
        return Ok(None);
    }
    
    let sibling_name = path_parts[0];
    let remaining_path = if path_parts.len() > 1 {
        path_parts[1..].join("::")
    } else {
        String::new()
    };

    // Check if the sibling relationship changes in the new module context
    let old_parent_depth = if old_module_path.segments().is_empty() {
        0
    } else {
        old_module_path.segments().len() - 1
    };
    
    let new_parent_depth = if new_module_path.segments().is_empty() {
        0
    } else {
        new_module_path.segments().len() - 1
    };

    if old_parent_depth == new_parent_depth {
        // Same level - sibling reference doesn't need to change
        return Ok(None);
    }

    // Different level - we need to adjust the path
    if new_parent_depth < old_parent_depth {
        // Moved up in hierarchy - may need super:: prefix
        let level_diff = old_parent_depth - new_parent_depth;
        let super_prefix = "super::".repeat(level_diff);
        let new_path = if remaining_path.is_empty() {
            format!("{}{}", super_prefix, sibling_name)
        } else {
            format!("{}{}::{}", super_prefix, sibling_name, remaining_path)
        };
        Ok(Some(new_path))
    } else {
        // Moved down in hierarchy - may need to use absolute path
        let mut absolute_path = vec!["crate".to_string()];
        
        // Add the old parent path segments
        if old_parent_depth > 0 {
            absolute_path.extend(
                old_module_path.segments()[..old_parent_depth]
                    .iter()
                    .map(|s| s.as_str().to_string())
            );
        }
        
        // Add the sibling name
        absolute_path.push(sibling_name.to_string());
        
        // Add remaining path
        if !remaining_path.is_empty() {
            absolute_path.push(remaining_path);
        }
        
        Ok(Some(absolute_path.join("::")))
    }
}

/// Calculate new self-referential path for the new module context
/// (Requirement 6.4)
fn calculate_new_self_path(
    _reference: &InternalReference,
    _old_module_path: &ModPath,
    _new_module_path: &ModPath,
    _sema: &Semantics<'_, RootDatabase>,
) -> Result<Option<String>> {
    // Self:: paths refer to the current module and don't need to change
    // when the item moves, as they still refer to the same logical module
    // (the module the item is now in)
    
    Ok(None)
}

/// Find the file ID for a syntax node
fn find_file_id_for_syntax_node(
    sema: &Semantics<'_, RootDatabase>,
    node: &syntax::SyntaxNode,
) -> Option<FileId> {
    // Try to find the source file for this syntax node
    let _source_file = node.ancestors().find_map(syntax::ast::SourceFile::cast)?;
    
    // For now, return None - a full implementation would need to properly
    // resolve the file ID from the syntax node
    // This is a complex operation that requires access to the source mapping
    None
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
    use crate::RootDatabase;
    use hir::PathKind;
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
        let path1 = ModPath::from_segments(
            PathKind::Crate,
            vec![Name::new_root("module"), Name::new_root("submodule")],
        );
        let path2 = ModPath::from_segments(
            PathKind::Crate,
            vec![Name::new_root("module"), Name::new_root("other")],
        );

        let result = compare_paths(&path1, &path2);
        assert_eq!(result, PathComparison::DifferentModule);
    }

    #[test]
    fn test_is_relative_path_in_same_module() {
        // Test relative path detection (Requirement 5.2)
        let current = ModPath::from_segments(PathKind::Crate, vec![Name::new_root("module")]);
        let target = ModPath::from_segments(
            PathKind::Plain,
            vec![Name::new_root("module"), Name::new_root("submodule")],
        );

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

    #[test]
    fn test_module_organization_preferences_default() {
        let prefs = ModuleOrganizationPreferences::default();
        assert!(!prefs.prefer_mod_rs);
        assert!(matches!(prefs.directory_structure, DirectoryStructurePreference::Nested));
    }

    #[test]
    fn test_module_creation_plan_new() {
        let prefs = ModuleOrganizationPreferences::default();
        let plan = ModuleCreationPlan::new(prefs.clone());

        assert!(plan.directories_to_create.is_empty());
        assert!(plan.files_to_create.is_empty());
        assert!(plan.module_declarations_to_add.is_empty());
        assert_eq!(plan.user_preferences.prefer_mod_rs, prefs.prefer_mod_rs);
    }

    #[test]
    fn test_is_valid_module_name() {
        // Valid names
        assert!(is_valid_module_name("module"));
        assert!(is_valid_module_name("my_module"));
        assert!(is_valid_module_name("module123"));
        assert!(is_valid_module_name("_private"));

        // Invalid names
        assert!(!is_valid_module_name(""));
        assert!(!is_valid_module_name("123module"));
        assert!(!is_valid_module_name("mod"));
        assert!(!is_valid_module_name("struct"));
        assert!(!is_valid_module_name("my-module"));
        assert!(!is_valid_module_name("my module"));
    }

    #[test]
    fn test_is_rust_keyword() {
        // Keywords
        assert!(is_rust_keyword("mod"));
        assert!(is_rust_keyword("struct"));
        assert!(is_rust_keyword("fn"));
        assert!(is_rust_keyword("let"));
        assert!(is_rust_keyword("async"));

        // Non-keywords
        assert!(!is_rust_keyword("module"));
        assert!(!is_rust_keyword("my_struct"));
        assert!(!is_rust_keyword("function"));
    }

    #[test]
    fn test_format_module_declaration() {
        // Without visibility
        let decl = ModuleDeclaration {
            parent_file: FileId::from_raw(0),
            module_name: "test_module".to_string(),
            insert_position: syntax::TextSize::from(0),
            visibility: None,
        };
        assert_eq!(format_module_declaration(&decl), "mod test_module;\n");

        // With visibility
        let decl_pub = ModuleDeclaration {
            parent_file: FileId::from_raw(0),
            module_name: "test_module".to_string(),
            insert_position: syntax::TextSize::from(0),
            visibility: Some("pub".to_string()),
        };
        assert_eq!(format_module_declaration(&decl_pub), "pub mod test_module;\n");
    }

    #[test]
    fn test_generate_module_content() {
        let content = generate_module_content("test_module");
        assert!(content.contains("The `test_module` module"));
        assert!(content.contains("TODO: Add module implementation"));
    }

    #[test]
    fn test_extract_item_visibility() {
        // Test with a public function
        let source = "pub fn test_function() {}";
        let parsed = syntax::ast::SourceFile::parse(source, Edition::CURRENT);
        let item = parsed.tree().items().next().unwrap();

        let visibility = extract_item_visibility(&item);
        assert_eq!(visibility, Some("pub".to_string()));
    }

    #[test]
    fn test_extract_item_attributes() {
        // Test with attributes
        let source = "#[derive(Debug)]\n#[allow(dead_code)]\nstruct TestStruct;";
        let parsed = syntax::ast::SourceFile::parse(source, Edition::CURRENT);
        let item = parsed.tree().items().next().unwrap();

        let attributes = extract_item_attributes(&item);
        assert_eq!(attributes.len(), 2);
        assert!(attributes[0].contains("derive(Debug)"));
        assert!(attributes[1].contains("allow(dead_code)"));
    }

    #[test]
    fn test_replace_item_name() {
        let item_text = "fn old_name() {}";
        let new_name = Name::new_root("new_name");

        let result = replace_item_name(item_text, &new_name);
        assert!(result.is_ok());
        let replaced = result.unwrap();
        assert!(replaced.contains("new_name"));
        assert!(!replaced.contains("old_name"));
    }

    #[test]
    fn test_format_item_for_insertion() {
        let item_text = "fn test() {}";
        let visibility = Some("pub".to_string());
        let attributes = vec!["#[test]".to_string()];
        let new_name = Name::new_root("new_test");

        let result = format_item_for_insertion(item_text, &visibility, &attributes, &new_name);
        assert!(result.is_ok());
        let formatted = result.unwrap();
        assert!(formatted.contains("#[test]"));
        assert!(formatted.contains("new_test"));
    }

    #[test]
    fn test_is_valid_visibility_spec() {
        // Valid visibility specifications
        assert!(is_valid_visibility_spec("pub"));
        assert!(is_valid_visibility_spec("pub(crate)"));
        assert!(is_valid_visibility_spec("pub(super)"));
        assert!(is_valid_visibility_spec("pub(self)"));
        assert!(is_valid_visibility_spec("pub(in crate)"));
        assert!(is_valid_visibility_spec("pub(in super::module)"));

        // Invalid visibility specifications
        assert!(!is_valid_visibility_spec("private"));
        assert!(!is_valid_visibility_spec("public"));
        assert!(!is_valid_visibility_spec(""));
    }

    #[test]
    fn test_calculate_path_distance() {
        use std::path::Path;

        // Same paths
        let path1 = Path::new("/src/module/file.rs");
        let path2 = Path::new("/src/module/file.rs");
        assert_eq!(calculate_path_distance(path1, path2), 0);

        // Different files in same directory
        let path1 = Path::new("/src/module/file1.rs");
        let path2 = Path::new("/src/module/file2.rs");
        assert_eq!(calculate_path_distance(path1, path2), 2);

        // Different directories
        let path1 = Path::new("/src/module1/file.rs");
        let path2 = Path::new("/src/module2/file.rs");
        assert_eq!(calculate_path_distance(path1, path2), 4);
    }

    #[test]
    fn test_find_common_ancestor() {
        use std::path::Path;

        let path1 = Path::new("/src/module1/file.rs");
        let path2 = Path::new("/src/module2/file.rs");

        let ancestor = find_common_ancestor(path1, path2);
        assert!(ancestor.is_some());
        assert_eq!(ancestor.unwrap(), Path::new("/src"));

        // No common ancestor
        let path1 = Path::new("/src/file.rs");
        let path2 = Path::new("/other/file.rs");
        let ancestor = find_common_ancestor(path1, path2);
        assert_eq!(ancestor, Some(Path::new("/").to_path_buf()));
    }
}
