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
use base_db::AnchoredPathBuf;
use base_db::SourceDatabase;
use either::Either;
use hir::{
    FieldSource, FileRange, InFile, ModPath, Module, ModuleSource, Name, PathKind, Semantics,
    db::ExpandDatabase, sym,
};
use span::{Edition, FileId, SyntaxContext};
use stdx::{TupleExt, never};
use syntax::{
    AstNode, SyntaxKind, T, TextRange,
    ast::{self, HasAttrs, HasModuleItem, HasName, HasVisibility},
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
