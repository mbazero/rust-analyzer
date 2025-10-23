//! Renaming functionality.
//!
//! This is mostly front-end for [`ide_db::rename`], but it also includes the
//! tests. This module also implements a couple of magic tricks, like renaming
//! `self` and to `self` (to switch between associated function and method).

use hir::{attach_db, AsAssocItem, InFile, Module, ModuleSource, Name, Semantics, sym};
use ide_db::{
    FileId, FileRange, RootDatabase,
    base_db::{AnchoredPathBuf, RootQueryDb, SourceDatabase, VfsPath},
    defs::{Definition, NameClass, NameRefClass},
    rename::{IdentifierKind, RenameDefinition, bail, format_err, source_edit_from_references},
    search::{FileReference, ReferenceCategory},
    source_change::{FileSystemEdit, SourceChangeBuilder},
};
use itertools::Itertools;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;
use std::mem;
use stdx::{always, format_to, never};
use syntax::{
    AstNode, SyntaxKind, SyntaxNode, TextRange, TextSize,
    ast::{
        self, HasArgList, HasGenericArgs, HasModuleItem, HasName, PathSegmentKind, edit::IndentLevel,
        prec::ExprPrecedence,
    },
};

use span::Edition;

use ide_db::text_edit::{TextEdit, TextEditBuilder};

use crate::{FilePosition, RangeInfo, SourceChange};

pub use ide_db::rename::RenameError;

type RenameResult<T> = Result<T, RenameError>;

#[derive(Clone, Debug, PartialEq, Eq)]
struct MoveOperation {
    source_module_path: Vec<Name>,
    dest_module_path: Vec<Name>,
    source_item_name: Name,
    dest_item_name: Name,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ModuleFileSystemPlan {
    anchor: FileId,
    destination_relative_path: String,
    file_creations: Vec<PlannedFileCreation>,
    module_declarations: Vec<ModuleDeclarationPlan>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct PlannedFileCreation {
    relative_path: String,
    initial_contents: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ModuleDeclarationPlan {
    file_id: FileId,
    insert_offset: TextSize,
    text: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ItemRelocationPlan {
    moved_items: Vec<PlannedItem>,
    destination: RelocationDestination,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct PlannedItem {
    file_id: FileId,
    editioned_file_id: ide_db::EditionedFileId,
    range: TextRange,
    text: String,
    syntax: SyntaxNode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum RelocationDestination {
    ExistingFile { file_id: FileId, insert_offset: TextSize },
    InlineModule { file_id: FileId, insert_offset: TextSize, indent: IndentLevel },
    NewFile { relative_path: String },
}

#[derive(Clone, Debug)]
struct ExternalReferenceUpdatePlan {
    edits: Vec<ExternalReferenceEditPlan>,
}

#[derive(Clone, Debug)]
struct ExternalReferenceEditPlan {
    file_id: FileId,
    edit: TextEdit,
}

#[derive(Clone, Debug, Default)]
struct InternalReferenceUpdatePlan {
    required_imports: Vec<RequiredImportPlan>,
    path_rewrites: Vec<InternalPathRewrite>,
}

#[derive(Clone, Debug)]
struct RequiredImportPlan {
    path: String,
}

#[derive(Clone, Debug)]
struct InternalPathRewrite {
    file_id: ide_db::EditionedFileId,
    range: TextRange,
    replacement: String,
}

impl ExternalReferenceUpdatePlan {
    #[allow(dead_code)]
    fn is_empty(&self) -> bool {
        self.edits.is_empty()
    }

    #[allow(dead_code)]
    fn iter(&self) -> impl Iterator<Item = &ExternalReferenceEditPlan> {
        self.edits.iter()
    }

    #[cfg(test)]
    fn edits(&self) -> &[ExternalReferenceEditPlan] {
        &self.edits
    }
}

impl ExternalReferenceEditPlan {
    #[allow(dead_code)]
    fn file_id(&self) -> FileId {
        self.file_id
    }

    #[allow(dead_code)]
    fn text_edit(&self) -> &TextEdit {
        &self.edit
    }
}

impl InternalReferenceUpdatePlan {
    #[allow(dead_code)]
    fn is_empty(&self) -> bool {
        self.required_imports.is_empty() && self.path_rewrites.is_empty()
    }

    #[allow(dead_code)]
    fn required_imports(&self) -> &[RequiredImportPlan] {
        &self.required_imports
    }

    #[allow(dead_code)]
    fn path_rewrites(&self) -> &[InternalPathRewrite] {
        &self.path_rewrites
    }

    #[cfg(test)]
    fn import_paths(&self) -> Vec<&str> {
        self.required_imports.iter().map(|import| import.path.as_str()).collect()
    }

    #[cfg(test)]
    fn rewrites(&self) -> &[InternalPathRewrite] {
        &self.path_rewrites
    }
}

impl RequiredImportPlan {
    #[allow(dead_code)]
    fn path(&self) -> &str {
        &self.path
    }
}

impl InternalPathRewrite {
    #[allow(dead_code)]
    fn file_id(&self) -> ide_db::EditionedFileId {
        self.file_id
    }

    #[allow(dead_code)]
    fn range(&self) -> TextRange {
        self.range
    }

    #[allow(dead_code)]
    fn replacement(&self) -> &str {
        &self.replacement
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ParsedRenameTarget {
    final_name_text: String,
    move_target: Option<MoveTarget>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct MoveTarget {
    kind: MovePathHead,
    module_path: Vec<Name>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum MovePathHead {
    Crate,
}

impl ParsedRenameTarget {
    fn parse(edition: Edition, raw: &str) -> RenameResult<Self> {
        if !raw.contains("::") {
            return Ok(Self { final_name_text: raw.to_string(), move_target: None });
        }

        let path = ast::make::path_from_text_with_edition(raw, edition);
        if path.syntax().descendants().any(|node| node.kind() == SyntaxKind::ERROR) {
            bail!("Invalid path `{raw}` for rename-to-move");
        }

        let segments: Vec<_> = path.segments().collect();
        if segments.is_empty() {
            bail!("Invalid path `{raw}` for rename-to-move");
        }

        let (kind, start_idx) = match segments[0].kind() {
            Some(PathSegmentKind::CrateKw) => (MovePathHead::Crate, 1),
            Some(PathSegmentKind::SelfKw | PathSegmentKind::SuperKw | PathSegmentKind::SelfTypeKw) => {
                bail!("Rename-to-move paths must start with `crate::`")
            }
            Some(PathSegmentKind::Name(_)) => bail!("Rename-to-move paths must start with `crate::`"),
            Some(PathSegmentKind::Type { .. }) | None => {
                bail!("Rename-to-move path segments cannot contain generics or type arguments")
            }
        };

        if start_idx >= segments.len() {
            bail!("Rename-to-move requires a destination item name");
        }

        let mut module_path = Vec::new();
        for segment in &segments[start_idx..segments.len() - 1] {
            let Some(PathSegmentKind::Name(name_ref)) = segment.kind() else {
                bail!("Invalid module path segment in `{raw}`");
            };

            if segment.generic_arg_list().is_some()
                || segment.parenthesized_arg_list().is_some()
                || segment.type_anchor().is_some()
                || segment.ret_type().is_some()
            {
                bail!(
                    "Rename-to-move path segment `{}` contains unsupported syntax",
                    name_ref.text()
                );
            }

            let segment_text = name_ref.text().to_string();
            let (segment_name, ident_kind) = IdentifierKind::classify(edition, &segment_text)?;
            if ident_kind != IdentifierKind::Ident {
                bail!("Invalid module path segment `{segment_text}`");
            }
            module_path.push(segment_name);
        }

        let Some(PathSegmentKind::Name(name_ref)) = segments
            .last()
            .and_then(|segment| segment.kind())
        else {
            bail!("Rename-to-move requires a destination item name");
        };

        let last_segment = segments.last().unwrap();
        if last_segment.generic_arg_list().is_some()
            || last_segment.parenthesized_arg_list().is_some()
            || last_segment.type_anchor().is_some()
            || last_segment.ret_type().is_some()
        {
            bail!(
                "Rename-to-move path segment `{}` contains unsupported syntax",
                name_ref.text()
            );
        }

        let final_name_text = name_ref.text().to_string();
        if final_name_text.is_empty() {
            bail!("Rename-to-move requires a destination item name");
        }

        Ok(Self {
            final_name_text,
            move_target: Some(MoveTarget { kind, module_path }),
        })
    }

    fn final_name_text(&self) -> &str {
        &self.final_name_text
    }

    fn move_target(&self) -> Option<&MoveTarget> {
        self.move_target.as_ref()
    }
}

impl MoveTarget {
    #[allow(dead_code)]
    fn module_path(&self) -> &[Name] {
        &self.module_path
    }

    fn requires_move_from(&self, source_module_path: &[Name]) -> bool {
        self.module_path != source_module_path
    }
}

fn module_path_from_root(module: Module, db: &RootDatabase) -> Vec<Name> {
    module
        .path_to_root(db)
        .into_iter()
        .rev()
        .filter_map(|module| module.name(db))
        .collect()
}

fn detect_move_operation(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    resolved_name: &Name,
    move_target: &MoveTarget,
) -> RenameResult<Option<MoveOperation>> {
    if move_target.kind != MovePathHead::Crate {
        return Ok(None);
    }

    let Some(source_module) = def.module(sema.db) else {
        return Ok(None);
    };
    let source_module_path = module_path_from_root(source_module, sema.db);
    if !move_target.requires_move_from(&source_module_path) {
        return Ok(None);
    }

    let Some(source_item_name) = def.name(sema.db) else {
        return Ok(None);
    };

    Ok(Some(MoveOperation {
        source_module_path,
        dest_module_path: move_target.module_path.clone(),
        source_item_name,
        dest_item_name: resolved_name.clone(),
    }))
}

impl ModuleFileSystemPlan {
    #[allow(dead_code)]
    fn is_no_op(&self) -> bool {
        self.file_creations.is_empty() && self.module_declarations.is_empty()
    }

    #[allow(dead_code)]
    fn file_system_edits(&self) -> Vec<FileSystemEdit> {
        self.file_creations
            .iter()
            .map(|creation| FileSystemEdit::CreateFile {
                dst: AnchoredPathBuf { anchor: self.anchor, path: creation.relative_path.clone() },
                initial_contents: creation.initial_contents.clone(),
            })
            .collect()
    }

    #[allow(dead_code)]
    fn destination_path(&self) -> &str {
        &self.destination_relative_path
    }

    #[allow(dead_code)]
    fn module_declaration_edits(&self) -> Vec<(FileId, TextEdit)> {
        self.module_declarations
            .iter()
            .map(|decl| {
                (
                    decl.file_id,
                    TextEdit::insert(decl.insert_offset, decl.text.clone()),
                )
            })
            .collect()
    }

    #[cfg(test)]
    fn file_paths(&self) -> Vec<&str> {
        self.file_creations.iter().map(|creation| creation.relative_path.as_str()).collect()
    }

    #[cfg(test)]
    fn file_initial_contents(&self) -> Vec<&str> {
        self.file_creations
            .iter()
            .map(|creation| creation.initial_contents.as_str())
            .collect()
    }

    #[cfg(test)]
    fn module_declaration_texts(&self) -> Vec<&str> {
        self.module_declarations.iter().map(|decl| decl.text.as_str()).collect()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ModuleCreationStyle {
    File,
    ModRs,
}

fn plan_module_file_system_ops(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    move_op: &MoveOperation,
) -> RenameResult<ModuleFileSystemPlan> {
    let db = sema.db;
    let krate = def.krate(db).ok_or_else(|| format_err!("Cannot move items outside of a crate"))?;
    let root_module = krate.root_module();
    let root_definition = root_module.definition_source(db);
    let root_file_id = root_definition.file_id.original_file(db);
    let root_anchor = root_file_id.file_id(db);

    let root_path = vfs_path(db, root_anchor).ok_or_else(|| {
        format_err!("Unable to resolve path for crate root when planning move operation")
    })?;
    let root_dir = root_path.parent().unwrap_or_else(|| root_path.clone());

    let mut current_parent_module = root_module;
    let mut current_parent_dir = String::new();
    let mut existing_prefix_len = 0usize;
    let mut existing_paths: Vec<String> = Vec::new();

    for segment in &move_op.dest_module_path {
        let child = find_child_module_by_name(current_parent_module, db, segment);
        let Some(child_module) = child else { break };

        if child_module.has_path(db) {
            bail!("Cannot move items into modules declared with #[path] attributes yet");
        }
        if child_module.is_inline(db) {
            break;
        }

        let Some(file_id) = child_module.as_source_file_id(db) else {
            bail!("Destination module `{}` has no associated source file", segment.as_str());
        };
        let module_path = relative_path_from_anchor(db, root_anchor, file_id.file_id(db), &root_dir)
            .ok_or_else(|| {
                format_err!(
                    "Failed to resolve file path for destination module `{}`",
                    segment.as_str()
                )
            })?;
        existing_paths.push(module_path.clone());
        current_parent_dir =
            module_directory_from_file_path(&module_path, child_module.is_mod_rs(db))
                .ok_or_else(|| {
                    format_err!(
                        "Failed to resolve destination directory for module `{}`",
                        segment.as_str()
                    )
                })?;
        current_parent_module = child_module;
        existing_prefix_len += 1;
    }

    let mut file_creations = Vec::new();
    let mut module_declarations = Vec::new();
    let mut parent_context: Option<ModuleCreationContext> =
        Some(ModuleCreationContext::Existing { module: current_parent_module });
    if existing_prefix_len == 0 {
        parent_context = Some(ModuleCreationContext::Existing { module: root_module });
    }

    let mut destination_relative_path = if existing_paths.is_empty() {
        relative_path_from_anchor(db, root_anchor, root_anchor, &root_dir)
            .unwrap_or_else(|| root_definition_path_from_anchor(&root_dir, &root_path))
    } else {
        existing_paths.last().cloned().unwrap()
    };

    for segment in move_op.dest_module_path.iter().skip(existing_prefix_len) {
        match parent_context {
            Some(ModuleCreationContext::Existing { module }) => {
                if let Some(decl) =
                    plan_module_declaration_for_existing_parent(db, module, segment)?
                {
                    module_declarations.push(decl);
                }
            }
            Some(ModuleCreationContext::New { .. }) => {
                if let Some(prev_creation) = file_creations.last_mut() {
                    append_child_declaration_to_new_module(prev_creation, segment);
                }
            }
            None => (),
        }

        let style = match parent_context {
            Some(ModuleCreationContext::Existing { module }) => infer_child_style(module, db),
            Some(ModuleCreationContext::New { preferred_style }) => preferred_style,
            None => ModuleCreationStyle::File,
        };

        let file_path = build_module_file_path(&current_parent_dir, segment.as_str(), style);
        file_creations.push(PlannedFileCreation {
            relative_path: file_path.clone(),
            initial_contents: String::new(),
        });

        current_parent_dir.push_str(segment.as_str());
        current_parent_dir.push('/');
        current_parent_dir = normalize_directory_path(&current_parent_dir);
        destination_relative_path = file_path.clone();
        parent_context =
            Some(ModuleCreationContext::New { preferred_style: ModuleCreationStyle::File });
    }

    Ok(ModuleFileSystemPlan {
        anchor: root_anchor,
        destination_relative_path,
        file_creations,
        module_declarations,
    })
}

fn find_child_module_by_name(
    parent: Module,
    db: &RootDatabase,
    segment: &Name,
) -> Option<Module> {
    parent
        .children(db)
        .find(|child| child.name(db).is_some_and(|name| name == *segment))
}

fn vfs_path(db: &RootDatabase, file_id: FileId) -> Option<VfsPath> {
    let source_root_input = db.file_source_root(file_id);
    let source_root_id = source_root_input.source_root_id(db);
    let source_root = db.source_root(source_root_id).source_root(db);
    source_root.path_for_file(&file_id).cloned()
}

fn relative_path_from_anchor(
    db: &RootDatabase,
    anchor: FileId,
    target: FileId,
    anchor_dir: &VfsPath,
) -> Option<String> {
    let anchor_source_root = db.file_source_root(anchor).source_root_id(db);
    let target_source_root = db.file_source_root(target).source_root_id(db);
    if anchor_source_root != target_source_root {
        return None;
    }
    let target_path = vfs_path(db, target)?;
    let rel = target_path.strip_prefix(anchor_dir)?;
    let trimmed = rel.as_str().trim_start_matches('/');
    Some(trimmed.to_owned())
}

fn root_definition_path_from_anchor(anchor_dir: &VfsPath, root_path: &VfsPath) -> String {
    if let Some(rel) = root_path.strip_prefix(anchor_dir) {
        rel.as_str().trim_start_matches('/').to_owned()
    } else {
        "mod.rs".to_owned()
    }
}

fn module_directory_from_file_path(rel_path: &str, is_mod_rs: bool) -> Option<String> {
    if is_mod_rs {
        let dir = rel_path.strip_suffix("mod.rs")?;
        return Some(dir.to_owned());
    }
    let stripped = rel_path.strip_suffix(".rs")?;
    Some(format!("{stripped}/"))
}

fn build_module_file_path(
    current_dir: &str,
    segment: &str,
    style: ModuleCreationStyle,
) -> String {
    match style {
        ModuleCreationStyle::File => format!("{current_dir}{segment}.rs"),
        ModuleCreationStyle::ModRs => format!("{current_dir}{segment}/mod.rs"),
    }
}

fn normalize_directory_path(path: &str) -> String {
    if path.is_empty() {
        return String::new();
    }
    let mut normalized = path.to_owned();
    if !normalized.ends_with('/') {
        normalized.push('/');
    }
    normalized
}

fn append_child_declaration_to_new_module(creation: &mut PlannedFileCreation, segment: &Name) {
    if !creation.initial_contents.is_empty() && !creation.initial_contents.ends_with('\n') {
        creation.initial_contents.push('\n');
    }
    creation.initial_contents.push_str("pub mod ");
    creation.initial_contents.push_str(segment.as_str());
    creation.initial_contents.push_str(";\n");
}

fn extract_definition_item(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
) -> RenameResult<PlannedItem> {
    let (editioned_file_id, syntax) = match def {
        Definition::Function(fun) => {
            let src = sema.source(*fun).ok_or_else(|| format_err!("Cannot find source for function"))?;
            (src.file_id.original_file(sema.db), src.value.syntax().clone())
        }
        Definition::Adt(hir::Adt::Struct(strukt)) => {
            let src = sema.source(*strukt).ok_or_else(|| format_err!("Cannot find source for struct"))?;
            (src.file_id.original_file(sema.db), src.value.syntax().clone())
        }
        Definition::Adt(hir::Adt::Enum(enm)) => {
            let src = sema.source(*enm).ok_or_else(|| format_err!("Cannot find source for enum"))?;
            (src.file_id.original_file(sema.db), src.value.syntax().clone())
        }
        Definition::Adt(hir::Adt::Union(union)) => {
            let src = sema.source(*union).ok_or_else(|| format_err!("Cannot find source for union"))?;
            (src.file_id.original_file(sema.db), src.value.syntax().clone())
        }
        Definition::Trait(trait_def) => {
            let src = sema.source(*trait_def).ok_or_else(|| format_err!("Cannot find source for trait"))?;
            (src.file_id.original_file(sema.db), src.value.syntax().clone())
        }
        Definition::Const(konst) => {
            let src = sema.source(*konst).ok_or_else(|| format_err!("Cannot find source for const"))?;
            (src.file_id.original_file(sema.db), src.value.syntax().clone())
        }
        Definition::Static(stat) => {
            let src = sema.source(*stat).ok_or_else(|| format_err!("Cannot find source for static"))?;
            (src.file_id.original_file(sema.db), src.value.syntax().clone())
        }
        Definition::TypeAlias(ty) => {
            let src = sema.source(*ty).ok_or_else(|| format_err!("Cannot find source for type alias"))?;
            (src.file_id.original_file(sema.db), src.value.syntax().clone())
        }
        _ => bail!("Unsupported definition type for relocation"),
    };

    let range = syntax.text_range();
    let vfs_file_id = editioned_file_id.file_id(sema.db);
    let file_text = sema.db.file_text(vfs_file_id);
    let text_slice = file_text.text(sema.db);
    let start: usize = range.start().into();
    let end: usize = range.end().into();
    let text = text_slice[start..end].to_string();

    Ok(PlannedItem { file_id: vfs_file_id, editioned_file_id, range, text, syntax })
}

fn collect_associated_impls(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    primary: &PlannedItem,
) -> Vec<PlannedItem> {
    let Some(type_name) = def.name(sema.db).map(|name| name.as_str().trim_start_matches("r#").to_string()) else {
        return Vec::new();
    };

    let editioned_file_id = primary.editioned_file_id;
    let source_file = sema.parse(editioned_file_id);
    let file_text = sema.db.file_text(primary.file_id);
    let text_slice = file_text.text(sema.db);

    source_file
        .syntax()
        .children()
        .filter_map(ast::Item::cast)
        .filter_map(|item| match item {
            ast::Item::Impl(impl_block) => Some(impl_block),
            _ => None,
        })
        .filter(|impl_block| impl_targets_type(impl_block, &type_name))
        .map(|impl_block| {
            let range = impl_block.syntax().text_range();
            let start: usize = range.start().into();
            let end: usize = range.end().into();
            let text = text_slice[start..end].to_string();
            PlannedItem {
                file_id: primary.file_id,
                editioned_file_id,
                range,
                text,
                syntax: impl_block.syntax().clone(),
            }
        })
        .collect()
}

fn impl_targets_type(impl_block: &ast::Impl, type_name: &str) -> bool {
    let Some(self_ty) = impl_block.self_ty() else { return false; };
    let path_type = match self_ty {
        ast::Type::PathType(path_type) => path_type,
        _ => return false,
    };
    let Some(path) = path_type.path() else { return false; };
    let Some(segment) = path_last_segment(&path) else { return false; };
    segment.text().trim_start_matches("r#") == type_name
}

fn path_last_segment(path: &ast::Path) -> Option<ast::NameRef> {
    path.segments().last()?.name_ref()
}

fn plan_item_relocation(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    move_op: &MoveOperation,
    module_plan: &ModuleFileSystemPlan,
) -> RenameResult<ItemRelocationPlan> {
    let primary = extract_definition_item(sema, def)?;
    let mut moved_items = vec![primary.clone()];
    moved_items.extend(collect_associated_impls(sema, def, &primary));
    moved_items.sort_by_key(|item| (item.file_id, item.range.start()));
    moved_items.dedup_by(|a, b| a.file_id == b.file_id && a.range == b.range);

    let root_module = def
        .krate(sema.db)
        .ok_or_else(|| format_err!("Cannot determine crate for definition"))?
        .root_module();
    let destination_module = resolve_existing_destination_module(root_module, move_op, sema.db);

    let destination = if let Some(module) = destination_module {
        match plan_insertion_for_existing_module(sema, module) {
            Ok(dest) => dest,
            Err(_) => RelocationDestination::NewFile {
                relative_path: module_plan.destination_relative_path.clone(),
            },
        }
    } else {
        RelocationDestination::NewFile { relative_path: module_plan.destination_relative_path.clone() }
    };

    Ok(ItemRelocationPlan { moved_items, destination })
}

fn plan_external_reference_updates(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    move_op: &MoveOperation,
    relocation_plan: &ItemRelocationPlan,
) -> RenameResult<ExternalReferenceUpdatePlan> {
    attach_db(sema.db, || {
        let usages = def.usages(sema).all();
        if usages.is_empty() {
            return Ok(ExternalReferenceUpdatePlan { edits: Vec::new() });
        }

        let edition = def
            .krate(sema.db)
            .map(|krate| krate.edition(sema.db))
            .unwrap_or(Edition::LATEST);
        let new_path = format_crate_item_path(
            sema.db,
            edition,
            &move_op.dest_module_path,
            &move_op.dest_item_name,
        );

        let mut per_file_replacements: BTreeMap<FileId, Vec<(TextRange, String)>> = BTreeMap::new();

        for (file_id, references) in usages.iter() {
            for reference in references {
                if relocation_plan
                    .moved_items
                    .iter()
                    .any(|item| item.editioned_file_id == file_id && item.range.contains_range(reference.range))
                {
                    continue;
                }

                let replacement = if reference.category.contains(ReferenceCategory::IMPORT) {
                    plan_import_reference(reference, &new_path)
                } else {
                    plan_non_import_reference(reference, &new_path)
                };
                let Some((range, text)) = replacement else { continue };

                let actual_file_id = file_id.file_id(sema.db);
                let entry = per_file_replacements.entry(actual_file_id).or_default();
                if entry.iter().any(|(existing, _)| *existing == range) {
                    continue;
                }
                entry.push((range, text));
            }
        }

        let mut edits = Vec::new();
        for (file_id, replacements) in per_file_replacements {
            if replacements.is_empty() {
                continue;
            }
            let mut builder = TextEdit::builder();
            for (range, text) in replacements {
                builder.replace(range, text);
            }
            let edit = builder.finish();
            if edit.is_empty() {
                continue;
            }
            edits.push(ExternalReferenceEditPlan { file_id, edit });
        }

        Ok(ExternalReferenceUpdatePlan { edits })
    })
}

fn plan_internal_reference_updates(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    relocation_plan: &ItemRelocationPlan,
) -> RenameResult<InternalReferenceUpdatePlan> {
    let Some(source_module) = def.module(sema.db) else {
        return Ok(InternalReferenceUpdatePlan::default());
    };
    let edition = def
        .krate(sema.db)
        .map(|krate| krate.edition(sema.db))
        .unwrap_or(Edition::LATEST);

    attach_db(sema.db, || {
        let moved_ranges: Vec<_> = relocation_plan
            .moved_items
            .iter()
            .map(|item| (item.editioned_file_id, item.range))
            .collect();

        let mut import_paths = BTreeSet::new();
        let mut plan = InternalReferenceUpdatePlan::default();

        for moved_item in &relocation_plan.moved_items {
            for path in moved_item.syntax.descendants().filter_map(ast::Path::cast) {
                if path.syntax().parent().and_then(ast::Path::cast).is_some() {
                    continue;
                }
                let path_text = path.syntax().text();
                if path_text.is_empty() {
                    continue;
                }

                let Some(resolution) = sema.resolve_path(&path) else { continue; };
                let definition = Definition::from(resolution);
                if definition == *def {
                    continue;
                }
                if definition_is_within_moved_items(&definition, &moved_ranges, sema) {
                    continue;
                }

                let Some(def_module) = definition.module(sema.db) else { continue; };
                if !module_within_source_tree(def_module, source_module, sema.db) {
                    continue;
                }

                if path_text.to_string().starts_with("crate::") {
                    continue;
                }

                let in_use_tree = path_in_use_tree(&path);
                let qualifier = path.qualifier();
                if qualifier.is_none() && !in_use_tree {
                    let Some(def_name) = definition.name(sema.db) else { continue; };
                    let module_path = module_path_from_root(def_module, sema.db);
                    let import_path = format_crate_item_path(sema.db, edition, &module_path, &def_name);
                    if import_paths.insert(import_path.clone()) {
                        plan.required_imports.push(RequiredImportPlan { path: import_path });
                    }
                    continue;
                }

                let Some(def_name) = definition.name(sema.db) else { continue; };
                let module_path = module_path_from_root(def_module, sema.db);
                let absolute_path = format_crate_item_path(sema.db, edition, &module_path, &def_name);
                if let Some((range, replacement)) = compute_path_replacement(&path, &absolute_path) {
                    if !plan.path_rewrites.iter().any(|rewrite| {
                        rewrite.file_id == moved_item.editioned_file_id && rewrite.range == range
                    }) {
                        plan.path_rewrites.push(InternalPathRewrite {
                            file_id: moved_item.editioned_file_id,
                            range,
                            replacement,
                        });
                    }
                }
            }
        }

        Ok(plan)
    })
}

fn orchestrate_move(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    resolved_name: &Name,
    move_op: &MoveOperation,
) -> RenameResult<SourceChange> {
    let mut module_plan = plan_module_file_system_ops(sema, def, move_op)?;
    let mut relocation_plan = plan_item_relocation(sema, def, move_op, &module_plan)?;
    let external_plan = plan_external_reference_updates(sema, def, move_op, &relocation_plan)?;
    let internal_plan = plan_internal_reference_updates(sema, def, &relocation_plan)?;

    let mut reference_change =
        def.rename(sema, resolved_name.as_str(), RenameDefinition::ReferencesOnly)?;
    let mut per_item_edits = extract_reference_edits_for_moved_items(
        &mut reference_change,
        &relocation_plan,
        &external_plan,
    )?;

    let internal_rewrites = collect_internal_rewrites(&relocation_plan, &internal_plan)?;
    for (target, additional) in per_item_edits.iter_mut().zip(internal_rewrites) {
        target.extend(additional);
    }
    apply_edits_to_moved_items(&mut relocation_plan.moved_items, per_item_edits)?;

    let def_index = apply_definition_rename(sema, def, resolved_name, &mut relocation_plan)?;
    let insertion_order = build_insertion_order(relocation_plan.moved_items.len(), def_index);

    let move_change = build_move_change(
        &mut module_plan,
        &relocation_plan,
        &external_plan,
        &insertion_order,
    )?;

    Ok(move_change.merge(reference_change))
}

fn extract_reference_edits_for_moved_items(
    reference_change: &mut SourceChange,
    relocation_plan: &ItemRelocationPlan,
    external_plan: &ExternalReferenceUpdatePlan,
) -> RenameResult<Vec<Vec<(TextRange, String)>>> {
    let mut per_item_edits = vec![Vec::new(); relocation_plan.moved_items.len()];

    let mut moved_ranges: BTreeMap<FileId, Vec<(usize, TextRange)>> = BTreeMap::new();
    for (idx, item) in relocation_plan.moved_items.iter().enumerate() {
        moved_ranges.entry(item.file_id).or_default().push((idx, item.range));
    }

    let mut external_ranges: BTreeMap<FileId, Vec<TextRange>> = BTreeMap::new();
    for plan in &external_plan.edits {
        for indel in plan.edit.iter() {
            external_ranges.entry(plan.file_id).or_default().push(indel.delete);
        }
    }

    let original_edits = mem::take(&mut reference_change.source_file_edits);
    let mut filtered = original_edits.clone();
    filtered.clear();

    for (file_id, (text_edit, snippet)) in original_edits {
        let mut builder = TextEditBuilder::default();
        for indel in text_edit.into_iter() {
            if external_ranges
                .get(&file_id)
                .is_some_and(|ranges| ranges.iter().any(|range| ranges_overlap(indel.delete, *range)))
            {
                continue;
            }

            if let Some((item_idx, item_range)) = moved_ranges
                .get(&file_id)
                .and_then(|ranges| ranges.iter().find(|(_, range)| range.contains_range(indel.delete)))
            {
                let relative = to_relative_range(*item_range, indel.delete)?;
                per_item_edits[*item_idx].push((relative, indel.insert.clone()));
                continue;
            }

            builder.indel(indel);
        }
        let new_edit = builder.finish();
        if !new_edit.is_empty() || snippet.is_some() {
            filtered.insert(file_id, (new_edit, snippet));
        }
    }

    reference_change.source_file_edits = filtered;
    Ok(per_item_edits)
}

fn collect_internal_rewrites(
    relocation_plan: &ItemRelocationPlan,
    internal_plan: &InternalReferenceUpdatePlan,
) -> RenameResult<Vec<Vec<(TextRange, String)>>> {
    let mut per_item_edits = vec![Vec::new(); relocation_plan.moved_items.len()];
    for rewrite in &internal_plan.path_rewrites {
        let Some((idx, item)) = relocation_plan
            .moved_items
            .iter()
            .enumerate()
            .find(|(_, item)| item.editioned_file_id == rewrite.file_id && item.range.contains_range(rewrite.range))
        else {
            continue;
        };
        let relative = to_relative_range(item.range, rewrite.range)?;
        per_item_edits[idx].push((relative, rewrite.replacement.clone()));
    }
    Ok(per_item_edits)
}

fn apply_edits_to_moved_items(
    moved_items: &mut [PlannedItem],
    edits: Vec<Vec<(TextRange, String)>>,
) -> RenameResult<()> {
    if moved_items.len() != edits.len() {
        bail!("Mismatch between moved items and planned edits");
    }
    for (item, item_edits) in moved_items.iter_mut().zip(edits.into_iter()) {
        if item_edits.is_empty() {
            continue;
        }
        apply_relative_edits(&mut item.text, item_edits);
    }
    Ok(())
}

fn apply_definition_rename(
    sema: &Semantics<'_, RootDatabase>,
    def: &Definition,
    resolved_name: &Name,
    relocation_plan: &mut ItemRelocationPlan,
) -> RenameResult<usize> {
    let def_range = def
        .range_for_rename(sema)
        .ok_or_else(|| format_err!("Cannot locate definition for move operation"))?;
    let edition = def_range.file_id.edition(sema.db);
    let replacement = resolved_name.display(sema.db, edition).to_string();

    let Some((idx, item)) = relocation_plan
        .moved_items
        .iter_mut()
        .enumerate()
        .find(|(_, item)| item.editioned_file_id == def_range.file_id && item.range.contains_range(def_range.range))
    else {
        bail!("Primary definition not found among moved items");
    };

    let relative = to_relative_range(item.range, def_range.range)?;
    apply_relative_edits(&mut item.text, vec![(relative, replacement)]);
    Ok(idx)
}

fn build_insertion_order(len: usize, def_index: usize) -> Vec<usize> {
    let mut order: Vec<_> = (0..len).collect();
    if def_index < order.len() {
        order.remove(def_index);
        order.insert(0, def_index);
    }
    order
}

fn build_move_change(
    module_plan: &mut ModuleFileSystemPlan,
    relocation_plan: &ItemRelocationPlan,
    external_plan: &ExternalReferenceUpdatePlan,
    insertion_order: &[usize],
) -> RenameResult<SourceChange> {
    let mut change = SourceChange::default();

    let mut inserted_text = String::new();
    for (idx, item_idx) in insertion_order.iter().enumerate() {
        let item = &relocation_plan.moved_items[*item_idx];
        if idx > 0 && !inserted_text.ends_with('\n') {
            inserted_text.push('\n');
        }
        inserted_text.push_str(&item.text);
        if !inserted_text.ends_with('\n') {
            inserted_text.push('\n');
        }
    }

    match &relocation_plan.destination {
        RelocationDestination::ExistingFile { file_id, insert_offset } => {
            if !inserted_text.is_empty() {
                change.insert_source_edit(*file_id, TextEdit::insert(*insert_offset, inserted_text.clone()));
            }
        }
        RelocationDestination::InlineModule { file_id, insert_offset, indent } => {
            if !inserted_text.is_empty() {
                let indented = indent_text_block(&inserted_text, &indent.to_string());
                change.insert_source_edit(*file_id, TextEdit::insert(*insert_offset, indented));
            }
        }
        RelocationDestination::NewFile { relative_path } => {
            if let Some(creation) = module_plan
                .file_creations
                .iter_mut()
                .find(|creation| creation.relative_path == *relative_path)
            {
                if !creation.initial_contents.is_empty() && !creation.initial_contents.ends_with('\n') {
                    creation.initial_contents.push('\n');
                }
                creation.initial_contents.push_str(&inserted_text);
            } else {
                bail!("Destination file creation missing for `{relative_path}`");
            }
        }
    }

    let mut deletions: BTreeMap<FileId, Vec<TextRange>> = BTreeMap::new();
    for item in &relocation_plan.moved_items {
        deletions.entry(item.file_id).or_default().push(item.range);
    }
    for (file_id, mut ranges) in deletions {
        ranges.sort_by_key(|range| range.start());
        let mut builder = TextEditBuilder::default();
        for range in ranges.into_iter().rev() {
            builder.delete(range);
        }
        let edit = builder.finish();
        if !edit.is_empty() {
            change.insert_source_edit(file_id, edit);
        }
    }

    change.extend(module_plan.module_declaration_edits());

    for edit_plan in &external_plan.edits {
        change.insert_source_edit(edit_plan.file_id, edit_plan.edit.clone());
    }

    for fs_edit in module_plan.file_system_edits() {
        change.push_file_system_edit(fs_edit);
    }

    Ok(change)
}

fn indent_text_block(text: &str, indent: &str) -> String {
    if indent.is_empty() || text.is_empty() {
        return text.to_owned();
    }
    let mut result = String::with_capacity(text.len() + indent.len());
    for chunk in text.split_inclusive('\n') {
        result.push_str(indent);
        result.push_str(chunk);
    }
    result
}

fn apply_relative_edits(text: &mut String, mut edits: Vec<(TextRange, String)>) {
    edits.sort_by_key(|(range, _)| range.start());
    for (range, replacement) in edits.into_iter().rev() {
        let start: usize = range.start().into();
        let end: usize = range.end().into();
        text.replace_range(start..end, &replacement);
    }
}

fn ranges_overlap(lhs: TextRange, rhs: TextRange) -> bool {
    if lhs.start() < rhs.end() && rhs.start() < lhs.end() {
        return true;
    }
    if lhs.is_empty() {
        return rhs.contains_inclusive(lhs.start());
    }
    if rhs.is_empty() {
        return lhs.contains_inclusive(rhs.start());
    }
    false
}

fn to_relative_range(base: TextRange, range: TextRange) -> RenameResult<TextRange> {
    if !base.contains_range(range) {
        bail!("Edit range is not contained within moved item");
    }
    let start = range.start() - base.start();
    let end = range.end() - base.start();
    Ok(TextRange::new(start, end))
}

fn definition_is_within_moved_items(
    definition: &Definition,
    moved_ranges: &[(ide_db::EditionedFileId, TextRange)],
    sema: &Semantics<'_, RootDatabase>,
) -> bool {
    definition
        .range_for_rename(sema)
        .map(|file_range| {
            moved_ranges.iter().any(|(file_id, range)| {
                file_range.file_id == *file_id && range.contains_range(file_range.range)
            })
        })
        .unwrap_or(false)
}

fn module_within_source_tree(
    mut module: Module,
    source: Module,
    db: &RootDatabase,
) -> bool {
    loop {
        if module == source {
            return true;
        }
        match module.parent(db) {
            Some(parent) => module = parent,
            None => return false,
        }
    }
}

fn path_in_use_tree(path: &ast::Path) -> bool {
    path.syntax().ancestors().any(|node| ast::UseTree::cast(node).is_some())
}

fn compute_path_replacement(path: &ast::Path, new_path: &str) -> Option<(TextRange, String)> {
    let name_ref = path.segment()?.name_ref()?;
    let path_range = path.syntax().text_range();
    let path_text = path.syntax().text();

    let offset_start = path_range.start();
    let name_range = name_ref.syntax().text_range();
    let suffix_start = name_range.end() - offset_start;
    let suffix_end = path_range.end() - offset_start;
    let suffix = path_text.slice(TextRange::new(suffix_start, suffix_end)).to_string();

    let mut replacement = String::from(new_path);
    replacement.push_str(&suffix);

    Some((path_range, replacement))
}

fn resolve_existing_destination_module(
    mut current: Module,
    move_op: &MoveOperation,
    db: &RootDatabase,
) -> Option<Module> {
    for segment in &move_op.dest_module_path {
        let child = find_child_module_by_name(current, db, segment)?;
        current = child;
    }
    Some(current)
}

fn plan_insertion_for_existing_module(
    sema: &Semantics<'_, RootDatabase>,
    module: Module,
) -> RenameResult<RelocationDestination> {
    let source = module.definition_source(sema.db);
    match source.value {
        ModuleSource::SourceFile(_) => {
            let hir_file = module.definition_source_file_id(sema.db);
            let editioned_file = hir_file.original_file(sema.db);
            let file_id = editioned_file.file_id(sema.db);
            let source_file = sema.parse(editioned_file);
            let insert_offset = source_file.syntax().text_range().end();
            Ok(RelocationDestination::ExistingFile { file_id, insert_offset })
        }
        ModuleSource::Module(ref module_ast) => {
            let item_list = module_ast
                .item_list()
                .ok_or_else(|| format_err!("Inline module missing item list"))?;
            let insert_offset = item_list
                .r_curly_token()
                .map(|tok| tok.text_range().start())
                .ok_or_else(|| format_err!("Inline module missing closing brace"))?;
            let indent = IndentLevel::from_node(item_list.syntax()) + 1;
            let file_id = source.file_id.original_file(sema.db).file_id(sema.db);
            Ok(RelocationDestination::InlineModule { file_id, insert_offset, indent })
        }
        ModuleSource::BlockExpr(_) => {
            bail!("Moving items into block expression modules is not supported yet")
        }
    }
}

fn plan_module_declaration_for_existing_parent(
    db: &RootDatabase,
    parent_module: Module,
    child_name: &Name,
) -> RenameResult<Option<ModuleDeclarationPlan>> {
    if module_declaration_exists(db, parent_module, child_name) {
        return Ok(None);
    }

    let visibility_prefix = if parent_module.parent(db).is_some() { "pub " } else { "" };
    if parent_module.is_inline(db) {
        let InFile { file_id, value: source } = parent_module.definition_source(db);
        let ModuleSource::Module(module_ast) = source else {
            bail!("Inline module missing syntax body");
        };
        let vfs_file_id = file_id.original_file(db).file_id(db);
        let plan = plan_mod_decl_in_inline_module(&module_ast, vfs_file_id, visibility_prefix, child_name.as_str())?;
        Ok(Some(plan))
    } else {
        let def_hir_file = parent_module.definition_source_file_id(db);
        let editioned_file_id = def_hir_file.original_file(db);
        let vfs_file_id = editioned_file_id.file_id(db);
        let parsed = db.parse(editioned_file_id);
        let source_file = parsed.tree();
        let plan = plan_mod_decl_in_source_file(
            db,
            vfs_file_id,
            &source_file,
            visibility_prefix,
            child_name.as_str(),
        );
        Ok(Some(plan))
    }
}

fn module_declaration_exists(db: &RootDatabase, module: Module, child_name: &Name) -> bool {
    if module.is_inline(db) {
        let InFile { value: source, .. } = module.definition_source(db);
        let ModuleSource::Module(module_ast) = source else {
            return false;
        };
        module_declaration_exists_in_inline_module(&module_ast, child_name)
    } else {
        let def_hir_file = module.definition_source_file_id(db);
        let editioned_file_id = def_hir_file.original_file(db);
        let source_file = db.parse(editioned_file_id).tree();
        module_declaration_exists_in_source_file(&source_file, child_name)
    }
}

fn module_declaration_exists_in_source_file(
    source_file: &ast::SourceFile,
    child_name: &Name,
) -> bool {
    source_file
        .items()
        .filter_map(|item| match item {
            ast::Item::Module(module) => Some(module),
            _ => None,
        })
        .any(|module| module_name_matches(&module, child_name.as_str()))
}

fn module_declaration_exists_in_inline_module(
    module_ast: &ast::Module,
    child_name: &Name,
) -> bool {
    module_ast
        .item_list()
        .into_iter()
        .flat_map(|list| list.items())
        .filter_map(|item| match item {
            ast::Item::Module(module) => Some(module),
            _ => None,
        })
        .any(|module| module_name_matches(&module, child_name.as_str()))
}

fn module_name_matches(module: &ast::Module, target: &str) -> bool {
    module.name().is_some_and(|name| name.text() == target)
}

#[allow(dead_code)]
fn plan_import_reference(
    reference: &FileReference,
    new_path: &str,
) -> Option<(TextRange, String)> {
    let name_ref = reference.name.as_name_ref()?;
    let use_tree = name_ref.syntax().ancestors().find_map(ast::UseTree::cast)?;

    let mut replacement = String::from(new_path);
    if let Some(rename) = use_tree.rename() {
        format_to!(replacement, " {}", rename.syntax().text());
    }

    Some((use_tree.syntax().text_range(), replacement))
}

#[allow(dead_code)]
fn plan_non_import_reference(
    reference: &FileReference,
    new_path: &str,
) -> Option<(TextRange, String)> {
    let name_ref = reference.name.as_name_ref()?;
    let segment = name_ref.syntax().ancestors().find_map(ast::PathSegment::cast)?;
    let path = segment.parent_path();

    if path.qualifier().is_none() {
        return None;
    }

    let path_syntax = path.syntax();
    let path_range = path_syntax.text_range();
    let path_text = path_syntax.text();

    let offset_start = path_range.start();
    let name_range = name_ref.syntax().text_range();
    let suffix_start = name_range.end() - offset_start;
    let suffix_end = path_range.end() - offset_start;
    let suffix = path_text.slice(TextRange::new(suffix_start, suffix_end)).to_string();

    let mut replacement = String::from(new_path);
    replacement.push_str(&suffix);

    Some((path_range, replacement))
}

#[allow(dead_code)]
fn format_crate_item_path(
    db: &RootDatabase,
    edition: Edition,
    module_path: &[Name],
    item_name: &Name,
) -> String {
    let mut result = String::from("crate");
    for segment in module_path {
        format_to!(result, "::{}", segment.display(db, edition));
    }
    format_to!(result, "::{}", item_name.display(db, edition));
    result
}

fn plan_mod_decl_in_source_file(
    db: &RootDatabase,
    file_id: FileId,
    source_file: &ast::SourceFile,
    visibility_prefix: &str,
    child_name: &str,
) -> ModuleDeclarationPlan {
    let file_text = db.file_text(file_id).text(db);
    let mut text = String::new();
    if !file_text.is_empty() && !file_text.ends_with('\n') {
        text.push('\n');
    }
    if !file_text.trim().is_empty() && (text.is_empty() || !text.ends_with('\n')) {
        // ensure separation from existing items
        text.push('\n');
    }
    text.push_str(visibility_prefix);
    text.push_str("mod ");
    text.push_str(child_name);
    text.push_str(";\n");
    let insert_offset = source_file.syntax().text_range().end();
    ModuleDeclarationPlan { file_id, insert_offset, text }
}

fn plan_mod_decl_in_inline_module(
    module_ast: &ast::Module,
    file_id: FileId,
    visibility_prefix: &str,
    child_name: &str,
) -> RenameResult<ModuleDeclarationPlan> {
    let item_list = module_ast
        .item_list()
        .ok_or_else(|| format_err!("Inline module missing item list"))?;
    let insert_offset = item_list
        .r_curly_token()
        .map(|tok| tok.text_range().start())
        .ok_or_else(|| format_err!("Inline module missing closing brace"))?;
    let indent = IndentLevel::from_node(item_list.syntax()) + 1;
    let text = format!("{indent}{visibility_prefix}mod {child_name};\n");
    Ok(ModuleDeclarationPlan { file_id, insert_offset, text })
}

enum ModuleCreationContext {
    Existing { module: Module },
    New { preferred_style: ModuleCreationStyle },
}

fn infer_child_style(module: Module, db: &RootDatabase) -> ModuleCreationStyle {
    let mut saw_mod_rs = false;
    let mut saw_file = false;
    for child in module.children(db) {
        if child.is_inline(db) {
            continue;
        }
        if child.is_mod_rs(db) {
            saw_mod_rs = true;
        } else {
            saw_file = true;
        }
    }
    match (saw_mod_rs, saw_file) {
        (true, false) => ModuleCreationStyle::ModRs,
        _ => ModuleCreationStyle::File,
    }
}

/// This is similar to `collect::<Result<Vec<_>, _>>`, but unlike it, it succeeds if there is *any* `Ok` item.
fn ok_if_any<T, E>(iter: impl Iterator<Item = Result<T, E>>) -> Result<Vec<T>, E> {
    let mut err = None;
    let oks = iter
        .filter_map(|item| match item {
            Ok(it) => Some(it),
            Err(it) => {
                err = Some(it);
                None
            }
        })
        .collect::<Vec<_>>();
    if !oks.is_empty() {
        Ok(oks)
    } else if let Some(err) = err {
        Err(err)
    } else {
        Ok(Vec::new())
    }
}

/// Prepares a rename. The sole job of this function is to return the TextRange of the thing that is
/// being targeted for a rename.
pub(crate) fn prepare_rename(
    db: &RootDatabase,
    position: FilePosition,
) -> RenameResult<RangeInfo<()>> {
    let sema = Semantics::new(db);
    let source_file = sema.parse_guess_edition(position.file_id);
    let syntax = source_file.syntax();

    let res = find_definitions(&sema, syntax, position, &Name::new_symbol_root(sym::underscore))?
        .filter(|(_, _, def, _, _)| def.range_for_rename(&sema).is_some())
        .map(|(frange, kind, _, _, _)| {
            always!(
                frange.range.contains_inclusive(position.offset)
                    && frange.file_id == position.file_id
            );

            Ok(match kind {
                SyntaxKind::LIFETIME => {
                    TextRange::new(frange.range.start() + TextSize::from(1), frange.range.end())
                }
                _ => frange.range,
            })
        })
        .reduce(|acc, cur| match (acc, cur) {
            // ensure all ranges are the same
            (Ok(acc_inner), Ok(cur_inner)) if acc_inner == cur_inner => Ok(acc_inner),
            (e @ Err(_), _) | (_, e @ Err(_)) => e,
            _ => bail!("inconsistent text range"),
        });

    match res {
        // ensure at least one definition was found
        Some(res) => res.map(|range| RangeInfo::new(range, ())),
        None => bail!("No references found at position"),
    }
}

// Feature: Rename
//
// Renames the item below the cursor and all of its references
//
// | Editor  | Shortcut |
// |---------|----------|
// | VS Code | <kbd>F2</kbd> |
//
// ![Rename](https://user-images.githubusercontent.com/48062697/113065582-055aae80-91b1-11eb-8ade-2b58e6d81883.gif)
pub(crate) fn rename(
    db: &RootDatabase,
    position: FilePosition,
    new_name: &str,
) -> RenameResult<SourceChange> {
    let sema = Semantics::new(db);
    let file_id = sema
        .attach_first_edition(position.file_id)
        .ok_or_else(|| format_err!("No references found at position"))?;
    let source_file = sema.parse(file_id);
    let syntax = source_file.syntax();

    let edition = file_id.edition(db);
    let parsed_target = ParsedRenameTarget::parse(edition, new_name)?;
    let final_name_text = parsed_target.final_name_text();
    let (final_name, kind) = IdentifierKind::classify(edition, final_name_text)?;

    let defs = find_definitions(&sema, syntax, position, &final_name)?;
    let move_target_opt = parsed_target.move_target().cloned();
    let alias_fallback = if move_target_opt.is_some() {
        None
    } else {
        alias_fallback(syntax, position, &final_name.display(db, edition).to_string())
    };
    let move_target_ref = move_target_opt.as_ref();

    let ops: RenameResult<Vec<SourceChange>> = match alias_fallback {
        Some(_) => ok_if_any(
            defs
                // FIXME: This can use the `ide_db::rename_reference` (or def.rename) method once we can
                // properly find "direct" usages/references.
                .map(|(.., def, resolved_name, _)| {
                    match kind {
                        IdentifierKind::Ident => (),
                        IdentifierKind::Lifetime => {
                            bail!("Cannot alias reference to a lifetime identifier")
                        }
                        IdentifierKind::Underscore => bail!("Cannot alias reference to `_`"),
                        IdentifierKind::LowercaseSelf => {
                            bail!("Cannot rename alias reference to `self`")
                        }
                    };
                    let mut usages = def.usages(&sema).all();

                    // FIXME: hack - removes the usage that triggered this rename operation.
                    match usages.references.get_mut(&file_id).and_then(|refs| {
                        refs.iter()
                            .position(|ref_| ref_.range.contains_inclusive(position.offset))
                            .map(|idx| refs.remove(idx))
                    }) {
                        Some(_) => (),
                        None => never!(),
                    };

                    let mut source_change = SourceChange::default();
                    source_change.extend(usages.references.get_mut(&file_id).iter().map(|refs| {
                        (
                            position.file_id,
                            source_edit_from_references(db, refs, def, &resolved_name, edition),
                        )
                    }));

                    Ok(source_change)
                }),
        ),
        None => ok_if_any(defs.map(|(.., def, resolved_name, rename_def)| {
            if let Some(move_target) = move_target_ref {
                if rename_def == RenameDefinition::Yes {
                    if let Some(move_operation) =
                        detect_move_operation(&sema, &def, &resolved_name, move_target)?
                    {
                        return orchestrate_move(&sema, &def, &resolved_name, &move_operation);
                    }
                }
            }
            if let Definition::Local(local) = def {
                if let Some(self_param) = local.as_self_param(sema.db) {
                    cov_mark::hit!(rename_self_to_param);
                    return rename_self_to_param(&sema, local, self_param, &resolved_name, kind);
                }
                if kind == IdentifierKind::LowercaseSelf {
                    cov_mark::hit!(rename_to_self);
                    return rename_to_self(&sema, local);
                }
            }
            def.rename(&sema, resolved_name.as_str(), rename_def)
        })),
    };

    ops?.into_iter()
        .chain(alias_fallback)
        .reduce(|acc, elem| acc.merge(elem))
        .ok_or_else(|| format_err!("No references found at position"))
}

/// Called by the client when it is about to rename a file.
pub(crate) fn will_rename_file(
    db: &RootDatabase,
    file_id: FileId,
    new_name_stem: &str,
) -> Option<SourceChange> {
    let sema = Semantics::new(db);
    let module = sema.file_to_module_def(file_id)?;
    let def = Definition::Module(module);
    let mut change = def.rename(&sema, new_name_stem, RenameDefinition::Yes).ok()?;
    change.file_system_edits.clear();
    Some(change)
}

// FIXME: Should support `extern crate`.
fn alias_fallback(
    syntax: &SyntaxNode,
    FilePosition { file_id, offset }: FilePosition,
    new_name: &str,
) -> Option<SourceChange> {
    let use_tree = syntax
        .token_at_offset(offset)
        .flat_map(|syntax| syntax.parent_ancestors())
        .find_map(ast::UseTree::cast)?;

    let last_path_segment = use_tree.path()?.segments().last()?.name_ref()?;
    if !last_path_segment.syntax().text_range().contains_inclusive(offset) {
        return None;
    };

    let mut builder = SourceChangeBuilder::new(file_id);

    match use_tree.rename() {
        Some(rename) => {
            let offset = rename.syntax().text_range();
            builder.replace(offset, format!("as {new_name}"));
        }
        None => {
            let offset = use_tree.syntax().text_range().end();
            builder.insert(offset, format!(" as {new_name}"));
        }
    }

    Some(builder.finish())
}

fn find_definitions(
    sema: &Semantics<'_, RootDatabase>,
    syntax: &SyntaxNode,
    FilePosition { file_id, offset }: FilePosition,
    new_name: &Name,
) -> RenameResult<impl Iterator<Item = (FileRange, SyntaxKind, Definition, Name, RenameDefinition)>>
{
    let maybe_format_args =
        syntax.token_at_offset(offset).find(|t| matches!(t.kind(), SyntaxKind::STRING));

    if let Some((range, _, _, Some(resolution))) =
        maybe_format_args.and_then(|token| sema.check_for_format_args_template(token, offset))
    {
        return Ok(vec![(
            FileRange { file_id, range },
            SyntaxKind::STRING,
            Definition::from(resolution),
            new_name.clone(),
            RenameDefinition::Yes,
        )]
        .into_iter());
    }

    let original_ident = syntax
        .token_at_offset(offset)
        .max_by_key(|t| {
            t.kind().is_any_identifier() || matches!(t.kind(), SyntaxKind::LIFETIME_IDENT)
        })
        .map(|t| {
            if t.kind() == SyntaxKind::LIFETIME_IDENT {
                Name::new_lifetime(t.text())
            } else {
                Name::new_root(t.text())
            }
        })
        .ok_or_else(|| format_err!("No references found at position"))?;
    let symbols =
        sema.find_namelike_at_offset_with_descend(syntax, offset).map(|name_like| {
            let kind = name_like.syntax().kind();
            let range = sema
                .original_range_opt(name_like.syntax())
                .ok_or_else(|| format_err!("No references found at position"))?;
            let res = match &name_like {
                // renaming aliases would rename the item being aliased as the HIR doesn't track aliases yet
                ast::NameLike::Name(name)
                    if name
                        .syntax()
                        .parent().is_some_and(|it| ast::Rename::can_cast(it.kind()))
                        // FIXME: uncomment this once we resolve to usages to extern crate declarations
                        // && name
                        //     .syntax()
                        //     .ancestors()
                        //     .nth(2)
                        //     .map_or(true, |it| !ast::ExternCrate::can_cast(it.kind()))
                        =>
                {
                    bail!("Renaming aliases is currently unsupported")
                }
                ast::NameLike::Name(name) => NameClass::classify(sema, name)
                    .map(|class| match class {
                        NameClass::Definition(it) | NameClass::ConstReference(it) => it,
                        NameClass::PatFieldShorthand { local_def, field_ref: _, adt_subst: _ } => {
                            Definition::Local(local_def)
                        }
                    })
                    .ok_or_else(|| format_err!("No references found at position")),
                ast::NameLike::NameRef(name_ref) => {
                    NameRefClass::classify(sema, name_ref)
                        .map(|class| match class {
                            NameRefClass::Definition(def, _) => def,
                            NameRefClass::FieldShorthand { local_ref, field_ref: _, adt_subst: _ } => {
                                Definition::Local(local_ref)
                            }
                            NameRefClass::ExternCrateShorthand { decl, .. } => {
                                Definition::ExternCrateDecl(decl)
                            }
                        })
                        // FIXME: uncomment this once we resolve to usages to extern crate declarations
                        .filter(|def| !matches!(def, Definition::ExternCrateDecl(..)))
                        .ok_or_else(|| format_err!("No references found at position"))
                        .and_then(|def| {
                            // if the name differs from the definitions name it has to be an alias
                            if def
                                .name(sema.db).is_some_and(|it| it.as_str() != name_ref.text().trim_start_matches("r#"))
                            {
                                Err(format_err!("Renaming aliases is currently unsupported"))
                            } else {
                                Ok(def)
                            }
                        })
                }
                ast::NameLike::Lifetime(lifetime) => {
                    NameRefClass::classify_lifetime(sema, lifetime)
                        .and_then(|class| match class {
                            NameRefClass::Definition(def, _) => Some(def),
                            _ => None,
                        })
                        .or_else(|| {
                            NameClass::classify_lifetime(sema, lifetime).and_then(|it| match it {
                                NameClass::Definition(it) => Some(it),
                                _ => None,
                            })
                        })
                        .ok_or_else(|| format_err!("No references found at position"))
                }
            };
            res.map(|def| {
                let n = def.name(sema.db)?;
                if n == original_ident {
                    Some((range, kind, def, new_name.clone(), RenameDefinition::Yes))
                } else if let Some(suffix) =  n.as_str().strip_prefix(original_ident.as_str()) {
                    Some((range, kind, def, Name::new_root(&format!("{}{suffix}", new_name.as_str())), RenameDefinition::No))
                } else {
                     n.as_str().strip_suffix(original_ident.as_str().trim_start_matches('\''))
                        .map(|prefix| (range, kind, def, Name::new_root(&format!("{prefix}{}", new_name.as_str())), RenameDefinition::No))
                }
            })
        });

    let res: RenameResult<Vec<_>> = ok_if_any(symbols.filter_map(Result::transpose));
    match res {
        Ok(v) => {
            // remove duplicates, comparing `Definition`s
            Ok(v.into_iter()
                .unique_by(|&(.., def, _, _)| def)
                .map(|(a, b, c, d, e)| (a.into_file_id(sema.db), b, c, d, e))
                .collect::<Vec<_>>()
                .into_iter())
        }
        Err(e) => Err(e),
    }
}

fn transform_assoc_fn_into_method_call(
    sema: &Semantics<'_, RootDatabase>,
    source_change: &mut SourceChange,
    f: hir::Function,
) {
    let calls = Definition::Function(f).usages(sema).all();
    for (file_id, calls) in calls {
        for call in calls {
            let Some(fn_name) = call.name.as_name_ref() else { continue };
            let Some(path) = fn_name.syntax().parent().and_then(ast::PathSegment::cast) else {
                continue;
            };
            let path = path.parent_path();
            // The `PathExpr` is the direct parent, above it is the `CallExpr`.
            let Some(call) =
                path.syntax().parent().and_then(|it| ast::CallExpr::cast(it.parent()?))
            else {
                continue;
            };

            let Some(arg_list) = call.arg_list() else { continue };
            let mut args = arg_list.args();
            let Some(mut self_arg) = args.next() else { continue };
            let second_arg = args.next();

            // Strip (de)references, as they will be taken automatically by auto(de)ref.
            loop {
                let self_ = match &self_arg {
                    ast::Expr::RefExpr(self_) => self_.expr(),
                    ast::Expr::ParenExpr(self_) => self_.expr(),
                    ast::Expr::PrefixExpr(self_)
                        if self_.op_kind() == Some(ast::UnaryOp::Deref) =>
                    {
                        self_.expr()
                    }
                    _ => break,
                };
                self_arg = match self_ {
                    Some(it) => it,
                    None => break,
                };
            }

            let self_needs_parens =
                self_arg.precedence().needs_parentheses_in(ExprPrecedence::Postfix);

            let replace_start = path.syntax().text_range().start();
            let replace_end = match second_arg {
                Some(second_arg) => second_arg.syntax().text_range().start(),
                None => arg_list
                    .r_paren_token()
                    .map(|it| it.text_range().start())
                    .unwrap_or_else(|| arg_list.syntax().text_range().end()),
            };
            let replace_range = TextRange::new(replace_start, replace_end);

            let Some(macro_mapped_self) = sema.original_range_opt(self_arg.syntax()) else {
                continue;
            };
            let mut replacement = String::new();
            if self_needs_parens {
                replacement.push('(');
            }
            replacement.push_str(macro_mapped_self.text(sema.db));
            if self_needs_parens {
                replacement.push(')');
            }
            replacement.push('.');
            format_to!(replacement, "{fn_name}");
            replacement.push('(');

            source_change.insert_source_edit(
                file_id.file_id(sema.db),
                TextEdit::replace(replace_range, replacement),
            );
        }
    }
}

fn rename_to_self(
    sema: &Semantics<'_, RootDatabase>,
    local: hir::Local,
) -> RenameResult<SourceChange> {
    if never!(local.is_self(sema.db)) {
        bail!("rename_to_self invoked on self");
    }

    let fn_def = match local.parent(sema.db) {
        hir::DefWithBody::Function(func) => func,
        _ => bail!("Cannot rename local to self outside of function"),
    };

    if fn_def.self_param(sema.db).is_some() {
        bail!("Method already has a self parameter");
    }

    let params = fn_def.assoc_fn_params(sema.db);
    let first_param = params
        .first()
        .ok_or_else(|| format_err!("Cannot rename local to self unless it is a parameter"))?;
    match first_param.as_local(sema.db) {
        Some(plocal) => {
            if plocal != local {
                bail!("Only the first parameter may be renamed to self");
            }
        }
        None => bail!("rename_to_self invoked on destructuring parameter"),
    }

    let assoc_item = fn_def
        .as_assoc_item(sema.db)
        .ok_or_else(|| format_err!("Cannot rename parameter to self for free function"))?;
    let impl_ = match assoc_item.container(sema.db) {
        hir::AssocItemContainer::Trait(_) => {
            bail!("Cannot rename parameter to self for trait functions");
        }
        hir::AssocItemContainer::Impl(impl_) => impl_,
    };
    let first_param_ty = first_param.ty();
    let impl_ty = impl_.self_ty(sema.db);
    let (ty, self_param) = if impl_ty.remove_ref().is_some() {
        // if the impl is a ref to the type we can just match the `&T` with self directly
        (first_param_ty.clone(), "self")
    } else {
        first_param_ty.remove_ref().map_or((first_param_ty.clone(), "self"), |ty| {
            (ty, if first_param_ty.is_mutable_reference() { "&mut self" } else { "&self" })
        })
    };

    if ty != impl_ty {
        bail!("Parameter type differs from impl block type");
    }

    let InFile { file_id, value: param_source } = sema
        .source(first_param.clone())
        .ok_or_else(|| format_err!("No source for parameter found"))?;

    let def = Definition::Local(local);
    let usages = def.usages(sema).all();
    let mut source_change = SourceChange::default();
    source_change.extend(usages.iter().map(|(file_id, references)| {
        (
            file_id.file_id(sema.db),
            source_edit_from_references(
                sema.db,
                references,
                def,
                &Name::new_symbol_root(sym::self_),
                file_id.edition(sema.db),
            ),
        )
    }));
    source_change.insert_source_edit(
        file_id.original_file(sema.db).file_id(sema.db),
        TextEdit::replace(param_source.syntax().text_range(), String::from(self_param)),
    );
    transform_assoc_fn_into_method_call(sema, &mut source_change, fn_def);
    Ok(source_change)
}

fn rename_self_to_param(
    sema: &Semantics<'_, RootDatabase>,
    local: hir::Local,
    self_param: hir::SelfParam,
    new_name: &Name,
    identifier_kind: IdentifierKind,
) -> RenameResult<SourceChange> {
    if identifier_kind == IdentifierKind::LowercaseSelf {
        // Let's do nothing rather than complain.
        cov_mark::hit!(rename_self_to_self);
        return Ok(SourceChange::default());
    }

    let InFile { file_id, value: self_param } =
        sema.source(self_param).ok_or_else(|| format_err!("cannot find function source"))?;

    let def = Definition::Local(local);
    let usages = def.usages(sema).all();
    let edit = text_edit_from_self_param(
        &self_param,
        new_name.display(sema.db, file_id.edition(sema.db)).to_string(),
    )
    .ok_or_else(|| format_err!("No target type found"))?;
    if usages.len() > 1 && identifier_kind == IdentifierKind::Underscore {
        bail!("Cannot rename reference to `_` as it is being referenced multiple times");
    }
    let mut source_change = SourceChange::default();
    source_change.insert_source_edit(file_id.original_file(sema.db).file_id(sema.db), edit);
    source_change.extend(usages.iter().map(|(file_id, references)| {
        (
            file_id.file_id(sema.db),
            source_edit_from_references(
                sema.db,
                references,
                def,
                new_name,
                file_id.edition(sema.db),
            ),
        )
    }));
    Ok(source_change)
}

fn text_edit_from_self_param(self_param: &ast::SelfParam, new_name: String) -> Option<TextEdit> {
    let mut replacement_text = new_name;
    replacement_text.push_str(": ");

    if self_param.amp_token().is_some() {
        replacement_text.push('&');
    }
    if let Some(lifetime) = self_param.lifetime() {
        write!(replacement_text, "{lifetime} ").unwrap();
    }
    if self_param.amp_token().and(self_param.mut_token()).is_some() {
        replacement_text.push_str("mut ");
    }

    replacement_text.push_str("Self");

    Some(TextEdit::replace(self_param.syntax().text_range(), replacement_text))
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use ide_db::source_change::SourceChange;
    use ide_db::text_edit::TextEdit;
    use itertools::Itertools;
    use stdx::trim_indent;
    use test_utils::assert_eq_text;

    use crate::fixture;

    use hir::{Name, Semantics};
    use syntax::AstNode;
    use span::Edition;

    use super::{
        detect_move_operation, find_definitions, plan_external_reference_updates, plan_internal_reference_updates,
        plan_item_relocation, plan_module_file_system_ops, ExternalReferenceUpdatePlan, IdentifierKind,
        InternalPathRewrite, InternalReferenceUpdatePlan, ItemRelocationPlan, ModuleFileSystemPlan, ParsedRenameTarget,
        RangeInfo, RelocationDestination, RenameDefinition, RenameError,
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

    #[track_caller]
    fn check_conflicts(new_name: &str, #[rust_analyzer::rust_fixture] ra_fixture: &str) {
        let (analysis, position, conflicts) = fixture::annotations(ra_fixture);
        let source_change = analysis.rename(position, new_name).unwrap().unwrap();
        let expected_conflicts = conflicts
            .into_iter()
            .map(|(file_range, _)| (file_range.file_id, file_range.range))
            .sorted_unstable_by_key(|(file_id, range)| (*file_id, range.start()))
            .collect_vec();
        let found_conflicts = source_change
            .source_file_edits
            .iter()
            .filter(|(_, (edit, _))| edit.change_annotation().is_some())
            .flat_map(|(file_id, (edit, _))| {
                edit.into_iter().map(move |edit| (*file_id, edit.delete))
            })
            .sorted_unstable_by_key(|(file_id, range)| (*file_id, range.start()))
            .collect_vec();
        assert_eq!(
            expected_conflicts, found_conflicts,
            "rename conflicts mismatch: {source_change:#?}"
        );
    }

    fn check_expect(
        new_name: &str,
        #[rust_analyzer::rust_fixture] ra_fixture: &str,
        expect: Expect,
    ) {
        let (analysis, position) = fixture::position(ra_fixture);
        let source_change =
            analysis.rename(position, new_name).unwrap().expect("Expect returned a RenameError");
        expect.assert_eq(&filter_expect(source_change))
    }

    fn check_expect_will_rename_file(
        new_name: &str,
        #[rust_analyzer::rust_fixture] ra_fixture: &str,
        expect: Expect,
    ) {
        let (analysis, position) = fixture::position(ra_fixture);
        let source_change = analysis
            .will_rename_file(position.file_id, new_name)
            .unwrap()
            .expect("Expect returned a RenameError");
        expect.assert_eq(&filter_expect(source_change))
    }

    fn check_prepare(#[rust_analyzer::rust_fixture] ra_fixture: &str, expect: Expect) {
        let (analysis, position) = fixture::position(ra_fixture);
        let result = analysis
            .prepare_rename(position)
            .unwrap_or_else(|err| panic!("PrepareRename was cancelled: {err}"));
        match result {
            Ok(RangeInfo { range, info: () }) => {
                let source = analysis.file_text(position.file_id).unwrap();
                expect.assert_eq(&format!("{range:?}: {}", &source[range]))
            }
            Err(RenameError(err)) => expect.assert_eq(&err),
        };
    }

    fn filter_expect(source_change: SourceChange) -> String {
        let source_file_edits = source_change
            .source_file_edits
            .into_iter()
            .map(|(id, (text_edit, _))| (id, text_edit.into_iter().collect::<Vec<_>>()))
            .collect::<Vec<_>>();

        format!(
            "source_file_edits: {:#?}\nfile_system_edits: {:#?}\n",
            source_file_edits, source_change.file_system_edits
        )
    }

    #[test]
    fn parse_move_target_from_crate_path() {
        let parsed = ParsedRenameTarget::parse(
            Edition::Edition2021,
            "crate::mod_one::finish::Final",
        )
        .unwrap();

        assert_eq!(parsed.final_name_text(), "Final");
        let target = parsed.move_target().expect("expected move target");
        let segments: Vec<_> =
            target.module_path().iter().map(|name| name.as_str()).collect();
        assert_eq!(segments, ["mod_one", "finish"]);

        let source_path = vec![Name::new_root("mod_one")];
        assert!(target.requires_move_from(&source_path));
    }

    #[test]
    fn parse_move_target_same_module_is_simple_rename() {
        let parsed =
            ParsedRenameTarget::parse(Edition::Edition2021, "crate::alpha::NewName").unwrap();

        assert_eq!(parsed.final_name_text(), "NewName");
        let target = parsed.move_target().expect("expected move target");
        let source_path = vec![Name::new_root("alpha")];
        assert!(!target.requires_move_from(&source_path));
    }

    #[test]
    fn parse_move_target_requires_crate_prefix() {
        let err =
            ParsedRenameTarget::parse(Edition::Edition2021, "alpha::beta::Item").unwrap_err();
        assert_eq!(err.to_string(), "Rename-to-move paths must start with `crate::`");
    }

    fn compute_move_plans(
        new_name: &str,
        #[rust_analyzer::rust_fixture] ra_fixture_before: &str,
    ) -> (ModuleFileSystemPlan, ItemRelocationPlan) {
        let (analysis, position) = fixture::position(ra_fixture_before);
        let db = &analysis.db;
        let sema = Semantics::new(db);

        let file_id = sema
            .attach_first_edition(position.file_id)
            .expect("failed to attach file edition for move plan");
        let source_file = sema.parse(file_id);
        let syntax = source_file.syntax();

        let edition = file_id.edition(db);
        let parsed_target = ParsedRenameTarget::parse(edition, new_name)
            .expect("expected move target parsing to succeed");
        let move_target = parsed_target
            .move_target()
            .expect("compute_move_plan requires a move target");
        let (final_name, _) =
            IdentifierKind::classify(edition, parsed_target.final_name_text()).unwrap();

        let definitions = find_definitions(&sema, syntax, position, &final_name)
            .expect("failed to find definitions")
            .collect::<Vec<_>>();
        assert!(
            !definitions.is_empty(),
            "expected at least one definition when computing move plan"
        );

        for (_, _, def, resolved_name, rename_def) in definitions {
            if rename_def != RenameDefinition::Yes {
                continue;
            }
            if let Some(move_operation) =
                detect_move_operation(&sema, &def, &resolved_name, move_target)
                    .expect("move detection failed")
            {
                let module_plan =
                    plan_module_file_system_ops(&sema, &def, &move_operation)
                        .expect("module filesystem planning failed");
                let relocation_plan = plan_item_relocation(&sema, &def, &move_operation, &module_plan)
                    .expect("item relocation planning failed");
                return (module_plan, relocation_plan);
            }
        }
        panic!("No move operation was detected for the provided fixture");
    }

    fn compute_external_reference_plan(
        new_name: &str,
        #[rust_analyzer::rust_fixture] ra_fixture_before: &str,
    ) -> (crate::Analysis, ExternalReferenceUpdatePlan) {
        let (analysis, position) = fixture::position(ra_fixture_before);
        let db = &analysis.db;
        let sema = Semantics::new(db);

        let file_id = sema
            .attach_first_edition(position.file_id)
            .expect("failed to attach file edition for external reference plan");
        let source_file = sema.parse(file_id);
        let syntax = source_file.syntax();

        let edition = file_id.edition(db);
        let parsed_target = ParsedRenameTarget::parse(edition, new_name)
            .expect("expected move target parsing to succeed");
        let move_target = parsed_target
            .move_target()
            .expect("external reference plan requires a move target");
        let (final_name, _) =
            IdentifierKind::classify(edition, parsed_target.final_name_text()).unwrap();

        let definitions = find_definitions(&sema, syntax, position, &final_name)
            .expect("failed to find definitions")
            .collect::<Vec<_>>();
        assert!(
            !definitions.is_empty(),
            "expected at least one definition when computing external reference plan"
        );

        for (_, _, def, resolved_name, rename_def) in definitions {
            if rename_def != RenameDefinition::Yes {
                continue;
            }
            if let Some(move_operation) =
                detect_move_operation(&sema, &def, &resolved_name, move_target)
                    .expect("move detection failed")
            {
                let module_plan =
                    plan_module_file_system_ops(&sema, &def, &move_operation)
                        .expect("module filesystem planning failed");
                let relocation_plan = plan_item_relocation(&sema, &def, &move_operation, &module_plan)
                    .expect("item relocation planning failed");
                let external_plan = plan_external_reference_updates(
                    &sema,
                    &def,
                    &move_operation,
                    &relocation_plan,
                )
                .expect("external reference planning failed");
                return (analysis, external_plan);
            }
        }
        panic!("No move operation was detected for the provided fixture");
    }

    fn compute_internal_reference_plan(
        new_name: &str,
        #[rust_analyzer::rust_fixture] ra_fixture_before: &str,
    ) -> (crate::Analysis, ItemRelocationPlan, InternalReferenceUpdatePlan) {
        let (analysis, position) = fixture::position(ra_fixture_before);
        let db = &analysis.db;
        let sema = Semantics::new(db);

        let file_id = sema
            .attach_first_edition(position.file_id)
            .expect("failed to attach file edition for internal reference plan");
        let source_file = sema.parse(file_id);
        let syntax = source_file.syntax();

        let edition = file_id.edition(db);
        let parsed_target = ParsedRenameTarget::parse(edition, new_name)
            .expect("expected move target parsing to succeed");
        let move_target = parsed_target
            .move_target()
            .expect("internal reference plan requires a move target");
        let (final_name, _) =
            IdentifierKind::classify(edition, parsed_target.final_name_text()).unwrap();

        let definitions = find_definitions(&sema, syntax, position, &final_name)
            .expect("failed to find definitions")
            .collect::<Vec<_>>();
        assert!(
            !definitions.is_empty(),
            "expected at least one definition when computing internal reference plan"
        );

        for (_, _, def, resolved_name, rename_def) in definitions {
            if rename_def != RenameDefinition::Yes {
                continue;
            }
            if let Some(move_operation) =
                detect_move_operation(&sema, &def, &resolved_name, move_target)
                    .expect("move detection failed")
            {
                let module_plan =
                    plan_module_file_system_ops(&sema, &def, &move_operation)
                        .expect("module filesystem planning failed");
                let relocation_plan = plan_item_relocation(&sema, &def, &move_operation, &module_plan)
                    .expect("item relocation planning failed");
                let internal_plan =
                    plan_internal_reference_updates(&sema, &def, &relocation_plan)
                        .expect("internal reference planning failed");
                return (analysis, relocation_plan, internal_plan);
            }
        }
        panic!("No move operation was detected for the provided fixture");
    }

    #[test]
    fn module_fs_plan_creates_nested_modules() {
        let (plan, _) = compute_move_plans(
            "crate::mod_two::finish::Final",
            r#"
//- /lib.rs
mod mod_one;
struct ToMove$0;

//- /mod_one.rs
pub struct Existing;
"#,
        );

        assert_eq!(plan.file_paths(), ["mod_two.rs", "mod_two/finish.rs"]);
        assert_eq!(plan.destination_path(), "mod_two/finish.rs");
        assert_eq!(
            plan.file_initial_contents(),
            ["pub mod finish;\n", ""]
        );
        let decls: Vec<_> = plan
            .module_declaration_texts()
            .into_iter()
            .map(|text| text.trim().to_owned())
            .collect();
        assert_eq!(decls, ["mod mod_two;"]);
    }

    #[test]
    fn module_fs_plan_uses_existing_destination() {
        let (plan, _) = compute_move_plans(
            "crate::mod_one::Final",
            r#"
//- /lib.rs
mod mod_one;
struct ToMove$0;

//- /mod_one.rs
struct Existing;
"#,
        );

        assert!(plan.file_paths().is_empty());
        assert_eq!(plan.destination_path(), "mod_one.rs");
        assert!(plan.file_initial_contents().is_empty());
        assert!(plan.module_declaration_texts().is_empty());
    }

    #[test]
    fn module_fs_plan_prefers_mod_rs_when_siblings_use_it() {
        let (plan, _) = compute_move_plans(
            "crate::new_mod::Final",
            r#"
//- /lib.rs
mod existing;
struct Item$0;

//- /existing/mod.rs
pub struct Something;
"#,
        );

        assert_eq!(plan.file_paths(), ["new_mod/mod.rs"]);
        assert_eq!(plan.destination_path(), "new_mod/mod.rs");
        assert_eq!(plan.file_initial_contents(), [""]);
        let decls: Vec<_> = plan
            .module_declaration_texts()
            .into_iter()
            .map(|text| text.trim().to_owned())
            .collect();
        assert_eq!(decls, ["mod new_mod;"]);
    }

    #[test]
    fn module_fs_plan_skips_duplicate_declarations() {
        let (plan, _) = compute_move_plans(
            "crate::mod_one::finish::Final",
            r#"
//- /lib.rs
mod mod_one;
struct ToMove$0;

//- /mod_one.rs
pub mod finish;
"#,
        );

        assert_eq!(plan.file_paths(), ["mod_one/finish.rs"]);
        assert_eq!(plan.file_initial_contents(), [""]);
        assert!(plan.module_declaration_texts().is_empty());
    }

    #[test]
    fn relocation_plan_existing_destination() {
        let (_, relocation) = compute_move_plans(
            "crate::mod_one::Final",
            r#"
//- /lib.rs
mod mod_one;
struct ToMove$0 {
    field: i32,
}

impl ToMove {
    fn new() -> Self { Self { field: 0 } }
}

//- /mod_one.rs
struct Existing;
"#,
        );

        assert_eq!(relocation.moved_items.len(), 2);
        let texts: Vec<_> = relocation
            .moved_items
            .iter()
            .map(|item| item.text.trim().to_owned())
            .collect();
        assert!(texts[0].starts_with("struct ToMove"));
        assert!(texts[1].starts_with("impl ToMove"));
        match relocation.destination {
            RelocationDestination::ExistingFile { .. } => {}
            _ => panic!("expected existing file destination"),
        }
    }

    #[test]
    fn relocation_plan_new_file_destination() {
        let (module_plan, relocation) = compute_move_plans(
            "crate::fresh::ToMove",
            r#"
//- /lib.rs
struct ToMove$0;
"#,
        );

        assert_eq!(module_plan.file_paths(), ["fresh.rs"]);
        assert_eq!(relocation.moved_items.len(), 1);
        match relocation.destination {
            RelocationDestination::NewFile { ref relative_path } => {
                assert_eq!(relative_path, "fresh.rs");
            }
            _ => panic!("expected new file destination"),
        }
    }

    #[test]
    fn external_reference_plan_updates_use_and_paths() {
        let (analysis, plan) = compute_external_reference_plan(
            "crate::beta::Final",
            r#"
//- /lib.rs
mod alpha;
mod beta;

//- /alpha.rs
pub struct ToMove$0;

//- /beta.rs
use crate::alpha::ToMove;

fn f() {
    let _ = crate::alpha::ToMove::default();
}
"#,
        );

        let edits = plan.edits();
        assert_eq!(edits.len(), 1, "expected edits in beta.rs only");
        let edit_plan = &edits[0];
        let mut text = analysis.file_text(edit_plan.file_id).unwrap().to_string();
        edit_plan.edit.apply(&mut text);
        let expected = trim_indent(
            r#"
use crate::beta::Final;

fn f() {
    let _ = crate::beta::Final::default();
}
"#
        );
        assert_eq_text!(expected.as_str(), text.as_str());
    }

    #[test]
    fn external_reference_plan_updates_relative_imports() {
        let (analysis, plan) = compute_external_reference_plan(
            "crate::beta::Final",
            r#"
//- /lib.rs
mod alpha;
mod beta;

//- /alpha.rs
pub struct ToMove$0;

pub mod nested {
    use super::ToMove;

    pub fn access() {
        let _ = super::super::alpha::ToMove;
    }
}
"#,
        );

        let edits = plan.edits();
        assert_eq!(edits.len(), 1, "expected edits in alpha.rs only");
        let edit_plan = &edits[0];
        let mut text = analysis.file_text(edit_plan.file_id).unwrap().to_string();
        edit_plan.edit.apply(&mut text);
        let expected = trim_indent(
            r#"
pub struct ToMove;

pub mod nested {
    use crate::beta::Final;

    pub fn access() {
        let _ = crate::beta::Final;
    }
}
"#
        );
        assert_eq_text!(expected.as_str(), text.as_str());
    }

    #[test]
    fn external_reference_plan_preserves_use_alias() {
        let (analysis, plan) = compute_external_reference_plan(
            "crate::beta::Final",
            r#"
//- /lib.rs
mod alpha;
mod beta;
mod consumer;

//- /alpha.rs
pub struct ToMove$0;

//- /consumer.rs
use crate::alpha::ToMove as OldAlias;

pub fn make() -> OldAlias {
    OldAlias {}
}
"#,
        );

        let edits = plan.edits();
        assert_eq!(edits.len(), 1, "expected edits in consumer.rs only");
        let edit_plan = &edits[0];
        let mut text = analysis.file_text(edit_plan.file_id).unwrap().to_string();
        edit_plan.edit.apply(&mut text);
        let expected = trim_indent(
            r#"
use crate::beta::Final as OldAlias;

pub fn make() -> OldAlias {
    OldAlias {}
}
"#
        );
        assert_eq_text!(expected.as_str(), text.as_str());
    }

    #[test]
    fn internal_reference_plan_adds_imports_for_unqualified_references() {
        let (analysis, relocation, plan) = compute_internal_reference_plan(
            "crate::beta::Foo",
            r#"
//- /lib.rs
mod alpha;
mod beta;

//- /alpha.rs
pub struct Helper;
pub fn helper_fn() {}

pub struct Foo$0 {
    helper: Helper,
}

impl Foo {
    fn call() {
        helper_fn();
    }
}
"#,
        );

        let mut paths = plan.import_paths();
        paths.sort();
        assert_eq!(paths, ["crate::alpha::Helper", "crate::alpha::helper_fn"]);
        assert!(plan.rewrites().is_empty());

        // Ensure relocated text remains unchanged before rewrite stage.
        let moved_file = relocation.moved_items[0].file_id;
        let original = analysis.file_text(moved_file).unwrap();
        let updated = apply_rewrites(
            original.to_string(),
            plan.rewrites()
                .iter()
                .filter(|rewrite| rewrite.file_id().file_id(&analysis.db) == moved_file)
                .cloned()
                .collect::<Vec<_>>()
                .as_slice(),
        );
        assert_eq_text!(original.as_ref(), updated.as_str());
    }

    #[test]
    fn internal_reference_plan_rewrites_relative_paths() {
        let (analysis, relocation, plan) = compute_internal_reference_plan(
            "crate::beta::Foo",
            r#"
//- /lib.rs
mod alpha;
mod beta;

//- /alpha.rs
pub mod utils {
    pub fn helper() {}
}

pub struct Foo$0;

impl Foo {
    fn call() {
        self::utils::helper();
    }
}
"#,
        );

        assert!(plan.import_paths().is_empty());
        let source_file = relocation.moved_items[0].editioned_file_id.file_id(&analysis.db);
        let mut rewrites = plan
            .rewrites()
            .iter()
            .filter(|rewrite| rewrite.file_id().file_id(&analysis.db) == source_file)
            .cloned()
            .collect::<Vec<_>>();
        assert_eq!(rewrites.len(), 1);
        rewrites.sort_by_key(|rewrite| rewrite.range().start());

        let original = analysis.file_text(source_file).unwrap().to_string();
        let updated = apply_rewrites(original, &rewrites);
        assert!(updated.contains("crate::alpha::utils::helper();"));
    }

    #[test]
    fn internal_reference_plan_skips_moved_definitions() {
        let (_, _, plan) = compute_internal_reference_plan(
            "crate::beta::Foo",
            r#"
//- /lib.rs
mod alpha;
mod beta;

//- /alpha.rs
pub struct Foo$0;

impl Foo {
    fn new() -> Foo {
        Foo {}
    }
}
"#,
        );

        assert!(plan.import_paths().is_empty());
        assert!(plan.rewrites().is_empty());
    }

    fn apply_rewrites(mut text: String, rewrites: &[InternalPathRewrite]) -> String {
        let mut sorted = rewrites.to_vec();
        sorted.sort_by_key(|rewrite| rewrite.range().start());
        for rewrite in sorted.into_iter().rev() {
            let start: usize = rewrite.range().start().into();
            let end: usize = rewrite.range().end().into();
            text.replace_range(start..end, rewrite.replacement());
        }
        text
    }

    #[test]
    fn rename_will_shadow() {
        check_conflicts(
            "new_name",
            r#"
fn foo() {
    let mut new_name = 123;
    let old_name$0 = 456;
     // ^^^^^^^^
    new_name = 789 + new_name;
}
        "#,
        );
    }

    #[test]
    fn rename_will_be_shadowed() {
        check_conflicts(
            "new_name",
            r#"
fn foo() {
    let mut old_name$0 = 456;
         // ^^^^^^^^
    let new_name = 123;
    old_name = 789 + old_name;
 // ^^^^^^^^         ^^^^^^^^
}
        "#,
        );
    }

    #[test]
    fn test_prepare_rename_namelikes() {
        check_prepare(r"fn name$0<'lifetime>() {}", expect![[r#"3..7: name"#]]);
        check_prepare(r"fn name<'lifetime$0>() {}", expect![[r#"9..17: lifetime"#]]);
        check_prepare(r"fn name<'lifetime>() { name$0(); }", expect![[r#"23..27: name"#]]);
    }

    #[test]
    fn test_prepare_rename_in_macro() {
        check_prepare(
            r"macro_rules! foo {
    ($ident:ident) => {
        pub struct $ident;
    }
}
foo!(Foo$0);",
            expect![[r#"83..86: Foo"#]],
        );
    }

    #[test]
    fn test_prepare_rename_keyword() {
        check_prepare(r"struct$0 Foo;", expect![[r#"No references found at position"#]]);
    }

    #[test]
    fn test_prepare_rename_tuple_field() {
        check_prepare(
            r#"
struct Foo(i32);

fn baz() {
    let mut x = Foo(4);
    x.0$0 = 5;
}
"#,
            expect![[r#"No references found at position"#]],
        );
    }

    #[test]
    fn test_prepare_rename_builtin() {
        check_prepare(
            r#"
fn foo() {
    let x: i32$0 = 0;
}
"#,
            expect![[r#"No references found at position"#]],
        );
    }

    #[test]
    fn test_prepare_rename_self() {
        check_prepare(
            r#"
struct Foo {}

impl Foo {
    fn foo(self) -> Self$0 {
        self
    }
}
"#,
            expect![[r#"No references found at position"#]],
        );
    }

    #[test]
    fn test_rename_to_underscore() {
        check("_", r#"fn main() { let i$0 = 1; }"#, r#"fn main() { let _ = 1; }"#);
    }

    #[test]
    fn test_rename_to_raw_identifier() {
        check("r#fn", r#"fn main() { let i$0 = 1; }"#, r#"fn main() { let r#fn = 1; }"#);
    }

    #[test]
    fn test_rename_to_invalid_identifier1() {
        check(
            "invalid!",
            r#"fn main() { let i$0 = 1; }"#,
            "error: Invalid name `invalid!`: not an identifier",
        );
    }

    #[test]
    fn test_rename_to_invalid_identifier2() {
        check(
            "multiple tokens",
            r#"fn main() { let i$0 = 1; }"#,
            "error: Invalid name `multiple tokens`: not an identifier",
        );
    }

    #[test]
    fn test_rename_to_invalid_identifier3() {
        check(
            "super",
            r#"fn main() { let i$0 = 1; }"#,
            "error: Invalid name `super`: cannot rename to a keyword",
        );
    }

    #[test]
    fn test_rename_to_invalid_identifier_lifetime() {
        cov_mark::check!(rename_not_an_ident_ref);
        check(
            "'foo",
            r#"fn main() { let i$0 = 1; }"#,
            "error: Invalid name `'foo`: not an identifier",
        );
    }

    #[test]
    fn test_rename_to_invalid_identifier_lifetime2() {
        check(
            "_",
            r#"fn main<'a>(_: &'a$0 ()) {}"#,
            r#"error: Invalid name `_`: not a lifetime identifier"#,
        );
    }

    #[test]
    fn test_rename_accepts_lifetime_without_apostrophe() {
        check("foo", r#"fn main<'a>(_: &'a$0 ()) {}"#, r#"fn main<'foo>(_: &'foo ()) {}"#);
    }

    #[test]
    fn test_rename_to_underscore_invalid() {
        cov_mark::check!(rename_underscore_multiple);
        check(
            "_",
            r#"fn main(foo$0: ()) {foo;}"#,
            "error: Cannot rename reference to `_` as it is being referenced multiple times",
        );
    }

    #[test]
    fn test_rename_mod_invalid() {
        check(
            "'foo",
            r#"mod foo$0 {}"#,
            "error: Invalid name `'foo`: cannot rename module to 'foo",
        );
    }

    #[test]
    fn test_rename_mod_invalid_raw_ident() {
        check(
            "r#self",
            r#"mod foo$0 {}"#,
            "error: Invalid name `self`: cannot rename module to self",
        );
    }

    #[test]
    fn test_rename_for_local() {
        check(
            "k",
            r#"
fn main() {
    let mut i = 1;
    let j = 1;
    i = i$0 + j;

    { i = 0; }

    i = 5;
}
"#,
            r#"
fn main() {
    let mut k = 1;
    let j = 1;
    k = k + j;

    { k = 0; }

    k = 5;
}
"#,
        );
    }

    #[test]
    fn test_rename_unresolved_reference() {
        check(
            "new_name",
            r#"fn main() { let _ = unresolved_ref$0; }"#,
            "error: No references found at position",
        );
    }

    #[test]
    fn test_rename_macro_multiple_occurrences() {
        check(
            "Baaah",
            r#"macro_rules! foo {
    ($ident:ident) => {
        const $ident: () = ();
        struct $ident {}
    };
}

foo!($0Foo);
const _: () = Foo;
const _: Foo = Foo {};
    "#,
            r#"
macro_rules! foo {
    ($ident:ident) => {
        const $ident: () = ();
        struct $ident {}
    };
}

foo!(Baaah);
const _: () = Baaah;
const _: Baaah = Baaah {};
    "#,
        )
    }

    #[test]
    fn test_rename_for_macro_args() {
        check(
            "b",
            r#"
macro_rules! foo {($i:ident) => {$i} }
fn main() {
    let a$0 = "test";
    foo!(a);
}
"#,
            r#"
macro_rules! foo {($i:ident) => {$i} }
fn main() {
    let b = "test";
    foo!(b);
}
"#,
        );
    }

    #[test]
    fn test_rename_for_macro_args_rev() {
        check(
            "b",
            r#"
macro_rules! foo {($i:ident) => {$i} }
fn main() {
    let a = "test";
    foo!(a$0);
}
"#,
            r#"
macro_rules! foo {($i:ident) => {$i} }
fn main() {
    let b = "test";
    foo!(b);
}
"#,
        );
    }

    #[test]
    fn test_rename_for_macro_define_fn() {
        check(
            "bar",
            r#"
macro_rules! define_fn {($id:ident) => { fn $id{} }}
define_fn!(foo);
fn main() {
    fo$0o();
}
"#,
            r#"
macro_rules! define_fn {($id:ident) => { fn $id{} }}
define_fn!(bar);
fn main() {
    bar();
}
"#,
        );
    }

    #[test]
    fn test_rename_for_macro_define_fn_rev() {
        check(
            "bar",
            r#"
macro_rules! define_fn {($id:ident) => { fn $id{} }}
define_fn!(fo$0o);
fn main() {
    foo();
}
"#,
            r#"
macro_rules! define_fn {($id:ident) => { fn $id{} }}
define_fn!(bar);
fn main() {
    bar();
}
"#,
        );
    }

    #[test]
    fn test_rename_for_param_inside() {
        check("j", r#"fn foo(i : u32) -> u32 { i$0 }"#, r#"fn foo(j : u32) -> u32 { j }"#);
    }

    #[test]
    fn test_rename_refs_for_fn_param() {
        check("j", r#"fn foo(i$0 : u32) -> u32 { i }"#, r#"fn foo(j : u32) -> u32 { j }"#);
    }

    #[test]
    fn test_rename_for_mut_param() {
        check("j", r#"fn foo(mut i$0 : u32) -> u32 { i }"#, r#"fn foo(mut j : u32) -> u32 { j }"#);
    }

    #[test]
    fn test_rename_struct_field() {
        check(
            "foo",
            r#"
struct Foo { field$0: i32 }

impl Foo {
    fn new(i: i32) -> Self {
        Self { field: i }
    }
}
"#,
            r#"
struct Foo { foo: i32 }

impl Foo {
    fn new(i: i32) -> Self {
        Self { foo: i }
    }
}
"#,
        );
    }

    #[test]
    fn test_rename_field_in_field_shorthand() {
        cov_mark::check!(test_rename_field_in_field_shorthand);
        check(
            "field",
            r#"
struct Foo { foo$0: i32 }

impl Foo {
    fn new(foo: i32) -> Self {
        Self { foo }
    }
}
"#,
            r#"
struct Foo { field: i32 }

impl Foo {
    fn new(foo: i32) -> Self {
        Self { field: foo }
    }
}
"#,
        );
    }

    #[test]
    fn test_rename_local_in_field_shorthand() {
        cov_mark::check!(test_rename_local_in_field_shorthand);
        check(
            "j",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn new(i$0: i32) -> Self {
        Self { i }
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn new(j: i32) -> Self {
        Self { i: j }
    }
}
"#,
        );
    }

    #[test]
    fn test_field_shorthand_correct_struct() {
        check(
            "j",
            r#"
struct Foo { i$0: i32 }
struct Bar { i: i32 }

impl Bar {
    fn new(i: i32) -> Self {
        Self { i }
    }
}
"#,
            r#"
struct Foo { j: i32 }
struct Bar { i: i32 }

impl Bar {
    fn new(i: i32) -> Self {
        Self { i }
    }
}
"#,
        );
    }

    #[test]
    fn test_shadow_local_for_struct_shorthand() {
        check(
            "j",
            r#"
struct Foo { i: i32 }

fn baz(i$0: i32) -> Self {
     let x = Foo { i };
     {
         let i = 0;
         Foo { i }
     }
}
"#,
            r#"
struct Foo { i: i32 }

fn baz(j: i32) -> Self {
     let x = Foo { i: j };
     {
         let i = 0;
         Foo { i }
     }
}
"#,
        );
    }

    #[test]
    fn test_rename_mod() {
        check_expect(
            "foo2",
            r#"
//- /lib.rs
mod bar;

//- /bar.rs
mod foo$0;

//- /bar/foo.rs
// empty
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "foo2",
                                delete: 4..7,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            2,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                2,
                            ),
                            path: "foo2.rs",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn rename_move_struct_into_new_module() {
        check_expect(
            "crate::fresh::Final",
            r#"
//- /lib.rs
struct ToMove$0;

fn make() -> ToMove {
    ToMove
}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "",
                                delete: 0..14,
                            },
                            Indel {
                                insert: "Final",
                                delete: 29..35,
                            },
                            Indel {
                                insert: "Final",
                                delete: 42..48,
                            },
                            Indel {
                                insert: "\nmod fresh;\n",
                                delete: 51..51,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    CreateFile {
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                0,
                            ),
                            path: "fresh.rs",
                        },
                        initial_contents: "struct Final;\n",
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_mod_in_use_tree() {
        check_expect(
            "quux",
            r#"
//- /main.rs
pub mod foo;
pub mod bar;
fn main() {}

//- /foo.rs
pub struct FooContent;

//- /bar.rs
use crate::foo$0::FooContent;
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "quux",
                                delete: 8..11,
                            },
                        ],
                    ),
                    (
                        FileId(
                            2,
                        ),
                        [
                            Indel {
                                insert: "quux",
                                delete: 11..14,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "quux.rs",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_mod_in_dir() {
        check_expect(
            "foo2",
            r#"
//- /lib.rs
mod fo$0o;
//- /foo/mod.rs
// empty
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "foo2",
                                delete: 4..7,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveDir {
                        src: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "../foo",
                        },
                        src_id: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "../foo2",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_unusually_nested_mod() {
        check_expect(
            "bar",
            r#"
//- /lib.rs
mod outer { mod fo$0o; }

//- /outer/foo.rs
// empty
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "bar",
                                delete: 16..19,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "bar.rs",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_module_rename_in_path() {
        check(
            "baz",
            r#"
mod $0foo {
    pub use self::bar as qux;
    pub fn bar() {}
}

fn main() { foo::bar(); }
"#,
            r#"
mod baz {
    pub use self::bar as qux;
    pub fn bar() {}
}

fn main() { baz::bar(); }
"#,
        );
    }

    #[test]
    fn test_rename_mod_filename_and_path() {
        check_expect(
            "foo2",
            r#"
//- /lib.rs
mod bar;
fn f() {
    bar::foo::fun()
}

//- /bar.rs
pub mod foo$0;

//- /bar/foo.rs
// pub fn fun() {}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "foo2",
                                delete: 27..30,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "foo2",
                                delete: 8..11,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            2,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                2,
                            ),
                            path: "foo2.rs",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_mod_recursive() {
        check_expect(
            "foo2",
            r#"
//- /lib.rs
mod foo$0;

//- /foo.rs
mod bar;
mod corge;

//- /foo/bar.rs
mod qux;

//- /foo/bar/qux.rs
mod quux;

//- /foo/bar/qux/quux/mod.rs
// empty

//- /foo/corge.rs
// empty
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "foo2",
                                delete: 4..7,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo2.rs",
                        },
                    },
                    MoveDir {
                        src: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo",
                        },
                        src_id: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo2",
                        },
                    },
                ]
            "#]],
        )
    }
    #[test]
    fn test_rename_mod_ref_by_super() {
        check(
            "baz",
            r#"
        mod $0foo {
        struct X;

        mod bar {
            use super::X;
        }
    }
            "#,
            r#"
        mod baz {
        struct X;

        mod bar {
            use super::X;
        }
    }
            "#,
        )
    }

    #[test]
    fn test_rename_mod_in_macro() {
        check(
            "bar",
            r#"
//- /foo.rs

//- /lib.rs
macro_rules! submodule {
    ($name:ident) => {
        mod $name;
    };
}

submodule!($0foo);
"#,
            r#"
macro_rules! submodule {
    ($name:ident) => {
        mod $name;
    };
}

submodule!(bar);
"#,
        )
    }

    #[test]
    fn test_rename_mod_for_crate_root() {
        check_expect_will_rename_file(
            "main",
            r#"
//- /lib.rs
use crate::foo as bar;
fn foo() {}
mod bar$0;
"#,
            expect![[r#"
                source_file_edits: []
                file_system_edits: []
            "#]],
        )
    }

    #[test]
    fn test_rename_mod_to_raw_ident() {
        check_expect(
            "r#fn",
            r#"
//- /lib.rs
mod foo$0;

fn main() { foo::bar::baz(); }

//- /foo.rs
pub mod bar;

//- /foo/bar.rs
pub fn baz() {}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "r#fn",
                                delete: 4..7,
                            },
                            Indel {
                                insert: "r#fn",
                                delete: 22..25,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "fn.rs",
                        },
                    },
                    MoveDir {
                        src: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo",
                        },
                        src_id: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "fn",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_mod_from_raw_ident() {
        check_expect(
            "foo",
            r#"
//- /lib.rs
mod r#fn$0;

fn main() { r#fn::bar::baz(); }

//- /fn.rs
pub mod bar;

//- /fn/bar.rs
pub fn baz() {}
"#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "foo",
                                delete: 4..8,
                            },
                            Indel {
                                insert: "foo",
                                delete: 23..27,
                            },
                        ],
                    ),
                ]
                file_system_edits: [
                    MoveFile {
                        src: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo.rs",
                        },
                    },
                    MoveDir {
                        src: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "fn",
                        },
                        src_id: FileId(
                            1,
                        ),
                        dst: AnchoredPathBuf {
                            anchor: FileId(
                                1,
                            ),
                            path: "foo",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_rename_each_usage_gets_appropriate_rawness() {
        check_expect(
            "dyn",
            r#"
//- /a.rs crate:a edition:2015
pub fn foo() {}

//- /b.rs crate:b edition:2018 deps:a new_source_root:local
fn bar() {
    a::foo$0();
}
    "#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "dyn",
                                delete: 7..10,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "r#dyn",
                                delete: 18..21,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );

        check_expect(
            "dyn",
            r#"
//- /a.rs crate:a edition:2018
pub fn foo() {}

//- /b.rs crate:b edition:2015 deps:a new_source_root:local
fn bar() {
    a::foo$0();
}
    "#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "r#dyn",
                                delete: 7..10,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "dyn",
                                delete: 18..21,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );

        check_expect(
            "r#dyn",
            r#"
//- /a.rs crate:a edition:2018
pub fn foo$0() {}

//- /b.rs crate:b edition:2015 deps:a new_source_root:local
fn bar() {
    a::foo();
}
    "#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "r#dyn",
                                delete: 7..10,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "dyn",
                                delete: 18..21,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );
    }

    #[test]
    fn rename_raw_identifier() {
        check_expect(
            "abc",
            r#"
//- /a.rs crate:a edition:2015
pub fn dyn() {}

fn foo() {
    dyn$0();
}

//- /b.rs crate:b edition:2018 deps:a new_source_root:local
fn bar() {
    a::r#dyn();
}
    "#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "abc",
                                delete: 7..10,
                            },
                            Indel {
                                insert: "abc",
                                delete: 32..35,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "abc",
                                delete: 18..23,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );

        check_expect(
            "abc",
            r#"
//- /a.rs crate:a edition:2018
pub fn r#dyn() {}

fn foo() {
    r#dyn$0();
}

//- /b.rs crate:b edition:2015 deps:a new_source_root:local
fn bar() {
    a::dyn();
}
    "#,
            expect![[r#"
                source_file_edits: [
                    (
                        FileId(
                            0,
                        ),
                        [
                            Indel {
                                insert: "abc",
                                delete: 7..12,
                            },
                            Indel {
                                insert: "abc",
                                delete: 34..39,
                            },
                        ],
                    ),
                    (
                        FileId(
                            1,
                        ),
                        [
                            Indel {
                                insert: "abc",
                                delete: 18..21,
                            },
                        ],
                    ),
                ]
                file_system_edits: []
            "#]],
        );
    }

    #[test]
    fn test_enum_variant_from_module_1() {
        cov_mark::check!(rename_non_local);
        check(
            "Baz",
            r#"
mod foo {
    pub enum Foo { Bar$0 }
}

fn func(f: foo::Foo) {
    match f {
        foo::Foo::Bar => {}
    }
}
"#,
            r#"
mod foo {
    pub enum Foo { Baz }
}

fn func(f: foo::Foo) {
    match f {
        foo::Foo::Baz => {}
    }
}
"#,
        );
    }

    #[test]
    fn test_enum_variant_from_module_2() {
        check(
            "baz",
            r#"
mod foo {
    pub struct Foo { pub bar$0: uint }
}

fn foo(f: foo::Foo) {
    let _ = f.bar;
}
"#,
            r#"
mod foo {
    pub struct Foo { pub baz: uint }
}

fn foo(f: foo::Foo) {
    let _ = f.baz;
}
"#,
        );
    }

    #[test]
    fn test_parameter_to_self() {
        cov_mark::check!(rename_to_self);
        check(
            "self",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(foo$0: &mut Foo) -> i32 {
        foo.i
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(&mut self) -> i32 {
        self.i
    }
}
"#,
        );
        check(
            "self",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(foo$0: Foo) -> i32 {
        foo.i
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(self) -> i32 {
        self.i
    }
}
"#,
        );
    }

    #[test]
    fn test_parameter_to_self_error_no_impl() {
        check(
            "self",
            r#"
struct Foo { i: i32 }

fn f(foo$0: &mut Foo) -> i32 {
    foo.i
}
"#,
            "error: Cannot rename parameter to self for free function",
        );
        check(
            "self",
            r#"
struct Foo { i: i32 }
struct Bar;

impl Bar {
    fn f(foo$0: &mut Foo) -> i32 {
        foo.i
    }
}
"#,
            "error: Parameter type differs from impl block type",
        );
    }

    #[test]
    fn test_parameter_to_self_error_not_first() {
        check(
            "self",
            r#"
struct Foo { i: i32 }
impl Foo {
    fn f(x: (), foo$0: &mut Foo) -> i32 {
        foo.i
    }
}
"#,
            "error: Only the first parameter may be renamed to self",
        );
    }

    #[test]
    fn test_parameter_to_self_impl_ref() {
        check(
            "self",
            r#"
struct Foo { i: i32 }
impl &Foo {
    fn f(foo$0: &Foo) -> i32 {
        foo.i
    }
}
"#,
            r#"
struct Foo { i: i32 }
impl &Foo {
    fn f(self) -> i32 {
        self.i
    }
}
"#,
        );
    }

    #[test]
    fn test_self_to_parameter() {
        check(
            "foo",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(&mut $0self) -> i32 {
        self.i
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(foo: &mut Self) -> i32 {
        foo.i
    }
}
"#,
        );
    }

    #[test]
    fn test_owned_self_to_parameter() {
        cov_mark::check!(rename_self_to_param);
        check(
            "foo",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f($0self) -> i32 {
        self.i
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(foo: Self) -> i32 {
        foo.i
    }
}
"#,
        );
    }

    #[test]
    fn test_owned_self_to_parameter_with_lifetime() {
        cov_mark::check!(rename_self_to_param);
        check(
            "foo",
            r#"
struct Foo<'a> { i: &'a i32 }

impl<'a> Foo<'a> {
    fn f(&'a $0self) -> i32 {
        self.i
    }
}
"#,
            r#"
struct Foo<'a> { i: &'a i32 }

impl<'a> Foo<'a> {
    fn f(foo: &'a Self) -> i32 {
        foo.i
    }
}
"#,
        );
    }

    #[test]
    fn test_self_outside_of_methods() {
        check(
            "foo",
            r#"
fn f($0self) -> i32 {
    self.i
}
"#,
            r#"
fn f(foo: Self) -> i32 {
    foo.i
}
"#,
        );
    }

    #[test]
    fn no_type_value_ns_confuse() {
        // Test that we don't rename items from different namespaces.
        check(
            "bar",
            r#"
struct foo {}
fn f(foo$0: i32) -> i32 {
    use foo as _;
}
"#,
            r#"
struct foo {}
fn f(bar: i32) -> i32 {
    use foo as _;
}
"#,
        );
    }

    #[test]
    fn test_self_in_path_to_parameter() {
        check(
            "foo",
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(&self) -> i32 {
        let self_var = 1;
        self$0.i
    }
}
"#,
            r#"
struct Foo { i: i32 }

impl Foo {
    fn f(foo: &Self) -> i32 {
        let self_var = 1;
        foo.i
    }
}
"#,
        );
    }

    #[test]
    fn test_rename_field_put_init_shorthand() {
        cov_mark::check!(test_rename_field_put_init_shorthand);
        check(
            "bar",
            r#"
struct Foo { i$0: i32 }

fn foo(bar: i32) -> Foo {
    Foo { i: bar }
}
"#,
            r#"
struct Foo { bar: i32 }

fn foo(bar: i32) -> Foo {
    Foo { bar }
}
"#,
        );
    }

    #[test]
    fn test_rename_local_simple() {
        check(
            "i",
            r#"
fn foo(bar$0: i32) -> i32 {
    bar
}
"#,
            r#"
fn foo(i: i32) -> i32 {
    i
}
"#,
        );
    }

    #[test]
    fn test_rename_local_put_init_shorthand() {
        cov_mark::check!(test_rename_local_put_init_shorthand);
        check(
            "i",
            r#"
struct Foo { i: i32 }

fn foo(bar$0: i32) -> Foo {
    Foo { i: bar }
}
"#,
            r#"
struct Foo { i: i32 }

fn foo(i: i32) -> Foo {
    Foo { i }
}
"#,
        );
    }

    #[test]
    fn test_struct_field_pat_into_shorthand() {
        cov_mark::check!(test_rename_field_put_init_shorthand_pat);
        check(
            "baz",
            r#"
struct Foo { i$0: i32 }

fn foo(foo: Foo) {
    let Foo { i: ref baz @ qux } = foo;
    let _ = qux;
}
"#,
            r#"
struct Foo { baz: i32 }

fn foo(foo: Foo) {
    let Foo { baz: ref baz @ qux } = foo;
    let _ = qux;
}
"#,
        );
        check(
            "baz",
            r#"
struct Foo { i$0: i32 }

fn foo(foo: Foo) {
    let Foo { i: ref baz } = foo;
    let _ = qux;
}
"#,
            r#"
struct Foo { baz: i32 }

fn foo(foo: Foo) {
    let Foo { ref baz } = foo;
    let _ = qux;
}
"#,
        );
    }

    #[test]
    fn test_struct_local_pat_into_shorthand() {
        cov_mark::check!(test_rename_local_put_init_shorthand_pat);
        check(
            "field",
            r#"
struct Foo { field: i32 }

fn foo(foo: Foo) {
    let Foo { field: qux$0 } = foo;
    let _ = qux;
}
"#,
            r#"
struct Foo { field: i32 }

fn foo(foo: Foo) {
    let Foo { field } = foo;
    let _ = field;
}
"#,
        );
        check(
            "field",
            r#"
struct Foo { field: i32 }

fn foo(foo: Foo) {
    let Foo { field: x @ qux$0 } = foo;
    let _ = qux;
}
"#,
            r#"
struct Foo { field: i32 }

fn foo(foo: Foo) {
    let Foo { field: x @ field } = foo;
    let _ = field;
}
"#,
        );
    }

    #[test]
    fn test_rename_binding_in_destructure_pat() {
        let expected_fixture = r#"
struct Foo {
    i: i32,
}

fn foo(foo: Foo) {
    let Foo { i: bar } = foo;
    let _ = bar;
}
"#;
        check(
            "bar",
            r#"
struct Foo {
    i: i32,
}

fn foo(foo: Foo) {
    let Foo { i: b } = foo;
    let _ = b$0;
}
"#,
            expected_fixture,
        );
        check(
            "bar",
            r#"
struct Foo {
    i: i32,
}

fn foo(foo: Foo) {
    let Foo { i } = foo;
    let _ = i$0;
}
"#,
            expected_fixture,
        );
    }

    #[test]
    fn test_rename_binding_in_destructure_param_pat() {
        check(
            "bar",
            r#"
struct Foo {
    i: i32
}

fn foo(Foo { i }: Foo) -> i32 {
    i$0
}
"#,
            r#"
struct Foo {
    i: i32
}

fn foo(Foo { i: bar }: Foo) -> i32 {
    bar
}
"#,
        )
    }

    #[test]
    fn test_struct_field_complex_ident_pat() {
        cov_mark::check!(rename_record_pat_field_name_split);
        check(
            "baz",
            r#"
struct Foo { i$0: i32 }

fn foo(foo: Foo) {
    let Foo { ref i } = foo;
}
"#,
            r#"
struct Foo { baz: i32 }

fn foo(foo: Foo) {
    let Foo { baz: ref i } = foo;
}
"#,
        );
    }

    #[test]
    fn test_rename_lifetimes() {
        check(
            "'yeeee",
            r#"
trait Foo<'a> {
    fn foo() -> &'a ();
}
impl<'a> Foo<'a> for &'a () {
    fn foo() -> &'a$0 () {
        unimplemented!()
    }
}
"#,
            r#"
trait Foo<'a> {
    fn foo() -> &'a ();
}
impl<'yeeee> Foo<'yeeee> for &'yeeee () {
    fn foo() -> &'yeeee () {
        unimplemented!()
    }
}
"#,
        )
    }

    #[test]
    fn test_rename_bind_pat() {
        check(
            "new_name",
            r#"
fn main() {
    enum CustomOption<T> {
        None,
        Some(T),
    }

    let test_variable = CustomOption::Some(22);

    match test_variable {
        CustomOption::Some(foo$0) if foo == 11 => {}
        _ => (),
    }
}"#,
            r#"
fn main() {
    enum CustomOption<T> {
        None,
        Some(T),
    }

    let test_variable = CustomOption::Some(22);

    match test_variable {
        CustomOption::Some(new_name) if new_name == 11 => {}
        _ => (),
    }
}"#,
        );
    }

    #[test]
    fn test_rename_label() {
        check(
            "'foo",
            r#"
fn foo<'a>() -> &'a () {
    'a: {
        'b: loop {
            break 'a$0;
        }
    }
}
"#,
            r#"
fn foo<'a>() -> &'a () {
    'foo: {
        'b: loop {
            break 'foo;
        }
    }
}
"#,
        )
    }

    #[test]
    fn test_rename_label_new_name_without_apostrophe() {
        check(
            "foo",
            r#"
fn main() {
    'outer$0: loop {
        'inner: loop {
            break 'outer;
        }
    }
}
        "#,
            r#"
fn main() {
    'foo: loop {
        'inner: loop {
            break 'foo;
        }
    }
}
        "#,
        );
    }

    #[test]
    fn test_self_to_self() {
        cov_mark::check!(rename_self_to_self);
        check(
            "self",
            r#"
struct Foo;
impl Foo {
    fn foo(self$0) {}
}
"#,
            r#"
struct Foo;
impl Foo {
    fn foo(self) {}
}
"#,
        )
    }

    #[test]
    fn test_rename_field_in_pat_in_macro_doesnt_shorthand() {
        // ideally we would be able to make this emit a short hand, but I doubt this is easily possible
        check(
            "baz",
            r#"
macro_rules! foo {
    ($pattern:pat) => {
        let $pattern = loop {};
    };
}
struct Foo {
    bar$0: u32,
}
fn foo() {
    foo!(Foo { bar: baz });
}
"#,
            r#"
macro_rules! foo {
    ($pattern:pat) => {
        let $pattern = loop {};
    };
}
struct Foo {
    baz: u32,
}
fn foo() {
    foo!(Foo { baz: baz });
}
"#,
        )
    }

    #[test]
    fn test_rename_tuple_field() {
        check(
            "foo",
            r#"
struct Foo(i32);

fn baz() {
    let mut x = Foo(4);
    x.0$0 = 5;
}
"#,
            "error: No references found at position",
        );
    }

    #[test]
    fn test_rename_builtin() {
        check(
            "foo",
            r#"
fn foo() {
    let x: i32$0 = 0;
}
"#,
            "error: Cannot rename builtin type",
        );
    }

    #[test]
    fn test_rename_self() {
        check(
            "foo",
            r#"
struct Foo {}

impl Foo {
    fn foo(self) -> Self$0 {
        self
    }
}
"#,
            "error: No references found at position",
        );
    }

    #[test]
    fn test_rename_ignores_self_ty() {
        check(
            "Fo0",
            r#"
struct $0Foo;

impl Foo where Self: {}
"#,
            r#"
struct Fo0;

impl Fo0 where Self: {}
"#,
        );
    }

    #[test]
    fn test_rename_fails_on_aliases() {
        check(
            "Baz",
            r#"
struct Foo;
use Foo as Bar$0;
"#,
            "error: Renaming aliases is currently unsupported",
        );
        check(
            "Baz",
            r#"
struct Foo;
use Foo as Bar;
use Bar$0;
"#,
            "error: Renaming aliases is currently unsupported",
        );
    }

    #[test]
    fn test_rename_trait_method() {
        let res = r"
trait Foo {
    fn foo(&self) {
        self.foo();
    }
}

impl Foo for () {
    fn foo(&self) {
        self.foo();
    }
}";
        check(
            "foo",
            r#"
trait Foo {
    fn bar$0(&self) {
        self.bar();
    }
}

impl Foo for () {
    fn bar(&self) {
        self.bar();
    }
}"#,
            res,
        );
        check(
            "foo",
            r#"
trait Foo {
    fn bar(&self) {
        self.bar$0();
    }
}

impl Foo for () {
    fn bar(&self) {
        self.bar();
    }
}"#,
            res,
        );
        check(
            "foo",
            r#"
trait Foo {
    fn bar(&self) {
        self.bar();
    }
}

impl Foo for () {
    fn bar$0(&self) {
        self.bar();
    }
}"#,
            res,
        );
        check(
            "foo",
            r#"
trait Foo {
    fn bar(&self) {
        self.bar();
    }
}

impl Foo for () {
    fn bar(&self) {
        self.bar$0();
    }
}"#,
            res,
        );
    }

    #[test]
    fn test_rename_trait_method_prefix_of_second() {
        check(
            "qux",
            r#"
trait Foo {
    fn foo$0() {}
    fn foobar() {}
}
"#,
            r#"
trait Foo {
    fn qux() {}
    fn foobar() {}
}
"#,
        );
    }

    #[test]
    fn test_rename_trait_const() {
        let res = r"
trait Foo {
    const FOO: ();
}

impl Foo for () {
    const FOO: ();
}
fn f() { <()>::FOO; }";
        check(
            "FOO",
            r#"
trait Foo {
    const BAR$0: ();
}

impl Foo for () {
    const BAR: ();
}
fn f() { <()>::BAR; }"#,
            res,
        );
        check(
            "FOO",
            r#"
trait Foo {
    const BAR: ();
}

impl Foo for () {
    const BAR$0: ();
}
fn f() { <()>::BAR; }"#,
            res,
        );
        check(
            "FOO",
            r#"
trait Foo {
    const BAR: ();
}

impl Foo for () {
    const BAR: ();
}
fn f() { <()>::BAR$0; }"#,
            res,
        );
    }

    #[test]
    fn defs_from_macros_arent_renamed() {
        check(
            "lol",
            r#"
macro_rules! m { () => { fn f() {} } }
m!();
fn main() { f$0()  }
"#,
            "error: No identifier available to rename",
        )
    }

    #[test]
    fn attributed_item() {
        check(
            "function",
            r#"
//- proc_macros: identity

#[proc_macros::identity]
fn func$0() {
    func();
}
"#,
            r#"

#[proc_macros::identity]
fn function() {
    function();
}
"#,
        )
    }

    #[test]
    fn in_macro_multi_mapping() {
        check(
            "a",
            r#"
fn foo() {
    macro_rules! match_ast2 {
        ($node:ident {
            $( $res:expr, )*
        }) => {{
            $( if $node { $res } else )*
            { loop {} }
        }};
    }
    let $0d = 3;
    match_ast2! {
        d {
            d,
            d,
        }
    };
}
"#,
            r#"
fn foo() {
    macro_rules! match_ast2 {
        ($node:ident {
            $( $res:expr, )*
        }) => {{
            $( if $node { $res } else )*
            { loop {} }
        }};
    }
    let a = 3;
    match_ast2! {
        a {
            a,
            a,
        }
    };
}
"#,
        )
    }

    #[test]
    fn rename_multi_local() {
        check(
            "bar",
            r#"
fn foo((foo$0 | foo | foo): ()) {
    foo;
    let foo;
}
"#,
            r#"
fn foo((bar | bar | bar): ()) {
    bar;
    let foo;
}
"#,
        );
        check(
            "bar",
            r#"
fn foo((foo | foo$0 | foo): ()) {
    foo;
    let foo;
}
"#,
            r#"
fn foo((bar | bar | bar): ()) {
    bar;
    let foo;
}
"#,
        );
        check(
            "bar",
            r#"
fn foo((foo | foo | foo): ()) {
    foo$0;
    let foo;
}
"#,
            r#"
fn foo((bar | bar | bar): ()) {
    bar;
    let foo;
}
"#,
        );
    }

    #[test]
    fn regression_13498() {
        check(
            "Testing",
            r"
mod foo {
    pub struct Test$0;
}

use foo::Test as Tester;

fn main() {
    let t = Tester;
}
",
            r"
mod foo {
    pub struct Testing;
}

use foo::Testing as Tester;

fn main() {
    let t = Tester;
}
",
        )
    }

    #[test]
    fn extern_crate() {
        check_prepare(
            r"
//- /lib.rs crate:main deps:foo
extern crate foo$0;
use foo as qux;
//- /foo.rs crate:foo
",
            expect![[r#"No references found at position"#]],
        );
        // FIXME: replace above check_prepare with this once we resolve to usages to extern crate declarations
        //         check(
        //             "bar",
        //             r"
        // //- /lib.rs crate:main deps:foo
        // extern crate foo$0;
        // use foo as qux;
        // //- /foo.rs crate:foo
        // ",
        //             r"
        // extern crate foo as bar;
        // use bar as qux;
        // ",
        //         );
    }

    #[test]
    fn extern_crate_rename() {
        check_prepare(
            r"
//- /lib.rs crate:main deps:foo
extern crate foo as qux$0;
use qux as frob;
//- /foo.rs crate:foo
",
            expect!["Renaming aliases is currently unsupported"],
        );
        // FIXME: replace above check_prepare with this once we resolve to usages to extern crate
        // declarations
        //         check(
        //             "bar",
        //             r"
        // //- /lib.rs crate:main deps:foo
        // extern crate foo as qux$0;
        // use qux as frob;
        // //- /foo.rs crate:foo
        // ",
        //             r"
        // extern crate foo as bar;
        // use bar as frob;
        // ",
        //         );
    }

    #[test]
    fn extern_crate_self() {
        check_prepare(
            r"
extern crate self$0;
use self as qux;
",
            expect!["No references found at position"],
        );
        // FIXME: replace above check_prepare with this once we resolve to usages to extern crate declarations
        //         check(
        //             "bar",
        //             r"
        // extern crate self$0;
        // use self as qux;
        // ",
        //             r"
        // extern crate self as bar;
        // use self as qux;
        // ",
        //         );
    }

    #[test]
    fn extern_crate_self_rename() {
        check_prepare(
            r"
//- /lib.rs crate:main deps:foo
extern crate self as qux$0;
use qux as frob;
//- /foo.rs crate:foo
",
            expect!["Renaming aliases is currently unsupported"],
        );
        // FIXME: replace above check_prepare with this once we resolve to usages to extern crate declarations
        //         check(
        //             "bar",
        //             r"
        // //- /lib.rs crate:main deps:foo
        // extern crate self as qux$0;
        // use qux as frob;
        // //- /foo.rs crate:foo
        // ",
        //             r"
        // extern crate self as bar;
        // use bar as frob;
        // ",
        //         );
    }

    #[test]
    fn disallow_renaming_for_non_local_definition() {
        check(
            "Baz",
            r#"
//- /lib.rs crate:lib new_source_root:library
pub struct S;
//- /main.rs crate:main deps:lib new_source_root:local
use lib::S;
fn main() { let _: S$0; }
"#,
            "error: Cannot rename a non-local definition",
        );
    }

    #[test]
    fn disallow_renaming_for_builtin_macros() {
        check(
            "Baz",
            r#"
//- minicore: derive, hash
//- /main.rs crate:main
use core::hash::Hash;
#[derive(H$0ash)]
struct A;
            "#,
            "error: Cannot rename a non-local definition",
        );
    }

    #[test]
    fn implicit_format_args() {
        check(
            "fbar",
            r#"
//- minicore: fmt
fn test() {
    let foo = "foo";
    format_args!("hello {foo} {foo$0} {}", foo);
}
"#,
            r#"
fn test() {
    let fbar = "foo";
    format_args!("hello {fbar} {fbar} {}", fbar);
}
"#,
        );
    }

    #[test]
    fn implicit_format_args2() {
        check(
            "fo",
            r#"
//- minicore: fmt
fn test() {
    let foo = "foo";
    format_args!("hello {foo} {foo$0} {}", foo);
}
"#,
            r#"
fn test() {
    let fo = "foo";
    format_args!("hello {fo} {fo} {}", fo);
}
"#,
        );
    }

    #[test]
    fn asm_operand() {
        check(
            "bose",
            r#"
//- minicore: asm
fn test() {
    core::arch::asm!(
        "push {base}",
        base$0 = const 0
    );
}
"#,
            r#"
fn test() {
    core::arch::asm!(
        "push {bose}",
        bose = const 0
    );
}
"#,
        );
    }

    #[test]
    fn asm_operand2() {
        check(
            "bose",
            r#"
//- minicore: asm
fn test() {
    core::arch::asm!(
        "push {base$0}",
        "push {base}",
        boo = const 0,
        virtual_free = sym VIRTUAL_FREE,
        base = const 0,
        boo = const 0,
    );
}
"#,
            r#"
fn test() {
    core::arch::asm!(
        "push {bose}",
        "push {bose}",
        boo = const 0,
        virtual_free = sym VIRTUAL_FREE,
        bose = const 0,
        boo = const 0,
    );
}
"#,
        );
    }

    #[test]
    fn rename_path_inside_use_tree() {
        check(
            "Baz",
            r#"
//- /main.rs crate:main
mod module;
mod foo { pub struct Foo; }
mod bar { use super::Foo; }

use foo::Foo$0;

fn main() { let _: Foo; }
//- /module.rs
use crate::foo::Foo;
"#,
            r#"
mod module;
mod foo { pub struct Foo; }
mod bar { use super::Baz; }

use foo::Foo as Baz;

fn main() { let _: Baz; }
"#,
        )
    }

    #[test]
    fn rename_path_inside_use_tree_foreign() {
        check(
            "Baz",
            r#"
//- /lib.rs crate:lib new_source_root:library
pub struct S;
//- /main.rs crate:main deps:lib new_source_root:local
use lib::S$0;
fn main() { let _: S; }
"#,
            r#"
use lib::S as Baz;
fn main() { let _: Baz; }
"#,
        );
    }

    #[test]
    fn rename_type_param_ref_in_use_bound() {
        check(
            "U",
            r#"
fn foo<T>() -> impl use<T$0> Trait {}
"#,
            r#"
fn foo<U>() -> impl use<U> Trait {}
"#,
        );
    }

    #[test]
    fn rename_type_param_in_use_bound() {
        check(
            "U",
            r#"
fn foo<T$0>() -> impl use<T> Trait {}
"#,
            r#"
fn foo<U>() -> impl use<U> Trait {}
"#,
        );
    }

    #[test]
    fn rename_lifetime_param_ref_in_use_bound() {
        check(
            "u",
            r#"
fn foo<'t>() -> impl use<'t$0> Trait {}
"#,
            r#"
fn foo<'u>() -> impl use<'u> Trait {}
"#,
        );
    }

    #[test]
    fn rename_lifetime_param_in_use_bound() {
        check(
            "u",
            r#"
fn foo<'t$0>() -> impl use<'t> Trait {}
"#,
            r#"
fn foo<'u>() -> impl use<'u> Trait {}
"#,
        );
    }

    #[test]
    fn rename_parent_type_param_in_use_bound() {
        check(
            "U",
            r#"
trait Trait<T> {
    fn foo() -> impl use<T$0> Trait {}
}
"#,
            r#"
trait Trait<U> {
    fn foo() -> impl use<U> Trait {}
}
"#,
        );
    }

    #[test]
    fn rename_macro_generated_type_from_type_with_a_suffix() {
        check(
            "Bar",
            r#"
//- proc_macros: generate_suffixed_type
#[proc_macros::generate_suffixed_type]
struct Foo$0;
fn usage(_: FooSuffix) {}
usage(FooSuffix);
"#,
            r#"
#[proc_macros::generate_suffixed_type]
struct Bar;
fn usage(_: BarSuffix) {}
usage(BarSuffix);
"#,
        );
    }

    #[test]
    // FIXME
    #[should_panic]
    fn rename_macro_generated_type_from_type_usage_with_a_suffix() {
        check(
            "Bar",
            r#"
//- proc_macros: generate_suffixed_type
#[proc_macros::generate_suffixed_type]
struct Foo;
fn usage(_: FooSuffix) {}
usage(FooSuffix);
fn other_place() { Foo$0; }
"#,
            r#"
#[proc_macros::generate_suffixed_type]
struct Bar;
fn usage(_: BarSuffix) {}
usage(BarSuffix);
fn other_place() { Bar; }
"#,
        );
    }

    #[test]
    fn rename_macro_generated_type_from_variant_with_a_suffix() {
        check(
            "Bar",
            r#"
//- proc_macros: generate_suffixed_type
#[proc_macros::generate_suffixed_type]
enum Quux {
    Foo$0,
}
fn usage(_: FooSuffix) {}
usage(FooSuffix);
"#,
            r#"
#[proc_macros::generate_suffixed_type]
enum Quux {
    Bar,
}
fn usage(_: BarSuffix) {}
usage(BarSuffix);
"#,
        );
    }

    #[test]
    // FIXME
    #[should_panic]
    fn rename_macro_generated_type_from_variant_usage_with_a_suffix() {
        check(
            "Bar",
            r#"
//- proc_macros: generate_suffixed_type
#[proc_macros::generate_suffixed_type]
enum Quux {
    Foo,
}
fn usage(_: FooSuffix) {}
usage(FooSuffix);
fn other_place() { Quux::Foo$0; }
"#,
            r#"
#[proc_macros::generate_suffixed_type]
enum Quux {
    Bar,
}
fn usage(_: BarSuffix) {}
usage(BartSuffix);
fn other_place() { Quux::Bar$0; }
"#,
        );
    }

    #[test]
    fn rename_to_self_callers() {
        check(
            "self",
            r#"
//- minicore: add
struct Foo;
impl core::ops::Add for Foo {
    type Target = Foo;
    fn add(self, _: Self) -> Foo { Foo }
}

impl Foo {
    fn foo(th$0is: &Self) {}
}

fn bar(v: &Foo) {
    Foo::foo(v);
}

fn baz() {
    Foo::foo(&Foo);
    Foo::foo(Foo + Foo);
}
        "#,
            r#"
struct Foo;
impl core::ops::Add for Foo {
    type Target = Foo;
    fn add(self, _: Self) -> Foo { Foo }
}

impl Foo {
    fn foo(&self) {}
}

fn bar(v: &Foo) {
    v.foo();
}

fn baz() {
    Foo.foo();
    (Foo + Foo).foo();
}
        "#,
        );
        // Multiple arguments:
        check(
            "self",
            r#"
struct Foo;

impl Foo {
    fn foo(th$0is: &Self, v: i32) {}
}

fn bar(v: Foo) {
    Foo::foo(&v, 123);
}
        "#,
            r#"
struct Foo;

impl Foo {
    fn foo(&self, v: i32) {}
}

fn bar(v: Foo) {
    v.foo(123);
}
        "#,
        );
    }
}
