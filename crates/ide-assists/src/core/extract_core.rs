use ide_db::{
    source_change::SourceChange,
    text_edit::TextEdit,
    FileId, RootDatabase,
};
use ide_db::span::TextRange;
use ide_db::FileRange;

use crate::{AssistConfig, AssistResolveStrategy, assists};

/// Compute the SourceChange produced by the `extract_module` assist for a given range.
pub fn compute_extract_module_change(
    db: &RootDatabase,
    config: &AssistConfig,
    frange: FileRange,
) -> Result<SourceChange, String> {
    let mut res = assists(db, config, AssistResolveStrategy::All, frange)
        .into_iter()
        .filter_map(|a| if a.id.0 == "extract_module" { a.source_change } else { None })
        .collect::<Vec<_>>();
    res.pop().ok_or_else(|| "extract_module assist not applicable".to_string())
}

/// Takes a SourceChange produced by the `extract_module` assist and adapts it to:
/// - Use a concrete child module name instead of a placeholder (like `modname`).
/// - Replace the inline module insertion at `item_range` with a declaration `mod child;`.
/// - Extract the created module body contents and rename the moved item's header from `old_item_name` to `new_item_name`.
/// Returns the transformed SourceChange (without file system edits) and the module body content.
pub fn adapt_extract_module_change_to_child(
    _db: &RootDatabase,
    mut change: SourceChange,
    item_file: FileId,
    item_range: TextRange,
    placeholder_mod: &str,
    child_mod: &str,
    old_item_name: &str,
    new_item_name: &str,
    item_keyword: &str,
) -> Result<(SourceChange, String), String> {
    let mut result = SourceChange::default();
    let mut created_body: Option<String> = None;

    for (fid, (edit, snip)) in change.source_file_edits.into_iter() {
        let mut builder = TextEdit::builder();
        for indel in edit.into_iter() {
            let mut insert = indel.insert;
            if !insert.is_empty() {
                let from_path = format!("{}::{}", placeholder_mod, old_item_name);
                let to_path = format!("{}::{}", child_mod, new_item_name);
                insert = insert.replace(&from_path, &to_path);
                insert = insert.replace(placeholder_mod, child_mod);
            }
            if fid == item_file && indel.delete == item_range {
                let body = extract_module_body(&insert)
                    .ok_or_else(|| "Unexpected extract_module payload".to_string())?;
                let renamed_body = rename_item_in_body(&body, item_keyword, old_item_name, new_item_name);
                created_body = Some(renamed_body);
                let mod_decl = format!("mod {};", child_mod);
                builder.replace(indel.delete, mod_decl);
            } else {
                builder.replace(indel.delete, insert);
            }
        }
        result.insert_source_and_snippet_edit(fid, builder.finish(), snip);
    }

    let contents = created_body.ok_or_else(|| "Failed to extract module body".to_string())?;
    Ok((result, contents))
}

pub fn extract_module_body(module_block: &str) -> Option<String> {
    let start = module_block.find('{')? + 1;
    let end = module_block.rfind('}')?;
    if end <= start {
        return Some(String::new());
    }
    let mut inner = module_block[start..end].trim().to_string();
    if !inner.is_empty() && !inner.ends_with('\n') {
        inner.push('\n');
    }
    Some(inner)
}

pub fn rename_item_in_body(body: &str, keyword: &str, old: &str, new: &str) -> String {
    let mut out = body.to_string();
    if let Some(mut idx) = out.find(keyword) {
        while let Some(ch) = out[..idx].chars().last() {
            if ch.is_alphanumeric() || ch == '_' {
                if let Some(next) = out[idx + keyword.len()..].find(keyword) {
                    idx = idx + keyword.len() + next;
                    continue;
                } else {
                    break;
                }
            }
            break;
        }
        let after_kw = idx + keyword.len();
        let mut j = after_kw;
        while j < out.len() && out.as_bytes()[j].is_ascii_whitespace() {
            j += 1;
        }
        let has_raw = out[j..].starts_with("r#");
        if has_raw {
            j += 2;
        }
        if out[j..].starts_with(old) {
            out.replace_range(j..j + old.len(), new);
            return out;
        }
    }
    out
}
