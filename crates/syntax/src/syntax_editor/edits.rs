//! Structural editing for ast using `SyntaxEditor`

use itertools::{Itertools, chain};

use crate::{
    AstToken, Direction, SyntaxElement, SyntaxElementExt, SyntaxKind, SyntaxNode, SyntaxToken, T,
    algo::neighbor,
    ast::{
        self, AstNode, Fn, GenericParam, HasGenericParams, HasModuleItem, HasName, HasVisibility,
        edit::{AstNodeEdit, IndentLevel},
        make::{self, tokens},
        syntax_factory::SyntaxFactory,
    },
    syntax_editor::{Element, Position, SyntaxEditor},
    ted,
};

impl SyntaxEditor {
    /// Adds a new generic param to the function using `SyntaxEditor`
    pub fn add_generic_param(&mut self, function: &Fn, new_param: GenericParam) {
        match function.generic_param_list() {
            Some(generic_param_list) => match generic_param_list.generic_params().last() {
                Some(last_param) => {
                    // There exists a generic param list and it's not empty
                    let position = generic_param_list.r_angle_token().map_or_else(
                        || Position::last_child_of(function.syntax()),
                        Position::before,
                    );

                    if last_param
                        .syntax()
                        .next_sibling_or_token()
                        .is_some_and(|it| it.kind() == SyntaxKind::COMMA)
                    {
                        self.insert(
                            Position::after(last_param.syntax()),
                            new_param.syntax().clone(),
                        );
                        self.insert(
                            Position::after(last_param.syntax()),
                            make::token(SyntaxKind::WHITESPACE),
                        );
                        self.insert(
                            Position::after(last_param.syntax()),
                            make::token(SyntaxKind::COMMA),
                        );
                    } else {
                        let elements = vec![
                            make::token(SyntaxKind::COMMA).into(),
                            make::token(SyntaxKind::WHITESPACE).into(),
                            new_param.syntax().clone().into(),
                        ];
                        self.insert_all(position, elements);
                    }
                }
                None => {
                    // There exists a generic param list but it's empty
                    let position = Position::after(generic_param_list.l_angle_token().unwrap());
                    self.insert(position, new_param.syntax());
                }
            },
            None => {
                // There was no generic param list
                let position = if let Some(name) = function.name() {
                    Position::after(name.syntax)
                } else if let Some(fn_token) = function.fn_token() {
                    Position::after(fn_token)
                } else if let Some(param_list) = function.param_list() {
                    Position::before(param_list.syntax)
                } else {
                    Position::last_child_of(function.syntax())
                };
                let elements = vec![
                    make::token(SyntaxKind::L_ANGLE).into(),
                    new_param.syntax().clone().into(),
                    make::token(SyntaxKind::R_ANGLE).into(),
                ];
                self.insert_all(position, elements);
            }
        }
    }
}

fn get_or_insert_comma_after(editor: &mut SyntaxEditor, syntax: &SyntaxNode) -> SyntaxToken {
    let make = SyntaxFactory::without_mappings();
    match syntax
        .siblings_with_tokens(Direction::Next)
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == T![,])
    {
        Some(it) => it,
        None => {
            let comma = make.token(T![,]);
            editor.insert(Position::after(syntax), &comma);
            comma
        }
    }
}

impl ast::AssocItemList {
    /// Adds a new associated item after all of the existing associated items.
    ///
    /// Attention! This function does align the first line of `item` with respect to `self`,
    /// but it does _not_ change indentation of other lines (if any).
    pub fn add_items(&self, editor: &mut SyntaxEditor, items: Vec<ast::AssocItem>) {
        let (indent, position, whitespace) = match self.assoc_items().last() {
            Some(last_item) => (
                IndentLevel::from_node(last_item.syntax()),
                Position::after(last_item.syntax()),
                "\n\n",
            ),
            None => match self.l_curly_token() {
                Some(l_curly) => {
                    normalize_ws_between_braces(editor, self.syntax());
                    (IndentLevel::from_token(&l_curly) + 1, Position::after(&l_curly), "\n")
                }
                None => (IndentLevel::single(), Position::last_child_of(self.syntax()), "\n"),
            },
        };

        let elements: Vec<SyntaxElement> = items
            .into_iter()
            .enumerate()
            .flat_map(|(i, item)| {
                let whitespace = if i != 0 { "\n\n" } else { whitespace };
                vec![
                    make::tokens::whitespace(&format!("{whitespace}{indent}")).into(),
                    item.syntax().clone().into(),
                ]
            })
            .collect();
        editor.insert_all(position, elements);
    }
}

impl ast::Module {
    // RENTRY: Impl this for inline and file mod inserts
    // TODO: Merge with SourceFile version
    pub fn add_item_after_headers(
        &self,
        editor: &mut SyntaxEditor,
        items: impl IntoIterator<Item = ast::Item>,
    ) {
        let l_curly = self.item_list().and_then(|il| il.l_curly_token()).map(SyntaxElement::from);
        let r_curly = self.item_list().and_then(|il| il.r_curly_token()).map(SyntaxElement::from);

        let last_header = self
            .item_list()
            .into_iter()
            .flat_map(|il| il.items())
            .take_while(|item| {
                matches!(
                    item,
                    ast::Item::Use(_)
                        | ast::Item::Const(_)
                        | ast::Item::ExternBlock(_)
                        | ast::Item::ExternCrate(_)
                        | ast::Item::Static(_)
                )
            })
            .last()
            .map(|i| SyntaxElement::from(i.syntax().clone()));

        let first_body = last_header.as_ref().or(l_curly.as_ref()).and_then(|s| {
            let first_non_ws = s.siblings_with_tokens(Direction::Next).skip(1).find(|x| {
                if x.kind().is_whitespace() {
                    editor.delete(x);
                    false
                } else {
                    true
                }
            });
            first_non_ws.filter(|x| Some(x) != r_curly.as_ref())
        });

        let (prefix_ws, body_indent, insert_pos) = if let Some(last_header) = &last_header {
            ("\n\n", IndentLevel::from_element(last_header), Position::after(last_header))
        } else if let Some(l_curly) = &l_curly {
            ("\n", IndentLevel::from_element(l_curly) + 1, Position::after(l_curly))
        } else {
            ("\n", IndentLevel(0), Position::first_child_of(self.syntax()))
        };

        let (postfix_ws, postfix_indent) = if let Some(first_body) = &first_body {
            ("\n\n", IndentLevel::from_element(first_body))
        } else if let Some(r_curly) = &r_curly {
            ("\n", IndentLevel::from_element(r_curly))
        } else {
            ("\n", IndentLevel(0))
        };

        let elements = chain![
            tokens::whitespace_indent_opt(prefix_ws, body_indent).map(Into::into),
            Itertools::intersperse_with(
                items.into_iter().map(|i| i
                    .reset_indent()
                    .indent(body_indent)
                    .syntax()
                    .clone()
                    .into()),
                || { tokens::whitespace_indent("\n\n", body_indent).into() }
            ),
            tokens::whitespace_indent_opt(postfix_ws, postfix_indent).map(Into::into),
        ]
        .collect();

        editor.insert_all(insert_pos, elements);
    }
}

impl ast::SourceFile {
    pub fn add_item_after_headers(
        &self,
        editor: &mut SyntaxEditor,
        items: impl IntoIterator<Item = ast::Item>,
    ) {
        let mut list_items = self.items().peekable();
        let last_header = list_items
            .peeking_take_while(|item| {
                matches!(
                    item,
                    ast::Item::Use(_)
                        | ast::Item::Const(_)
                        | ast::Item::ExternBlock(_)
                        | ast::Item::ExternCrate(_)
                        | ast::Item::Static(_)
                )
            })
            .last();
        let first_body = list_items.next();

        let (indent, position) = match &last_header {
            Some(last_header) => {
                if let Some(neighbor_ws) = last_header.syntax().neighbor_ws(Direction::Next) {
                    editor.delete(neighbor_ws);
                }
                (
                    IndentLevel::from_node(last_header.syntax()),
                    Position::after(last_header.syntax()),
                )
            }
            None => {
                if let Some(SyntaxElement::Token(first_child_ws)) =
                    self.syntax().first_child_or_token()
                    && first_child_ws.kind().is_whitespace()
                {
                    editor.delete(first_child_ws);
                }
                (IndentLevel::from(0), Position::first_child_of(self.syntax()))
            }
        };

        let prefix_ws = if last_header.is_some() { "\n\n" } else { "" };
        let postfix_ws = if first_body.is_some() { "\n\n" } else { "\n" };

        let elements = chain![
            tokens::whitespace_indent_opt(prefix_ws, indent).map(Into::into),
            Itertools::intersperse_with(
                items.into_iter().map(|i| i.reset_indent().indent(indent).syntax().clone().into()),
                || { tokens::whitespace_indent("\n\n", indent).into() }
            ),
            tokens::whitespace_indent_opt(postfix_ws, IndentLevel(0)).map(Into::into), // TODO: Clean up
        ]
        .collect();

        editor.insert_all(position, elements);
    }
}

impl ast::VariantList {
    pub fn add_variant(&self, editor: &mut SyntaxEditor, variant: &ast::Variant) {
        let make = SyntaxFactory::without_mappings();
        let (indent, position) = match self.variants().last() {
            Some(last_item) => (
                IndentLevel::from_node(last_item.syntax()),
                Position::after(get_or_insert_comma_after(editor, last_item.syntax())),
            ),
            None => match self.l_curly_token() {
                Some(l_curly) => {
                    normalize_ws_between_braces(editor, self.syntax());
                    (IndentLevel::from_token(&l_curly) + 1, Position::after(&l_curly))
                }
                None => (IndentLevel::single(), Position::last_child_of(self.syntax())),
            },
        };
        let elements: Vec<SyntaxElement> = vec![
            make.whitespace(&format!("{}{indent}", "\n")).into(),
            variant.syntax().clone().into(),
            make.token(T![,]).into(),
        ];
        editor.insert_all(position, elements);
    }
}

impl ast::Fn {
    pub fn replace_or_insert_body(&self, editor: &mut SyntaxEditor, body: ast::BlockExpr) {
        if let Some(old_body) = self.body() {
            editor.replace(old_body.syntax(), body.syntax());
        } else {
            let single_space = make::tokens::single_space();
            let elements = vec![single_space.into(), body.syntax().clone().into()];

            if let Some(semicolon) = self.semicolon_token() {
                editor.replace_with_many(semicolon, elements);
            } else {
                editor.insert_all(Position::last_child_of(self.syntax()), elements);
            }
        }
    }
}

fn normalize_ws_between_braces(editor: &mut SyntaxEditor, node: &SyntaxNode) -> Option<()> {
    let make = SyntaxFactory::without_mappings();
    let l = node
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == T!['{'])?;
    let r = node
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == T!['}'])?;

    let indent = IndentLevel::from_node(node);

    match l.next_sibling_or_token() {
        Some(ws) if ws.kind() == SyntaxKind::WHITESPACE => {
            if ws.next_sibling_or_token()?.into_token()? == r {
                editor.replace(ws, make.whitespace(&format!("\n{indent}")));
            }
        }
        Some(ws) if ws.kind() == T!['}'] => {
            editor.insert(Position::after(l), make.whitespace(&format!("\n{indent}")));
        }
        _ => (),
    }
    Some(())
}

pub trait Removable: AstNode {
    fn remove(&self, editor: &mut SyntaxEditor);
}

impl Removable for ast::TypeBoundList {
    fn remove(&self, editor: &mut SyntaxEditor) {
        match self.syntax().siblings_with_tokens(Direction::Prev).find(|it| it.kind() == T![:]) {
            Some(colon) => editor.delete_all(colon..=self.syntax().clone().into()),
            None => editor.delete(self.syntax()),
        }
    }
}

impl Removable for ast::Use {
    fn remove(&self, editor: &mut SyntaxEditor) {
        let make = SyntaxFactory::without_mappings();

        let next_ws = self
            .syntax()
            .next_sibling_or_token()
            .and_then(|it| it.into_token())
            .and_then(ast::Whitespace::cast);
        if let Some(next_ws) = next_ws {
            let ws_text = next_ws.syntax().text();
            if let Some(rest) = ws_text.strip_prefix('\n') {
                if rest.is_empty() {
                    editor.delete(next_ws.syntax());
                } else {
                    editor.replace(next_ws.syntax(), make.whitespace(rest));
                }
            }
        }

        editor.delete(self.syntax());
    }
}

impl Removable for ast::UseTree {
    fn remove(&self, editor: &mut SyntaxEditor) {
        for dir in [Direction::Next, Direction::Prev] {
            if let Some(next_use_tree) = neighbor(self, dir) {
                let separators = self
                    .syntax()
                    .siblings_with_tokens(dir)
                    .skip(1)
                    .take_while(|it| it.as_node() != Some(next_use_tree.syntax()));
                for sep in separators {
                    editor.delete(sep);
                }
                break;
            }
        }
        editor.delete(self.syntax());
    }
}

pub trait SetName: ast::HasName {
    fn set_name(&self, editor: &mut SyntaxEditor, new_name: ast::Name) {
        if let Some(name) = self.name() {
            editor.replace(name.syntax(), new_name.syntax());
        }
    }
}

impl<T: ast::HasName> SetName for T {}

// TODO: In PR, note that this was copied from HasVisibilityEdit
// - Also discuss idea of having common editor facade
pub trait SetVisibility: ast::HasVisibility {
    fn set_visibility(&self, editor: &mut SyntaxEditor, new_vis: Option<&ast::Visibility>) {
        match (self.visibility(), new_vis) {
            (Some(cur_vis), Some(new_vis)) => editor.replace(cur_vis.syntax(), new_vis.syntax()),
            (Some(cur_vis), None) => {
                editor.delete(cur_vis.syntax());
            }
            (None, Some(new_vis)) => {
                if let Some(insert_before) = self
                    .syntax()
                    .children_with_tokens()
                    .find(|it| {
                        !matches!(
                            it.kind(),
                            SyntaxKind::WHITESPACE | SyntaxKind::COMMENT | SyntaxKind::ATTR
                        )
                    })
                    .or_else(|| self.syntax().first_child_or_token())
                {
                    editor.insert_with_ws(Position::before(insert_before), new_vis.syntax());
                }
            }
            (None, None) => {}
        }
    }

    fn set_visibility_if_some(&self, editor: &mut SyntaxEditor, new_vis: Option<&ast::Visibility>) {
        if self.visibility().is_some() {
            self.set_visibility(editor, new_vis);
        }
    }

    fn set_visibility_if_none(&self, editor: &mut SyntaxEditor, new_vis: Option<&ast::Visibility>) {
        if self.visibility().is_none() {
            self.set_visibility(editor, new_vis);
        }
    }
}

impl<T: HasVisibility> SetVisibility for T {}

#[cfg(test)]
mod tests {
    use parser::Edition;
    use rustc_hash::FxHashMap;
    use stdx::trim_indent;
    use test_utils::assert_eq_text;

    use crate::{SourceFile, ast};

    use super::*;

    fn ast_from_text<N: AstNode>(text: &str) -> N {
        let parse = SourceFile::parse(text, Edition::CURRENT);
        let node = match parse.tree().syntax().descendants().find_map(N::cast) {
            Some(it) => it,
            None => {
                let node = std::any::type_name::<N>();
                panic!("Failed to make ast node `{node}` from text {text}")
            }
        };
        let node = node.clone_subtree();
        assert_eq!(node.syntax().text_range().start(), 0.into());
        node
    }

    #[test]
    fn add_variant_to_empty_enum() {
        let make = SyntaxFactory::without_mappings();
        let variant = make.variant(None, make.name("Bar"), None, None);

        check_add_variant(
            r#"
enum Foo {}
"#,
            r#"
enum Foo {
    Bar,
}
"#,
            variant,
        );
    }

    #[test]
    fn add_variant_to_non_empty_enum() {
        let make = SyntaxFactory::without_mappings();
        let variant = make.variant(None, make.name("Baz"), None, None);

        check_add_variant(
            r#"
enum Foo {
    Bar,
}
"#,
            r#"
enum Foo {
    Bar,
    Baz,
}
"#,
            variant,
        );
    }

    #[test]
    fn add_variant_with_tuple_field_list() {
        let make = SyntaxFactory::without_mappings();
        let variant = make.variant(
            None,
            make.name("Baz"),
            Some(make.tuple_field_list([make.tuple_field(None, make.ty("bool"))]).into()),
            None,
        );

        check_add_variant(
            r#"
enum Foo {
    Bar,
}
"#,
            r#"
enum Foo {
    Bar,
    Baz(bool),
}
"#,
            variant,
        );
    }

    #[test]
    fn add_variant_with_record_field_list() {
        let make = SyntaxFactory::without_mappings();
        let variant = make.variant(
            None,
            make.name("Baz"),
            Some(
                make.record_field_list([make.record_field(None, make.name("x"), make.ty("bool"))])
                    .into(),
            ),
            None,
        );

        check_add_variant(
            r#"
enum Foo {
    Bar,
}
"#,
            r#"
enum Foo {
    Bar,
    Baz { x: bool },
}
"#,
            variant,
        );
    }

    #[test]
    fn test_set_name() {
        let cases = [
            ("BarStruct", r#"struct FooStruct { f1: f64 }"#, r#"struct BarStruct { f1: f64 }"#),
            ("BarEnum", r#"enum FooEnum { Alpha, Beta }"#, r#"enum BarEnum { Alpha, Beta }"#),
            (
                "BarUnion",
                r#"union FooUnion { f1: u32, f2: u32 }"#,
                r#"union BarUnion { f1: u32, f2: u32 }"#,
            ),
            ("BAR_CONST", r#"const FOO_CONST: usize = 0;"#, r#"const BAR_CONST: usize = 0;"#),
            ("BAR_STATIC", r#"static FOO_STATIC: i32 = 0;"#, r#"static BAR_STATIC: i32 = 0;"#),
        ];

        let make = SyntaxFactory::without_mappings();
        for (new_name, before, expected) in cases {
            let new_name = make.name(new_name);
            check_node::<ast::AnyHasName>(before, expected, |editor, node| {
                node.set_name(editor, new_name);
            })
        }
    }

    #[test]
    fn test_set_visibility() {
        let exp_by_vis: FxHashMap<_, _> = [
            (r#"struct FooStruct;"#),
            (r#"pub struct FooStruct;"#),
            (r#"pub(crate) struct FooStruct;"#),
            (r#"pub(super) struct FooStruct;"#),
            (r#"pub(in crate::foo) struct FooStruct;"#),
        ]
        .into_iter()
        .map(|text| (ast_from_text::<ast::AnyHasVisibility>(text).visibility(), text))
        .collect();

        for &before in exp_by_vis.values() {
            for new_vis in exp_by_vis.keys() {
                let expected = exp_by_vis[new_vis];
                check_node::<ast::AnyHasVisibility>(before, expected, |editor, node| {
                    let new_vis = new_vis.as_ref().map(|v| v.clone_for_update());
                    node.set_visibility(editor, new_vis.as_ref());
                })
            }
        }
    }

    fn check_add_variant(before: &str, expected: &str, variant: ast::Variant) {
        check_node::<ast::Enum>(before, expected, |editor, enum_| {
            if let Some(it) = enum_.variant_list() {
                it.add_variant(editor, &variant)
            }
        });
    }

    fn check_node<N: AstNode>(
        before: &str,
        expected: &str,
        check_fn: impl FnOnce(&mut SyntaxEditor, N),
    ) {
        let node = ast_from_text::<N>(before);
        let mut editor = SyntaxEditor::new(node.syntax().clone());
        check_fn(&mut editor, node);
        let edit = editor.finish();
        let after = edit.new_root.to_string();
        assert_eq_text!(&trim_indent(expected.trim()), &trim_indent(after.trim()));
    }
}
