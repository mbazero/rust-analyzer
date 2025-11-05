//! This module defines Concrete Syntax Tree (CST), used by rust-analyzer.
//!
//! The CST includes comments and whitespace, provides a single node type,
//! `SyntaxNode`, and a basic traversal API (parent, children, siblings).
//!
//! The *real* implementation is in the (language-agnostic) `rowan` crate, this
//! module just wraps its API.

use std::ops::RangeInclusive;

use either::Either;
use rowan::{Direction, GreenNodeBuilder, Language};

use crate::{ted::Element, Parse, SyntaxError, SyntaxKind, TextSize};

pub(crate) use rowan::{GreenNode, GreenToken, NodeOrToken};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RustLanguage {}
impl Language for RustLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        SyntaxKind::from(raw.0)
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
    }
}

pub type SyntaxNode = rowan::SyntaxNode<RustLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<RustLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<RustLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<RustLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<RustLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<RustLanguage>;

#[derive(Default)]
pub struct SyntaxTreeBuilder {
    errors: Vec<SyntaxError>,
    inner: GreenNodeBuilder<'static>,
}

impl SyntaxTreeBuilder {
    pub(crate) fn finish_raw(self) -> (GreenNode, Vec<SyntaxError>) {
        let green = self.inner.finish();
        (green, self.errors)
    }

    pub fn finish(self) -> Parse<SyntaxNode> {
        let (green, errors) = self.finish_raw();
        // Disable block validation, see https://github.com/rust-lang/rust-analyzer/pull/10357
        #[allow(clippy::overly_complex_bool_expr)]
        if cfg!(debug_assertions) && false {
            let node = SyntaxNode::new_root(green.clone());
            crate::validation::validate_block_structure(&node);
        }
        Parse::new(green, errors)
    }

    pub fn token(&mut self, kind: SyntaxKind, text: &str) {
        let kind = RustLanguage::kind_to_raw(kind);
        self.inner.token(kind, text);
    }

    pub fn start_node(&mut self, kind: SyntaxKind) {
        let kind = RustLanguage::kind_to_raw(kind);
        self.inner.start_node(kind);
    }

    pub fn finish_node(&mut self) {
        self.inner.finish_node();
    }

    pub fn error(&mut self, error: String, text_pos: TextSize) {
        self.errors.push(SyntaxError::new_at_offset(error, text_pos));
    }
}

pub trait SyntaxElementExt {
    fn siblings_with_tokens(&self, direction: Direction) -> impl Iterator<Item = SyntaxElement>;

    fn neighbor(&self, direction: Direction) -> Option<SyntaxElement> {
        self.siblings_with_tokens(direction).next()
    }

    fn neighbor_ws(&self, direction: Direction) -> Option<SyntaxElement> {
        self.siblings_with_tokens(direction)
            .skip(1)
            .take_while(|s| s.as_token().is_some_and(|t| t.kind().is_whitespace()))
            .next()
    }

    fn range_with_neighboring_ws(&self) -> RangeInclusive<SyntaxElement>
    where
        Self: Clone + Into<SyntaxElement>,
    {
        let start =
            self.ws_neighbors(Direction::Prev).last().unwrap_or_else(|| self.clone().into());
        let end = self.ws_neighbors(Direction::Next).last().unwrap_or_else(|| self.clone().into());
        start..=end
    }

    // TODO: Delete in favor of neighbor_ws
    // It appears as if the parser collapses all whitespace tokens into a single element,
    // so there will never be more than a single neighbor ws element.
    fn ws_neighbors(&self, direction: Direction) -> impl Iterator<Item = SyntaxElement> {
        self.siblings_with_tokens(direction)
            .skip(1)
            .take_while(|s| s.as_token().is_some_and(|t| t.kind().is_whitespace()))
    }
}

impl SyntaxElementExt for SyntaxElement {
    fn siblings_with_tokens(&self, direction: Direction) -> impl Iterator<Item = SyntaxElement> {
        match self {
            NodeOrToken::Node(node) => Either::Left(node.siblings_with_tokens(direction)),
            NodeOrToken::Token(token) => Either::Right(token.siblings_with_tokens(direction)),
        }
    }
}

impl SyntaxElementExt for SyntaxNode {
    fn siblings_with_tokens(&self, direction: Direction) -> impl Iterator<Item = SyntaxElement> {
        self.siblings_with_tokens(direction)
    }
}

impl SyntaxElementExt for SyntaxToken {
    fn siblings_with_tokens(&self, direction: Direction) -> impl Iterator<Item = SyntaxElement> {
        self.siblings_with_tokens(direction)
    }
}
