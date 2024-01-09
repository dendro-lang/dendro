mod expr;
mod ident;
mod pat;
mod pointer;

use dendro_span::{
    span::{DelimSpan, Span, DUMMY_SPAN},
    symbol::Symbol,
};

pub use self::{expr::*, ident::*, pat::*, pointer::P};
pub use crate::id::DUMMY_ID;
use crate::{
    id::NodeId,
    token::{CommentKind, Delimiter},
    token_stream::TokenStream,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub kind: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(kind: T, span: Span) -> Self {
        Spanned { kind, span }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            kind: f(self.kind),
            span: self.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttrArgs {
    /// `#[attr]`
    Empty,
    /// `#[attr(tt..)]`
    Delimited(DelimSpan, Delimiter, TokenStream),
    /// `#[attr = expr]`
    Eq(Span, P<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttrKind {
    Normal(P<Expr>, AttrArgs),
    /// `///`, `//!`, `/** */` or `/*! */` => `#[doc = ""]`
    Comment(CommentKind, Symbol),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AttrStyle {
    Outer,
    Inner,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub id: NodeId,
    pub style: AttrStyle,
    pub kind: AttrKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind {
    /// `expr;`
    Expr(P<Expr>),
    /// `expr`
    Semi(P<Expr>),
    /// `;`
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
}

/// A binary unit: library/executable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Leaf {
    pub id: NodeId,
    pub attrs: Vec<Attribute>,
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

impl Leaf {
    pub fn dummy() -> Self {
        Self::default()
    }

    pub fn load_block(self, block: &mut Block) -> Vec<Attribute> {
        assert_eq!(block.kind, BlockKind::Unloaded);
        block.kind = BlockKind::Loaded {
            stmts: self.stmts,
            is_inline: true,
            span: self.span,
        };
        self.attrs
    }
}

impl Default for Leaf {
    fn default() -> Self {
        Leaf {
            id: DUMMY_ID,
            attrs: vec![],
            stmts: vec![],
            span: DUMMY_SPAN,
        }
    }
}
