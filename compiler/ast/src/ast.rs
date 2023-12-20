mod expr;
mod ident;
mod pat;
mod pointer;

use dendro_span::{
    span::{DelimSpan, Span},
    symbol::Symbol,
};

pub use self::{expr::*, ident::*, pat::*, pointer::P};
use crate::{
    token::{CommentKind, Delimiter},
    token_stream::TokenStream,
};

pub const DUMMY_ID: u32 = u32::MAX;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub kind: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(kind: T, span: Span) -> Self {
        Spanned { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VisibilityKind {
    /// `pub`
    Public,
    /// Inherited
    Inherited,
    /// `pub(in expr)`
    Restricted { path: P<Expr>, id: u32 },
}

pub type Visibility = Spanned<VisibilityKind>;

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
    pub id: u32,
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
    pub id: u32,
    pub kind: StmtKind,
    pub span: Span,
}

/// A binary unit: library/executable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Leaf {
    pub id: u32,
    pub attrs: Vec<Attribute>,
    pub stmts: Vec<P<Stmt>>,
    pub span: Span,
}

impl Leaf {
    pub fn load_block(self, mut block: Block) -> (Block, Vec<Attribute>) {
        assert_eq!(block.kind, BlockKind::Unloaded);

        block.kind = BlockKind::Loaded {
            stmts: self.stmts,
            is_inline: true,
            span: self.span,
        };
        block.id = self.id;
        (block, self.attrs)
    }
}
