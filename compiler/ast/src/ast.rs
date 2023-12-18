mod expr;
mod ident;
mod pat;
mod pointer;

use dendro_span::{
    ident::Ident,
    span::{DelimSpan, Span},
    symbol::Symbol,
};

pub use self::{expr::*, ident::*, pat::*, pointer::P};
use crate::{
    token::{CommentKind, Delimiter},
    token_stream::TokenStream,
};

pub const DUMMY_ID: u32 = u32::MAX;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub kind: T,
    pub span: Span,
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
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UseTreeKind {
    /// `use prefix::ident`
    Simple(Option<Ident>),
    /// `use prefix::{nested..}`
    Nested(Vec<UseTree>),
    /// `use prefix::*`
    Glob,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseTree {
    pub prefix: P<Expr>,
    pub kind: UseTreeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind {
    /// `use prefix::{a, b, c};`
    Use(Visibility, UseTree),
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
