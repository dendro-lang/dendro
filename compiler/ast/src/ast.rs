mod expr;
mod ident;
mod pat;
mod pointer;

use dendro_span::{ident::Ident, span::Span};

pub use self::{expr::*, ident::*, pat::*, pointer::P};

pub const DUMMY_ID: u32 = u32::MAX;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub kind: T,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VisibilityKind {
    Public,
    Inherited,
    Restricted { path: P<Path>, id: u32 },
}

pub type Visibility = Spanned<VisibilityKind>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Inline {
    Yes,
    No,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleKind {
    Loaded(Vec<P<Item>>, Inline, Span),
    Unloaded,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UseTreeKind {
    Simple(Option<Ident>),
    Nested(Vec<UseTree>),
    Glob,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseTree {
    pub prefix: Path,
    pub kind: UseTreeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ItemKind {
    /// `mod abc;` or `mod abc {}`
    Module(Ident, ModuleKind),
    /// `use prefix::{a, b, c};`
    Use(UseTree),
    /// `expr;`
    Expr(P<Expr>),
    /// `expr`
    Semi(P<Expr>),
    /// `;`
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Item {
    pub id: u32,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AttrStyle {
    Outer,
    Inner,
}
