use dendro_span::{ident::Ident, span::Span};

use super::{BinOp, Expr, Lifetime, Mutability, Path, RangeLimits, P};
use crate::token;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatField {
    pub id: u32,
    pub ident: Ident,
    pub pat: P<Pat>,
    pub span: Span,
    pub is_shorthand: bool,
    pub is_placeholder: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingMode {
    ByRef,
    ByValue,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatKind {
    /// `_`
    Wildcard,
    /// `ref mut a @ pat`
    Ident(BindingMode, Mutability, Ident, Option<P<Pat>>),
    /// `[a, b, c]`
    Array(Vec<P<Pat>>),
    /// `(a, b, c)`
    Tuple(Vec<P<Pat>>),
    /// `@{ a = x; b; c }`
    Struct(Vec<PatField>),
    /// `A | B`
    Or(Vec<P<Pat>>),
    /// `some::module::item`
    Path(Path),
    /// `&'a mut x`
    Ref(Option<Lifetime>, Mutability, P<Pat>),
    /// `123456u32`
    Literal(token::Lit, Span),
    /// `prefix + "target" + suffix`
    Binary(P<Pat>, BinOp, P<Pat>),
    /// `a..b`
    Range(Option<P<Expr>>, RangeLimits, Option<P<Expr>>),
    /// `(pat)`
    Paren(P<Pat>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pat {
    pub id: u32,
    pub kind: PatKind,
    pub span: Span,
}
