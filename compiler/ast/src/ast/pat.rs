use dendro_span::{ident::Ident, span::Span};

use super::{Lifetime, Mutability, PathRoot, RangeLimits, P};
use crate::token;

/// `#[attrs] ident: pat`
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
    /// `abc::def`,
    Path(PathRoot, Vec<P<Pat>>),
    /// `ref mut a @ pat`
    Ident(BindingMode, Mutability, Ident, Option<P<Pat>>),
    /// `[a, b, c]`
    Array(Vec<P<Pat>>),
    /// `(a, b, c)`
    Tuple(Vec<P<Pat>>),
    /// `Struct { a: x, b, c }`
    Struct(Vec<PatField>, bool),
    /// `A | B`
    Or(Vec<P<Pat>>),
    /// `&'a mut x`
    Deref(Option<Lifetime>, Mutability, P<Pat>),
    /// `123456u32`
    Literal(token::Lit),
    /// `a..b`
    Range(Option<P<Pat>>, RangeLimits, Option<P<Pat>>),
    /// `(pat)`
    Paren(P<Pat>),
    /// `func arg`
    Call(P<Pat>, P<Pat>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pat {
    pub id: u32,
    pub kind: PatKind,
    pub span: Span,
}
