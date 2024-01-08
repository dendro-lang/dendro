use dendro_span::{
    ident::{kw, Ident},
    span::Span,
};

use super::{Attribute, Expr, Lifetime, Mutability, Operator, RangeLimits, DUMMY_ID, P};
use crate::token;

/// `#[attrs] ident: pat`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatField {
    pub id: u32,
    pub ident: Ident,
    pub pat: P<Pat>,
    pub span: Span,
    pub attrs: Vec<Attribute>,
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
    /// `` `abc.def ``
    Path(P<Expr>),
    /// `(+)`,
    Operator(Operator),
    /// `ref mut a @ pat`
    Ident(BindingMode, Mutability, Ident, Option<P<Pat>>),
    /// `[a, b, c]`
    Array(Vec<P<Pat>>),
    /// `(a, b, c)`
    Tuple(Vec<P<Pat>>),
    /// `{ a: x, b, c }`
    Struct(Vec<PatField>, bool),
    /// `{ * }`
    StructGlob,
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
    /// `func ..?implicit_args ..args`
    Call(P<Pat>, Vec<P<Pat>>, Vec<P<Pat>>),
    /// error pattern
    Err,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pat {
    pub id: u32,
    pub kind: PatKind,
    pub span: Span,
}

impl Pat {
    pub const fn from_ident(ident: Ident) -> Self {
        Pat {
            id: DUMMY_ID,
            kind: PatKind::Ident(BindingMode::ByValue, Mutability::kw(kw::MOVE), ident, None),
            span: ident.span,
        }
    }
}
