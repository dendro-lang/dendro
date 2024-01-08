use dendro_span::{
    ident::{kw, Ident},
    span::Span,
};

use super::{Attribute, Expr, Lifetime, Mutability, Operator, RangeLimits, DUMMY_ID, P};
use crate::{id::NodeId, token};

/// `#[attrs] ident: pat`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatField {
    pub id: NodeId,
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
    pub id: NodeId,
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

    pub fn as_path_ident(&self) -> Option<Ident> {
        match self.kind {
            PatKind::Ident(_, _, ident, _) => Some(ident),

            PatKind::Deref(_, _, ref pat)
            | PatKind::Paren(ref pat)
            | PatKind::Call(ref pat, ..) => pat.as_path_ident(),

            PatKind::Wildcard
            | PatKind::StructGlob
            | PatKind::Err
            | PatKind::Path(_)
            | PatKind::Operator(_)
            | PatKind::Literal(_)
            | PatKind::Array(_)
            | PatKind::Tuple(_)
            | PatKind::Or(_)
            | PatKind::Struct(..)
            | PatKind::Range(..) => None,
        }
    }

    pub fn walk(&self, it: &mut impl FnMut(&Pat) -> bool) {
        if !it(self) {
            return;
        }

        match &self.kind {
            // Walk into the pattern associated with `Ident` (if any).
            PatKind::Ident(_, _, _, Some(p)) => p.walk(it),

            // Walk into each field of struct.
            PatKind::Struct(fields, _) => fields.iter().for_each(|field| field.pat.walk(it)),

            // Sequence of patterns.
            PatKind::Tuple(s) | PatKind::Array(s) | PatKind::Or(s) => {
                s.iter().for_each(|p| p.walk(it))
            }

            PatKind::Call(func, iargs, args) => {
                func.walk(it);
                iargs.iter().for_each(|arg| arg.walk(it));
                args.iter().for_each(|arg| arg.walk(it));
            }

            // Trivial wrappers over inner patterns.
            PatKind::Deref(_, _, s) | PatKind::Paren(s) => s.walk(it),

            // These patterns do not contain subpatterns, skip.
            PatKind::Wildcard
            | PatKind::StructGlob
            | PatKind::Err
            | PatKind::Literal(_)
            | PatKind::Operator(_)
            | PatKind::Range(..)
            | PatKind::Ident(..)
            | PatKind::Path(..) => {}
        }
    }
}
