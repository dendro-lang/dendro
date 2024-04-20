mod expr;
mod ident;
mod pat;
mod pointer;

use dendro_error::{DiagCx, Error};
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

impl Attribute {
    pub fn parse_builtin<T>(
        this: &mut Vec<Attribute>,
        sym: Symbol,
        mut parse: impl FnMut(Span, AttrArgs) -> T,
    ) -> Vec<T> {
        let mut output = Vec::new();

        let mut index = 0;
        while index < this.len() {
            if let AttrKind::Normal(expr, _) = &this[index].kind
                && let expr::ExprKind::Ident(i) = expr.kind
                && i == sym
            {
                let attr = this.swap_remove(index);
                let AttrKind::Normal(_, args) = attr.kind else {
                    unreachable!()
                };
                output.push(parse(i.span, args));
                continue;
            }

            index += 1;
        }

        output
    }

    pub fn parse_builtin_eq<T>(
        this: &mut Vec<Attribute>,
        sym: Symbol,
        diag: &DiagCx,
        mut parse: impl FnMut(Span, P<Expr>) -> Result<T, Error>,
    ) -> Vec<Result<T, Error>> {
        Self::parse_builtin(this, sym, |span, args| match args {
            AttrArgs::Eq(_, expr) => parse(span, expr),
            _ => Err(diag.span_err(span, "expected an assignment: #[attr = ...]")),
        })
    }
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

    pub fn load_block(self, block: &mut Block, is_inline: bool) -> Vec<Attribute> {
        assert_eq!(block.kind, BlockKind::Unloaded);
        block.kind = BlockKind::Loaded {
            stmts: self.stmts,
            is_inline,
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
