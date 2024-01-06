use dendro_span::{ident::Ident, span::Span};

use super::{Attribute, Lifetime, Mutability, Pat, Spanned, Stmt, DUMMY_ID, P};
use crate::token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
    /// `^`
    BitXor,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `==`
    Eq,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `!=`
    Ne,
    /// `>=`
    Ge,
    /// `>`
    Gt,
    /// `&&`
    And,
    /// `||`
    Or,
}

pub type BinOp = Spanned<BinOpKind>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpKind {
    /// `*`
    Deref,
    /// `!`
    Not,
    /// `-`
    Neg,
}

pub type UnOp = Spanned<UnOpKind>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operator {
    /// `(+)`
    Binary(BinOp),
    /// ``(`!)``
    Unary(UnOp),
    /// `(=)`
    Assign(Span),
    /// `(+=)`
    AssignBinary(BinOp),
}

impl Operator {
    pub fn from_bin(kind: BinOpKind, span: Span) -> Self {
        Operator::Binary(BinOp { kind, span })
    }

    pub fn from_un(kind: UnOpKind, span: Span) -> Self {
        Operator::Unary(UnOp { kind, span })
    }

    pub fn from_assign(span: Span) -> Self {
        Operator::Assign(span)
    }

    pub fn from_assign_bin(kind: BinOpKind, span: Span) -> Self {
        Operator::AssignBinary(BinOp { kind, span })
    }
}

/// `forall a where a > 1 ::`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prerequisites {
    pub id: u32,
    pub forall: Vec<Ident>,
    pub where_clause: Vec<P<Expr>>,
    pub span: Span,
}

impl Prerequisites {
    pub const fn from_span(span: Span) -> Self {
        Prerequisites {
            id: DUMMY_ID,
            forall: Vec::new(),
            where_clause: Vec::new(),
            span,
        }
    }
}

/// `let x a = a`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub pat: P<Pat>,
    pub ty: Option<P<Expr>>,
    pub expr: P<Expr>,
}

/// `\pat: ty`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumField {
    pub prerequisites: Prerequisites,
    pub attrs: Vec<Attribute>,
    pub id: u32,
    pub pat: P<Pat>,
    pub ty: Option<P<Expr>>,
}

/// `\pat -> expr`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchArm {
    pub prerequisites: Prerequisites,
    pub attrs: Vec<Attribute>,
    pub pat: P<Pat>,
    pub expr: P<Expr>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum RangeLimits {
    /// Inclusive at the beginning, exclusive at the end
    HalfOpen,
    /// Inclusive at the beginning and end
    Closed,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StructFieldKind {
    /// `pat: expr`
    Simple { pat: P<Pat>, expr: P<Expr> },
    /// `ident`
    Ident(Ident),
    /// `..default`
    Reuse(P<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub prerequisites: Prerequisites,
    pub attrs: Vec<Attribute>,
    pub id: u32,
    pub span: Span,
    pub kind: StructFieldKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BlockKind {
    /// { stmts } or from another loaded file.
    Loaded {
        stmts: Vec<P<Stmt>>,
        is_inline: bool,
        /// Excluding the braces.
        span: Span,
    },
    /// From another file which is not loaded yet.
    Unloaded,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub id: u32,
    pub kind: BlockKind,
}

impl Block {
    pub const fn unloaded() -> Self {
        Block {
            id: DUMMY_ID,
            kind: BlockKind::Unloaded,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    /// `ident`
    Ident(Ident),
    /// `"abcde"`
    Literal(token::Lit),
    /// `(+)`
    Operator(Operator),
    /// `a.b.c`
    Projection(Vec<P<Expr>>),
    /// `forall a where a > 1 => expr`
    Prereq(Prerequisites, P<Expr>),
    /// `let x a = a`
    Let(Let),
    /// `if predicate then body else other`
    If(P<Expr>, P<Expr>, Option<P<Expr>>),
    /// `while predicate do body`
    While(P<Expr>, P<Expr>),
    /// `loop body`
    Loop(P<Expr>),
    /// `for item in expr do body`
    ForLoop(P<Pat>, P<Expr>, P<Expr>),
    /// `match predicate { arms.. }`
    Match(P<Expr>, Vec<MatchArm>),
    /// `&'lifetime #mutability expr`
    AddrOf(Option<Lifetime>, Mutability, P<Expr>),
    /// `!a`
    Unary(UnOp, P<Expr>),
    /// `a + b`
    Binary(P<Expr>, BinOp, P<Expr>),
    /// `[a, b, c]`
    Array(Vec<P<Expr>>),
    /// `[repeated; count]`
    ArrayRepeated(P<Expr>, P<Expr>),
    /// `(a, b, c)`
    Tuple(Vec<P<Expr>>),
    /// `` `{ x: a, y: b } ``
    Struct(Vec<StructField>),
    /// `` `[ Some a, None ] ``
    Enum(Vec<EnumField>),
    /// `'life: expr`
    Annotated(Lifetime, P<Expr>),
    /// `{ expr }`
    Block(P<Block>),
    /// `\param: type -> body`
    Lambda(P<Pat>, Option<P<Expr>>, P<Expr>),
    /// `lhs = rhs`
    ///
    /// `span` is the span of `=`.
    Assign(P<Expr>, Span, P<Expr>),
    /// `lhs += rhs`
    AssignOp(P<Expr>, BinOp, P<Expr>),
    /// `start..end` or `start..=end`
    Range(Option<P<Expr>>, RangeLimits, Option<P<Expr>>),
    /// `_`
    Underscore,
    /// `break 'outer value`
    Break(Option<Lifetime>, Option<P<Expr>>),
    /// `continue 'outer`
    Continue(Option<Lifetime>),
    /// `return value`
    Return(Option<P<Expr>>),
    /// `caller ..?implicit_args ..args`
    Call(P<Expr>, Vec<P<Expr>>, Vec<P<Expr>>),
    /// `try expr`
    Try(P<Expr>),
    /// `exists expr`
    Exists(P<Expr>),
    /// `a: b`
    BelongsTo(P<Expr>, P<Expr>),
    /// `(expr)`
    Paren(P<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub id: u32,
    pub kind: ExprKind,
    pub span: Span,
    pub attrs: Vec<Attribute>,
}
