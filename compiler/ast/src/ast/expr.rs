use dendro_span::{ident::Ident, span::Span};

use super::{Item, Lifetime, Pat, Path, Spanned, Visibility, P};
use crate::token;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
    /// `&&`
    And,
    /// `||`
    Or,
    /// `^`
    BitXor,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
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
    /// `.`
    Infix,
}

pub type BinOp = Spanned<BinOpKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnOpKind {
    /// `*`
    Deref,
    /// `!`
    Not,
    /// `-`
    Neg,
}

pub type UnOp = Spanned<UnOpKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prerequisites {
    pub for_all: Vec<Ident>,
    pub where_clause: Vec<P<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MutabilityKind {
    Const,
    Mut,
    Move,
}

pub type Mutability = Spanned<MutabilityKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub prerequisites: Prerequisites,
    pub visibility: Visibility,
    pub pat: P<Pat>,
    pub expr: P<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchArm {
    pub prerequisites: Prerequisites,
    pub pat: P<Pat>,
    pub guard: Option<P<Expr>>,
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
pub struct StructField {
    pub prerequisites: Prerequisites,
    pub visibility: Visibility,
    pub lhs: P<Expr>,
    pub span: Span,
    pub rhs: P<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub id: u32,
    pub stmts: Vec<Stmt>,
    pub is_unsafe: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    /// `ident`
    Ident(Ident),
    /// `forall a where a > 1: let x a = a`
    Let(Let),
    /// `exists expr`
    Exists(Prerequisites, P<Expr>),
    /// `if predicate then body else other`
    If(P<Expr>, P<Expr>, Option<P<Expr>>),
    /// `while predicate do body`
    While(P<Expr>, P<Expr>),
    /// `loop body`
    Loop(P<Expr>),
    /// `for item in expr do body`
    For(P<Pat>, P<Expr>, P<Expr>),
    /// `match predicate on { arms.. }`
    Match(P<Expr>, Vec<MatchArm>),
    /// `&'lifetime mutability expr`
    Deref(Option<Lifetime>, Mutability, P<Expr>),
    /// `a + b`
    Binary(P<Expr>, BinOp, P<Expr>),
    /// `!a`
    Unary(UnOp, P<Expr>),
    /// `[a, b, c]`
    Array(Vec<P<Expr>>),
    /// `[repeated; count]`
    ArrayRepeated(P<Expr>, P<Expr>),
    /// `(a, b, c)`
    Tuple(Vec<(Visibility, P<Expr>)>),
    /// `@{ x = a; y = { b } z = c; }`
    Struct(Vec<StructField>),
    /// `"abcde"`
    Literal(token::Lit, Span),
    /// `'life: expr`
    Annotated(Lifetime, P<Expr>),
    /// `{ expr }` or `unsafe { expr }`
    Block(P<Block>),
    /// `forall a where a > 1: a -> body`
    Lambda(Prerequisites, P<Pat>, P<Expr>),
    /// `lhs = rhs`
    ///
    /// `span` is the span of `=`.
    Assignment(P<Expr>, Span, P<Expr>),
    /// `slice[index]`
    Index(P<Expr>, P<Expr>),
    /// `start..end` or `start..=end`
    Range(Option<P<Expr>>, RangeLimits, Option<P<Expr>>),
    /// `_`
    Underscore,
    /// `some::module::item`
    Path(Path),
    /// `break 'outer value`
    Break(Option<Lifetime>, Option<P<Expr>>),
    /// `continue 'outer`
    Continue(Option<Lifetime>),
    /// `return value`
    Return(Option<P<Expr>>),
    /// `caller callee`
    Call(P<Expr>, P<Expr>),
    /// `expr?`.
    Try(P<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub id: u32,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind {
    Item(P<Item>),
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
