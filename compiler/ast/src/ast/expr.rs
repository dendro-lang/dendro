use dendro_span::{ident::Ident, span::Span};

use super::{Attribute, Item, Lifetime, Mutability, Pat, Path, Spanned, Visibility, P};
use crate::token;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    /// `.`
    Infix,
    /// `:`
    BelongsTo,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnOpKind {
    /// `*`
    Deref,
    /// `!`
    Not,
    /// `-`
    Neg,
    /// `exists`
    Exists,
}

pub type UnOp = Spanned<UnOpKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prerequisites {
    pub forall: Vec<Ident>,
    pub where_clause: Vec<P<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub visibility: Visibility,
    pub pat: P<Pat>,
    pub expr: P<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchArm {
    pub prerequisites: Prerequisites,
    pub attrs: Vec<Attribute>,
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
pub struct TupleField {
    pub prerequisites: Prerequisites,
    pub attrs: Vec<Attribute>,
    pub visibility: Visibility,
    pub expr: P<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub prerequisites: Prerequisites,
    pub attrs: Vec<Attribute>,
    pub id: u32,
    pub span: Span,
    pub visibility: Visibility,
    pub pat: P<Pat>,
    pub expr: P<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub id: u32,
    pub items: Vec<Item>,
    pub is_unsafe: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    /// `ident`
    Ident(Ident),
    /// `"abcde"`
    Literal(token::Lit, Span),
    /// `some::module::item`
    Path(Path),
    /// `forall a where a > 1 :: expr`
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
    /// `match predicate on { arms.. }`
    Match(P<Expr>, Vec<MatchArm>),
    /// `&'lifetime #mutability expr`
    Deref(Option<Lifetime>, Mutability, P<Expr>),
    /// `!a`
    Unary(UnOp, P<Expr>),
    /// `a + b`
    Binary(P<Expr>, BinOp, P<Expr>),
    /// `[a, b, c]`
    Array(Vec<P<Expr>>),
    /// `[repeated; count]`
    ArrayRepeated(P<Expr>, P<Expr>),
    /// `(a, b, c)`
    Tuple(Vec<TupleField>),
    /// `Struct { x = a; y = { b } z = c; }`
    Struct(Ident, Vec<StructField>),
    /// `'life: expr`
    Annotated(Lifetime, P<Expr>),
    /// `{ expr }` or `unsafe { expr }`
    Block(P<Block>),
    /// `param -> body`
    Lambda(P<Pat>, P<Expr>),
    /// `lhs = rhs`
    ///
    /// `span` is the span of `=`.
    Assign(P<Expr>, Span, P<Expr>),
    /// `lhs += rhs`
    AssignOp(P<Expr>, BinOp, P<Expr>),
    /// `slice[index]`
    Index(P<Expr>, P<Expr>),
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
    pub attrs: Vec<Attribute>,
}
