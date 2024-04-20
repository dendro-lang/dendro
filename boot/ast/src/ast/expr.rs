use std::fmt;

use dendro_span::{ident::Ident, span::Span};

use super::{Attribute, Lifetime, Mutability, Pat, Spanned, Stmt, DUMMY_ID, P};
use crate::{id::NodeId, token};

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

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOpKind::Mul => write!(f, "*"),
            BinOpKind::Div => write!(f, "/"),
            BinOpKind::Rem => write!(f, "%"),
            BinOpKind::Add => write!(f, "+"),
            BinOpKind::Sub => write!(f, "-"),
            BinOpKind::Shl => write!(f, "<<"),
            BinOpKind::Shr => write!(f, ">>"),
            BinOpKind::BitXor => write!(f, "^"),
            BinOpKind::BitAnd => write!(f, "&"),
            BinOpKind::BitOr => write!(f, "|"),
            BinOpKind::Eq => write!(f, "=="),
            BinOpKind::Lt => write!(f, "<"),
            BinOpKind::Le => write!(f, "<="),
            BinOpKind::Ne => write!(f, "!="),
            BinOpKind::Ge => write!(f, ">="),
            BinOpKind::Gt => write!(f, ">"),
            BinOpKind::And => write!(f, "&&"),
            BinOpKind::Or => write!(f, "||"),
        }
    }
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

impl fmt::Display for UnOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOpKind::Deref => write!(f, "*"),
            UnOpKind::Not => write!(f, "!"),
            UnOpKind::Neg => write!(f, "-"),
        }
    }
}

pub type UnOp = Spanned<UnOpKind>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpKind {
    /// `(+)`
    Binary(BinOpKind),
    /// ``(`!)``
    Unary(UnOpKind),
    /// `(=)`
    Assign,
    /// `(+=)`
    AssignBinary(BinOpKind),
}

impl fmt::Display for OpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpKind::Binary(op) => write!(f, "{}", op),
            OpKind::Unary(op) => write!(f, "{}", op),
            OpKind::Assign => write!(f, "="),
            OpKind::AssignBinary(op) => write!(f, "{}=", op),
        }
    }
}

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

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind().fmt(f)
    }
}

impl Operator {
    pub const fn from_bin(kind: BinOpKind, span: Span) -> Self {
        Operator::Binary(BinOp { kind, span })
    }

    pub const fn from_un(kind: UnOpKind, span: Span) -> Self {
        Operator::Unary(UnOp { kind, span })
    }

    pub const fn from_assign(span: Span) -> Self {
        Operator::Assign(span)
    }

    pub const fn from_assign_bin(kind: BinOpKind, span: Span) -> Self {
        Operator::AssignBinary(BinOp { kind, span })
    }

    pub const fn kind(self) -> OpKind {
        match self {
            Operator::Binary(op) => OpKind::Binary(op.kind),
            Operator::Unary(op) => OpKind::Unary(op.kind),
            Operator::Assign(_) => OpKind::Assign,
            Operator::AssignBinary(op) => OpKind::AssignBinary(op.kind),
        }
    }

    pub const fn span(self) -> Span {
        match self {
            Operator::Binary(op) => op.span,
            Operator::Unary(op) => op.span,
            Operator::Assign(span) => span,
            Operator::AssignBinary(op) => op.span,
        }
    }
}

/// `forall a where a > 1 ::`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prerequisites {
    pub id: NodeId,
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
    pub id: NodeId,
    pub span: Span,
    pub pat: P<Pat>,
    pub ty: Option<P<Expr>>,
}

/// `\pat -> expr`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchArm {
    pub prerequisites: Prerequisites,
    pub attrs: Vec<Attribute>,
    pub id: NodeId,
    pub span: Span,
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
    pub id: NodeId,
    pub span: Span,
    pub kind: StructFieldKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BlockKind {
    /// { stmts } or from another loaded file.
    Loaded {
        stmts: Vec<Stmt>,
        is_inline: bool,
        /// Excluding the braces.
        span: Span,
    },
    /// From another file which is not loaded yet.
    Unloaded,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub id: NodeId,
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
    /// `\..?implicit_args ..args: type -> body`
    Lambda(Vec<P<Pat>>, Vec<P<Pat>>, Option<P<Expr>>, P<Expr>),
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
    /// error expression
    Err,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
    pub attrs: Vec<Attribute>,
}

impl PartialEq<Ident> for Expr {
    fn eq(&self, other: &Ident) -> bool {
        matches!(&self.kind, ExprKind::Ident(id) if id == other)
    }
}
