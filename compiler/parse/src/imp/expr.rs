use dendro_ast::ast::{BinOp, BinOpKind, Expr, ExprKind, DUMMY_ID, P};
use dendro_span::span::{Pos, Span};

pub fn parse_bin(lhs: (Pos, P<Expr>), op: (Pos, BinOpKind, Pos), rhs: (P<Expr>, Pos)) -> P<Expr> {
    P(Expr {
        id: DUMMY_ID,
        kind: ExprKind::Binary(
            lhs.1,
            BinOp {
                kind: op.1,
                span: Span::new(op.0, op.2),
            },
            rhs.0,
        ),
        span: Span::new(lhs.0, rhs.1),
        attrs: vec![],
    })
}
