use dendro_ast::ast::{BindingMode, Mutability, Pat, PatKind, DUMMY_ID, P};
use dendro_span::{ident::Ident, span::Span};

pub fn parse_ident(span: Span, mode: BindingMode, m: Mutability, ident: Ident) -> P<Pat> {
    P(Pat {
        id: DUMMY_ID,
        kind: PatKind::Ident(mode, m, ident, None),
        span,
    })
}
