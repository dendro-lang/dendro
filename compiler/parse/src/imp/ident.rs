use dendro_error::DiagCx;
use dendro_span::{ident::Ident, span::Span, symbol::Symbol};

use super::ParseError;

pub fn parse_ident(
    cx: &DiagCx,
    span: Span,
    (sym, is_raw): (Symbol, bool),
) -> Result<Ident, ParseError> {
    if !is_raw && sym.is_keyword() {
        let mut err = cx.error(None, false);
        err.push_fmt(
            span,
            format_args!(
                "`{sym}` is a keyword; try to use another identifier, or prefix it with `r#`"
            ),
        );
        return Err(err.into());
    }
    Ok(Ident::new(sym, span))
}
