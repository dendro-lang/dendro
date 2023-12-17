use dendro_ast::ast::{Mutability, DUMMY_ID};
use dendro_error::{Diagnostic, Error};
use dendro_span::{
    ident::{kw, Ident},
    span::Span,
    symbol::Symbol,
};

use super::ParseError;

pub fn parse_ident(span: Span, (sym, is_raw): (Symbol, bool)) -> Result<Ident, ParseError> {
    if !is_raw && sym.is_keyword() {
        let mut err = Diagnostic::error(None, false);
        err.push_fmt(
            span,
            format_args!(
                "`{sym}` is a keyword; try to use another identifier, or prefix it with `r#`"
            ),
        );
        err.report()?;
    }
    Ok(Ident::new(sym, span))
}

pub fn parse_mutability(ident: Ident, prefixed: bool) -> Result<Mutability, ParseError> {
    let is_keyword = ident.is(kw::CONST) || ident.is(kw::MUT) || ident.is(kw::MOVE);

    if !is_keyword && !prefixed {
        Diagnostic::error(None, false)
            .push(ident.span, "generic mutabilities must be prefixed with `#`")
            .report()?;
    }

    Ok(Mutability {
        id: DUMMY_ID,
        ident,
    })
}

pub fn parse_keyword(ident: Ident, keyword: Symbol) -> Result<(), Error> {
    if ident.is(keyword) {
        return Ok(());
    }
    Err(Diagnostic::error(None, false)
        .push_fmt(
            ident.span,
            format_args!("expect keyword `{keyword}`; found ident `{ident}`"),
        )
        .into_err())
}
