lalrpop_mod!(#[allow(unused_imports)] ast, "/src/ast.rs");

use dendro_ast::{
    ast::{Expr, ExprKind, DUMMY_ID, P},
    token::{Delimiter, Token, TokenKind},
    token_stream::{Cursor, TokenStream, TokenTree},
};
use dendro_error::{Diagnostic, Error};
use dendro_span::{
    ident::Ident,
    span::{Pos, Span},
    symbol::Symbol,
};
use lalrpop_util::lalrpop_mod;

type ParseError = lalrpop_util::ParseError<Pos, Unspanned, Error>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Unspanned {
    Single(TokenKind),
    Delimited(Delimiter, TokenStream),
}

struct LalrpopIter(Cursor);

impl Iterator for LalrpopIter {
    type Item = Result<(Pos, Unspanned, Pos), Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let tt = self.0.next()?;
        Some(Ok(match tt {
            TokenTree::Token(Token { kind, span }) => {
                (span.start, Unspanned::Single(kind), span.end)
            }
            TokenTree::Delimited(span, delim, ts) => (
                span.open.start,
                Unspanned::Delimited(delim, ts),
                span.close.end,
            ),
        }))
    }
}

fn parse_delim(_delim: Delimiter, _ts: TokenStream) -> Result<P<Expr>, ParseError> {
    todo!()
}

fn parse_ident(span: Span, sym: Symbol, is_raw: bool) -> Result<P<Expr>, ParseError> {
    if !is_raw && sym.is_keyword() {
        Diagnostic::error(None, false)
            .push_spanned_fmt(
                span,
                format_args!(
                    "`{sym}` is a keyword; try to use another identifier, or prefix it with `r#`"
                ),
            )
            .report()?;
    }
    Ok(P(Expr {
        id: DUMMY_ID,
        kind: ExprKind::Ident(Ident::new(sym, span)),
        span,
        attrs: vec![],
    }))
}

#[cfg(test)]
mod tests {
    use super::{ast::ExprParser, *};

    #[test]
    fn t() {
        let tt = dendro_lexer::parse("where").unwrap();

        let p = ExprParser::new();
        let ts: P<Expr> = p.parse(LalrpopIter(tt.into_trees())).unwrap();

        println!("{:#?}", ts);
        println!("{:#?}", dendro_error::take());
    }
}
