lalrpop_mod!(
    #[allow(unused_imports)]
    ast,
    "/src/ast.rs"
);

mod ident;

use dendro_ast::{
    ast::{Expr, P},
    token::{Delimiter, Token, TokenKind},
    token_stream::{Cursor, TokenStream, TokenTree},
};
use dendro_error::{DiagCx, DiagnosticBuilder};
use dendro_span::span::Pos;
use lalrpop_util::lalrpop_mod;

type ParseError<'a> = lalrpop_util::ParseError<Pos, Unspanned, DiagnosticBuilder<'a>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Unspanned {
    Single(TokenKind),
    Delimited(Delimiter, TokenStream),
}

struct LalrpopIter<'a>(Cursor, &'a DiagCx);

impl<'a> Iterator for LalrpopIter<'a> {
    type Item = Result<(Pos, Unspanned, Pos), DiagnosticBuilder<'a>>;

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

fn parse_delim(
    _cx: &DiagCx,
    (_delim, _tt): (Delimiter, TokenStream),
) -> Result<P<Expr>, ParseError<'_>> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::{ast::ExprParser, *};

    #[test]
    fn t() {
        let cx = DiagCx::new();
        let tt = dendro_lexer::parse("abcde", &cx);

        let p = ExprParser::new();
        let ts: P<Expr> = p.parse(&cx, LalrpopIter(tt.into_trees(), &cx)).unwrap();

        println!("{:#?}", ts);
    }
}
