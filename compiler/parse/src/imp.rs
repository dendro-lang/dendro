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
use dendro_error::Error;
use dendro_span::span::Pos;
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

fn parse_delim((_delim, _tt): (Delimiter, TokenStream)) -> Result<P<Expr>, ParseError> {
    todo!()
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
