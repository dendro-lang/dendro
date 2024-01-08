use dendro_ast::{
    ast::{AttrArgs, AttrKind, AttrStyle, Attribute, Expr, DUMMY_ID, P},
    token::{self, Token, TokenKind},
    token_stream::{Spacing, TokenStream, TokenTree},
};
use dendro_error::{DiagCx, Error};
use dendro_span::span::{Pos, Span};

use super::{ParseError, SpacedToken};
use crate::imp::{ast, InnerAttr, OuterAttr, ParseCx, TokenFrames};

impl<'a, 'diag> TokenFrames<'a, 'diag> {
    pub fn parse_attr(
        &mut self,
        kind: TokenKind,
        span: Span,
    ) -> Option<(Pos, SpacedToken<'diag>, Pos)> {
        if kind != token::Pound {
            return None;
        }
        let style = match self.current.lookahead(0) {
            Some(TokenTree::Delimited(_, _, token::Bracket, _)) => AttrStyle::Outer,
            Some(TokenTree::Token(tk, _)) if tk.kind == token::Not => {
                match self.current.lookahead(1) {
                    Some(TokenTree::Delimited(_, _, token::Bracket, _)) => AttrStyle::Inner,
                    _ => return None,
                }
            }
            _ => return None,
        };

        if style == AttrStyle::Inner {
            Self::expect_token(self.current.next(), token::Not);
        }
        let (dspan, tts) = Self::expect_delimited(self.current.next(), token::Bracket);
        let span = Span::new(span.start, dspan.close.end);

        let (path, args) = match parse_attr_inner(self.diag, tts) {
            Ok(args) => args,
            Err(err) => {
                let err = if style == AttrStyle::Inner {
                    (Spacing::Alone, InnerAttr(Err(err)), Spacing::Alone)
                } else {
                    (Spacing::Alone, OuterAttr(Err(err)), Spacing::Alone)
                };
                return Some((span.start, err, span.end));
            }
        };

        let attr = Attribute {
            id: DUMMY_ID,
            style,
            kind: AttrKind::Normal(path, args),
            span: Span::new(span.start, dspan.close.end),
        };

        let token = if style == AttrStyle::Inner {
            (Spacing::Alone, InnerAttr(Ok(attr)), Spacing::Alone)
        } else {
            (Spacing::Alone, OuterAttr(Ok(attr)), Spacing::Alone)
        };
        Some((span.start, token, span.end))
    }
}

fn parse_attr_inner(diag: &DiagCx, tts: &TokenStream) -> Result<(P<Expr>, AttrArgs), Error> {
    let trees = tts.trees();
    let (path, mut arg) = trees.split_first(|tt| match tt {
        TokenTree::Delimited(..) => true,
        TokenTree::Token(tk, _) if tk.kind == token::Eq => true,
        _ => false,
    });

    let path: P<Expr> = ast::PathParser::new()
        .parse(diag, &mut ParseCx::default(), TokenFrames::new(diag, path))
        .map_err(|err| to_diag(diag, err))?;

    match arg.next() {
        None => Ok((path, AttrArgs::Empty)),
        Some(&TokenTree::Token(Token { kind: token::Eq, span }, _)) => {
            let expr: P<Expr> = ast::ExprParser::new()
                .parse(diag, &mut ParseCx::default(), TokenFrames::new(diag, arg))
                .map_err(|err| to_diag(diag, err))?;

            Ok((path, AttrArgs::Eq(span, expr)))
        }
        Some(&TokenTree::Delimited(span, _, delim, ref tts)) => {
            Ok((path, AttrArgs::Delimited(span, delim, tts.clone())))
        }
        _ => unreachable!(),
    }
}

fn to_diag<'diag>(diag: &'diag DiagCx, err: ParseError<'diag>) -> Error {
    match err {
        lalrpop_util::ParseError::InvalidToken { .. } => unreachable!(),
        lalrpop_util::ParseError::UnrecognizedEof { location, .. } => {
            let mut err = diag.error(None, false);
            err.push(
                Span::from(location),
                "expected path expression, found end of the attribute",
            );
            err.emit()
        }
        lalrpop_util::ParseError::UnrecognizedToken {
            token: (start, (_, token, _), end),
            ..
        } => {
            let mut err = diag.error(None, false);
            err.push(
                Span::new(start, end),
                format_args!("expected path expression, found {token:?}"),
            );
            err.emit()
        }
        lalrpop_util::ParseError::ExtraToken { .. } => todo!(),
        lalrpop_util::ParseError::User { error } => error,
    }
}

#[cfg(test)]
mod tests {
    use dendro_error::DiagCx;
    use dendro_span::source::SourceFile;

    #[test]
    fn test_attr() {
        let diag = DiagCx::new();
        let tts = dendro_lexer::parse(&SourceFile::test("self.path = 0"), &diag);
        let a = super::parse_attr_inner(&diag, &tts).unwrap();
        println!("{a:?}");
    }
}
