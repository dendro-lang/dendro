lalrpop_mod!(
    #[allow(unused_imports)]
    #[allow(clippy::ptr_arg)]
    ast,
    "/src/ast.rs"
);

mod expr;
mod ident;

use std::mem;

use dendro_ast::{
    ast::{Visibility, VisibilityKind, Attribute},
    token::{Delimiter, Token, TokenKind},
    token_stream::{Cursor, TokenStream, TokenTree},
};
use dendro_error::{DiagCx, DiagnosticBuilder};
use dendro_span::span::{Pos, Span};
use lalrpop_util::lalrpop_mod;

type ParseError<'diag> = lalrpop_util::ParseError<Pos, TokenKind, DiagnosticBuilder<'diag>>;

struct ParseCx {
    inner_attrs: Vec<Attribute>,
}

impl ParseCx {
    fn take_attr(&mut self, mut attrs: Vec<Attribute>) -> Vec<Attribute> {
        attrs.append(&mut self.inner_attrs);
        attrs
    }

    fn push_attr(&mut self, attr: Attribute) {
        self.inner_attrs.push(attr);
    }
}

#[derive(Debug)]
struct Frame {
    cursor: Cursor,
    delim: Delimiter,
    close_span: Span,
}

#[derive(Debug)]
struct LalrpopIter {
    current: Cursor,
    stack: Vec<Frame>,
}

impl LalrpopIter {
    fn new(tts: TokenStream) -> Self {
        LalrpopIter {
            current: tts.into_trees(),
            stack: Vec::new(),
        }
    }
}

impl Iterator for LalrpopIter {
    type Item = (Pos, TokenKind, Pos);

    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.current.next() {
            Some(TokenTree::Token(Token { kind, span })) => (span.start, kind, span.end),
            Some(TokenTree::Delimited(span, delim, tts)) => {
                self.stack.push(Frame {
                    cursor: mem::replace(&mut self.current, tts.into_trees()),
                    delim,
                    close_span: span.close,
                });
                (span.open.start, TokenKind::OpenDelim(delim), span.open.end)
            }
            None => match self.stack.pop() {
                Some(frame) => {
                    let close = frame.close_span;
                    self.current = frame.cursor;
                    (close.start, TokenKind::CloseDelim(frame.delim), close.end)
                }
                None => return None,
            },
        })
    }
}

fn parse_vis(input: (Pos, VisibilityKind, Pos)) -> Visibility {
    Visibility {
        kind: input.1,
        span: Span::new(input.0, input.2),
    }
}

#[cfg(test)]
mod tests {
    use dendro_ast::ast::{Expr, P};

    use super::{ast::ExprParser, *};

    #[test]
    fn t() {
        let diag = DiagCx::new();
        let mut cx = ParseCx {
            inner_attrs: Vec::new(),
        };
        let tts = dendro_lexer::parse("abcde", &diag);

        let p = ExprParser::new();
        let ts: P<Expr> = p
            .parse(&diag, &mut cx, LalrpopIter::new(tts))
            .unwrap();

        println!("{:#?}", ts);
    }
}
