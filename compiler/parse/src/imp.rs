lalrpop_mod!(
    #[allow(unused_imports)]
    #[allow(clippy::ptr_arg)]
    ast,
    "/src/ast.rs"
);

mod expr;
mod ident;
mod pat;

use std::{cell::RefCell, mem};

use dendro_ast::{
    ast::{Attribute, Visibility, VisibilityKind},
    token::{Delimiter, Token, TokenKind},
    token_stream::{Cursor, TokenStream, TokenTree},
};
use dendro_error::{DiagCx, DiagnosticBuilder};
use dendro_span::span::{Pos, Span};
use lalrpop_util::lalrpop_mod;

type ParseError<'diag> = lalrpop_util::ParseError<Pos, TokenKind, DiagnosticBuilder<'diag>>;

#[derive(Debug)]
struct Frame {
    cursor: Cursor,
    delim: Delimiter,
    close_span: Span,
}

#[derive(Debug)]
struct TokenFrames {
    current: Cursor,
    stack: Vec<Frame>,
}

impl TokenFrames {
    fn new(tokens: TokenStream) -> Self {
        TokenFrames {
            current: tokens.into_trees(),
            stack: Vec::new(),
        }
    }

    fn next_token(&mut self) -> Option<(Pos, TokenKind, Pos)> {
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

    fn comsume_delim(&mut self, delim: Delimiter) -> (TokenStream, Span) {
        let frame = self.stack.pop().unwrap();
        assert_eq!(frame.delim, delim);
        let cursor = mem::replace(&mut self.current, frame.cursor);
        (cursor.into_stream(), frame.close_span)
    }
}

#[derive(Debug)]
struct ParseCx<'a> {
    inner_attrs: Vec<Attribute>,
    token_frames: &'a RefCell<TokenFrames>,
}

impl<'a> ParseCx<'a> {
    fn take_attr(&mut self, mut attrs: Vec<Attribute>) -> Vec<Attribute> {
        attrs.append(&mut self.inner_attrs);
        attrs
    }

    fn push_attr(&mut self, attr: Attribute) {
        self.inner_attrs.push(attr);
    }

    fn consume_delim(&mut self, delim: Delimiter) -> (TokenStream, Span) {
        self.token_frames.borrow_mut().comsume_delim(delim)
    }
}

#[derive(Debug)]
struct LalrpopIter<'a>(&'a RefCell<TokenFrames>);

impl<'a> Iterator for LalrpopIter<'a> {
    type Item = (Pos, TokenKind, Pos);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.borrow_mut().next_token()
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
        let tts = dendro_lexer::parse("abcde", &diag);

        let tf = RefCell::new(TokenFrames::new(tts));
        let mut cx = ParseCx {
            inner_attrs: Vec::new(),
            token_frames: &tf,
        };
        let p = ExprParser::new();
        let ts: P<Expr> = p.parse(&diag, &mut cx, LalrpopIter(&tf)).unwrap();

        println!("{:#?}", ts);
    }
}
