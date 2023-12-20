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
    ast::{Attribute, Leaf, Stmt, Visibility, VisibilityKind, DUMMY_ID, P},
    token::{Delimiter, Token, TokenKind},
    token_stream::{Cursor, Spacing, TokenStream, TokenTree},
};
use dendro_error::{DiagCx, DiagnosticBuilder};
use dendro_span::span::{Pos, Span};
use lalrpop_util::lalrpop_mod;

type SpacedToken = (Spacing, TokenKind, Spacing);

type ParseError<'diag> = lalrpop_util::ParseError<Pos, SpacedToken, DiagnosticBuilder<'diag>>;

#[derive(Debug)]
struct Frame {
    cursor: Cursor,
    delim: Delimiter,
    close_span: Span,
    close_spacing: Spacing,
}

#[derive(Debug)]
struct TokenFrames {
    last_spacing: Spacing,
    current: Cursor,
    stack: Vec<Frame>,
}

impl TokenFrames {
    fn new(tokens: TokenStream) -> Self {
        TokenFrames {
            last_spacing: Spacing::Alone,
            current: tokens.into_trees(),
            stack: Vec::new(),
        }
    }

    fn next_token(&mut self) -> Option<(Pos, SpacedToken, Pos)> {
        Some(match self.current.next_with_spacing() {
            Some((TokenTree::Token(Token { kind, span }), end)) => {
                let start = mem::replace(&mut self.last_spacing, end);
                (span.start, (start, kind, end), span.end)
            }
            Some((TokenTree::Delimited(span, delim, tts), s)) => {
                let start = mem::replace(&mut self.last_spacing, s);
                self.stack.push(Frame {
                    cursor: mem::replace(&mut self.current, tts.into_trees()),
                    delim,
                    close_span: span.close,
                    close_spacing: s,
                });
                (
                    span.open.start,
                    (start, TokenKind::OpenDelim(delim), Spacing::Alone),
                    span.open.end,
                )
            }
            None => match self.stack.pop() {
                Some(frame) => {
                    let close = frame.close_span;
                    let end = frame.close_spacing;
                    let start = mem::replace(&mut self.last_spacing, end);
                    self.current = frame.cursor;
                    (
                        close.start,
                        (start, TokenKind::CloseDelim(frame.delim), end),
                        close.end,
                    )
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
    type Item = (Pos, SpacedToken, Pos);

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

pub fn parse(diag: &DiagCx, input: TokenStream) -> Result<Leaf, ParseError> {
    let tf = RefCell::new(TokenFrames::new(input));
    let mut cx = ParseCx {
        inner_attrs: Vec::new(),
        token_frames: &tf,
    };
    let stmts: Vec<P<Stmt>> = ast::StmtsParser::new().parse(&diag, &mut cx, LalrpopIter(&tf))?;
    Ok(Leaf {
        id: DUMMY_ID,
        attrs: cx.inner_attrs,
        stmts,
        span: Span::new(Pos(0), Pos(1)),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t() {
        let diag = DiagCx::new();
        let tts = dendro_lexer::parse("abcde", &diag);

        let ts = parse(&diag, tts).unwrap();

        println!("{:#?}", ts);
    }
}
