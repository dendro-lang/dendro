lalrpop_mod!(
    #[allow(unused_imports)]
    #[allow(clippy::ptr_arg)]
    #[allow(clippy::unit_arg)]
    ast,
    "/src/ast.rs"
);

mod expr;
mod ident;
mod macros;
mod pat;

use std::mem;

use dendro_ast::{
    ast::{Attribute, Leaf, Stmt, Visibility, VisibilityKind, DUMMY_ID, P},
    token::{Delimiter, Token, TokenKind},
    token_stream::{CursorRef, Spacing, TokenStream, TokenTree},
};
use dendro_error::{DiagCx, DiagnosticBuilder, PResult};
use dendro_span::span::{DelimSpan, Pos, Span};
use lalrpop_util::lalrpop_mod;

#[derive(Debug, Clone)]
pub enum Tk<'diag> {
    T(TokenKind),
    InnerAttr(PResult<'diag, Attribute>),
    OuterAttr(PResult<'diag, Attribute>),
    CusErr(DiagnosticBuilder<'diag>),
}
use Tk::*;

type SpacedToken<'diag> = (Spacing, Tk<'diag>, Spacing);

type ParseError<'diag> =
    lalrpop_util::ParseError<Pos, SpacedToken<'diag>, DiagnosticBuilder<'diag>>;

#[derive(Debug)]
struct Frame<'a> {
    cursor: CursorRef<'a>,
    delim: Delimiter,
    close_span: Span,
    close_spacing: Spacing,
}

#[derive(Debug)]
struct TokenFrames<'a, 'diag> {
    diag: &'diag DiagCx,
    last_spacing: Spacing,
    current: CursorRef<'a>,
    stack: Vec<Frame<'a>>,
}

impl<'a, 'diag> TokenFrames<'a, 'diag> {
    fn new(diag: &'diag DiagCx, tokens: CursorRef<'a>) -> Self {
        TokenFrames {
            diag,
            last_spacing: Spacing::Alone,
            current: tokens,
            stack: Vec::new(),
        }
    }

    fn expect_token(tt: Option<&'a TokenTree>, kind: TokenKind) {
        match tt {
            Some(TokenTree::Token(tk)) if tk.kind == kind => {}
            _ => panic!("unwrap_token: expected token"),
        }
    }

    fn expect_delimited(
        tt: Option<&'a TokenTree>,
        delim: Delimiter,
    ) -> (DelimSpan, &'a TokenStream) {
        match tt {
            Some(&TokenTree::Delimited(dspan, d, ref tts)) if d == delim => (dspan, tts),
            _ => panic!("unwrap_delimited: expected delimited token"),
        }
    }

    fn next_token(&mut self) -> Option<(Pos, SpacedToken<'diag>, Pos)> {
        Some(match self.current.next_with_spacing() {
            Some(&(TokenTree::Token(Token { kind, span }), end)) => {
                if let Some(ret) = self.parse_attr(kind, span) {
                    self.last_spacing = Spacing::Alone;
                    return Some(ret);
                }
                let start = mem::replace(&mut self.last_spacing, end);
                (span.start, (start, T(kind), end), span.end)
            }
            Some(&(TokenTree::Delimited(span, delim, ref tts), s)) => {
                let start = mem::replace(&mut self.last_spacing, s);
                self.stack.push(Frame {
                    cursor: mem::replace(&mut self.current, tts.trees()),
                    delim,
                    close_span: span.close,
                    close_spacing: s,
                });
                (
                    span.open.start,
                    (start, T(TokenKind::OpenDelim(delim)), Spacing::Alone),
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
                        (start, T(TokenKind::CloseDelim(frame.delim)), end),
                        close.end,
                    )
                }
                None => return None,
            },
        })
    }
}

impl<'a, 'diag> IntoIterator for TokenFrames<'a, 'diag> {
    type IntoIter = Iter<'a, 'diag>;
    type Item = (Pos, SpacedToken<'diag>, Pos);

    fn into_iter(self) -> Self::IntoIter {
        Iter(self)
    }
}

#[derive(Debug, Default)]
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
struct Iter<'a, 'diag>(TokenFrames<'a, 'diag>);

impl<'a, 'diag> Iterator for Iter<'a, 'diag> {
    type Item = (Pos, SpacedToken<'diag>, Pos);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next_token()
    }
}

fn parse_vis(input: (Pos, VisibilityKind, Pos)) -> Visibility {
    Visibility {
        kind: input.1,
        span: Span::new(input.0, input.2),
    }
}

pub fn parse<'diag>(diag: &'diag DiagCx, input: &TokenStream) -> Result<Leaf, ParseError<'diag>> {
    let tf = TokenFrames::new(diag, input.trees());
    let mut cx = ParseCx::default();
    let stmts: Vec<P<Stmt>> = ast::StmtsParser::new().parse(diag, &mut cx, tf)?;
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
    fn ident() {
        let diag = DiagCx::new();
        let tts = dendro_lexer::parse("abcde", &diag);

        let ts = parse(&diag, &tts).unwrap();

        println!("{:#?}", ts);
    }

    #[test]
    fn let_expr() {
        let diag = DiagCx::new();
        let tts = dendro_lexer::parse(
            "
            forall r where r: u32 ::
            #[allow(unused)]
            pub unsafe let a r := r;",
            &diag,
        );

        let ts = parse(&diag, &tts).unwrap();

        println!("{:#?}", ts);
    }

    #[test]
    fn let_function() {
        let diag = DiagCx::new();
        let tts = dendro_lexer::parse(
            "
            forall t, a, b, c where a: t, b: t, c: t ::
            pub let delta a b c := (b.pow 2) - ((*) 4 a) * c;",
            &diag,
        );

        let ts = parse(&diag, &tts).unwrap();

        println!("{:#?}", ts);
    }
}
