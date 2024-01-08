lalrpop_mod!(
    #[allow(unused_imports)]
    #[allow(clippy::ptr_arg)]
    #[allow(clippy::unit_arg)]
    ast,
    "/ast.rs"
);

mod macros;

use std::mem;

use dendro_ast::{
    ast::{Attribute, Leaf},
    token::{Delimiter, Token, TokenKind},
    token_stream::{CursorRef, Spacing, TokenStream, TokenTree},
};
use dendro_error::{DiagCx, DiagnosticBuilder, PResult};
use dendro_span::{
    fatal_error,
    span::{DelimSpan, Pos, Span},
};
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
            Some(TokenTree::Token(tk, _)) if tk.kind == kind => {}
            _ => fatal_error!("unwrap_token: expected token"),
        }
    }

    fn expect_delimited(
        tt: Option<&'a TokenTree>,
        delim: Delimiter,
    ) -> (DelimSpan, &'a TokenStream) {
        match tt {
            Some(&TokenTree::Delimited(dspan, _, d, ref tts)) if d == delim => (dspan, tts),
            _ => fatal_error!("unwrap_delimited: expected delimited token"),
        }
    }

    fn next_token(&mut self) -> Option<(Pos, SpacedToken<'diag>, Pos)> {
        Some(match self.current.next() {
            Some(&TokenTree::Token(Token { kind, span }, end)) => {
                if let Some(ret) = self.parse_attr(kind, span) {
                    self.last_spacing = Spacing::Alone;
                    return Some(ret);
                }
                let start = mem::replace(&mut self.last_spacing, end);
                (span.start, (start, T(kind), end), span.end)
            }
            Some(&TokenTree::Delimited(span, spacing, delim, ref tts)) => {
                let start = mem::replace(&mut self.last_spacing, spacing.open);
                self.stack.push(Frame {
                    cursor: mem::replace(&mut self.current, tts.trees()),
                    delim,
                    close_span: span.close,
                    close_spacing: spacing.close,
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

    fn extend_attr(&mut self, iter: impl IntoIterator<Item = Attribute>) {
        self.inner_attrs.extend(iter);
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

pub fn parse<'diag>(diag: &'diag DiagCx, input: &TokenStream) -> Result<Leaf, ParseError<'diag>> {
    let tf = TokenFrames::new(diag, input.trees());
    let mut cx = ParseCx::default();
    ast::LeafParser::new().parse(diag, &mut cx, tf)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ident() {
        let diag = DiagCx::new();
        let tts = dendro_lexer::parse("abcde", &diag);

        let ts = parse(&diag, &tts).unwrap();

        println!("{:?}", ts);
    }

    #[test]
    fn let_expr() {
        let diag = DiagCx::new();
        let tts = dendro_lexer::parse(
            "
            forall r where r: u32 =>
            #[allow(unused)]
            let a r = r;",
            &diag,
        );

        let ts = parse(&diag, &tts).unwrap();

        println!("{:?}", ts);
    }

    #[test]
    fn let_function() {
        let diag = DiagCx::new();
        let tts = dendro_lexer::parse(
            "
            forall t, a, b, c where a: t, b: t, c: t =>
            let delta a b c = (b.pow 2) - ((*) 4 a) * c;",
            &diag,
        );

        let ts = parse(&diag, &tts).unwrap();

        println!("{:?}", ts);
    }

    #[test]
    fn chained_call() {
        let diag = DiagCx::new();
        let unparen = parse(
            &diag,
            &dendro_lexer::parse(
                "
            iter.(map (\\x -> x + 1))
                .(take 3)
                .(collect ?(Vec i32))",
                &diag,
            ),
        )
        .unwrap();

        println!("{unparen:?}");
    }

    #[test]
    fn use_tree() {
        let diag = DiagCx::new();
        let tts = dendro_lexer::parse(
            "
            #[alias]
            let { * } = `{
                ..std.cmp.`{ PartialEq, Eq },
                ..std.hash.Hash,
            };",
            &diag,
        );

        let ts = parse(&diag, &tts).unwrap();

        println!("{:?}", ts);
    }
}
