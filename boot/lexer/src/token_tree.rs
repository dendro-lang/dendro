use dendro_ast::{
    token::{self, Token},
    token_stream::{DelimSpacing, Spacing, TokenStream, TokenTree},
};
use dendro_error::{DiagCx, PResult};
use dendro_span::span::DelimSpan;
use pest::iterators::Pair;

use super::token::Tokens;
use crate::imp::Rule;

pub(crate) struct TokenTrees<'i, I>
where
    I: Iterator<Item = Pair<'i, Rule>> + 'i,
{
    tokens: Tokens<'i, I>,
    current: Option<Token>,
    diag_cx: &'i DiagCx,
}

impl<'i, I> TokenTrees<'i, I>
where
    I: Iterator<Item = Pair<'i, Rule>> + 'i,
{
    pub fn new(tokens: Tokens<'i, I>, cx: &'i DiagCx) -> Self {
        TokenTrees {
            tokens,
            current: None,
            diag_cx: cx,
        }
    }

    fn next_token_tree(&mut self) -> PResult<'i, TokenTree> {
        match self.current {
            Some(Token {
                kind: token::OpenDelim(delim),
                span: open,
            }) => {
                let open_spacing = self.bump();

                let inner = self.token_trees_until_close_delim()?;
                let Some(current) = self.current else {
                    let mut err = self.diag_cx.error(None, true);
                    err.push(
                        open,
                        format_args!("unclosed delimiter {delim:?}; found unexpected EOF"),
                    );
                    return Err(err);
                };
                let close = current.span;
                let close_spacing = match current.kind {
                    token::TokenKind::CloseDelim(d) if d == delim => self.bump(),
                    token::TokenKind::CloseDelim(d) => {
                        self.bump();
                        let mut err = self.diag_cx.error(None, true);
                        err.push(open, format_args!("unclosed delimiter {delim:?}"));
                        err.push(close, format_args!("found {d:?}"));
                        return Err(err);
                    }
                    _ => unreachable!(),
                };

                Ok(TokenTree::Delimited(
                    DelimSpan::from_pair(open, close),
                    DelimSpacing::new(open_spacing, close_spacing),
                    delim,
                    inner,
                ))
            }
            Some(Token { kind: token::CloseDelim(_), .. }) => unreachable!(),
            Some(token) => {
                let spacing = self.bump();
                Ok(TokenTree::Token(token, spacing))
            }
            None => unreachable!(),
        }
    }

    fn bump(&mut self) -> Spacing {
        let (spacing, next) = self.tokens.next_token();
        self.current = next;
        spacing
    }

    fn token_trees_until_close_delim(&mut self) -> PResult<'i, TokenStream> {
        let mut vec = TokenStreamBuilder::new();
        loop {
            let is_eof_or_close = matches!(
                self.current,
                None | Some(Token { kind: token::CloseDelim(_), .. })
            );
            if is_eof_or_close {
                break Ok(vec.build());
            }
            vec.push(self.next_token_tree()?);
        }
    }

    pub fn parse(mut self) -> TokenStream {
        let mut vec = TokenStreamBuilder::new();
        self.bump();
        while self.current.is_some() {
            match self.next_token_tree() {
                Ok(tt) => vec.push(tt),
                Err(mut e) => {
                    e.emit();
                    return vec.build();
                }
            }
        }
        vec.build()
    }
}

struct TokenStreamBuilder {
    vec: Vec<TokenTree>,
}

impl TokenStreamBuilder {
    fn new() -> Self {
        TokenStreamBuilder { vec: Vec::new() }
    }

    fn push(&mut self, tree: TokenTree) {
        if let Some(TokenTree::Token(prev_token, Spacing::Joint)) = self.vec.last()
            && let TokenTree::Token(ref token, joint) = tree
            && let Some(glued) = prev_token.glue(token)
        {
            self.vec.pop();
            self.vec.push(TokenTree::Token(glued, joint));
            return;
        }
        self.vec.push(tree)
    }

    fn build(self) -> TokenStream {
        TokenStream::new(self.vec)
    }
}
