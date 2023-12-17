use dendro_ast::{
    token::{self, Token},
    token_stream::{Spacing, TokenStream, TokenTree},
};
use dendro_error::{Diagnostic, Error};
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
}

impl<'i, I> TokenTrees<'i, I>
where
    I: Iterator<Item = Pair<'i, Rule>> + 'i,
{
    pub fn new(tokens: Tokens<'i, I>) -> Self {
        TokenTrees {
            tokens,
            current: None,
        }
    }

    fn next_token_tree(&mut self) -> Result<(TokenTree, Spacing), Error> {
        match self.current {
            Some(token) => match token.kind {
                token::OpenDelim(delim) => {
                    let open = token.span;
                    self.bump();

                    let inner = self.token_trees_until_close_delim()?;
                    let Some(current) = self.current else {
                        return Err(Diagnostic::error(None, true)
                            .push_fmt(
                                open,
                                format_args!("unclosed delimiter {delim:?}; found unexpected EOF"),
                            )
                            .into_err());
                    };
                    let close = current.span;
                    match current.kind {
                        token::TokenKind::CloseDelim(d) if d == delim => {
                            self.bump();
                        }
                        token::TokenKind::CloseDelim(d) => {
                            self.bump();
                            return Err(Diagnostic::error(None, true)
                                .push_fmt(open, format_args!("unclosed delimiter {delim:?}"))
                                .push_fmt(close, format_args!("found {d:?}"))
                                .into_err());
                        }
                        _ => unreachable!(),
                    }

                    let delimited =
                        TokenTree::Delimited(DelimSpan::from_pair(open, close), delim, inner);
                    Ok(delimited.into())
                }
                token::CloseDelim(_) => unreachable!(),
                _ => {
                    let spacing = self.bump();
                    Ok((TokenTree::Token(token), spacing))
                }
            },
            None => unreachable!(),
        }
    }

    fn bump(&mut self) -> Spacing {
        let (spacing, next) = self.tokens.next_token();
        self.current = next;
        spacing
    }

    fn token_trees_until_close_delim(&mut self) -> Result<TokenStream, Error> {
        let mut vec = TokenStreamBuilder::new();
        loop {
            let is_eof_or_close = matches!(
                self.current,
                None | Some(Token {
                    kind: token::CloseDelim(_),
                    ..
                })
            );
            if is_eof_or_close {
                break Ok(vec.build());
            }
            vec.push(self.next_token_tree()?);
        }
    }

    pub fn parse(mut self) -> Result<TokenStream, Error> {
        let mut vec = TokenStreamBuilder::new();
        self.bump();
        while self.current.is_some() {
            vec.push(self.next_token_tree()?);
        }
        Ok(vec.build())
    }
}

struct TokenStreamBuilder {
    vec: Vec<(TokenTree, Spacing)>,
}

impl TokenStreamBuilder {
    fn new() -> Self {
        TokenStreamBuilder { vec: Vec::new() }
    }

    fn push(&mut self, (tree, joint): (TokenTree, Spacing)) {
        if let Some((TokenTree::Token(prev_token), Spacing::Joint)) = self.vec.last()
            && let TokenTree::Token(token) = &tree
            && let Some(glued) = prev_token.glue(token)
        {
            self.vec.pop();
            self.vec.push((TokenTree::Token(glued), joint));
            return;
        }
        self.vec.push((tree, joint))
    }

    fn build(self) -> TokenStream {
        TokenStream::new(self.vec)
    }
}
