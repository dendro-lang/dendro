use dendro_ast::token::{AttrStyle, CommentKind, Delimiter, Lit, LitKind, Token, TokenKind::*};
use dendro_lexer::Rule;
use dendro_span::symbol::Symbol;
use pest::{error::Error, iterators::Pair, Span};

pub struct Lexer<'input, I>
where
    I: Iterator<Item = Pair<'input, Rule>> + 'input,
{
    iter: I,
}

impl<'input, I> Lexer<'input, I>
where
    I: Iterator<Item = Pair<'input, Rule>> + 'input,
{
    pub fn parse(
        input: &'input str,
    ) -> Result<Lexer<'input, impl Iterator<Item = Pair<'input, Rule>> + 'input>, Box<Error<Rule>>>
    {
        dendro_lexer::parse(input).map(|iter| Lexer { iter })
    }
}

impl<'input, I> Iterator for Lexer<'input, I>
where
    I: Iterator<Item = Pair<'input, Rule>> + 'input,
{
    type Item = Token<'input>;

    fn next(&mut self) -> Option<Token<'input>> {
        let pair = self.iter.next()?;
        lex_pair(&mut self.iter, pair)
    }
}

fn lex_pair<'a, I: Iterator<Item = Pair<'a, Rule>>>(
    _: I,
    pair: Pair<'a, Rule>,
) -> Option<Token<'a>> {
    Some(match pair.as_rule() {
        Rule::LineComment => lex_comment(CommentKind::Line, pair)?,
        Rule::BlockComment => lex_comment(CommentKind::Block, pair)?,
        Rule::Ident => Token {
            kind: Ident(Symbol::new(pair.as_str()), false),
            span: pair.as_span(),
        },
        Rule::RawIdent => {
            let inner = pair.into_inner().next().unwrap();
            debug_assert_eq!(inner.as_rule(), Rule::Ident);
            Token {
                kind: Ident(Symbol::new(inner.as_str()), true),
                span: inner.as_span(),
            }
        }
        Rule::Prefix => todo!(),
        Rule::Lifetime => todo!(),
        Rule::Literal => {
            let pair = pair.into_inner().next().unwrap();
            let span = pair.as_span();

            let suffix = pair
                .clone()
                .into_inner()
                .find(|i| matches!(i.as_rule(), Rule::LiteralSuffix | Rule::LiteralSuffixNoE));
            let suffix_start = suffix.as_ref().map_or(span.end(), |s| s.as_span().start());

            let suffix = suffix.map(|s| Symbol::new(s.as_str()));

            let (kind, symbol) = lex_literal(pair, suffix_start);
            Token {
                kind: Literal(Lit {
                    kind,
                    symbol,
                    suffix,
                }),
                span,
            }
        }
        Rule::Semi => todo!(),
        Rule::Comma => todo!(),
        Rule::Dot => todo!(),
        Rule::OpenParen => Token {
            kind: OpenDelim(Delimiter::Parenthesis),
            span: pair.as_span(),
        },
        Rule::CloseParen => Token {
            kind: CloseDelim(Delimiter::Parenthesis),
            span: pair.as_span(),
        },
        Rule::OpenBrace => Token {
            kind: OpenDelim(Delimiter::Brace),
            span: pair.as_span(),
        },
        Rule::CloseBrace => Token {
            kind: CloseDelim(Delimiter::Brace),
            span: pair.as_span(),
        },
        Rule::OpenBracket => Token {
            kind: OpenDelim(Delimiter::Bracket),
            span: pair.as_span(),
        },
        Rule::CloseBracket => Token {
            kind: CloseDelim(Delimiter::Bracket),
            span: pair.as_span(),
        },
        Rule::At => todo!(),
        Rule::Pound => todo!(),
        Rule::Tilde => todo!(),
        Rule::Question => todo!(),
        Rule::Colon => todo!(),
        Rule::Dollar => todo!(),
        Rule::Eq => todo!(),
        Rule::Bang => todo!(),
        Rule::Lt => todo!(),
        Rule::Gt => todo!(),
        Rule::Minus => todo!(),
        Rule::And => todo!(),
        Rule::Or => todo!(),
        Rule::Plus => todo!(),
        Rule::Star => todo!(),
        Rule::Slash => todo!(),
        Rule::BackSlash => todo!(),
        Rule::Percent => todo!(),
        Rule::Caret => todo!(),
        _ => unreachable!(),
    })
}

fn lex_comment(kind: CommentKind, pair: Pair<'_, Rule>) -> Option<Token<'_>> {
    let span = pair.as_span();
    if let Some(inner) = pair.into_inner().next() {
        debug_assert!(matches!(
            inner.as_rule(),
            Rule::LineDocStyle | Rule::BlockDocStyle
        ));
        let style = match inner.as_str() {
            "/" | "*" => AttrStyle::Outer,
            "!" => AttrStyle::Inner,
            _ => unreachable!(),
        };
        let start = inner.as_span().end_pos();
        let span = start.span(&span.end_pos());
        return Some(Token {
            kind: DocComment(kind, style, Symbol::new(span.as_str())),
            span,
        });
    }
    None
}

fn lex_literal(pair: Pair<'_, Rule>, suffix_start: usize) -> (LitKind, Symbol) {
    let span = pair.as_span();
    let start = span.start();

    let (kind, start_offset, end_offset) = match pair.as_rule() {
        Rule::Integer => (LitKind::Integer, 0, 0),
        Rule::Float => (LitKind::Float, 0, 0),
        Rule::Char => (LitKind::Char, 1, 1),
        Rule::String => (LitKind::Str, 1, 1),
        Rule::Byte => (LitKind::Byte, 2, 1),
        Rule::ByteString => (LitKind::ByteStr, 2, 1),
        Rule::CString => (LitKind::CStr, 2, 1),
        _ => unreachable!(),
    };
    let span = Span::new(
        span.get_input(),
        start + start_offset,
        suffix_start - end_offset,
    );
    let symbol = Symbol::new(span.unwrap().as_str());
    (kind, symbol)
}
