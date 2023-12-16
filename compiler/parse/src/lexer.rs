use dendro_ast::token::{self, AttrStyle, CommentKind, Lit, LitKind, Token};
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
    let span = pair.as_span();

    let kind = match pair.as_rule() {
        Rule::LineComment => return lex_comment(token::Line, pair),
        Rule::BlockComment => return lex_comment(token::Block, pair),
        Rule::Ident => token::Ident(Symbol::new(pair.as_str()), false),
        Rule::RawIdent => {
            let inner = pair.into_inner().next().unwrap();
            debug_assert_eq!(inner.as_rule(), Rule::Ident);
            return Some(Token {
                kind: token::Ident(Symbol::new(inner.as_str()), true),
                span: inner.as_span(),
            });
        }
        Rule::Prefix => todo!(),
        Rule::Lifetime => {
            let inner = pair.into_inner().next().unwrap();
            debug_assert_eq!(inner.as_rule(), Rule::Ident);
            return Some(Token {
                kind: token::Lifetime(Symbol::new(inner.as_str())),
                span: inner.as_span(),
            });
        }
        Rule::Literal => {
            let inner = pair.into_inner().next().unwrap();
            let span = inner.as_span();

            let suffix = inner
                .clone()
                .into_inner()
                .find(|i| matches!(i.as_rule(), Rule::LiteralSuffix | Rule::LiteralSuffixNoE));
            let suffix_start = suffix.as_ref().map_or(span.end(), |s| s.as_span().start());

            let suffix = suffix.map(|s| Symbol::new(s.as_str()));

            let (kind, symbol) = lex_literal(inner, suffix_start);
            return Some(Token {
                kind: token::Literal(Lit {
                    kind,
                    symbol,
                    suffix,
                }),
                span,
            });
        }
        Rule::Semi => token::Semi,
        Rule::Comma => token::Comma,
        Rule::Dot => token::Dot,
        Rule::OpenParen => token::OpenDelim(token::Parenthesis),
        Rule::CloseParen => token::CloseDelim(token::Parenthesis),
        Rule::OpenBrace => token::OpenDelim(token::Brace),
        Rule::CloseBrace => token::CloseDelim(token::Brace),
        Rule::OpenBracket => token::OpenDelim(token::Bracket),
        Rule::CloseBracket => token::CloseDelim(token::Bracket),
        Rule::At => token::At,
        Rule::Pound => token::Pound,
        Rule::Tilde => token::Tilde,
        Rule::Question => token::Question,
        Rule::Colon => token::Colon,
        Rule::Dollar => token::Dollar,
        Rule::Eq => token::Eq,
        Rule::Bang => token::Not,
        Rule::Lt => token::Lt,
        Rule::Gt => token::Gt,
        Rule::Minus => token::BinOp(token::Minus),
        Rule::And => token::BinOp(token::And),
        Rule::Or => token::BinOp(token::Or),
        Rule::Plus => token::BinOp(token::Plus),
        Rule::Star => token::BinOp(token::Star),
        Rule::Slash => token::BinOp(token::Slash),
        Rule::BackSlash => token::BinOp(token::BackSlash),
        Rule::Percent => token::BinOp(token::Percent),
        Rule::Caret => token::BinOp(token::Caret),
        _ => unreachable!(),
    };

    Some(Token { kind, span })
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
            kind: token::DocComment(kind, style, Symbol::new(span.as_str())),
            span,
        });
    }
    None
}

fn lex_literal(pair: Pair<'_, Rule>, suffix_start: usize) -> (LitKind, Symbol) {
    let span = pair.as_span();
    let start = span.start();

    let (kind, start_offset, end_offset) = match pair.as_rule() {
        Rule::Integer => (token::Integer, 0, 0),
        Rule::Float => (token::Float, 0, 0),
        Rule::Char => (token::Char, 1, 1),
        Rule::String => (token::Str, 1, 1),
        Rule::Byte => (token::Byte, 2, 1),
        Rule::ByteString => (token::ByteStr, 2, 1),
        Rule::CString => (token::CStr, 2, 1),
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
