use dendro_error::{DiagCx, Error};
use dendro_span::{
    source::SourceFile,
    span::{RelPos, Span},
};
use pest::{error::InputLocation, iterators::Pair, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "lexer.pest"]
pub struct RawTokens;

impl Rule {
    pub fn is_bytes(&self) -> bool {
        matches!(self, Rule::Byte | Rule::ByteString)
    }

    pub fn in_double_quotes(&self) -> bool {
        matches!(self, Rule::String | Rule::ByteString | Rule::CString)
    }

    pub fn in_single_quotes(&self) -> bool {
        matches!(self, Rule::Char | Rule::Byte)
    }
}

pub fn parse<'a>(
    input: &'a SourceFile,
    diag: &'a DiagCx,
) -> Result<impl Iterator<Item = Pair<'a, Rule>> + 'a, Error> {
    match RawTokens::parse(Rule::Tokens, &input.src) {
        Ok(p) => Ok(p.flat_map(|p| p.into_inner().flat_map(|p| p.into_inner()))),
        Err(err) => Err({
            let start_pos = input.start_pos;
            let span = match err.location {
                InputLocation::Pos(p) => Span::pos(start_pos + RelPos(p)),
                InputLocation::Span((start, end)) => {
                    Span::new(start_pos + RelPos(start), start_pos + RelPos(end))
                }
            };
            diag.span_err(span, err.variant.message())
        }),
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use super::{RawTokens, Rule};

    enum Expect {
        Invalid,
        Same,
        Specific(&'static str),
    }

    use self::Expect::*;

    #[track_caller]
    fn check(rule: Rule, input: &str, expect: Expect) {
        let t = RawTokens::parse(rule, input);
        let mut t = if matches!(expect, Invalid) {
            t.unwrap_err();
            return;
        } else {
            t.unwrap()
        };

        let ret = t.next().map(|p| p.as_str());
        match expect {
            Invalid => unreachable!(),
            Same => assert_eq!(ret, Some(input)),
            Specific(s) => assert_eq!(ret, Some(s)),
        }
        assert!(t.next().is_none());
    }

    #[test]
    fn ident() {
        check(Rule::Ident, "_cjAFo_48ru", Same);
        check(Rule::Ident, "_", Same);
        check(Rule::Ident, "a8374", Same);

        check(Rule::Ident, "12345", Invalid);

        check(Rule::Ident, "df98u3*&^", Specific("df98u3"));
    }

    #[test]
    fn integer() {
        check(Rule::Integer, "0", Same);
        check(Rule::Integer, "0001_2345", Same);
        check(Rule::Integer, "0o1735_4u8", Same);
        check(Rule::Integer, "0b_100_10i32", Same);
        check(Rule::Integer, "0xad9_83fusize", Same);

        check(Rule::Dec, "0bffff", Specific("0"));
        check(Rule::Bin, "0bffff", Invalid);
        check(Rule::Integer, "0bffff", Same); // "bffff" goes to suffix.
    }

    #[test]
    fn float() {
        check(Rule::Float, "0.5f64", Same);
        check(Rule::Literal, "0.5f64", Same);
        check(Rule::Float, "0.", Same);
        check(Rule::Float, "457.32e-10f32", Same);
    }

    #[test]
    fn characters() {
        check(Rule::Char, r#"'c'"#, Same);
        check(Rule::Char, r#"'"'"#, Same);
        check(Rule::Char, r#"'\"'"#, Same);
        check(Rule::Char, r#"'\n'"#, Same);
        check(Rule::Char, r#"'\x4f'"#, Same);
        check(Rule::Char, r#"'\u{2d7f3}'"#, Same);
        check(Rule::Char, r#"'⛵'"#, Same);
        check(Rule::Char, r#"'啊'"#, Same);

        check(Rule::Char, "'\n'", Invalid);
        check(Rule::Char, "'\t'", Invalid);
    }

    #[test]
    fn all() {
        let s = r#"/// Wow!
let main = println /* on console */ "Hello, {} {}!" 0x2a 2.5f64;
// End of input.
"#;
        let mut p = RawTokens::parse(Rule::Tokens, s)
            .unwrap()
            .flat_map(|p| p.into_inner().flat_map(|p| p.into_inner()));
        assert_eq!(p.next().unwrap().as_str(), "/// Wow!");
        assert_eq!(p.next().unwrap().as_str(), "let");
        assert_eq!(p.next().unwrap().as_str(), "main");
        assert_eq!(p.next().unwrap().as_str(), "=");
        assert_eq!(p.next().unwrap().as_str(), "println");
        assert_eq!(p.next().unwrap().as_str(), "/* on console */");
        assert_eq!(p.next().unwrap().as_str(), "\"Hello, {} {}!\"");
        assert_eq!(p.next().unwrap().as_str(), "0x2a");
        assert_eq!(p.next().unwrap().as_str(), "2.5f64");
        assert_eq!(p.next().unwrap().as_str(), ";");
        assert_eq!(p.next().unwrap().as_str(), "// End of input.");
        assert_eq!(p.next(), None);

        for p in RawTokens::parse(Rule::Tokens, s).unwrap() {
            println!("{p:?}")
        }
    }
}
