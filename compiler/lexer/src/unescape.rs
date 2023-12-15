use std::str::Chars;

use pest::Span;

use crate::Rule;

/// Errors and warnings that can occur during string unescaping.
#[derive(Debug, PartialEq, Eq)]
pub enum EscapeError {
    /// Expected 1 char, but 0 were found.
    ZeroChars,
    /// Expected 1 char, but more than 1 were found.
    MoreThanOneChar,

    /// Escaped '\' character without continuation.
    LoneSlash,
    /// Invalid escape character (e.g. '\z').
    InvalidEscape,
    /// Raw '\r' encountered.
    BareCarriageReturn,
    /// Raw '\r' encountered in raw string.
    BareCarriageReturnInRawString,
    /// Unescaped character that was expected to be escaped (e.g. raw '\t').
    EscapeOnlyChar,

    /// Numeric character escape is too short (e.g. '\x1').
    TooShortHexEscape,
    /// Invalid character in numeric escape (e.g. '\xz')
    InvalidCharInHexEscape,
    /// Character code in numeric escape is non-ascii (e.g. '\xFF').
    OutOfRangeHexEscape,

    /// '\u' not followed by '{'.
    NoBraceInUnicodeEscape,
    /// Non-hexadecimal value in '\u{..}'.
    InvalidCharInUnicodeEscape,
    /// '\u{}'
    EmptyUnicodeEscape,
    /// No closing brace in '\u{..}', e.g. '\u{12'.
    UnclosedUnicodeEscape,
    /// '\u{_12}'
    LeadingUnderscoreUnicodeEscape,
    /// More than 6 characters in '\u{..}', e.g. '\u{10FFFF_FF}'
    OverlongUnicodeEscape,
    /// Invalid in-bound unicode character code, e.g. '\u{DFFF}'.
    LoneSurrogateUnicodeEscape,
    /// Out of bounds unicode character code, e.g. '\u{FFFFFF}'.
    OutOfRangeUnicodeEscape,

    /// Unicode escape code in byte literal.
    UnicodeEscapeInByte,
    /// Non-ascii character in byte literal.
    NonAsciiCharInByte,
    /// Non-ascii character in byte string literal.
    NonAsciiCharInByteString,

    /// After a line ending with '\', the next line contains whitespace
    /// characters that are not skipped.
    UnskippedWhitespaceWarning,

    /// After a line ending with '\', multiple lines are skipped.
    MultipleSkippedLinesWarning,
}

impl EscapeError {
    /// Returns true for actual errors, as opposed to warnings.
    pub fn is_fatal(&self) -> bool {
        !matches!(
            self,
            EscapeError::UnskippedWhitespaceWarning | EscapeError::MultipleSkippedLinesWarning
        )
    }
}

fn is_ascii(x: u32) -> bool {
    x <= 0x7F
}

#[inline]
fn ascii_check(first_char: char, rule: Rule) -> Result<char, EscapeError> {
    if rule.is_bytes() && !first_char.is_ascii() {
        // Byte literal can't be a non-ascii character.
        Err(EscapeError::NonAsciiCharInByte)
    } else {
        Ok(first_char)
    }
}

fn scan_escape<T: From<u8> + From<char>>(
    chars: &mut Chars<'_>,
    rule: Rule,
) -> Result<T, EscapeError> {
    // Previous character was '\\', unescape what follows.

    let res = match chars.next().ok_or(EscapeError::LoneSlash)? {
        '"' => b'"',
        'n' => b'\n',
        'r' => b'\r',
        't' => b'\t',
        '\\' => b'\\',
        '\'' => b'\'',
        '0' => b'\0',

        'x' => {
            // Parse hexadecimal character code.

            let hi = chars.next().ok_or(EscapeError::TooShortHexEscape)?;
            let hi = hi.to_digit(16).ok_or(EscapeError::InvalidCharInHexEscape)?;

            let lo = chars.next().ok_or(EscapeError::TooShortHexEscape)?;
            let lo = lo.to_digit(16).ok_or(EscapeError::InvalidCharInHexEscape)?;

            let value = hi * 16 + lo;

            // For a byte literal verify that it is within ASCII range.
            if !rule.is_bytes() && !is_ascii(value) {
                return Err(EscapeError::OutOfRangeHexEscape);
            }
            value as u8
        }

        'u' => return scan_unicode(chars, rule).map(Into::into),
        _ => return Err(EscapeError::InvalidEscape),
    };
    Ok(res.into())
}

fn scan_unicode(chars: &mut Chars<'_>, rule: Rule) -> Result<char, EscapeError> {
    if chars.next() != Some('{') {
        return Err(EscapeError::NoBraceInUnicodeEscape);
    }
    let mut n_digits = 1;
    let mut value: u32 = match chars.next().ok_or(EscapeError::UnclosedUnicodeEscape)? {
        '_' => return Err(EscapeError::LeadingUnderscoreUnicodeEscape),
        '}' => return Err(EscapeError::EmptyUnicodeEscape),
        c => c
            .to_digit(16)
            .ok_or(EscapeError::InvalidCharInUnicodeEscape)?,
    };
    Ok(loop {
        match chars.next() {
            None => return Err(EscapeError::UnclosedUnicodeEscape),
            Some('_') => continue,
            Some('}') => {
                if n_digits > 6 {
                    return Err(EscapeError::OverlongUnicodeEscape);
                }

                // Incorrect syntax has higher priority for error reporting
                // than unallowed value for a literal.
                if rule.is_bytes() {
                    return Err(EscapeError::UnicodeEscapeInByte);
                }

                break std::char::from_u32(value).ok_or({
                    if value > 0x10FFFF {
                        EscapeError::OutOfRangeUnicodeEscape
                    } else {
                        EscapeError::LoneSurrogateUnicodeEscape
                    }
                })?;
            }
            Some(c) => {
                let digit = c
                    .to_digit(16)
                    .ok_or(EscapeError::InvalidCharInUnicodeEscape)?;
                n_digits += 1;
                if n_digits > 6 {
                    // Stop updating value since we're sure that it's is incorrect already.
                    continue;
                }
                value = value * 16 + digit;
            }
        };
    })
}

fn unescape_char_inner(chars: &mut Chars<'_>, rule: Rule) -> Result<char, EscapeError> {
    assert!(rule.in_single_quotes());

    let first_char = chars.next().ok_or(EscapeError::ZeroChars)?;
    let res = match first_char {
        '\\' => scan_escape(chars, rule),
        '\n' | '\t' | '\'' => Err(EscapeError::EscapeOnlyChar),
        '\r' => Err(EscapeError::BareCarriageReturn),
        _ => ascii_check(first_char, rule),
    }?;
    if chars.next().is_some() {
        return Err(EscapeError::MoreThanOneChar);
    }
    Ok(res)
}

fn unescape_string_inner<'a, F, T: From<u8> + From<char>>(
    span: Span<'a>,
    rule: Rule,
    mut callback: F,
) where
    F: FnMut(Result<T, (EscapeError, Span<'a>)>),
{
    assert!(rule.in_double_quotes());

    let src = span.as_str();
    let initial_len = src.len();
    let mut chars = src.chars();
    while let Some(first_char) = chars.next() {
        let start = initial_len - chars.as_str().len() - first_char.len_utf8();

        let res = match first_char {
            '\\' => {
                match chars.clone().next() {
                    Some('\n') => {
                        // Rust language specification requires us to skip whitespaces
                        // if unescaped '\' character is followed by '\n'.
                        // For details see [Rust language reference]
                        // (https://doc.rust-lang.org/reference/tokens.html#string-literals).
                        skip_ascii_whitespace(&mut chars, start, span, |err, span| {
                            callback(Err((err, span)))
                        });
                        continue;
                    }
                    _ => scan_escape::<T>(&mut chars, rule),
                }
            }
            '"' => Err(EscapeError::EscapeOnlyChar),
            '\r' => Err(EscapeError::BareCarriageReturn),
            _ => ascii_check(first_char, rule).map(Into::into),
        };
        let end = initial_len - chars.as_str().len();
        callback(res.map_err(|err| (err, span.get(start..end).unwrap())));
    }

    fn skip_ascii_whitespace<'a, F>(
        chars: &mut Chars<'_>,
        start: usize,
        span: Span<'a>,
        mut callback: F,
    ) where
        F: FnMut(EscapeError, Span<'a>),
    {
        let tail = chars.as_str();
        let first_non_space = tail
            .bytes()
            .position(|b| b != b' ' && b != b'\t' && b != b'\n' && b != b'\r')
            .unwrap_or(tail.len());
        if tail[1..first_non_space].contains('\n') {
            // The +1 accounts for the escaping slash.
            let end = start + first_non_space + 1;
            callback(
                EscapeError::MultipleSkippedLinesWarning,
                span.get(start..end).unwrap(),
            );
        }
        let tail = &tail[first_non_space..];
        if let Some(c) = tail.chars().next() {
            // For error reporting, we would like the span to contain the character that was
            // not skipped.  The +1 is necessary to account for the leading \
            // that started the escape.
            let end = start + first_non_space + c.len_utf8() + 1;
            if c.is_whitespace() {
                callback(
                    EscapeError::UnskippedWhitespaceWarning,
                    span.get(start..end).unwrap(),
                );
            }
        }
        *chars = tail.chars();
    }
}

pub fn byte_from_char(c: char) -> u8 {
    let res = c as u32;
    assert!(res <= u8::MAX as u32, "guaranteed because of Mode::ByteStr");
    res as u8
}

pub fn unescape_char(span: Span<'_>) -> Result<char, (EscapeError, Span<'_>)> {
    let s = span.as_str();
    let mut chars = s.chars();
    let result = unescape_char_inner(&mut chars, Rule::Char);
    result.map_err(|err| {
        let span = span.get(..(s.len() - chars.as_str().len())).unwrap();
        (err, span)
    })
}

pub fn unescape_byte(span: Span<'_>) -> Result<u8, (EscapeError, Span<'_>)> {
    let s = span.as_str();
    let mut chars = s.chars();
    let result = unescape_char_inner(&mut chars, Rule::Byte);
    result.map(byte_from_char).map_err(|err| {
        let span = span.get(..(s.len() - chars.as_str().len())).unwrap();
        (err, span)
    })
}

pub fn unescape_string<'a, F>(span: Span<'a>, callback: F)
where
    F: FnMut(Result<char, (EscapeError, Span<'a>)>),
{
    unescape_string_inner(span, Rule::String, callback)
}

pub fn unescape_byte_string<'a, F>(span: Span<'a>, callback: F)
where
    F: FnMut(Result<char, (EscapeError, Span<'a>)>),
{
    unescape_string_inner(span, Rule::ByteString, callback)
}

/// A unit within CStr. Must not be a nul character.
pub enum CStrUnit {
    Byte(u8),
    Char(char),
}

impl From<u8> for CStrUnit {
    fn from(value: u8) -> Self {
        CStrUnit::Byte(value)
    }
}

impl From<char> for CStrUnit {
    fn from(value: char) -> Self {
        CStrUnit::Char(value)
    }
}

pub fn unescape_c_string<'a, F>(span: Span<'a>, callback: F)
where
    F: FnMut(Result<CStrUnit, (EscapeError, Span<'a>)>),
{
    unescape_string_inner(span, Rule::CString, callback)
}
