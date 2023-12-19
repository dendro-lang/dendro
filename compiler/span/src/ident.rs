use std::{convert::Infallible, fmt, str::FromStr};

use crate::{
    span::{Span, DUMMY_SPAN},
    symbol::Symbol,
};

macro_rules! keywords {
    [$($name:ident: $value:literal),* $(,)?] => {
        pub(crate) const SYMBOL_PREFILL: &[&str] = &[
            $($value),*
        ];

        pub mod kw {
            use crate::symbol::Symbol;
            $(
                pub const $name: Symbol = Symbol::prefill(${index()});
            )*

            pub(crate) static MAP: phf::Map<&str, Symbol> = phf::phf_map! {
                $($value => $name,)*
            };
        }
    };
}

keywords![
    EMPTY: "",
    UNDERSCORE: "_",

    FORALL: "forall",
    EXISTS: "exists",
    WHERE: "where",
    LET: "let",
    PUB: "pub",
    STATIC: "static",
    UNSAFE: "unsafe",
    DEFAULT: "default",
    CONST: "const",
    MUT: "mut",
    MOVE: "move",
    RETURN: "return",
    IN: "in",
    IF: "if",
    ELSE: "else",
    LOOP: "loop",
    WHILE: "while",
    BREAK: "break",
    CONTINUE: "continue",
    FOR: "for",
    MATCH: "match",
    LEAF: "leaf",
    TRY: "try",
    REF: "ref",
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    pub const fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }

    pub const fn with_dummy_span(name: Symbol) -> Self {
        Self::new(name, DUMMY_SPAN)
    }

    pub const fn empty() -> Self {
        Self::with_dummy_span(kw::EMPTY)
    }

    pub fn with_span(self, span: Span) -> Self {
        Self::new(self.name, span)
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }

    pub fn is_keyword(&self) -> bool {
        self.name.is_keyword()
    }

    pub fn is(&self, other: Symbol) -> bool {
        self.name == other
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl FromStr for Ident {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from(s))
    }
}

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self::with_dummy_span(Symbol::new(value))
    }
}
