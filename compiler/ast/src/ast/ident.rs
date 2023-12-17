use dendro_span::{
    ident::{keywords, Ident},
    span::Span,
    symbol::Symbol,
};

use super::DUMMY_ID;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lifetime {
    pub id: u32,
    pub ident: Ident,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Mutability {
    pub id: u32,
    pub ident: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<PathSegment>,
}

impl PartialEq<Symbol> for Path {
    fn eq(&self, symbol: &Symbol) -> bool {
        self.segments.len() == 1 && self.segments[0].ident.name == *symbol
    }
}

impl Path {
    pub fn from_ident(ident: Ident) -> Self {
        Path {
            span: ident.span,
            segments: vec![PathSegment::from_ident(ident)],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathSegment {
    pub ident: Ident,
    pub id: u32,
}

impl PathSegment {
    pub fn from_ident(ident: Ident) -> Self {
        PathSegment {
            ident,
            id: DUMMY_ID,
        }
    }

    pub fn root(span: Span) -> Self {
        Self::from_ident(Ident::new(keywords::EMPTY, span))
    }

    pub fn span(&self) -> Span {
        self.ident.span
    }
}
