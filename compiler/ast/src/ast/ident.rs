use dendro_span::{
    ident::{kw, Ident},
    span::{Span, DUMMY_SPAN},
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

    pub fn push(&mut self, segment: impl Into<PathSegment>) {
        let segment: PathSegment = segment.into();
        self.span.end = segment.ident.span.end;
        self.segments.push(segment);
    }
}

impl<T: Into<PathSegment>> Extend<T> for Path {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.segments.extend(iter.into_iter().map(Into::into));
        if let Some(last) = self.segments.last() {
            self.span.end = last.ident.span.end;
        }
    }
}

impl<T: Into<PathSegment>> FromIterator<T> for Path {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let segments = iter
            .into_iter()
            .map(Into::into)
            .collect::<Vec<PathSegment>>();

        let span = if let (Some(first), Some(last)) = (segments.first(), segments.last()) {
            Span::new(first.ident.span.start, last.ident.span.end)
        } else {
            DUMMY_SPAN
        };
        Self { segments, span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathSegment {
    pub ident: Ident,
    pub id: u32,
}

impl From<Ident> for PathSegment {
    fn from(value: Ident) -> Self {
        Self::from_ident(value)
    }
}

impl PathSegment {
    pub fn from_ident(ident: Ident) -> Self {
        PathSegment {
            ident,
            id: DUMMY_ID,
        }
    }

    pub fn root(span: Span) -> Self {
        Self::from_ident(Ident::new(kw::EMPTY, span))
    }

    pub fn span(&self) -> Span {
        self.ident.span
    }
}
