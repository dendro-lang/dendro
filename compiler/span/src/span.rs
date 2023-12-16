use std::ops::{Add, Sub};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos(pub usize);

impl From<usize> for Pos {
    fn from(value: usize) -> Self {
        Pos(value)
    }
}

impl Add for Pos {
    type Output = Pos;

    fn add(self, rhs: Self) -> Self::Output {
        Pos(self.0 + rhs.0)
    }
}

impl Sub for Pos {
    type Output = Pos;

    fn sub(self, rhs: Self) -> Self::Output {
        Pos(self.0 - rhs.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl From<pest::Span<'_>> for Span {
    fn from(value: pest::Span<'_>) -> Self {
        Span {
            start: value.start().into(),
            end: value.end().into(),
        }
    }
}

impl From<Pos> for Span {
    fn from(value: Pos) -> Self {
        Span {
            start: value,
            end: value + 1.into(),
        }
    }
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        assert!(start <= end);
        Span { start, end }
    }

    pub fn to(&self, other: &Span) -> Span {
        Span::new(self.start, other.end)
    }

    pub fn between(&self, other: &Span) -> Span {
        Span::new(self.end, other.start)
    }
}
