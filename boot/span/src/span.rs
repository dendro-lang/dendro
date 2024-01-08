use std::ops::{Add, Sub};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Pos(pub usize);

impl From<usize> for Pos {
    fn from(value: usize) -> Self {
        Pos(value)
    }
}

impl Add<RelPos> for Pos {
    type Output = Pos;

    fn add(self, rhs: RelPos) -> Self::Output {
        Pos(self.0 + rhs.0)
    }
}

impl Sub<RelPos> for Pos {
    type Output = Pos;

    fn sub(self, rhs: RelPos) -> Self::Output {
        Pos(self.0 - rhs.0)
    }
}

impl Sub for Pos {
    type Output = RelPos;

    fn sub(self, rhs: Self) -> Self::Output {
        RelPos(self.0 - rhs.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct RelPos(pub usize);

impl From<usize> for RelPos {
    fn from(value: usize) -> Self {
        RelPos(value)
    }
}

impl Add for RelPos {
    type Output = RelPos;

    fn add(self, rhs: Self) -> Self::Output {
        RelPos(self.0 + rhs.0)
    }
}

impl Sub for RelPos {
    type Output = RelPos;

    fn sub(self, rhs: Self) -> Self::Output {
        RelPos(self.0 - rhs.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

pub const DUMMY_SPAN: Span = Span::new(Pos(0), Pos(0));

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
    pub const fn new(start: Pos, end: Pos) -> Self {
        assert!(start.0 <= end.0);
        Span { start, end }
    }

    pub fn to(&self, other: &Span) -> Span {
        Span::new(self.start, other.end)
    }

    pub fn between(&self, other: &Span) -> Span {
        Span::new(self.end, other.start)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DelimSpan {
    pub open: Span,
    pub close: Span,
}

impl DelimSpan {
    pub fn from_single(sp: Span) -> Self {
        DelimSpan { open: sp, close: sp }
    }

    pub fn from_pair(open: Span, close: Span) -> Self {
        DelimSpan { open, close }
    }

    pub fn dummy() -> Self {
        Self::from_single(DUMMY_SPAN)
    }

    pub fn entire(self) -> Span {
        self.open.to(&self.close)
    }
}
