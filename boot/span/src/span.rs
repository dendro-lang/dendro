use std::ops::{Add, RangeBounds, Sub};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Pos(pub usize);

impl Pos {
    pub const fn checked_add(self, rel: RelPos) -> Option<Self> {
        match self.0.checked_add(rel.0) {
            Some(x) => Some(Pos(x)),
            None => None,
        }
    }
}

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

    pub const fn pos(pos: Pos) -> Self {
        Self::new(pos, Pos(pos.0 + 1))
    }

    pub fn to(&self, other: &Span) -> Span {
        Span::new(self.start, other.end)
    }

    pub fn between(&self, other: &Span) -> Span {
        Span::new(self.end, other.start)
    }

    pub fn get(&self, bound: impl RangeBounds<RelPos>) -> Option<Span> {
        let start_bound = self.start.checked_add(match bound.start_bound() {
            std::ops::Bound::Included(&pos) => pos,
            std::ops::Bound::Excluded(&pos) => RelPos(pos.0.checked_add(1)?),
            std::ops::Bound::Unbounded => RelPos(0),
        })?;

        let end_bound = match bound.end_bound() {
            std::ops::Bound::Included(&pos) => {
                self.start.checked_add(RelPos(pos.0.checked_sub(1)?))?
            }
            std::ops::Bound::Excluded(&pos) => self.start.checked_add(pos)?,
            std::ops::Bound::Unbounded => self.end,
        };

        (start_bound <= end_bound && end_bound <= self.end)
            .then(|| Span::new(start_bound, end_bound))
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
