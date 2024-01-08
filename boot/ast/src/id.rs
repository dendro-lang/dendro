#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct NodeId(u32);

impl NodeId {
    pub const MAX_VALUE: u32 = 0xffff_ff00;
    pub const MAX: NodeId = NodeId(Self::MAX_VALUE);

    pub const fn from_u32(value: u32) -> Self {
        assert!(value < Self::MAX_VALUE);
        NodeId(value)
    }

    pub const fn from_usize(value: usize) -> Self {
        Self::from_u32(value as u32)
    }

    pub const fn as_u32(self) -> u32 {
        self.0
    }

    pub const fn as_usize(self) -> usize {
        self.as_u32() as usize
    }

    pub const fn index(&self) -> usize {
        self.as_usize()
    }
}
pub const DUMMY_ID: NodeId = NodeId::MAX;
