use dendro_span::{
    ident::{kw, Ident},
    symbol::Symbol,
};

use super::DUMMY_ID;
use crate::id::NodeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lifetime {
    pub id: NodeId,
    pub ident: Ident,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Mutability {
    pub id: NodeId,
    pub ident: Ident,
}

impl Mutability {
    pub const fn kw(kw: Symbol) -> Self {
        Mutability {
            id: DUMMY_ID,
            ident: Ident::with_dummy_span(kw),
        }
    }

    pub const fn is_kw(&self) -> bool {
        matches!(self.ident.name, kw::MOVE | kw::CONST | kw::MUT)
    }
}
