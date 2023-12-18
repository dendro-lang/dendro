use dendro_span::{ident::Ident, symbol::Symbol};

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

impl Mutability {
    pub fn kw(kw: Symbol) -> Self {
        Mutability {
            id: DUMMY_ID,
            ident: Ident::with_dummy_span(kw),
        }
    }
}
