use dendro_span::ident::Ident;

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
