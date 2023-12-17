use std::{fmt, ptr::NonNull};

use strpool::Pool;

use crate::ident::{SYMBOL_PREFILL, keywords};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Symbol {
    Interned(NonNull<str>),
    Const(usize),
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

unsafe impl Send for Symbol {}
unsafe impl Sync for Symbol {}

impl Symbol {
    pub(crate) const fn prefill(index: usize) -> Self {
        Self::Const(index)
    }

    pub fn new(s: &str) -> Self {
        Symbol::Interned(NonNull::from(&*Pool::get_static_pool().intern(s)))
    }

    pub fn as_str(&self) -> &str {
        match *self {
            Symbol::Const(index) => SYMBOL_PREFILL[index],
            Symbol::Interned(ptr) => unsafe { ptr.as_ref() },
        }
    }

    pub fn is_keyword(&self) -> bool {
        keywords::MAP.contains_key(self.as_str())
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}
