use std::{fmt, ptr::NonNull};

use strpool::Pool;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(NonNull<str>);

unsafe impl Send for Symbol {}
unsafe impl Sync for Symbol {}

impl Symbol {
    pub fn new(s: &str) -> Self {
        Symbol(NonNull::from(&*Pool::get_static_pool().intern(s)))
    }

    pub fn new_owned(s: String) -> Self {
        Self::new(&s)
    }

    pub fn as_str(&self) -> &str {
        unsafe { self.0.as_ref() }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}
