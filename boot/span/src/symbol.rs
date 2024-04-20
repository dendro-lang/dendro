use std::{collections::BTreeMap, fmt, hash::Hash, mem, sync::Mutex};

use strpool::Pool;

use crate::ident::{kw, SYMBOL_PREFILL};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize);

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

unsafe impl Send for Symbol {}
unsafe impl Sync for Symbol {}

impl Symbol {
    pub(crate) const fn prefill(index: usize) -> Self {
        Symbol(index)
    }

    pub fn new(s: &str) -> Self {
        INTERNER.intern(s)
    }

    pub fn as_str(&self) -> &str {
        INTERNER.get(*self)
    }

    pub fn is_keyword(&self) -> bool {
        kw::MAP.contains_key(self.as_str())
    }
}

impl PartialEq<&str> for Symbol {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

struct InternerInner {
    strings: Vec<&'static str>,
    map: BTreeMap<&'static str, Symbol>,
    pool: Option<Pool>,
}

struct Interner(Mutex<InternerInner>);

impl Interner {
    const fn new() -> Self {
        Interner(Mutex::new(InternerInner {
            strings: Vec::new(),
            map: BTreeMap::new(),
            pool: None,
        }))
    }

    fn intern(&self, s: &str) -> Symbol {
        if let Some(&sym) = kw::MAP.get(s) {
            return sym;
        }
        let mut inner = self.0.lock().unwrap();
        let sym = Symbol(inner.strings.len() + kw::MAP.len());

        let pstr = inner.pool.get_or_insert_with(Pool::new).intern(s);
        let s = unsafe { mem::transmute::<&str, &'static str>(&*pstr) };
        mem::forget(pstr);

        inner.strings.push(s);
        inner.map.insert(s, sym);

        sym
    }

    fn get(&self, symbol: Symbol) -> &'static str {
        match symbol.0.checked_sub(SYMBOL_PREFILL.len()) {
            Some(index) => self.0.lock().unwrap().strings[index],
            None => SYMBOL_PREFILL[symbol.0],
        }
    }
}

static INTERNER: Interner = Interner::new();
