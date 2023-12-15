
use internment::Intern;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(Intern<String>);

impl Symbol {
    pub fn new(s: &str) -> Self {
        Symbol(Intern::from_ref(s))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

