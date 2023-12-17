use std::{borrow::Cow, fmt, mem, num::NonZeroU16, sync::Mutex};

use dendro_span::span::{Pos, Span};
use internment::Intern;
use pest::{error::InputLocation, RuleType};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Level {
    Error {
        code: Option<NonZeroU16>,
        is_fatal: bool,
    },
    Warning,
    #[default]
    Info,
}

impl Level {
    pub fn is_fatal(&self) -> bool {
        matches!(self, Level::Error { is_fatal: true, .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct DiagUnit {
    pub span: Option<Span>,
    pub message: Intern<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Diagnostic {
    pub level: Level,
    pub messages: Vec<DiagUnit>,
}

impl<R: RuleType> From<pest::error::Error<R>> for Diagnostic {
    fn from(err: pest::error::Error<R>) -> Self {
        let mut diag = Diagnostic::new(Level::Error {
            code: None,
            is_fatal: true,
        });
        let value = DiagUnit {
            span: Some(match err.location {
                InputLocation::Pos(p) => Span::new(Pos(p), Pos(p + 1)),
                InputLocation::Span((start, end)) => Span::new(Pos(start), Pos(end)),
            }),
            message: match err.variant.message() {
                Cow::Borrowed(s) => Intern::from_ref(s),
                Cow::Owned(s) => Intern::new(s),
            },
        };
        diag.messages.push(value);
        diag
    }
}

impl Diagnostic {
    pub fn new(level: Level) -> Self {
        Self {
            level,
            messages: Vec::new(),
        }
    }

    pub fn error(code: Option<NonZeroU16>, is_fatal: bool) -> Self {
        Self::new(Level::Error { code, is_fatal })
    }

    pub fn warning() -> Self {
        Self::new(Level::Warning)
    }

    pub fn info() -> Self {
        Self::new(Level::Info)
    }

    pub fn push_unspanned(&mut self, message: &str) -> &mut Self {
        self.messages.push(DiagUnit {
            span: None,
            message: Intern::from_ref(message),
        });
        self
    }

    pub fn push(&mut self, span: Span, message: &str) -> &mut Self {
        self.messages.push(DiagUnit {
            span: Some(span),
            message: Intern::from_ref(message),
        });
        self
    }

    pub fn push_unspanned_fmt(&mut self, args: fmt::Arguments) -> &mut Self {
        self.messages.push(DiagUnit {
            span: None,
            message: Intern::new(format!("{args}")),
        });
        self
    }

    pub fn push_fmt(&mut self, span: Span, args: fmt::Arguments) -> &mut Self {
        self.messages.push(DiagUnit {
            span: Some(span),
            message: Intern::new(format!("{args}")),
        });
        self
    }

    pub fn report(&mut self) -> Result<(), Error> {
        let this = mem::take(self);
        if this.level.is_fatal() {
            Err(Error(vec![this]))
        } else {
            DIAG.lock().unwrap().push(this);
            Ok(())
        }
    }

    pub fn into_err(&mut self) -> Error {
        Error(vec![mem::take(self)])
    }
}

static DIAG: Mutex<Vec<Diagnostic>> = Mutex::new(Vec::new());

pub fn report(e: impl Into<Diagnostic>) -> Result<(), Error> {
    Diagnostic::report(&mut e.into())
}

pub fn take() -> Vec<Diagnostic> {
    mem::take(&mut DIAG.lock().unwrap())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Error(Vec<Diagnostic>);

impl Error {
    pub fn with_diags(mut self) -> Self {
        self.0.append(&mut DIAG.lock().unwrap());
        self
    }
}
