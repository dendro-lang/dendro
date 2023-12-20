use std::{
    borrow::Cow,
    fmt, mem,
    num::NonZeroU16,
    ops::{Deref, DerefMut},
    sync::Mutex,
};

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
    pub const fn new(level: Level) -> Self {
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
}

#[derive(Debug, Default)]
pub struct DiagCx(Mutex<Vec<Diagnostic>>);

impl DiagCx {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_diag(&self, level: Level) -> DiagnosticBuilder {
        DiagnosticBuilder {
            cx: Some(self),
            diag: Diagnostic::new(level),
        }
    }

    pub fn push(&self, diag: Diagnostic) {
        self.0.lock().unwrap().push(diag);
    }

    pub fn take(&self) -> Vec<Diagnostic> {
        mem::take(&mut self.0.lock().unwrap())
    }

    pub fn error(&self, code: Option<NonZeroU16>, is_fatal: bool) -> DiagnosticBuilder {
        self.new_diag(Level::Error { code, is_fatal })
    }

    pub fn warning(&self) -> DiagnosticBuilder {
        self.new_diag(Level::Warning)
    }

    pub fn info(&self) -> DiagnosticBuilder {
        self.new_diag(Level::Info)
    }
}

#[derive(Debug)]
pub struct DiagnosticBuilder<'a> {
    cx: Option<&'a DiagCx>,
    pub diag: Diagnostic,
}

impl<'a> Deref for DiagnosticBuilder<'a> {
    type Target = Diagnostic;
    fn deref(&self) -> &Diagnostic {
        &self.diag
    }
}

impl<'a> DerefMut for DiagnosticBuilder<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.diag
    }
}

impl<'a> DiagnosticBuilder<'a> {
    pub fn cancel(mut self) {
        self.cx = None;
    }

    pub fn emit(&mut self) {
        if let Some(cx) = self.cx.take() {
            cx.0.lock().unwrap().push(mem::take(&mut self.diag))
        }
    }
}

pub type PResult<'a, T> = Result<T, DiagnosticBuilder<'a>>;
