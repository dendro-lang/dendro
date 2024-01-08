use std::{
    borrow::Cow,
    fmt, mem,
    num::NonZeroU16,
    ops::{Deref, DerefMut},
    sync::Mutex,
};

use dendro_span::span::Span;
use internment::Intern;

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

pub trait IntoMessage<'a> {
    fn into_message(self) -> Cow<'a, str>;
}

impl<'a> IntoMessage<'a> for &'a str {
    fn into_message(self) -> Cow<'a, str> {
        Cow::Borrowed(self)
    }
}

impl<'a> IntoMessage<'a> for String {
    fn into_message(self) -> Cow<'a, str> {
        Cow::Owned(self)
    }
}

impl<'a> IntoMessage<'a> for Cow<'a, str> {
    fn into_message(self) -> Cow<'a, str> {
        self
    }
}

impl<'a> IntoMessage<'a> for fmt::Arguments<'a> {
    fn into_message(self) -> Cow<'a, str> {
        match self.as_str() {
            Some(s) => Cow::Borrowed(s),
            None => Cow::Owned(format!("{self}")),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Diagnostic {
    pub level: Level,
    pub messages: Vec<DiagUnit>,
}

impl Diagnostic {
    pub const fn new(level: Level) -> Self {
        Self { level, messages: Vec::new() }
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

    pub fn push_unspanned<'a>(&mut self, message: impl IntoMessage<'a>) -> &mut Self {
        self.messages.push(DiagUnit {
            span: None,
            message: match message.into_message() {
                Cow::Borrowed(s) => Intern::from_ref(s),
                Cow::Owned(s) => Intern::new(s),
            },
        });
        self
    }

    pub fn push<'a>(&mut self, span: Span, message: impl IntoMessage<'a>) -> &mut Self {
        self.messages.push(DiagUnit {
            span: Some(span),
            message: match message.into_message() {
                Cow::Borrowed(s) => Intern::from_ref(s),
                Cow::Owned(s) => Intern::new(s),
            },
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

    pub fn span_err<'a>(&self, span: Span, message: impl IntoMessage<'a>) -> Error {
        let mut err = self.error(None, false);
        err.push(span, message);
        err.emit()
    }

    pub fn warning(&self) -> DiagnosticBuilder {
        self.new_diag(Level::Warning)
    }

    pub fn info(&self) -> DiagnosticBuilder {
        self.new_diag(Level::Info)
    }
}

#[derive(Debug, Clone)]
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

    pub fn emit(&mut self) -> Error {
        if let Some(cx) = self.cx.take() {
            cx.0.lock().unwrap().push(mem::take(&mut self.diag))
        }
        Error(())
    }
}

pub type PResult<'a, T> = Result<T, DiagnosticBuilder<'a>>;

#[derive(Debug, Clone)]
pub struct Error(());

impl std::fmt::Display for Error {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl std::error::Error for Error {}
