#![feature(macro_metavar_expr)]

use std::{ops::Range, sync::Arc};

use source::SourceFile;

pub mod ident;
pub mod source;
pub mod span;
pub mod symbol;

#[derive(Debug, Clone)]
pub struct Loc {
    pub file: Arc<SourceFile>,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct LocSpan {
    pub file: Arc<SourceFile>,
    pub lines: Range<usize>,
    pub start_col: usize,
    pub end_col: usize,
}
