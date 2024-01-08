#![feature(macro_metavar_expr)]

use std::sync::Arc;

use source::SourceFile;

pub mod ident;
pub mod source;
pub mod span;
pub mod symbol;

#[derive(Debug, Clone)]
pub struct Location {
    pub file: Arc<SourceFile>,
    pub line: usize,
    pub col: usize,
}
