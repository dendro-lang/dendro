#![feature(let_chains)]

mod imp;
pub mod unescape;

mod token;
mod token_tree;

use dendro_ast::token_stream::TokenStream;
use dendro_error::DiagCx;
use dendro_span::source::SourceFile;

use self::token_tree::TokenTrees;

pub fn parse(input: &SourceFile, diag: &DiagCx) -> TokenStream {
    match token::parse(input, diag) {
        Ok(tokens) => TokenTrees::new(tokens, diag).parse(),
        Err(_) => Default::default(),
    }
}
