use std::error::Error;

use dendro_ast::token_stream::TokenStream;

use self::token_tree::TokenTrees;

mod token;
mod token_tree;

pub fn parse(input: &str) -> Result<TokenStream, Box<dyn Error>> {
    TokenTrees::new(token::parse(input)?).parse()
}
