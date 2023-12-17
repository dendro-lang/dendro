#![feature(let_chains)]

mod imp;
pub mod unescape;

mod token;
mod token_tree;

use dendro_ast::token_stream::TokenStream;
use dendro_error::Error;

use self::token_tree::TokenTrees;

pub fn parse(input: &str) -> Result<TokenStream, Error> {
    TokenTrees::new(token::parse(input)?).parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    const S: &str = r#"/// Wow!
let main = println /* on console */ "Hello, {} {}!" 0x2a 2.5f64;
// End of input.
"#;

    #[test]
    fn token() {
        let mut tokens = token::parse(S).unwrap();
        while let (spacing, Some(token)) = tokens.next_token() {
            println!("{spacing:?} {token:?}");
        }
    }

    #[test]
    fn overall() {
        let tt = parse(S).unwrap();
        println!("{tt:?}")
    }
}
