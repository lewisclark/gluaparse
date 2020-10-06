pub mod ast;
pub mod error;
mod lexer;

use ast::AstNode;
use error::Error;
use lexer::Tokens;

pub fn parse(code: &str) -> Result<AstNode, Error> {
    let tokens = Tokens::from_source(code)?;

    println!("tokens: {:?}", tokens);

    Ok(AstNode::Invalid)
}
