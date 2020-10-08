pub mod ast;
pub mod error;
mod lexer;

use ast::AstNode;
use error::Error;
use lexer::Lexer;

pub fn parse(code: &str) -> Result<AstNode, Error> {
    let lexer = Lexer::from_source(code).lex()?;
    let tokens = lexer.tokens();

    println!("tokens: {:?}", tokens);

    Ok(AstNode::Invalid)
}
