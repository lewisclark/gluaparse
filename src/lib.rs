pub mod ast;
pub mod error;
mod lexer;

use ast::{AstConstructor, AstNode};
use error::Error;
use lexer::Lexer;

pub fn parse(code: &str) -> Result<AstNode, Error> {
    let lexer = Lexer::from_source(code).lex()?;
    let tokens = lexer.tokens();

    AstConstructor::new(tokens).create()
}
