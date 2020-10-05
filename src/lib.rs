pub mod ast;
pub mod error;

use ast::AstNode;

pub fn parse(_code: &str) -> AstNode {
    AstNode::Invalid
}
