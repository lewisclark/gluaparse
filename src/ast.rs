use crate::error::Error;
use crate::lexer::Token;
use std::fmt::{self, Display};

/* ---------- AstNode ---------- */

#[derive(Debug)]
pub enum AstNode {
    Chunk(Vec<AstNode>),
    Function,
    Statement,
    Invalid,
}

impl Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/* ---------- AstConstructor ---------- */

pub struct AstConstructor<'a> {
    tokens: &'a Vec<Token<'a>>,
    chunk: Option<AstNode>,
}

impl<'a> AstConstructor<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            tokens,
            chunk: None,
        }
    }

    pub fn get(self) -> Option<AstNode> {
        self.chunk
    }

    pub fn create(mut self) -> Result<Self, Error> {
        Ok(self)
    }
}
