use std::fmt::{self, Display};

#[derive(Debug)]
pub enum AstNode {
    Invalid,
}

impl Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
