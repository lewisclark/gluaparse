use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Error {
    err: String,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.err)
    }
}
