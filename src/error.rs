use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Error {
    err: String,
}

impl Error {
    pub fn new(err: String) -> Self {
        Self { err }
    }
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.err)
    }
}
