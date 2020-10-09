use crate::error::Error;
use crate::lexer::Token;
use std::fmt::{self, Display};

/* ---------- Reader ---------- */

struct Reader<'a> {
    tokens: &'a Vec<Token<'a>>,
    pos: usize,
}

impl<'a> Reader<'a> {
    fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn next(&mut self) -> Option<&Token<'a>> {
        let t = self.tokens.get(self.pos);
        self.pos += 1;

        t
    }

    fn peek_next(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.pos)
    }

    fn peek_prev(&mut self) -> Option<&Token<'a>> {
        self.tokens.get(self.pos - 2)
    }

    fn consume(&mut self, n: usize) {
        self.pos += n;
    }

    fn expect(&mut self, val: &Token<'a>) -> Result<&Token<'a>, Error> {
        match self.next() {
            Some(t) => {
                if t == val {
                    Ok(t)
                } else {
                    Err(Error::new(format!("Expected {:?}, found {:?}", val, t)))
                }
            }
            None => Err(Error::new(format!("Expected {:?}, found eof", val))),
        }
    }
}

/* ---------- AstConstructor ---------- */

pub struct AstConstructor<'a> {
    reader: Reader<'a>,
}

impl<'a> AstConstructor<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            reader: Reader::new(tokens),
        }
    }

    pub fn create(mut self) -> Result<Option<AstNode>, Error> {
        let mut chunk = Vec::new();

        while let Some(token) = self.reader.next() {
            match token {
                Token::Function => chunk.push(self.read_func()?),
                _ => (),
            };
        }

        Ok(Some(AstNode::Chunk(chunk)))
    }

    fn read_func(&mut self) -> Result<AstNode, Error> {
        let _is_local = match self.reader.peek_prev() {
            Some(t) => t == &Token::Local,
            None => false,
        };

        let is_anonymous = match self.reader.peek_next() {
            Some(t) => t == &Token::LeftParen,
            None => {
                return Err(Error::new(
                    "Expected '(' or identifier after 'function'".to_string(),
                ))
            }
        };

        if is_anonymous {
            let _params = self.read_params()?;
        } else {
            let _ident = self.read_ident()?;
            let _params = self.read_params()?;
        }

        Err(Error::new(String::new()))
    }

    fn read_params(&mut self) -> Result<AstNode, Error> {
        self.reader.expect(&Token::LeftParen)?;

        let mut params = Vec::new();

        while let Some(t) = self.reader.peek_next() {
            match t {
                Token::RightParen => break,
                Token::Comma => {
                    self.reader.consume(1);
                    continue;
                }
                _ => params.push(self.read_value()?),
            };
        }

        self.reader.expect(&Token::RightParen)?;

        Ok(AstNode::Params(params))
    }

    fn read_value(&mut self) -> Result<AstNode, Error> {
        match self.reader.next() {
            Some(t) => match t {
                Token::Function => Ok(self.read_func()?),
                Token::Ident(_) => Ok(self.read_ident()?),
                t => Err(Error::new(format!("Expected value, found {:?}", t))),
            },
            None => Err(Error::new("Expected value, found eof".to_string())),
        }
    }

    fn read_ident(&mut self) -> Result<AstNode, Error> {
        let ident = match self.reader.next() {
            Some(t) => match t {
                Token::Ident(s) => Ok(s),
                t => Err(Error::new(format!("Expected identifier, found {:?}", t))),
            },
            None => Err(Error::new("Expected identifier, found None".to_string())),
        }?;

        Ok(AstNode::Ident(ident.to_string()))
    }
}

/* ---------- AstNode ---------- */

#[derive(Debug)]
pub enum AstNode {
    Chunk(Vec<AstNode>),
    Function,
    Statement,
    Ident(String),
    Params(Vec<AstNode>),
    Invalid,
}

impl Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
