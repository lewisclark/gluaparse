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
        Ok(Some(self.read_block()?))
    }

    fn read_func(&mut self) -> Result<AstNode, Error> {
        self.reader.expect(&Token::Function)?;

        let is_local = match self.reader.peek_prev() {
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
            let params = self.read_params()?;
            let body = Box::new(self.read_block()?);

            Ok(AstNode::Function(
                Box::new(None),
                is_local,
                true,
                params,
                body,
            ))
        } else {
            let ident = self.read_ident()?;
            let params = self.read_params()?;
            let body = Box::new(self.read_block()?);

            Ok(AstNode::Function(
                Box::new(Some(ident)),
                is_local,
                is_anonymous,
                params,
                body,
            ))
        }
    }

    fn read_params(&mut self) -> Result<Vec<AstNode>, Error> {
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

        Ok(params)
    }

    fn read_block(&mut self) -> Result<AstNode, Error> {
        let mut block = Vec::new();

        while let Some(token) = self.reader.peek_next() {
            match token {
                Token::Function => block.push(self.read_func()?),
                _ => self.reader.consume(1),
            };
        }

        Ok(AstNode::Block(block))
    }

    fn read_value(&mut self) -> Result<AstNode, Error> {
        match self.reader.peek_next() {
            Some(t) => match t {
                Token::Function => Ok(self.read_func()?),
                Token::Ident(_) => Ok(self.read_ident()?),
                Token::Str(_) => Ok(self.read_str()?),
                Token::Int(_) => Ok(self.read_int()?),
                Token::Float(_) => Ok(self.read_float()?),
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
            None => Err(Error::new("Expected identifier, found eof".to_string())),
        }?;

        Ok(AstNode::Ident(ident.to_string()))
    }

    fn read_str(&mut self) -> Result<AstNode, Error> {
        let s = match self.reader.next() {
            Some(t) => match t {
                Token::Str(s) => Ok(s),
                t => Err(Error::new(format!("Expected string, found {:?}", t))),
            },
            None => Err(Error::new("Expected string, found eof".to_string())),
        }?;

        Ok(AstNode::Str(s.to_string()))
    }

    fn read_int(&mut self) -> Result<AstNode, Error> {
        let n = match self.reader.next() {
            Some(t) => match t {
                Token::Int(n) => Ok(n),
                t => Err(Error::new(format!("Expected int, found {:?}", t))),
            },
            None => Err(Error::new("Expected int, found eof".to_string())),
        }?;

        Ok(AstNode::Int(*n))
    }

    fn read_float(&mut self) -> Result<AstNode, Error> {
        let f = match self.reader.next() {
            Some(t) => match t {
                Token::Float(n) => Ok(n),
                t => Err(Error::new(format!("Expected float, found {:?}", t))),
            },
            None => Err(Error::new("Expected float, found eof".to_string())),
        }?;

        Ok(AstNode::Float(*f))
    }
}

/* ---------- AstNode ---------- */

#[derive(Debug)]
pub enum AstNode {
    Block(Vec<AstNode>),

    /* ident, is_local, is_anonymous, params, body */
    Function(Box<Option<AstNode>>, bool, bool, Vec<AstNode>, Box<AstNode>),
    Statement,
    Ident(String),
    Str(String),
    Int(isize),
    Float(f64),
    Invalid,
}

impl Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
