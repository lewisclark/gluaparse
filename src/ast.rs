use crate::error::Error;
use crate::lexer::Token;
use std::fmt::{self, Display};

/* ---------- Reader ---------- */

struct Reader<'a> {
    tokens: &'a [Token<'a>],
    pos: usize,
}

impl<'a> Reader<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn next(&mut self) -> Option<&Token<'a>> {
        let t = self.tokens.get(self.pos);
        self.pos += 1;

        t
    }

    fn peek(&self, n: isize) -> Option<&Token<'a>> {
        self.tokens.get((self.pos as isize + n - 1) as usize)
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
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self {
            reader: Reader::new(tokens),
        }
    }

    pub fn create(mut self) -> Result<Option<AstNode>, Error> {
        Ok(Some(self.read_chunk()?))
    }

    fn read_func(&mut self) -> Result<AstNode, Error> {
        self.reader.expect(&Token::Function)?;

        let is_local = match self.reader.peek(-1) {
            Some(t) => t == &Token::Local,
            None => false,
        };

        let is_anonymous = match self.reader.peek(1) {
            Some(t) => t == &Token::LeftParen,
            None => {
                return Err(Error::new(
                    "Expected '(' or identifier after 'function'".to_string(),
                ))
            }
        };

        if is_anonymous {
            let params = self.read_params()?;
            let body = Box::new(self.read_block(true)?);

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
            let body = Box::new(self.read_block(true)?);

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

        let params = self.read_comma_delimited(&Token::RightParen)?;

        self.reader.expect(&Token::RightParen)?;

        Ok(params)
    }

    fn read_comma_delimited(&mut self, stop_on: &Token) -> Result<Vec<AstNode>, Error> {
        let mut values = Vec::new();

        while let Some(t) = self.reader.peek(1) {
            match t {
                Token::Comma => {
                    self.reader.consume(1);
                    continue;
                }
                t => {
                    if t == stop_on {
                        break;
                    } else {
                        values.push(self.read_value()?);
                    }
                }
            };
        }

        Ok(values)
    }

    fn read_chunk(&mut self) -> Result<AstNode, Error> {
        self.read_block(false)
    }

    fn read_block(&mut self, break_on_end: bool) -> Result<AstNode, Error> {
        let mut block = Vec::new();

        while let Some(token) = self.reader.peek(1) {
            match token {
                Token::End => {
                    if break_on_end {
                        self.reader.consume(1);
                        break;
                    }
                }
                Token::Do => {
                    self.reader.consume(1);
                    block.push(self.read_block(true)?)
                }
                Token::Function => block.push(self.read_func()?),
                Token::Ident(_) | Token::Int(_) | Token::Float(_) => {
                    match self.reader.peek(2) {
                        Some(t) => match t {
                            Token::LeftParen => block.push(self.read_call()?),
                            Token::Plus
                            | Token::Minus
                            | Token::Asterisk
                            | Token::Slash
                            | Token::Caret
                            | Token::Percent
                            | Token::DotDot => (), //self.read_binop()?,
                            Token::LeftAngleBracket
                            | Token::LeftAngleBracketEqual
                            | Token::RightAngleBracket
                            | Token::RightAngleBracketEqual
                            | Token::EqualEqual
                            | Token::NotEqual => (), //self.read_comparisonop()?,
                            Token::And | Token::Or => (), //self.read_logicalop()?,
                            Token::Not | Token::Hashtag => (), //self.read_unaryop()?,
                            t => {
                                return Err(Error::new(format!(
                                    "Unexpected token {:?} after identifier",
                                    t
                                )))
                            }
                        },
                        None => {
                            return Err(Error::new(
                                "Expected token after identifier, found eof".to_string(),
                            ))
                        }
                    }
                }
                _ => self.reader.consume(1),
            };
        }

        Ok(AstNode::Block(block))
    }

    fn read_call(&mut self) -> Result<AstNode, Error> {
        let ident = self.read_ident()?;
        let params = self.read_params()?;

        Ok(AstNode::Call(Box::new(ident), params))
    }

    fn read_value(&mut self) -> Result<AstNode, Error> {
        match self.reader.peek(1) {
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

    /* ident, params */
    Call(Box<AstNode>, Vec<AstNode>),

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
