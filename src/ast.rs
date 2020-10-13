use crate::error::Error;
use crate::lexer::Token;
use std::convert::From;
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
        self.tokens.get((self.pos as isize + n) as usize)
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

        let is_local = match self.reader.peek(-2) {
            Some(t) => t == &Token::Local,
            None => false,
        };

        let is_anonymous = match self.reader.peek(0) {
            Some(t) => t == &Token::LeftParen,
            None => {
                return Err(Error::new(
                    "Expected '(' or identifier after 'function', found eof".to_string(),
                ))
            }
        };

        if is_anonymous {
            let params = self.read_params()?;
            let body = Box::new(self.read_block(|t| t == &Token::End)?);

            Ok(AstNode::Function(params, body))
        } else {
            let ident = self.read_variable(Some(is_local))?;
            let params = self.read_params()?;
            let body = Box::new(self.read_block(|t| t == &Token::End)?);

            let func = AstNode::Function(params, body);

            Ok(AstNode::Assignment(Box::new(ident), Box::new(func)))
        }
    }

    fn read_params(&mut self) -> Result<Vec<AstNode>, Error> {
        self.reader.expect(&Token::LeftParen)?;
        let params = self.read_comma_delimited(|t| t == &Token::RightParen)?;
        self.reader.expect(&Token::RightParen)?;

        Ok(params)
    }

    fn read_comma_delimited<F>(&mut self, stopper: F) -> Result<Vec<AstNode>, Error>
    where
        F: Fn(&Token<'_>) -> bool,
    {
        let mut exprs = Vec::new();

        while let Some(t) = self.reader.peek(0) {
            match t {
                Token::Comma => {
                    self.reader.consume(1);
                    continue;
                }
                t => {
                    if stopper(t) {
                        break;
                    } else {
                        exprs.push(self.read_expression()?);
                    }
                }
            };
        }

        Ok(exprs)
    }

    fn read_chunk(&mut self) -> Result<AstNode, Error> {
        self.read_block(|_| false)
    }

    fn read_block<F>(&mut self, breaker: F) -> Result<AstNode, Error>
    where
        F: Fn(&Token<'_>) -> bool,
    {
        let mut block = Vec::new();

        while let Some(token) = self.reader.peek(0) {
            match token {
                Token::Do => unimplemented!(),
                Token::Local => self.reader.consume(1),
                Token::Function => block.push(self.read_func()?),
                Token::Ident(_) => match self.reader.peek(1) {
                    Some(t) => match t {
                        Token::LeftParen => block.push(self.read_call()?),
                        _ => unimplemented!(),
                    },
                    None => unimplemented!(),
                },
                Token::Return => block.push(self.read_return()?),
                Token::If => block.push(self.read_if()?),
                t => {
                    if breaker(t) {
                        self.reader.consume(1);
                        break;
                    } else {
                        unimplemented!();
                    }
                }
            };
        }

        Ok(AstNode::Block(block))
    }

    fn read_return(&mut self) -> Result<AstNode, Error> {
        self.reader.expect(&Token::Return)?;

        Ok(AstNode::Return(Box::new(self.read_expression()?)))
    }

    fn read_call(&mut self) -> Result<AstNode, Error> {
        let variable = self.read_variable(None)?;
        let params = self.read_params()?;

        Ok(AstNode::Call(Box::new(variable), params))
    }

    fn read_expression(&mut self) -> Result<AstNode, Error> {
        let val = match self.reader.peek(0) {
            Some(t) => match t {
                Token::Function => self.read_func(),
                Token::Ident(_) => self.read_ident(),
                Token::Str(_) => self.read_str(),
                Token::Int(_) => self.read_int(),
                Token::Float(_) => self.read_float(),
                Token::True => self.read_bool(),
                Token::False => self.read_bool(),
                t => Err(Error::new(format!("Expected value, found {:?}", t))),
            },
            None => Err(Error::new("Expected value, found eof".to_string())),
        };

        Ok(AstNode::Expression(Box::new(val?)))
    }

    fn read_bool(&mut self) -> Result<AstNode, Error> {
        match self.reader.next() {
            Some(t) => match t {
                Token::True => Ok(AstNode::Bool(true)),
                Token::False => Ok(AstNode::Bool(false)),
                t => Err(Error::new(format!("Expected Bool, found {:?}", t))),
            },
            None => Err(Error::new("Expected Bool, found eof".to_string())),
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

    fn read_variable(&mut self, is_local: Option<bool>) -> Result<AstNode, Error> {
        let ident = self.read_ident()?;

        let is_local = match is_local {
            Some(b) => b,
            None => match self.reader.peek(-3) {
                Some(t) => match t {
                    Token::Local => true,
                    _ => false,
                },
                None => false,
            },
        };

        Ok(AstNode::Variable(Box::new(ident), is_local))
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

    fn read_if(&mut self) -> Result<AstNode, Error> {
        self.reader.expect(&Token::If)?;
        let expr = self.read_expression()?;
        self.reader.expect(&Token::Then)?;

        let block_true = self.read_block(|t| match t {
            Token::End | Token::Else | Token::ElseIf => true,
            _ => false,
        })?;

        match self.reader.peek(-1) {
            Some(t) => match t {
                Token::End => Ok(AstNode::If(Box::new(expr), Box::new(block_true), None)),
                Token::Else | Token::ElseIf => unimplemented!(),
                t => Err(Error::new(format!(
                    "Expected End, Else or ElseIf, found {:?}",
                    t
                ))),
            },
            None => Err(Error::new(
                "Expected End, Else or ElseIf, found eof".to_string(),
            )),
        }
    }
}

/* ---------- AstNode ---------- */

#[derive(Debug, Clone)]
pub enum AstNode {
    Block(Vec<AstNode>),

    /* params, body */
    Function(Vec<AstNode>, Box<AstNode>),

    /* variable, params */
    Call(Box<AstNode>, Vec<AstNode>),

    /* ident, value */
    Assignment(Box<AstNode>, Box<AstNode>),

    /* name */
    Ident(String),

    /* ident, is_local */
    Variable(Box<AstNode>, bool),

    /* expr */
    Expression(Box<AstNode>),

    /* expr */
    Return(Box<AstNode>),

    /* cond, block, else_block */
    If(Box<AstNode>, Box<AstNode>, Option<Box<AstNode>>),

    /* vec_if_stmts, else_block */
    IfElseIf(Vec<AstNode>, Option<Box<AstNode>>),

    Str(String),
    Int(isize),
    Float(f64),
    Bool(bool),
    Invalid,
}

impl Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buf = Vec::new();
        ptree::write_tree(self, std::io::Cursor::new(&mut buf)).unwrap();

        write!(f, "{}", std::str::from_utf8(&buf).unwrap())
    }
}

impl ptree::item::TreeItem for AstNode {
    type Child = AstNode;

    fn write_self<W: std::io::Write>(
        &self,
        f: &mut W,
        _style: &ptree::style::Style,
    ) -> std::io::Result<()> {
        match self {
            AstNode::Block(_) => write!(f, "{}", "Block"),
            AstNode::Function(params, _body) => write!(f, "Function {:?}", params),
            AstNode::Call(_variable, _params) => write!(f, "Call"),
            AstNode::Assignment(_ident, _value) => write!(f, "Assignment"),
            AstNode::Ident(name) => write!(f, "Ident {}", name),
            AstNode::Variable(_name, is_local) => write!(
                f,
                "Variable ({})",
                match is_local {
                    true => "local",
                    false => "non-local",
                },
            ),
            AstNode::Expression(_expr) => write!(f, "Expression"),
            AstNode::Return(_expr) => write!(f, "Return"),
            AstNode::If(_cond, _block, _else_block) => write!(f, "If"),
            AstNode::Str(s) => write!(f, "Str {}", s),
            AstNode::Int(i) => write!(f, "Int {}", i),
            AstNode::Float(fl) => write!(f, "Float {}", fl),
            AstNode::Bool(b) => write!(f, "Bool {}", b),
            _ => write!(f, "{}", "<unknown>"),
        }
    }

    fn children(&self) -> std::borrow::Cow<[Self::Child]> {
        let v = match self {
            AstNode::Block(v) => v.clone(),
            AstNode::Function(_params, body) => vec![*body.clone()],
            AstNode::Call(variable, _params) => vec![*variable.clone()],
            AstNode::Assignment(ident, value) => vec![*ident.clone(), *value.clone()],
            AstNode::Variable(name, _is_local) => vec![*name.clone()],
            AstNode::Expression(expr) => vec![*expr.clone()],
            AstNode::Return(expr) => vec![*expr.clone()],
            AstNode::If(cond, block, else_block) => {
                if let Some(else_block) = else_block {
                    vec![*cond.clone(), *block.clone(), *else_block.clone()]
                } else {
                    vec![*cond.clone(), *block.clone()]
                }
            }
            _ => vec![],
        };

        std::borrow::Cow::from(v)
    }
}
