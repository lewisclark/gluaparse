use crate::error::Error;
use crate::lexer::Token;
use std::convert::From;
use std::fmt::{self, Display};

macro_rules! expect {
    ( $found:expr, $expected_str:expr, $( $expected:pat ),+ ) => {
        match $found {
            Some(t) => match t {
                $( $expected => Ok(t), )*
                t => Err(Error::new(format!("Expected {:?}, found {:?}", $expected_str, t))),
            },
            None => Err(Error::new(format!("Expected {:?}, found eof", $expected_str))),
        }
    };
}

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

    pub fn create(mut self) -> Result<AstNode, Error> {
        self.read_chunk()
    }

    fn read_func(&mut self) -> Result<AstNode, Error> {
        expect!(self.reader.next(), "Function", Token::Function)?;

        let is_local = match self.reader.peek(-2) {
            Some(t) => t == &Token::Local,
            None => false,
        };

        expect!(
            self.reader.peek(0),
            "LeftParen or Ident",
            Token::LeftParen,
            Token::Ident(_)
        )?;
        let is_anonymous = self.reader.peek(0).unwrap() == &Token::LeftParen;

        if is_anonymous {
            let params = self.read_params()?;
            let body = Box::new(self.read_block()?);

            Ok(AstNode::Function(params, body))
        } else {
            let ident = self.read_variable(Some(is_local))?;
            let params = self.read_params()?;
            let body = Box::new(self.read_block()?);

            let func = AstNode::Function(params, body);

            Ok(AstNode::Assignment(Box::new(ident), Box::new(func)))
        }
    }

    fn read_params(&mut self) -> Result<Vec<AstNode>, Error> {
        expect!(self.reader.next(), "LeftParen", Token::LeftParen)?;
        let params = self.read_comma_delimited()?;
        expect!(self.reader.next(), "RightParen", Token::RightParen)?;

        Ok(params)
    }

    fn read_comma_delimited(&mut self) -> Result<Vec<AstNode>, Error> {
        let mut exprs = Vec::new();

        while let Some(t) = self.reader.peek(0) {
            match t {
                Token::Comma => self.reader.consume(1),
                Token::RightParen | Token::RightSquareBracket | Token::RightCurlyBracket => break,
                _ => exprs.push(self.read_expression()?),
            }
        }

        Ok(exprs)
    }

    fn read_chunk(&mut self) -> Result<AstNode, Error> {
        self.read_block()
    }

    fn read_block(&mut self) -> Result<AstNode, Error> {
        let mut block = Vec::new();

        while let Some(token) = self.reader.peek(0) {
            match token {
                Token::Do => {
                    self.reader.consume(1);
                    block.push(self.read_block()?)
                }
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
                Token::End | Token::Else | Token::ElseIf => {
                    self.reader.consume(1);
                    break;
                }
                t => unimplemented!("{:?}", t),
            };
        }

        Ok(AstNode::Block(block))
    }

    fn read_return(&mut self) -> Result<AstNode, Error> {
        expect!(self.reader.next(), "Return", Token::Return)?;

        Ok(AstNode::Return(Box::new(self.read_expression()?)))
    }

    fn read_call(&mut self) -> Result<AstNode, Error> {
        let variable = self.read_variable(None)?;
        let params = self.read_params()?;

        Ok(AstNode::Call(Box::new(variable), params))
    }

    fn read_expression(&mut self) -> Result<AstNode, Error> {
        let mut prev: Option<AstNode> = None;

        while let Some(t) = self.reader.peek(0) {
            let expr = match t {
                Token::Function => self.read_func(),
                Token::Ident(_) => self.read_ident(),
                Token::Str(_) => self.read_str(),
                Token::Int(_) => self.read_int(),
                Token::Float(_) => self.read_float(),
                Token::True | Token::False => self.read_bool(),
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Caret
                | Token::Percent
                | Token::DotDot
                | Token::LeftAngleBracket
                | Token::LeftAngleBracketEqual
                | Token::RightAngleBracket
                | Token::RightAngleBracketEqual
                | Token::EqualEqual
                | Token::NotEqual
                | Token::And
                | Token::Or => self.read_binaryop(prev),
                Token::End | Token::Comma | Token::RightParen | Token::Then => break,
                _ => unimplemented!(),
            }?;

            prev = Some(expr);
        }

        Ok(AstNode::Expression(Box::new(prev.unwrap())))
    }

    fn read_binaryop(&mut self, prev: Option<AstNode>) -> Result<AstNode, Error> {
        let prev = match prev {
            Some(t) => Ok(Box::new(AstNode::Expression(Box::new(t)))),
            None => Err(Error::new(
                "Expected value before binary operator".to_string(),
            )),
        }?;

        let t = expect!(
            self.reader.next(),
            "Binary Operator",
            Token::Plus,
            Token::Minus,
            Token::Asterisk,
            Token::Slash,
            Token::Caret,
            Token::Percent,
            Token::DotDot,
            Token::LeftAngleBracket,
            Token::LeftAngleBracketEqual,
            Token::RightAngleBracket,
            Token::RightAngleBracketEqual,
            Token::EqualEqual,
            Token::NotEqual,
            Token::And,
            Token::Or
        )?;

        match t {
            Token::Plus => Ok(AstNode::Add(prev, Box::new(self.read_expression()?))),
            Token::Minus => Ok(AstNode::Subtract(prev, Box::new(self.read_expression()?))),
            Token::Asterisk => Ok(AstNode::Multiply(prev, Box::new(self.read_expression()?))),
            Token::Slash => Ok(AstNode::Divide(prev, Box::new(self.read_expression()?))),
            Token::Caret => Ok(AstNode::Exponentiate(
                prev,
                Box::new(self.read_expression()?),
            )),
            Token::Percent => Ok(AstNode::Modulo(prev, Box::new(self.read_expression()?))),
            Token::DotDot => Ok(AstNode::Concat(prev, Box::new(self.read_expression()?))),
            Token::LeftAngleBracket => {
                Ok(AstNode::LessThan(prev, Box::new(self.read_expression()?)))
            }
            Token::LeftAngleBracketEqual => Ok(AstNode::LessThanOrEqual(
                prev,
                Box::new(self.read_expression()?),
            )),
            Token::RightAngleBracket => Ok(AstNode::GreaterThan(
                prev,
                Box::new(self.read_expression()?),
            )),
            Token::RightAngleBracketEqual => Ok(AstNode::GreaterThanOrEqual(
                prev,
                Box::new(self.read_expression()?),
            )),
            Token::EqualEqual => Ok(AstNode::Equal(prev, Box::new(self.read_expression()?))),
            Token::NotEqual => Ok(AstNode::NotEqual(prev, Box::new(self.read_expression()?))),
            Token::And => Ok(AstNode::And(prev, Box::new(self.read_expression()?))),
            Token::Or => Ok(AstNode::Or(prev, Box::new(self.read_expression()?))),
            _ => unimplemented!(),
        }
    }

    fn read_bool(&mut self) -> Result<AstNode, Error> {
        let t = expect!(self.reader.next(), "Bool", Token::True, Token::False)?;

        match t {
            Token::True => Ok(AstNode::Bool(true)),
            Token::False => Ok(AstNode::Bool(false)),
            _ => panic!(),
        }
    }

    fn read_ident(&mut self) -> Result<AstNode, Error> {
        let ident = match expect!(self.reader.next(), "Ident", Token::Ident(_))? {
            Token::Ident(s) => s,
            _ => panic!(),
        };

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
        let s = match expect!(self.reader.next(), "Str", Token::Str(_))? {
            Token::Str(s) => s,
            _ => panic!(),
        };

        Ok(AstNode::Str(s.to_string()))
    }

    fn read_int(&mut self) -> Result<AstNode, Error> {
        let n = match expect!(self.reader.next(), "Int", Token::Int(_))? {
            Token::Int(n) => n,
            _ => panic!(),
        };

        Ok(AstNode::Int(*n))
    }

    fn read_float(&mut self) -> Result<AstNode, Error> {
        let f = match expect!(self.reader.next(), "Float", Token::Float(_))? {
            Token::Float(f) => f,
            _ => panic!(),
        };

        Ok(AstNode::Float(*f))
    }

    fn read_if(&mut self) -> Result<AstNode, Error> {
        expect!(self.reader.next(), "If", Token::If)?;
        let expr = self.read_expression()?;
        expect!(self.reader.next(), "Then", Token::Then)?;
        let block_true = self.read_block()?;

        match expect!(
            self.reader.peek(-1),
            "End, Else or ElseIf",
            Token::End,
            Token::Else,
            Token::ElseIf
        )? {
            Token::End => {
                let stub = AstNode::IfStub(Some(Box::new(expr)), Box::new(block_true));

                Ok(AstNode::If(vec![stub]))
            }
            Token::Else => {
                let block_else = self.read_block()?;
                let stub = AstNode::IfStub(None, Box::new(block_else));

                Ok(AstNode::If(vec![stub]))
            }
            Token::ElseIf => {
                let first_stub = AstNode::IfStub(Some(Box::new(expr)), Box::new(block_true));
                let mut stubs = vec![first_stub];

                loop {
                    match self.reader.peek(-1).unwrap() {
                        Token::ElseIf => {
                            let expr = self.read_expression()?;
                            expect!(self.reader.next(), "Then", Token::Then)?;
                            let block = self.read_block()?;

                            stubs.push(AstNode::IfStub(Some(Box::new(expr)), Box::new(block)));
                        }
                        Token::Else => {
                            let block = self.read_block()?;

                            stubs.push(AstNode::IfStub(None, Box::new(block)));
                        }
                        Token::End => break Ok(AstNode::If(stubs)),
                        _ => panic!(),
                    }
                }
            }
            _ => panic!(),
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

    /* left expr, right expr */
    Add(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    Subtract(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    Multiply(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    Divide(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    Exponentiate(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    Modulo(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    Concat(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    LessThan(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    LessThanOrEqual(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    GreaterThan(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    GreaterThanOrEqual(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    Equal(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    NotEqual(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    And(Box<AstNode>, Box<AstNode>),

    /* left expr, right expr */
    Or(Box<AstNode>, Box<AstNode>),

    /* cond, block - if cond is None then it is an else */
    IfStub(Option<Box<AstNode>>, Box<AstNode>),

    /* vec of if statements
     * a single item represents an `if ... then ... end`
     * two items may represent an if + elseif or an if + else for example,
     * based on the Option in IfStub */
    If(Vec<AstNode>),

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
            AstNode::Block(_) => write!(f, "Block"),
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
            AstNode::If(_stubs) => write!(f, "If"),
            AstNode::IfStub(_cond, _body) => write!(f, "IfStub"),
            AstNode::Add(_left, _right) => write!(f, "Add"),
            AstNode::Subtract(_left, _right) => write!(f, "Subtract"),
            AstNode::Multiply(_left, _right) => write!(f, "Multiply"),
            AstNode::Divide(_left, _right) => write!(f, "Divide"),
            AstNode::Exponentiate(_left, _right) => write!(f, "Exponentiate"),
            AstNode::Modulo(_left, _right) => write!(f, "Modulo"),
            AstNode::Concat(_left, _right) => write!(f, "Concat"),
            AstNode::LessThan(_left, _right) => write!(f, "LessThan"),
            AstNode::LessThanOrEqual(_left, _right) => write!(f, "LessThanOrEqual"),
            AstNode::GreaterThan(_left, _right) => write!(f, "GreaterThan"),
            AstNode::GreaterThanOrEqual(_left, _right) => write!(f, "GreaterThanOrEqual"),
            AstNode::Equal(_left, _right) => write!(f, "Equal"),
            AstNode::NotEqual(_left, _right) => write!(f, "NotEqual"),
            AstNode::And(_left, _right) => write!(f, "And"),
            AstNode::Or(_left, _right) => write!(f, "Or"),
            AstNode::Str(s) => write!(f, "Str {}", s),
            AstNode::Int(i) => write!(f, "Int {}", i),
            AstNode::Float(fl) => write!(f, "Float {}", fl),
            AstNode::Bool(b) => write!(f, "Bool {}", b),
            _ => write!(f, "Unknown"),
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
            AstNode::If(stubs) => stubs.to_vec(),
            AstNode::IfStub(cond, body) => {
                if let Some(cond) = cond {
                    vec![*cond.clone(), *body.clone()]
                } else {
                    vec![*body.clone()]
                }
            }
            AstNode::Add(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::Subtract(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::Multiply(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::Divide(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::Exponentiate(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::Modulo(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::Concat(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::LessThan(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::LessThanOrEqual(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::GreaterThan(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::GreaterThanOrEqual(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::Equal(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::NotEqual(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::And(left, right) => vec![*left.clone(), *right.clone()],
            AstNode::Or(left, right) => vec![*left.clone(), *right.clone()],
            _ => vec![],
        };

        std::borrow::Cow::from(v)
    }
}
