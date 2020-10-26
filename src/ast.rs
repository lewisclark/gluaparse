use crate::error::Error;
use crate::lexer::Token;
use std::convert::From;
use std::fmt::{self, Display};

type AstResult = Result<AstNode, Error>;
type VecAstResult = Result<Vec<AstNode>, Error>;

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

    pub fn create(mut self) -> AstResult {
        self.read_chunk()
    }

    fn read_func(&mut self) -> AstResult {
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
            let ident = Box::new(self.read_ident()?);

            let has_colon_op = match self.reader.peek(-2) {
                Some(t) => matches!(t, Token::Colon),
                None => false,
            };

            let mut params = self.read_params()?;

            if has_colon_op {
                params.insert(0, AstNode::Ident("self".to_string()));
            }

            let body = Box::new(self.read_block()?);

            let func = Box::new(AstNode::Function(params, body));

            Ok(match is_local {
                true => AstNode::Declaration(ident, Some(func)),
                false => AstNode::Assignment(ident, func),
            })
        }
    }

    fn read_params(&mut self) -> VecAstResult {
        expect!(self.reader.next(), "LeftParen", Token::LeftParen)?;
        let params = self.read_comma_delimited(AstConstructor::read_ident)?;
        expect!(self.reader.next(), "RightParen", Token::RightParen)?;

        Ok(params)
    }

    fn read_arguments(&mut self) -> VecAstResult {
        let t = expect!(
            self.reader.peek(0),
            "LeftParen/Str/LeftCurlyBracket",
            Token::LeftParen,
            Token::Str(_),
            Token::LeftCurlyBracket
        )?;

        Ok(match t {
            Token::LeftParen => {
                self.reader.consume(1);
                let args = self.read_comma_delimited(AstConstructor::read_expression)?;
                expect!(self.reader.next(), "RightParen", Token::RightParen)?;

                args
            }
            Token::Str(_) => vec![self.read_str()?],
            Token::LeftCurlyBracket => vec![self.read_table()?],
            _ => panic!(),
        })
    }

    fn read_comma_delimited<F>(&mut self, reader: F) -> VecAstResult
    where
        F: Fn(&mut Self) -> AstResult,
    {
        let mut exprs = Vec::new();

        while let Some(t) = self.reader.peek(0) {
            match t {
                Token::Comma => self.reader.consume(1),
                Token::DotDotDot => {
                    self.reader.consume(1);
                    exprs.push(AstNode::Vararg);

                    if matches!(self.reader.peek(0), Some(Token::Comma)) {
                        return Err(Error::new(
                            "Vararg (...) must be the final parameter".to_string(),
                        ));
                    }
                }
                Token::RightParen | Token::RightSquareBracket | Token::RightCurlyBracket => break,
                _ => exprs.push(reader(self)?),
            }
        }

        Ok(exprs)
    }

    fn read_chunk(&mut self) -> AstResult {
        self.read_block()
    }

    fn read_block(&mut self) -> AstResult {
        let mut block = Vec::new();
        let mut prev = None;

        while let Some(token) = self.reader.peek(0) {
            match token {
                Token::Comment(_) => self.reader.consume(1),
                Token::Do => {
                    self.reader.consume(1);
                    block.push(self.read_block()?);
                }
                Token::Local => self.reader.consume(1),
                Token::Function => block.push(self.read_func()?),
                Token::Ident(_) => prev = Some(self.read_ident()?),
                Token::LeftParen | Token::Str(_) | Token::LeftCurlyBracket => {
                    block.push(self.read_call(prev.clone().ok_or_else(|| {
                        Error::new("Expected ident before LeftParen/Str/LeftCurlyBracket for call, found nothing".to_string())
                    })?)?)
                }
                Token::Equal => {
                    block.push(self.read_assignment(prev.clone().ok_or_else(|| {
                        Error::new("Expected ident before Equal, found nothing".to_string())
                    })?)?)
                }
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

    fn read_assignment(&mut self, ident: AstNode) -> AstResult {
        expect!(self.reader.next(), "Equal", Token::Equal)?;
        let expr = self.read_expression()?;

        Ok(AstNode::Assignment(Box::new(ident), Box::new(expr)))
    }

    fn read_return(&mut self) -> AstResult {
        expect!(self.reader.next(), "Return", Token::Return)?;

        let expr = match self.reader.peek(0) {
            Some(t) => {
                if matches!(t, Token::End) {
                    None
                } else {
                    Some(Box::new(self.read_expression()?))
                }
            }
            None => panic!(),
        };

        Ok(AstNode::Return(expr))
    }

    fn read_call(&mut self, ident: AstNode) -> AstResult {
        let has_colon_op = match self.reader.peek(-2) {
            Some(t) => matches!(t, Token::Colon),
            None => false,
        };

        let mut args = self.read_arguments()?;

        if has_colon_op {
            args.insert(0, AstNode::Ident("self".to_string()));
        }

        Ok(AstNode::Call(Box::new(ident), args))
    }

    fn read_value(&mut self) -> AstResult {
        let t = expect!(
            self.reader.peek(0),
            "Value",
            Token::Function,
            Token::Ident(_),
            Token::Str(_),
            Token::Int(_),
            Token::Float(_),
            Token::True,
            Token::False,
            Token::Nil,
            Token::LeftCurlyBracket,
            Token::DotDotDot
        )?;

        match t {
            Token::Function => self.read_func(),
            Token::Ident(_) => self.read_ident(),
            Token::Str(_) => self.read_str(),
            Token::Int(_) => self.read_int(),
            Token::Float(_) => self.read_float(),
            Token::True | Token::False => self.read_bool(),
            Token::Nil => self.read_nil(),
            Token::LeftCurlyBracket => self.read_table(),
            Token::DotDotDot => {
                self.reader.consume(1);
                Ok(AstNode::Vararg)
            }
            _ => panic!(),
        }
    }

    fn read_table(&mut self) -> AstResult {
        expect!(
            self.reader.next(),
            "LeftCurlyBracket",
            Token::LeftCurlyBracket
        )?;

        let mut kv = Vec::new();

        while let Some(t) = self.reader.peek(0) {
            if matches!(t, Token::RightCurlyBracket) {
                break;
            } else {
                let node = match self.read_table_key()? {
                    Some(key) => {
                        expect!(self.reader.next(), "Equal", Token::Equal)?;
                        let val = self.read_value()?;

                        AstNode::KeyValue(Box::new(key), Box::new(val))
                    }
                    None => self.read_value()?,
                };

                if self.reader.peek(0) == Some(&Token::Comma) {
                    self.reader.consume(1);
                }

                kv.push(node);
            }
        }

        expect!(
            self.reader.next(),
            "RightCurlyBracket",
            Token::RightCurlyBracket
        )?;

        Ok(AstNode::Table(kv))
    }

    fn read_table_key(&mut self) -> Result<Option<AstNode>, Error> {
        match self.reader.peek(0).unwrap() {
            Token::LeftSquareBracket => {
                expect!(
                    self.reader.next(),
                    "LeftSquareBracket",
                    Token::LeftSquareBracket
                )?;
                let k = self.read_value()?;
                expect!(
                    self.reader.next(),
                    "RightSquareBracket",
                    Token::RightSquareBracket
                )?;

                Ok(Some(k))
            }
            Token::Ident(_) => {
                if matches!(self.reader.peek(1), Some(Token::Equal)) {
                    Ok(Some(self.read_ident()?))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    fn read_expression(&mut self) -> AstResult {
        match self.reader.peek(0).unwrap() {
            Token::Not | Token::Hashtag => self.read_unaryop(),
            Token::LeftParen => {
                self.reader.consume(1);
                let expr = self.read_expression()?;
                expect!(self.reader.next(), "RightParen", Token::RightParen)?;

                Ok(expr)
            }
            _ => {
                let left = self.read_value()?;

                match self.reader.peek(0) {
                    Some(t) => match t {
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
                        | Token::Or => self.read_binaryop(left),
                        _ => Ok(left),
                    },
                    None => Ok(left),
                }
            }
        }
    }

    fn read_binaryop(&mut self, left: AstNode) -> AstResult {
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

        let left = Box::new(left);

        match t {
            Token::Plus => Ok(AstNode::Add(left, Box::new(self.read_expression()?))),
            Token::Minus => Ok(AstNode::Subtract(left, Box::new(self.read_expression()?))),
            Token::Asterisk => Ok(AstNode::Multiply(left, Box::new(self.read_expression()?))),
            Token::Slash => Ok(AstNode::Divide(left, Box::new(self.read_expression()?))),
            Token::Caret => Ok(AstNode::Exponentiate(
                left,
                Box::new(self.read_expression()?),
            )),
            Token::Percent => Ok(AstNode::Modulo(left, Box::new(self.read_expression()?))),
            Token::DotDot => Ok(AstNode::Concat(left, Box::new(self.read_expression()?))),
            Token::LeftAngleBracket => {
                Ok(AstNode::LessThan(left, Box::new(self.read_expression()?)))
            }
            Token::LeftAngleBracketEqual => Ok(AstNode::LessThanOrEqual(
                left,
                Box::new(self.read_expression()?),
            )),
            Token::RightAngleBracket => Ok(AstNode::GreaterThan(
                left,
                Box::new(self.read_expression()?),
            )),
            Token::RightAngleBracketEqual => Ok(AstNode::GreaterThanOrEqual(
                left,
                Box::new(self.read_expression()?),
            )),
            Token::EqualEqual => Ok(AstNode::Equal(left, Box::new(self.read_expression()?))),
            Token::NotEqual => Ok(AstNode::NotEqual(left, Box::new(self.read_expression()?))),
            Token::And => Ok(AstNode::And(left, Box::new(self.read_expression()?))),
            Token::Or => Ok(AstNode::Or(left, Box::new(self.read_expression()?))),
            _ => panic!(),
        }
    }

    fn read_unaryop(&mut self) -> AstResult {
        let t = expect!(
            self.reader.next(),
            "Not/Hashtag",
            Token::Not,
            Token::Hashtag
        )?;

        Ok(match t {
            Token::Not => AstNode::Not(Box::new(self.read_expression()?)),
            Token::Hashtag => AstNode::Length(Box::new(self.read_expression()?)),
            _ => panic!(),
        })
    }

    fn read_bool(&mut self) -> AstResult {
        let t = expect!(self.reader.next(), "Bool", Token::True, Token::False)?;

        match t {
            Token::True => Ok(AstNode::Bool(true)),
            Token::False => Ok(AstNode::Bool(false)),
            _ => panic!(),
        }
    }

    fn read_nil(&mut self) -> AstResult {
        expect!(self.reader.next(), "Nil", Token::Nil)?;

        Ok(AstNode::Nil)
    }

    fn read_ident(&mut self) -> AstResult {
        let mut ident = None;
        let mut dot_present = false;

        loop {
            let t = expect!(
                self.reader.peek(0),
                "Ident/Dot/LeftSquareBracket",
                Token::Ident(_),
                Token::Dot,
                Token::LeftSquareBracket,
                Token::Colon
            );

            match t {
                Ok(t) => match t {
                    Token::Ident(s) => {
                        let s = s.to_string();

                        self.reader.consume(1);

                        if dot_present {
                            dot_present = false;

                            ident = Some(AstNode::Index(
                                Box::new(ident.unwrap()),
                                Box::new(AstNode::Ident(s)),
                            ));
                        } else {
                            ident = Some(AstNode::Ident(s));
                        }
                    }
                    Token::Dot | Token::Colon => {
                        self.reader.consume(1);
                        dot_present = true;
                    }
                    Token::LeftSquareBracket => {
                        ident = Some(AstNode::Index(
                            Box::new(ident.unwrap()),
                            Box::new(self.read_table_key()?.unwrap()),
                        ));
                    }
                    _ => panic!(),
                },
                Err(_) => break,
            }
        }

        Ok(ident.unwrap())
    }

    fn read_str(&mut self) -> AstResult {
        let s = match expect!(self.reader.next(), "Str", Token::Str(_))? {
            Token::Str(s) => s,
            _ => panic!(),
        };

        Ok(AstNode::Str(s.to_string()))
    }

    fn read_int(&mut self) -> AstResult {
        let n = match expect!(self.reader.next(), "Int", Token::Int(_))? {
            Token::Int(n) => n,
            _ => panic!(),
        };

        Ok(AstNode::Int(*n))
    }

    fn read_float(&mut self) -> AstResult {
        let f = match expect!(self.reader.next(), "Float", Token::Float(_))? {
            Token::Float(f) => f,
            _ => panic!(),
        };

        Ok(AstNode::Float(*f))
    }

    fn read_if(&mut self) -> AstResult {
        expect!(self.reader.next(), "If", Token::If)?;
        let if_expr = self.read_expression()?;
        expect!(self.reader.next(), "Then", Token::Then)?;
        let if_block = self.read_block()?;
        let if_stub = AstNode::IfStub(Some(Box::new(if_expr)), Box::new(if_block));

        match expect!(
            self.reader.peek(-1),
            "End, Else or ElseIf",
            Token::End,
            Token::Else,
            Token::ElseIf
        )? {
            Token::End => Ok(AstNode::If(vec![if_stub])),
            Token::Else => {
                let else_block = self.read_block()?;
                let else_stub = AstNode::IfStub(None, Box::new(else_block));

                Ok(AstNode::If(vec![if_stub, else_stub]))
            }
            Token::ElseIf => {
                let mut stubs = vec![if_stub];

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

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Block(Vec<AstNode>),

    /* params, body */
    Function(Vec<AstNode>, Box<AstNode>),

    /* ident, params */
    Call(Box<AstNode>, Vec<AstNode>),

    /* ident, value */
    /* A Declaration is a local variable declaration */
    Declaration(Box<AstNode>, Option<Box<AstNode>>),

    /* ident, value */
    Assignment(Box<AstNode>, Box<AstNode>),

    /* name */
    Ident(String),

    /* expr */
    Return(Option<Box<AstNode>>),

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

    /* expr */
    Not(Box<AstNode>),

    /* expr */
    Length(Box<AstNode>),

    /* table, key */
    Index(Box<AstNode>, Box<AstNode>),

    /* cond, block - if cond is None then it is an else */
    IfStub(Option<Box<AstNode>>, Box<AstNode>),

    /* vec of if statements
     * a single item represents an `if ... then ... end`
     * two items may represent an if + elseif or an if + else for example,
     * based on the Option in IfStub */
    If(Vec<AstNode>),

    /* key, val */
    KeyValue(Box<AstNode>, Box<AstNode>),

    /* key values */
    Table(Vec<AstNode>),

    Vararg,

    Str(String),
    Int(isize),
    Float(f64),
    Bool(bool),
    Nil,
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
            AstNode::Call(_ident, _params) => write!(f, "Call"),
            AstNode::Declaration(_ident, _value) => write!(f, "Declaration"),
            AstNode::Assignment(_ident, _value) => write!(f, "Assignment"),
            AstNode::Ident(name) => write!(f, "Ident {}", name),
            AstNode::Return(_expr) => write!(f, "Return"),
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
            AstNode::Not(_expr) => write!(f, "Not"),
            AstNode::Length(_expr) => write!(f, "Length"),
            AstNode::Index(_t, _k) => write!(f, "Index"),
            AstNode::IfStub(_cond, _body) => write!(f, "IfStub"),
            AstNode::If(_stubs) => write!(f, "If"),
            AstNode::KeyValue(_key, _value) => write!(f, "KeyValue"),
            AstNode::Table(_kv) => write!(f, "Table"),
            AstNode::Vararg => write!(f, "..."),
            AstNode::Str(s) => write!(f, "Str \"{}\"", s),
            AstNode::Int(i) => write!(f, "Int {}", i),
            AstNode::Float(fl) => write!(f, "Float {}", fl),
            AstNode::Bool(b) => write!(f, "Bool {}", b),
            AstNode::Nil => write!(f, "Nil"),
        }
    }

    fn children(&self) -> std::borrow::Cow<[Self::Child]> {
        let v = match self {
            AstNode::Block(v) => v.clone(),
            AstNode::Function(_params, body) => vec![*body.clone()],
            AstNode::Call(ident, params) => {
                let mut v = params.clone();
                v.insert(0, *ident.clone());

                v
            }
            AstNode::Declaration(ident, value) => match value {
                Some(v) => vec![*ident.clone(), *v.clone()],
                None => vec![*ident.clone()],
            },
            AstNode::Assignment(ident, value) => vec![*ident.clone(), *value.clone()],
            AstNode::Return(expr) => match expr {
                Some(expr) => vec![*expr.clone()],
                None => vec![],
            },
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
            AstNode::Not(expr) => vec![*expr.clone()],
            AstNode::Length(expr) => vec![*expr.clone()],
            AstNode::Index(t, k) => vec![*t.clone(), *k.clone()],
            AstNode::IfStub(cond, body) => {
                if let Some(cond) = cond {
                    vec![*cond.clone(), *body.clone()]
                } else {
                    vec![*body.clone()]
                }
            }
            AstNode::If(stubs) => stubs.to_vec(),
            AstNode::KeyValue(key, value) => vec![*key.clone(), *value.clone()],
            AstNode::Table(kv) => kv.clone(),
            _ => vec![],
        };

        std::borrow::Cow::from(v)
    }
}
