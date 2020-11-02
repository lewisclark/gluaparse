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

    fn pos(&self) -> usize {
        self.pos
    }

    fn set_pos(&mut self, pos: usize) {
        self.pos = pos
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
        Ok(self.read_chunk()?.unwrap())
    }

    pub fn read_chunk(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_chunk");

        let mut stats = Vec::new();

        while let Some(mut stat) = self.read_stat()? {
            if self.reader.peek(0) == Some(&Token::Semicolon) {
                self.reader.consume(1);
            }

            stats.append(&mut stat);
        }

        if let Some(last_stat) = self.read_laststat() {
            if self.reader.peek(0) == Some(&Token::Semicolon) {
                self.reader.consume(1);
            }

            stats.push(last_stat);
        }

        Ok(Some(AstNode::Block(stats)))
    }

    fn read_stat(&mut self) -> Result<Option<Vec<AstNode>>, Error> {
        println!("read_stat");

        if let Some(varlist) = self.read_varlist_assignment()? {
            Ok(Some(varlist))
        } else if let Some(call) = self.read_call()? {
            Ok(Some(vec![call]))
        //} else if let Some(_block) = self.read_do_block() {
        //} else if let Some(_loop) = self.read_loop() {
        //} else if let Some(_if) = self.read_if() {
        //} else if let Some(_func) = self.read_func() {
        } else {
            Ok(None)
        }
    }

    fn read_laststat(&mut self) -> Option<AstNode> {
        None
    }

    fn read_varlist_assignment(&mut self) -> Result<Option<Vec<AstNode>>, Error> {
        println!("read_varlist_assignment");

        let pos = self.reader.pos();
        let varlist = self.read_varlist()?;

        if varlist.is_empty() || !matches!(self.reader.peek(0), Some(&Token::Equal)) {
            self.reader.set_pos(pos);

            return Ok(None);
        }

        self.reader.consume(1);

        let explist = self.read_explist()?;
        let mut assignments = Vec::new();

        for (i, var) in varlist.iter().enumerate() {
            let val = explist.get(i).or(Some(&AstNode::Nil)).unwrap();

            assignments.push(AstNode::Assignment(
                Box::new(var.clone()),
                Box::new(val.clone()),
            ));
        }

        Ok(Some(assignments))
    }

    fn read_varlist(&mut self) -> Result<Vec<AstNode>, Error> {
        let mut vars = Vec::new();

        while let Some(var) = self.read_var()? {
            vars.push(var);

            if matches!(self.reader.peek(0), Some(&Token::Comma)) {
                self.reader.consume(1);
            } else {
                break;
            }
        }

        Ok(vars)
    }

    fn read_var(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_var");

        if let Some(name) = self.read_name() {
            Ok(Some(name))
        } else if let Some(prefix_exp) = self.read_prefix_exp()? {
            match self.reader.peek(0) {
                Some(t) => match t {
                    Token::LeftSquareBracket => {
                        self.reader.consume(1);
                        let exp = self.read_exp()?.unwrap();

                        expect!(self.reader.next(), "]", Token::RightSquareBracket)?;

                        Ok(Some(AstNode::Index(Box::new(prefix_exp), Box::new(exp))))
                    }
                    Token::Dot => {
                        self.reader.consume(1);
                        let name = self.read_name().unwrap();

                        Ok(Some(AstNode::Index(Box::new(prefix_exp), Box::new(name))))
                    }
                    _ => Ok(None),
                },
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn read_name(&mut self) -> Option<AstNode> {
        println!("read_name");

        match self.reader.peek(0) {
            Some(t) => match t {
                Token::Ident(s) => {
                    let s = s.to_string();
                    self.reader.consume(1);
                    Some(AstNode::Ident(s))
                }
                _ => None,
            },
            None => None,
        }
    }

    fn read_prefix_exp(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_prefix_exp");

        if !matches!(self.reader.peek(0), Some(&Token::Ident(_)) | Some(&Token::LeftParen)) {
            return Ok(None);
        }

        if let Some(var) = self.read_var()? {
            Ok(Some(var))
        } else if let Some(call) = self.read_call()? {
            Ok(Some(call))
        } else if matches!(self.reader.peek(0), Some(&Token::LeftParen)) {
            self.reader.consume(1);
            let exp = self.read_exp()?;
            expect!(self.reader.next(), ")", Token::RightParen)?;

            Ok(exp)
        } else {
            Ok(None)
        }
    }

    fn read_exp(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_exp");

        if let Some(nil) = self.read_nil() {
            Ok(Some(nil))
        } else if let Some(b) = self.read_bool() {
            Ok(Some(b))
        } else if let Some(n) = self.read_number() {
            Ok(Some(n))
        } else if let Some(s) = self.read_string() {
            Ok(Some(s))
        } else if let Some(vararg) = self.read_vararg() {
            Ok(Some(vararg))
        } else if let Some(func) = self.read_func() {
            Ok(Some(func))
        } else if let Some(prefix_exp) = self.read_prefix_exp()? {
            Ok(Some(prefix_exp))
        } else if let Some(table) = self.read_table_constructor() {
            Ok(Some(table))
        } else if let Some(binop) = self.read_binop() {
            Ok(Some(binop))
        } else if let Some(unaryop) = self.read_unaryop() {
            Ok(Some(unaryop))
        } else {
            Ok(None)
        }
    }

    fn read_explist(&mut self) -> Result<Vec<AstNode>, Error> {
        println!("read_explist");

        let mut exps = Vec::new();

        while let Some(exp) = self.read_exp()? {
            exps.push(exp);

            if matches!(self.reader.peek(0), Some(&Token::Comma)) {
                self.reader.consume(1);
            } else {
                break;
            }
        }

        Ok(exps)
    }

    fn read_call(&mut self) -> Result<Option<AstNode>, Error> {
        if let Some(prefix_exp) = self.read_prefix_exp()? {
            if matches!(self.reader.peek(0), Some(&Token::Semicolon)) {
                unimplemented!();
            } else {
                if let Some(args) = self.read_args()? {
                    return Ok(Some(AstNode::Call(Box::new(prefix_exp), args)));
                }
            }
        }

        Ok(None)
    }

    fn read_args(&mut self) -> Result<Option<Vec<AstNode>>, Error> {
        match self.reader.peek(0) {
            Some(Token::LeftParen) => {
                self.reader.consume(1);
                let explist = self.read_explist()?;
                expect!(self.reader.next(), "RightParen", Token::RightParen)?;

                Ok(Some(explist))
            }
            _ => {
                if let Some(table) = self.read_table_constructor() {
                    Ok(Some(vec![table]))
                } else if let Some(s) = self.read_string() {
                    Ok(Some(vec![s]))
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn read_do_block(&mut self) -> Option<AstNode> {
        None
    }

    fn read_loop(&mut self) -> Option<AstNode> {
        None
    }

    fn read_if(&mut self) -> Option<AstNode> {
        None
    }

    fn read_func(&mut self) -> Option<AstNode> {
        None
    }

    fn read_nil(&mut self) -> Option<AstNode> {
        if matches!(self.reader.peek(0), Some(&Token::Nil)) {
            self.reader.consume(1);

            Some(AstNode::Nil)
        } else {
            None
        }
    }

    fn read_bool(&mut self) -> Option<AstNode> {
        match self.reader.peek(0) {
            Some(t) => match t {
                Token::True => {
                    self.reader.consume(1);
                    Some(AstNode::Bool(true))
                }
                Token::False => {
                    self.reader.consume(1);
                    Some(AstNode::Bool(false))
                }
                _ => None,
            },
            None => None,
        }
    }

    fn read_number(&mut self) -> Option<AstNode> {
        match self.reader.peek(0) {
            Some(t) => match t {
                Token::Number(n) => {
                    let n = *n;
                    self.reader.consume(1);

                    Some(AstNode::Number(n))
                }
                _ => None,
            },
            None => None,
        }
    }

    fn read_string(&mut self) -> Option<AstNode> {
        match self.reader.peek(0) {
            Some(t) => match t {
                Token::Str(s) => {
                    let s = s.to_string();
                    self.reader.consume(1);

                    Some(AstNode::Str(s))
                }
                _ => None,
            },
            None => None,
        }
    }

    fn read_vararg(&mut self) -> Option<AstNode> {
        if matches!(self.reader.peek(0), Some(&Token::DotDotDot)) {
            self.reader.consume(1);

            Some(AstNode::Vararg)
        } else {
            None
        }
    }

    fn read_table_constructor(&mut self) -> Option<AstNode> {
        None
    }

    fn read_binop(&mut self) -> Option<AstNode> {
        None
    }

    fn read_unaryop(&mut self) -> Option<AstNode> {
        None
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
    Declaration(Box<AstNode>, Box<AstNode>),

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

    /* keyvalues or values */
    Table(Vec<AstNode>),

    Vararg,

    Str(String),
    Number(f64),
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
            AstNode::Number(fl) => write!(f, "Number {}", fl),
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
            AstNode::Declaration(ident, value) => vec![*ident.clone(), *value.clone()],
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
