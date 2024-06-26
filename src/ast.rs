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

macro_rules! binaryop {
    ( $self:expr, $left:expr, $(($token:pat, $node:expr, $str:expr)),+ ) => {
        match $self.peek(0) {
            $( Some($token) => {
                $self.consume(1);

                Ok(Some($node(
                    Box::new($left),
                    Box::new($self.read_exp()?.ok_or_else(|| {
                        Error::new(format!("Expected expression after '{}'", $str))
                    })?),
                )))
            } )*
            _ => Ok(None)
        }
    }
}

/* ---------- AstConstructor ---------- */

pub struct AstConstructor<'a> {
    tokens: &'a [Token<'a>],
    pos: usize,
}

impl<'a> AstConstructor<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
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

    pub fn create(mut self) -> Result<AstNode, Error> {
        Ok(self.read_block()?)
    }

    pub fn read_block(&mut self) -> Result<AstNode, Error> {
        println!("read_block {:?}", self);

        let mut stats = Vec::new();

        while let Some(mut stat) = self.read_stat()? {
            if self.peek(0) == Some(&Token::Semicolon) {
                self.consume(1);
            }

            stats.append(&mut stat);
        }

        if let Some(last_stat) = self.read_laststat()? {
            if self.peek(0) == Some(&Token::Semicolon) {
                self.consume(1);
            }

            stats.push(last_stat);
        }

        Ok(AstNode::Block(stats))
    }

    fn read_stat(&mut self) -> Result<Option<Vec<AstNode>>, Error> {
        println!("read_stat {:?}", self);

        if let Some(varlist) = self.read_varlist_assignment()? {
            Ok(Some(varlist))
        } else if let Some(call) = self.read_call()? {
            Ok(Some(vec![call]))
        } else if let Some(do_block) = self.read_do_block()? {
            Ok(Some(vec![do_block]))
        } else if let Some(loop_node) = self.read_loop()? {
            Ok(Some(vec![loop_node]))
        } else if let Some(if_node) = self.read_if()? {
            Ok(Some(vec![if_node]))
        //} else if let Some(_func) = self.read_func()? {
        } else {
            Ok(None)
        }
    }

    fn read_laststat(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_laststat {:?}", self);

        match self.peek(0) {
            Some(Token::Return) => {
                self.consume(1);

                Ok(Some(AstNode::Return(self.read_explist()?)))
            }
            Some(Token::Break) => {
                self.consume(1);

                Ok(Some(AstNode::Break))
            }
            _ => Ok(None),
        }
    }

    fn read_varlist_assignment(&mut self) -> Result<Option<Vec<AstNode>>, Error> {
        println!("read_varlist_assignment {:?}", self);

        let pos = self.pos();
        let varlist = self.read_varlist()?;

        if varlist.is_empty() || !matches!(self.peek(0), Some(&Token::Equal)) {
            self.set_pos(pos);

            return Ok(None);
        }

        self.consume(1);

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
        println!("read_varlist {:?}", self);

        let mut vars = Vec::new();

        while let Some(var) = self.read_var()? {
            vars.push(var);

            if matches!(self.peek(0), Some(&Token::Comma)) {
                self.consume(1);
            } else {
                break;
            }
        }

        Ok(vars)
    }

    fn read_var(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_var {:?}", self);

        if !matches!(self.peek(0), Some(Token::Ident(_)) | Some(Token::LeftParen)) {
            return Ok(None);
        }

        let pos = self.pos();

        if let Some(name) = self.read_name() {
            Ok(Some(name))
        } else if let Some(prefix_exp) = self.read_prefix_exp()? {
            match self.peek(0) {
                Some(t) => match t {
                    Token::LeftSquareBracket => {
                        self.consume(1);
                        let exp = self.read_exp()?.ok_or_else(|| {
                            Error::new("Expected expression after '['".to_string())
                        })?;

                        expect!(self.next(), "]", Token::RightSquareBracket)?;

                        Ok(Some(AstNode::Index(Box::new(prefix_exp), Box::new(exp))))
                    }
                    Token::Dot => {
                        self.consume(1);
                        let name = self
                            .read_name()
                            .ok_or_else(|| Error::new("Expected ident after '.'".to_string()))?;

                        Ok(Some(AstNode::Index(Box::new(prefix_exp), Box::new(name))))
                    }
                    _ => {
                        self.set_pos(pos);
                        Ok(None)
                    }
                },
                None => {
                    self.set_pos(pos);
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }

    fn read_name(&mut self) -> Option<AstNode> {
        println!("read_name {:?}", self);

        match self.peek(0) {
            Some(Token::Ident(s)) => {
                let s = s.to_string();
                self.consume(1);
                Some(AstNode::Ident(s))
            }
            _ => None,
        }
    }

    fn read_namelist(&mut self) -> Vec<AstNode> {
        println!("read_namelist {:?}", self);

        let mut namelist = Vec::new();

        while let Some(name) = self.read_name() {
            namelist.push(name);

            if matches!(self.peek(0), Some(&Token::Comma)) {
                self.consume(1);
            } else {
                break;
            }
        }

        namelist
    }

    fn read_prefix_exp(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_prefix_exp {:?}", self);

        if !matches!(self.peek(0), Some(&Token::Ident(_)) | Some(&Token::LeftParen)) {
            return Ok(None);
        }

        if let Some(var) = self.read_var()? {
            Ok(Some(var))
        } else if let Some(call) = self.read_call()? {
            Ok(Some(call))
        } else if matches!(self.peek(0), Some(&Token::LeftParen)) {
            self.consume(1);
            let exp = self.read_exp()?;
            expect!(self.next(), ")", Token::RightParen)?;

            Ok(exp)
        } else {
            Ok(None)
        }
    }

    fn read_exp(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_exp {:?}", self);

        let exp = if let Some(nil) = self.read_nil() {
            Some(nil)
        } else if let Some(b) = self.read_bool() {
            Some(b)
        } else if let Some(n) = self.read_number() {
            Some(n)
        } else if let Some(s) = self.read_string() {
            Some(s)
        } else if let Some(vararg) = self.read_vararg() {
            Some(vararg)
        } else if let Some(func) = self.read_func()? {
            Some(func)
        } else if let Some(prefix_exp) = self.read_prefix_exp()? {
            Some(prefix_exp)
        } else if let Some(table) = self.read_table_constructor()? {
            Some(table)
        } else if let Some(unaryop) = self.read_unaryop()? {
            Some(unaryop)
        } else {
            None
        };

        match exp {
            Some(exp) => {
                if self.is_binaryop() {
                    self.read_binaryop(exp)
                } else {
                    Ok(Some(exp))
                }
            }
            None => Ok(None),
        }
    }

    fn read_explist(&mut self) -> Result<Vec<AstNode>, Error> {
        println!("read_explist {:?}", self);

        let mut exps = Vec::new();

        while let Some(exp) = self.read_exp()? {
            exps.push(exp);

            if matches!(self.peek(0), Some(&Token::Comma)) {
                self.consume(1);
            } else {
                break;
            }
        }

        Ok(exps)
    }

    fn read_call(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_call {:?}", self);

        if let Some(prefix_exp) = self.read_prefix_exp()? {
            if matches!(self.peek(0), Some(&Token::Semicolon)) {
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
        println!("read_args {:?}", self);

        match self.peek(0) {
            Some(Token::LeftParen) => {
                self.consume(1);
                let explist = self.read_explist()?;
                expect!(self.next(), "RightParen", Token::RightParen)?;

                Ok(Some(explist))
            }
            _ => {
                if let Some(table) = self.read_table_constructor()? {
                    Ok(Some(vec![table]))
                } else if let Some(s) = self.read_string() {
                    Ok(Some(vec![s]))
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn read_do_block(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_do_block {:?}", self);

        if matches!(self.peek(0), Some(&Token::Do)) {
            self.consume(1);
            let block = self.read_block()?;
            expect!(self.next(), "End", Token::End)?;

            Ok(Some(block))
        } else {
            Ok(None)
        }
    }

    fn read_loop(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_loop {:?}", self);

        if let Some(loop_while) = self.read_loop_while()? {
            Ok(Some(loop_while))
        } else if let Some(loop_repeat) = self.read_loop_repeat()? {
            Ok(Some(loop_repeat))
        } else if let Some(loop_for) = self.read_loop_for()? {
            Ok(Some(loop_for))
        } else {
            Ok(None)
        }
    }

    fn read_loop_while(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_loop_while {:?}", self);

        if matches!(self.peek(0), Some(&Token::While)) {
            self.consume(1);

            let cond = self
                .read_exp()?
                .ok_or_else(|| Error::new("Expected expression after 'While'".to_string()))?;

            expect!(self.next(), "Do", Token::Do)?;
            let block = self.read_block()?;
            expect!(self.next(), "End", Token::End)?;

            Ok(Some(AstNode::WhileLoop(Box::new(cond), Box::new(block))))
        } else {
            Ok(None)
        }
    }

    fn read_loop_repeat(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_loop_repeat {:?}", self);

        if matches!(self.peek(0), Some(&Token::Repeat)) {
            self.consume(1);

            let block = self.read_block()?;

            expect!(self.next(), "Until", Token::Until)?;

            let cond = self
                .read_exp()?
                .ok_or_else(|| Error::new("Expected expression after 'Until'".to_string()))?;

            Ok(Some(AstNode::RepeatLoop(Box::new(block), Box::new(cond))))
        } else {
            Ok(None)
        }
    }

    fn read_loop_for(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_loop_for {:?}", self);

        Ok(None)
    }

    fn read_if(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_if {:?}", self);

        if matches!(self.peek(0), Some(Token::If)) {
            let mut if_stubs = vec![self.read_if_stub()?];

            while let Some(t) = self.peek(0) {
                if matches!(t, Token::ElseIf) {
                    if_stubs.push(self.read_if_stub()?);
                } else {
                    break;
                }
            }

            if matches!(self.peek(0), Some(Token::Else)) {
                if_stubs.push(self.read_if_stub()?);
            }

            expect!(self.next(), "end", Token::End)?;

            Ok(Some(AstNode::If(if_stubs)))
        } else {
            Ok(None)
        }
    }

    fn read_if_stub(&mut self) -> Result<AstNode, Error> {
        println!("read_if_stub {:?}", self);

        match self.peek(0) {
            Some(Token::If) | Some(Token::ElseIf) => {
                self.consume(1);

                let cond = self.read_exp()?.ok_or_else(|| {
                    Error::new("Expected expression for if/elseif condition".to_string())
                })?;

                expect!(self.next(), "then", Token::Then)?;

                let block = self.read_block()?;

                Ok(AstNode::IfStub(Some(Box::new(cond)), Box::new(block)))
            }
            Some(Token::Else) => {
                self.consume(1);

                Ok(AstNode::IfStub(None, Box::new(self.read_block()?)))
            }
            _ => Err(Error::new(
                "Expected if/elseif/else for if stub".to_string(),
            )),
        }
    }

    fn read_func(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_func {:?}", self);

        if matches!(self.peek(0), Some(&Token::Function)) {
            self.consume(1);

            if let Some((parlist, body)) = self.read_funcbody()? {
                Ok(Some(AstNode::Function(parlist, Box::new(body))))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn read_funcbody(&mut self) -> Result<Option<(Vec<AstNode>, AstNode)>, Error> {
        println!("read_funcbody {:?}", self);

        if matches!(self.peek(0), Some(&Token::LeftParen)) {
            self.consume(1);
            let parlist = self.read_parlist();
            expect!(self.next(), "RightParen", Token::RightParen)?;
            let block = self.read_block()?;
            expect!(self.next(), "End", Token::End)?;

            Ok(Some((parlist, block)))
        } else {
            Ok(None)
        }
    }

    fn read_parlist(&mut self) -> Vec<AstNode> {
        println!("read_parlist {:?}", self);

        if matches!(self.peek(0), Some(&Token::DotDotDot)) {
            self.consume(1);

            vec![AstNode::Vararg]
        } else {
            let mut namelist = self.read_namelist();

            if matches!(self.peek(0), Some(&Token::DotDotDot)) {
                self.consume(1);
                namelist.push(AstNode::Vararg);
            }

            namelist
        }
    }

    fn read_nil(&mut self) -> Option<AstNode> {
        println!("read_nil {:?}", self);

        if matches!(self.peek(0), Some(&Token::Nil)) {
            self.consume(1);

            Some(AstNode::Nil)
        } else {
            None
        }
    }

    fn read_bool(&mut self) -> Option<AstNode> {
        println!("read_bool {:?}", self);

        match self.peek(0) {
            Some(t) => match t {
                Token::True => {
                    self.consume(1);
                    Some(AstNode::Bool(true))
                }
                Token::False => {
                    self.consume(1);
                    Some(AstNode::Bool(false))
                }
                _ => None,
            },
            None => None,
        }
    }

    fn read_number(&mut self) -> Option<AstNode> {
        println!("read_number {:?}", self);

        match self.peek(0) {
            Some(t) => match t {
                Token::Number(n) => {
                    let n = *n;
                    self.consume(1);

                    Some(AstNode::Number(n))
                }
                _ => None,
            },
            None => None,
        }
    }

    fn read_string(&mut self) -> Option<AstNode> {
        println!("read_string {:?}", self);

        match self.peek(0) {
            Some(t) => match t {
                Token::Str(s) => {
                    let s = s.to_string();
                    self.consume(1);

                    Some(AstNode::Str(s))
                }
                _ => None,
            },
            None => None,
        }
    }

    fn read_vararg(&mut self) -> Option<AstNode> {
        println!("read_vararg {:?}", self);

        if matches!(self.peek(0), Some(&Token::DotDotDot)) {
            self.consume(1);

            Some(AstNode::Vararg)
        } else {
            None
        }
    }

    fn read_table_constructor(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_table_constructor {:?}", self);

        if matches!(self.peek(0), Some(&Token::LeftCurlyBracket)) {
            self.consume(1);

            let table = AstNode::Table(self.read_fieldlist()?);

            expect!(self.next(), "}", Token::RightCurlyBracket)?;

            Ok(Some(table))
        } else {
            Ok(None)
        }
    }

    fn read_fieldlist(&mut self) -> Result<Vec<AstNode>, Error> {
        println!("read_fieldlist {:?}", self);

        let mut fieldlist = Vec::new();

        while let Some(field) = self.read_field()? {
            fieldlist.push(field);

            if matches!(self.peek(0), Some(Token::Comma) | Some(Token::Semicolon)) {
                self.consume(1);
            } else {
                break;
            }
        }

        Ok(fieldlist)
    }

    fn read_field(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_field {:?}", self);

        match self.peek(0) {
            Some(Token::LeftSquareBracket) => {
                self.consume(1);

                let key = self
                    .read_exp()?
                    .ok_or_else(|| Error::new("Expected table key".to_string()))?;

                expect!(self.next(), "]", Token::RightSquareBracket)?;
                expect!(self.next(), "=", Token::Equal)?;

                let val = self
                    .read_exp()?
                    .ok_or_else(|| Error::new("Expected table value".to_string()))?;

                Ok(Some(AstNode::KeyValue(Box::new(key), Box::new(val))))
            }
            _ => {
                if matches!(self.peek(0), Some(&Token::Ident(_)))
                    && matches!(self.peek(1), Some(&Token::Equal))
                {
                    let key = self.read_name().unwrap();

                    self.consume(1);

                    let val = self
                        .read_exp()?
                        .ok_or_else(|| Error::new("Expected table value".to_string()))?;

                    Ok(Some(AstNode::KeyValue(Box::new(key), Box::new(val))))
                } else {
                    self.read_exp()
                }
            }
        }
    }

    fn read_binaryop(&mut self, left: AstNode) -> Result<Option<AstNode>, Error> {
        println!("read_binaryop {:?}", self);

        binaryop!(
            self,
            left,
            (Token::Plus, AstNode::Add, "+"),
            (Token::Minus, AstNode::Subtract, "-"),
            (Token::Asterisk, AstNode::Multiply, "*"),
            (Token::Slash, AstNode::Divide, "/"),
            (Token::Caret, AstNode::Exponentiate, "^"),
            (Token::Percent, AstNode::Modulo, "%"),
            (Token::DotDot, AstNode::Concat, ".."),
            (Token::LeftAngleBracket, AstNode::LessThan, "<"),
            (Token::LeftAngleBracketEqual, AstNode::LessThanOrEqual, "<="),
            (Token::RightAngleBracket, AstNode::GreaterThan, ">"),
            (
                Token::RightAngleBracketEqual,
                AstNode::GreaterThanOrEqual,
                ">="
            ),
            (Token::EqualEqual, AstNode::Equal, "=="),
            (Token::NotEqual, AstNode::NotEqual, "~=/!="),
            (Token::And, AstNode::And, "and/&&"),
            (Token::Or, AstNode::Or, "or/||")
        )
    }

    fn read_unaryop(&mut self) -> Result<Option<AstNode>, Error> {
        println!("read_unaryop {:?}", self);

        if self.is_unaryop() {
            let op = self.next().unwrap();

            Ok(Some(match op {
                Token::Minus => AstNode::Negate(Box::new(if self.is_unaryop() {
                    self.read_unaryop()?.unwrap()
                } else {
                    self.read_exp()?.ok_or_else(|| {
                        Error::new("Expected expression after unary op '-'".to_string())
                    })?
                })),
                Token::Not => AstNode::Not(Box::new(if self.is_unaryop() {
                    self.read_unaryop()?.unwrap()
                } else {
                    self.read_exp()?.ok_or_else(|| {
                        Error::new("Expected expression after unary op 'not'".to_string())
                    })?
                })),
                Token::Hashtag => AstNode::Length(Box::new(if self.is_unaryop() {
                    self.read_unaryop()?.unwrap()
                } else {
                    self.read_exp()?.ok_or_else(|| {
                        Error::new("Expected expression after unary op '#'".to_string())
                    })?
                })),
                _ => panic!(),
            }))
        } else {
            Ok(None)
        }
    }

    fn is_unaryop(&self) -> bool {
        matches!(
            self.peek(0),
            Some(Token::Minus) | Some(Token::Not) | Some(Token::Hashtag)
        )
    }

    fn is_binaryop(&self) -> bool {
        matches!(
            self.peek(0),
            Some(Token::Plus)
                | Some(Token::Minus)
                | Some(Token::Asterisk)
                | Some(Token::Slash)
                | Some(Token::Caret)
                | Some(Token::Percent)
                | Some(Token::DotDot)
                | Some(Token::LeftAngleBracket)
                | Some(Token::LeftAngleBracketEqual)
                | Some(Token::RightAngleBracket)
                | Some(Token::RightAngleBracketEqual)
                | Some(Token::EqualEqual)
                | Some(Token::NotEqual)
                | Some(Token::And)
                | Some(Token::Or)
        )
    }
}

impl<'a> std::fmt::Debug for AstConstructor<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.pos, self.peek(0))
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
    Return(Vec<AstNode>),

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
    Negate(Box<AstNode>),

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

    /* cond, block */
    WhileLoop(Box<AstNode>, Box<AstNode>),

    /* block, cond */
    RepeatLoop(Box<AstNode>, Box<AstNode>),

    /* key, val */
    KeyValue(Box<AstNode>, Box<AstNode>),

    /* keyvalues or values */
    Table(Vec<AstNode>),

    Vararg,
    Break,

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
            AstNode::Return(_exprs) => write!(f, "Return"),
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
            AstNode::Negate(_expr) => write!(f, "Negate"),
            AstNode::Not(_expr) => write!(f, "Not"),
            AstNode::Length(_expr) => write!(f, "Length"),
            AstNode::Index(_t, _k) => write!(f, "Index"),
            AstNode::IfStub(_cond, _body) => write!(f, "IfStub"),
            AstNode::If(_stubs) => write!(f, "If"),
            AstNode::WhileLoop(_cond, _block) => write!(f, "WhileLoop"),
            AstNode::RepeatLoop(_block, _cond) => write!(f, "RepeatLoop"),
            AstNode::KeyValue(_key, _value) => write!(f, "KeyValue"),
            AstNode::Table(_kv) => write!(f, "Table"),
            AstNode::Vararg => write!(f, "..."),
            AstNode::Break => write!(f, "Break"),
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
            AstNode::Return(exprs) => exprs.clone(),
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
            AstNode::Negate(expr) => vec![*expr.clone()],
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
            AstNode::WhileLoop(cond, block) => vec![*cond.clone(), *block.clone()],
            AstNode::RepeatLoop(block, cond) => vec![*block.clone(), *cond.clone()],
            AstNode::KeyValue(key, value) => vec![*key.clone(), *value.clone()],
            AstNode::Table(kv) => kv.clone(),
            _ => vec![],
        };

        std::borrow::Cow::from(v)
    }
}
