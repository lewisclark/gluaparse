use crate::error::Error;
use std::str::FromStr;

/* ---------- Reader ---------- */

// This doesn't support iterating over grapheme clusters so unicode stuff probably won't be parsed nicely
#[derive(Debug)]
struct Reader<'a> {
    code: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    pub fn from_source(code: &'a str) -> Self {
        Reader {
            code: code.as_bytes(),
            pos: 0,
        }
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.code.len()
    }

    pub fn char(&mut self) -> char {
        let c = self.code[self.pos] as char;
        self.pos += 1;

        c
    }

    pub fn peek(&self) -> char {
        self.code[self.pos] as char
    }

    pub fn advance(&mut self, amount: usize) {
        self.pos += amount;
    }

    pub fn read_until<F>(&mut self, shift: isize, f: F) -> &'a str
    where
        F: Fn(char, Option<char>) -> bool,
    {
        let start = (self.pos as isize + shift) as usize;

        loop {
            let c = self.code[self.pos] as char;
            let cnext = match self.pos + 1 >= self.code.len() {
                true => None,
                false => Some(self.code[self.pos + 1] as char),
            };

            if f(c, cnext) {
                break;
            }

            self.pos += 1;
        }

        std::str::from_utf8(&self.code[start..self.pos]).unwrap() // Bad unwrap
    }

    pub fn read_until_delim(&mut self, shift: isize, delim: char) -> &'a str {
        self.read_until(shift, |c, _| c == delim)
    }

    pub fn read_until_nonalphanumeric(&mut self, shift: isize) -> &'a str {
        self.read_until(shift, |c, _| !c.is_alphanumeric())
    }
}

/* ---------- Lexer ---------- */

#[derive(Debug)]
pub struct Lexer<'a> {
    tokens: Vec<Token<'a>>,
    reader: Reader<'a>,
}

impl<'a> Lexer<'a> {
    pub fn from_source(code: &'a str) -> Self {
        Self {
            tokens: Vec::new(),
            reader: Reader::from_source(code),
        }
    }

    pub fn tokens(&self) -> &Vec<Token<'a>> {
        &self.tokens
    }

    fn read_num(&mut self, c: char) -> Result<Token<'a>, Error> {
        if c == '0' && self.reader.peek().to_ascii_lowercase() == 'x' {
            self.reader.advance(1);
            let num = self.reader.read_until(0, |c, _| !c.is_digit(16));

            Ok(Token::Int(
                isize::from_str_radix(num, 16).map_err(Error::from_err)?,
            ))
        } else {
            let num = self.reader.read_until(-1, |c, _| {
                !c.is_digit(10) && c.to_ascii_lowercase() != 'e' && c != '-' && c != '+' && c != '.'
            });

            if num
                .to_ascii_lowercase()
                .find(|c| c == 'e' || c == '.')
                .is_some()
            {
                Ok(Token::Float(f64::from_str(num).map_err(Error::from_err)?))
            } else {
                Ok(Token::Int(
                    isize::from_str_radix(num, 10).map_err(Error::from_err)?,
                ))
            }
        }
    }

    fn read_comment_single(&mut self) -> Token<'a> {
        let s = self
            .reader
            .read_until(-1, |c, cnext| c == '\n' || cnext.is_none());

        self.reader.advance(1);

        Token::Comment(s)
    }

    fn read_comment_multi(&mut self, is_cstyle: bool) -> Token<'a> {
        self.reader.advance(1);

        let s = if is_cstyle {
            self.reader.read_until(0, |c, cnext| {
                c == '*'
                    && match cnext {
                        Some(c) => c == '/',
                        None => false,
                    }
            })
        } else {
            self.reader.read_until(0, |c, cnext| {
                c == ']'
                    && match cnext {
                        Some(c) => c == ']',
                        None => false,
                    }
            })
        };

        self.reader.advance(2);

        Token::Comment(s)
    }

    pub fn lex(mut self) -> Result<Self, Error> {
        while !self.reader.eof() {
            let c = self.reader.char();

            if c.is_whitespace() || c == ';' {
                continue;
            }

            let token = match c {
                ',' => Token::Comma,
                '+' => {
                    if self.reader.peek().is_digit(10) {
                        self.read_num(c)?
                    } else {
                        Token::Plus
                    }
                }
                '-' => {
                    let p = self.reader.peek();

                    if p.is_digit(10) {
                        self.read_num(c)?
                    } else if p == '-' {
                        self.reader.advance(1);

                        if self.reader.char() == '[' && self.reader.peek() == '[' {
                            self.read_comment_multi(false)
                        } else {
                            self.read_comment_single()
                        }
                    } else {
                        Token::Minus
                    }
                }
                '*' => Token::Asterisk,
                '/' => {
                    let p = self.reader.peek();

                    if p == '/' {
                        self.reader.advance(2);
                        self.read_comment_single()
                    } else if p == '*' {
                        self.read_comment_multi(true)
                    } else {
                        Token::Slash
                    }
                }
                '^' => Token::Caret,
                '%' => Token::Percent,
                '{' => Token::LeftCurlyBracket,
                '}' => Token::RightCurlyBracket,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '#' => Token::Hashtag,
                '<' => {
                    if self.reader.peek() == '=' {
                        self.reader.advance(1);

                        Token::LeftAngleBracketEqual
                    } else {
                        Token::LeftAngleBracket
                    }
                }
                '>' => {
                    if self.reader.peek() == '=' {
                        self.reader.advance(1);

                        Token::RightAngleBracketEqual
                    } else {
                        Token::RightAngleBracket
                    }
                }
                '~' => {
                    if self.reader.char() != '=' {
                        return Err(Error::new("Expected = after ~".to_string()));
                    }

                    Token::NotEqual
                }
                '.' => {
                    let p = self.reader.peek();

                    if p == '.' {
                        self.reader.advance(1);

                        if self.reader.peek() == '.' {
                            self.reader.advance(1);

                            Token::DotDotDot
                        } else {
                            Token::DotDot
                        }
                    } else if p.is_digit(10) {
                        self.read_num(c)?
                    } else {
                        Token::Dot
                    }
                }
                ':' => Token::Colon,
                '=' => {
                    if self.reader.peek() == '=' {
                        self.reader.advance(1);

                        Token::EqualEqual
                    } else {
                        Token::Equal
                    }
                }
                '\"' | '\'' => {
                    let s = self.reader.read_until_delim(0, c);

                    if self.reader.char() == c {
                        Token::Str(s)
                    } else {
                        return Err(Error::new(format!("Expected string end delimiter {}", c)));
                    }
                }
                '[' => {
                    if self.reader.peek() == '[' {
                        self.reader.advance(1);
                        let s = self.reader.read_until(0, |c, cnext| {
                            c == ']'
                                && match cnext {
                                    Some(c) => c == ']',
                                    None => false,
                                }
                        });
                        self.reader.advance(2);

                        Token::Str(s)
                    } else {
                        Token::LeftSquareBracket
                    }
                }
                ']' => Token::RightSquareBracket,
                '!' => {
                    if self.reader.peek() == '=' {
                        self.reader.advance(1);

                        Token::NotEqual
                    } else {
                        Token::Not
                    }
                }
                '&' => {
                    if self.reader.char() == '&' {
                        Token::And
                    } else {
                        return Err(Error::new("Expected & after &".to_string()));
                    }
                }
                '|' => {
                    if self.reader.char() == '|' {
                        Token::Or
                    } else {
                        return Err(Error::new("Expected | after |".to_string()));
                    }
                }
                _ => {
                    if c.is_digit(10) {
                        self.read_num(c)?
                    } else {
                        // Read keyword or iden
                        let word = self.reader.read_until_nonalphanumeric(-1);

                        match word {
                            "do" => Token::Do,
                            "while" => Token::While,
                            "end" => Token::End,
                            "for" => Token::For,
                            "in" => Token::In,
                            "repeat" => Token::Repeat,
                            "until" => Token::Until,
                            "continue" => Token::Continue,
                            "break" => Token::Break,
                            "if" => Token::If,
                            "else" => Token::Else,
                            "elseif" => Token::ElseIf,
                            "then" => Token::Then,
                            "local" => Token::Local,
                            "function" => Token::Function,
                            "return" => Token::Return,
                            "goto" => Token::Goto,
                            "nil" => Token::Nil,
                            "false" => Token::False,
                            "true" => Token::True,
                            "and" => Token::And,
                            "or" => Token::Or,
                            "not" => Token::Not,
                            _ => Token::Ident(word),
                        }
                    }
                }
            };

            self.tokens.push(token);
        }

        Ok(self)
    }
}

/* ---------- Token ---------- */

#[derive(Debug)]
pub enum Token<'a> {
    // Keywords
    Do,
    While,
    End,
    For,
    In,
    Repeat,
    Until,
    Continue, // GLua specific
    Break,
    If,
    Else,
    ElseIf,
    Then,
    Local,
    Function,
    Return,
    Goto,
    Nil,
    False,
    True,

    // Symbols
    Comma,
    Equal,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftParen,
    RightParen,
    LeftSquareBracket,
    RightSquareBracket,
    Dot,
    DotDotDot,
    Colon,

    // Binary operators
    Plus,
    Minus, // Also a unary operator
    Asterisk,
    Slash,
    Caret,
    Percent,
    DotDot,
    LeftAngleBracket,
    LeftAngleBracketEqual,
    RightAngleBracket,
    RightAngleBracketEqual,
    EqualEqual,
    NotEqual,
    And,
    Or,

    // Unary operators
    Not,
    Hashtag,

    Ident(&'a str),
    Str(&'a str),
    Int(isize),
    Float(f64),
    Comment(&'a str),
}

impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}
