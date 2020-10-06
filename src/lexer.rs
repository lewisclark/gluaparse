use crate::error::Error;

/* ---------- Reader ---------- */

// This doesn't support iterating over grapheme clusters so unicode varaible names won't be parsed
// correctly
#[derive(Debug)]
pub struct Reader<'a> {
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

    //pub fn read_until(&mut self, f: fn(char) -> bool) -> &'a str {
    pub fn read_until<F>(&mut self, shift: isize, f: F) -> &'a str
    where
        F: Fn(char) -> bool,
    {
        let start = (self.pos as isize + shift) as usize;

        while !f(self.code[self.pos] as char) {
            self.pos += 1;
        }

        std::str::from_utf8(&self.code[start..self.pos]).unwrap() // Bad unwrap
    }

    pub fn read_until_delim(&mut self, shift: isize, delim: char) -> &'a str {
        self.read_until(shift, |c| c == delim)
    }

    pub fn read_until_nonalphanumeric(&mut self, shift: isize) -> &'a str {
        self.read_until(shift, |c| !c.is_alphanumeric())
    }
}

/* ---------- Tokens ---------- */

#[derive(Debug)]
pub struct Tokens<'a> {
    tokens: Vec<Token<'a>>,
}

// TODO: Support multi-line strings
fn read_string<'a>(reader: &mut Reader, c: char) -> Token<'a> {
    Token::Invalid
}

fn read_number<'a>(reader: &mut Reader) -> Token<'a> {
    Token::Invalid
}

fn read_keyword<'a>(reader: &mut Reader) -> Token<'a> {
    Token::Invalid
}

impl<'a> Tokens<'a> {
    pub fn from_source(code: &'a str) -> Result<Self, Error> {
        let mut reader = Reader::from_source(code);
        let mut tokens = Vec::new();

        while !reader.eof() {
            let c = reader.char();

            if c.is_whitespace() {
                continue;
            }

            let token = match c {
                ';' => Token::SemiColon,
                ',' => Token::Comma,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '^' => Token::Caret,
                '%' => Token::Percent,
                '{' => Token::LeftCurlyBracket,
                '}' => Token::RightCurlyBracket,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '#' => Token::Hashtag,
                '~' => {
                    if reader.char() != '=' {
                        return Err(Error::new("Expected = after ~".to_string()));
                    }

                    Token::TildeEqual
                }
                '.' => {
                    if reader.peek() == '.' {
                        reader.advance(1);

                        if reader.peek() == '.' {
                            reader.advance(1);

                            Token::DotDotDot
                        } else {
                            Token::DotDot
                        }
                    } else {
                        Token::Dot
                    }
                }
                '=' => {
                    if reader.peek() == '=' {
                        reader.advance(1);

                        Token::EqualEqual
                    } else {
                        Token::Equal
                    }
                }
                '\"' | '\'' => {
                    let s = reader.read_until_delim(0, c);

                    if reader.char() == c {
                        Token::Str(s)
                    } else {
                        return Err(Error::new(format!("Expected string end delimiter {}", c)));
                    }
                }
                '[' => {
                    if reader.peek() == '[' {
                        read_string(&mut reader, c)
                    } else {
                        Token::LeftSquareBracket
                    }
                }
                ']' => Token::RightSquareBracket,
                c => {
                    if c.is_digit(10) {
                        read_number(&mut reader)
                    } else {
                        // Read keyword or iden
                        let word = reader.read_until_nonalphanumeric(-1);

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
                            _ => Token::Ident(word),
                        }
                    }
                }
            };

            tokens.push(token);
        }

        Ok(Self { tokens })
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
    SemiColon,
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
    TildeEqual,
    And,
    Or,

    // Unary operators
    Not,
    Hashtag,

    Ident(&'a str),
    Str(&'a str),

    Invalid,
}
