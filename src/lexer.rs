use std::fmt;
use std::clone::Clone;
use strum_macros::EnumDiscriminants;
use crate::interpreter::Object;

//is this enum necessary
#[enumeration(rename_all = "snake_case")]
#[derive(Debug, Clone, PartialEq, enum_utils::FromStr)]
pub enum Keyword {
    And,
    Break,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Debug, Clone, PartialEq, EnumDiscriminants)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,

    Bang,
    BangEqual,
    EqualEqual,
    Equal,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,

    String(Object),             //TODO: how to handle invlaid variants like TokenType::String(Object::Number)?
    Number(Object),
    Keyword(Keyword),
    Identifier(String),

    Slash,
    Eof,
}

#[derive(Clone,Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

//impl fmt::Debug for Token {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        write!(f, "{:?}", self.token_type)
//    }
//}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            line,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.token_type, self.lexeme)
    }
}

#[derive(Debug, Clone)]
pub enum ScannerErrorReason {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidNumber,
}

#[derive(Debug, Clone)]
pub struct ScannerError(usize, ScannerErrorReason);

pub struct Scanner {
    source: String,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    source_iter: Vec<char>,
}

macro_rules! match_char {
    ($self:ident, $char:expr, $if_true:expr, $if_false:expr) => {
        if $self.match_char($char) {
            Ok(Some($self.make_token($if_true)))
        } else {
            Ok(Some($self.make_token($if_false)))
        }
    };
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source: String::new(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            source_iter: source.chars().collect::<Vec<_>>(),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, ScannerError> {
        while !self.is_at_end() {
            self.start = self.current;
            let token = self.scan_token()?;
            if let Some(token) = token {
                self.tokens.push(token);
            }
        }

        self.tokens.push(Token::new(TokenType::Eof, "".to_string(), self.line));
        Ok(&self.tokens)
    }

    fn scan_token(&mut self) -> Result<Option<Token>,ScannerError> {
        let c = self.advance();
        match c {
            '(' => Ok(Some(self.make_token(TokenType::LeftParen))),
            ')' => Ok(Some(self.make_token(TokenType::RightParen))),
            '{' => Ok(Some(self.make_token(TokenType::LeftBrace))),
            '}' => Ok(Some(self.make_token(TokenType::RightBrace))),
            ',' => Ok(Some(self.make_token(TokenType::Comma))),
            '.' => Ok(Some(self.make_token(TokenType::Dot))),
            '-' => Ok(Some(self.make_token(TokenType::Minus))),
            '+' => Ok(Some(self.make_token(TokenType::Plus))),
            '*' => Ok(Some(self.make_token(TokenType::Star))),
            '!' => match_char!(self, '=', TokenType::BangEqual, TokenType::Bang),
            '=' => match_char!(self, '=', TokenType::EqualEqual, TokenType::Equal),
            '<' => match_char!(self, '=', TokenType::LessEqual, TokenType::Less),
            '>' => match_char!(self, '=', TokenType::GreaterEqual, TokenType::Greater),
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    Ok(None)
                } else {
                    Ok(Some(self.make_token(TokenType::Slash)))
                }
            },
            ';' => Ok(Some(self.make_token(TokenType::Semicolon))),
            '"' => self.string(),
            ' ' | '\r' | '\t' => { Ok(None) },
            '\n' => {
                self.line += 1;
                Ok(None)
            },
            _  => {
                if self.is_digit(c) {
                    self.number()
                } else if self.is_alpha(c) {
                    self.identifier()
                } else {
                    Err(ScannerError(self.line, ScannerErrorReason::UnexpectedCharacter(c)))
                }
            }
        }
    }

    fn identifier(&mut self) -> Result<Option<Token>,ScannerError> {
        loop {
            let c = self.peek();
            if !self.is_alphanumeric(c) {
                break;
            }
            self.advance();
        }

        let value = self.source_iter[self.start..self.current].iter().collect::<String>();
        let keyword = value.parse::<Keyword>();
        if keyword.is_ok() {
            Ok(Some(self.make_token(TokenType::Keyword(keyword.unwrap()))))
        } else {
            Ok(Some(self.make_token(TokenType::Identifier(value))))
        }
    }

    fn string(&mut self) -> Result<Option<Token>,ScannerError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(ScannerError(self.line, ScannerErrorReason::UnterminatedString))
        }

        self.advance();
        let value = self.source_iter[self.start+1..self.current-1].iter().collect::<String>();
        Ok(Some(self.make_token(TokenType::String(Object::String(value)))))
    }

    fn number(&mut self) -> Result<Option<Token>,ScannerError> {
        loop {
            let c = self.peek();
            if !self.is_digit(c) {
                break;
            }
            self.advance();
        }

        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance();

            loop {
                let c = self.peek();
                if !self.is_digit(c) {
                    break;
                }
                self.advance();
            }
        }

        let value = self.source_iter[self.start..self.current].iter().collect::<String>();
        Ok(Some(self.make_token(TokenType::Number(Object::Number(value.parse::<f64>()
                .map_err(|_| ScannerError(self.line, ScannerErrorReason::InvalidNumber))?
        )))))
    }

    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || 
        (c >= 'A' && c <= 'Z') || 
         c == '_'
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn peek(&mut self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source_iter[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source_iter.len() {
            '\0'
        } else {
            self.source_iter[self.current+1]
        }
    }

    fn match_char(&mut self, chr: char) -> bool {
        if self.is_at_end() {
            false
        } else if self.source_iter[self.current] != chr { //TODO this also is very bad
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source_iter[self.current-1] //TODO this is very bad
    }

    fn make_token(&mut self, token_type: TokenType) -> Token {
        Token::new(token_type, 
           self.source_iter[self.start..self.current].iter().collect::<String>(), 
           self.line)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source_iter.len()
    }
}
