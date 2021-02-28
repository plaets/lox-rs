use std::io::prelude::*;
use std::io::{stderr};
use std::fmt;
use std::clone::Clone;
use std::vec::IntoIter;
use std::str::Chars;
use strum_macros::EnumDiscriminants;

#[enumeration(rename_all = "snake_case")]
#[derive(Debug, Clone, PartialEq, enum_utils::FromStr)]
pub enum Keyword {
    And,
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
    Semicolon,
    Dot,
    Minus,
    Plus,
    Star,

    QuestionMark,
    Colon,

    Bang,
    BangEqual,
    EqualEqual,
    Equal,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,

    String(String),
    Number(f64),
    Keyword(Keyword),
    Identifier(String),

    Slash,
    Eof,
}

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.token_type)
    }
}

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
            $self.add_token($if_true)
        } else {
            $self.add_token($if_false)
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

    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::Eof, "".to_string(), self.line));
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            '*' => self.add_token(TokenType::Star),
            '?' => self.add_token(TokenType::QuestionMark),
            ':' => self.add_token(TokenType::Colon),
            '!' => match_char!(self, '=', TokenType::BangEqual, TokenType::Bang),
            '=' => match_char!(self, '=', TokenType::EqualEqual, TokenType::Equal),
            '<' => match_char!(self, '=', TokenType::LessEqual, TokenType::Less),
            '>' => match_char!(self, '=', TokenType::GreaterEqual, TokenType::Greater),
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash)
                }
            },
            ';' => self.add_token(TokenType::Semicolon),
            '"' => self.string(),
            ' ' | '\r' | '\t' => {},
            '\n' => self.line += 1,
            _  => {
                if self.is_digit(c) {
                    self.number();
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    self.error(self.line, "Unexpected character");
                }
            }
        }
    }

    fn identifier(&mut self) {
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
            self.add_token(TokenType::Keyword(keyword.unwrap()))
        } else {
            self.add_token(TokenType::Identifier(value))
        }
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error(self.line, "Unterminated string");
            return
        }

        self.advance();
        let value = self.source_iter[self.start+1..self.current-1].iter().collect::<String>();
        self.add_token(TokenType::String(value))
    }

    fn number(&mut self) {
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
        self.add_token(TokenType::Number(value.parse::<f64>().unwrap()));
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

    fn add_token(&mut self, token_type: TokenType) {
        let text = self.source_iter[self.start..self.current].iter().collect::<String>();
        self.tokens.push(Token::new(token_type, text, self.line))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source_iter.len()
    }

    fn error(&self, line: usize, msg: &str) {
        self.report(line, "", msg)
    }

    fn report(&self, line: usize, err_where: &str, msg: &str) {
        let s: String = format!("[line {}] Error {}: {}", line, err_where, msg);
        stderr().write_all(s.as_bytes());
    }
}
