use std::fmt;
use crate::lexer::*;

#[derive(Debug)]
pub enum Expr {
    Literal(Box<Token>),
    Grouping(Box<Expr>),
    Unary(Box<Token>, Box<Expr>),
    Binary(Box<Expr>, Box<Token>, Box<Expr>),
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

macro_rules! binary {
    ($name:ident, $tokens:expr, $next:ident) => {
        fn $name(&mut self) -> Result<Expr,ParseError> {
            let mut expr = self.$next()?;

            while self.match_tokens($tokens.into_iter().map(TokenTypeDiscriminants::from).collect()) {
                let op = self.previous();
                let right = self.$next()?;
                expr = Expr::Binary(Box::new(expr), Box::new(op), Box::new(right));
            }

            Ok(expr)
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0
        }
    }

    pub fn parse(&mut self) -> Result<Expr,ParseError> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr,ParseError> {
        self.equality()
    }

    binary!(equality, vec![TokenType::BangEqual, TokenType::EqualEqual], comparison);
    binary!(comparison, vec![TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual], addition);
    binary!(addition, vec![TokenType::Minus, TokenType::Plus], multiplication);
    binary!(multiplication, vec![TokenType::Slash, TokenType::Star], unary);

    fn unary(&mut self) -> Result<Expr,ParseError> {
        if self.match_tokens(vec![TokenTypeDiscriminants::Bang, TokenTypeDiscriminants::Minus]) {
            let op = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary(Box::new(op), Box::new(right)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr,ParseError> {
        if self.check(TokenTypeDiscriminants::Keyword) {
            let token = self.advance();
            let token_type = token.token_type.clone();
            if let TokenType::Keyword(id) = token_type {
                match id {
                    Keyword::False | Keyword::True | Keyword::Nil => return Ok(Expr::Literal(Box::new(token))),
                    _ => ()
                }
            } else {
                return Err(ParseError(token, ParseErrorReason::Other("FATAL Expected a keyword".to_string())))
            }
        }

        if self.match_tokens(vec![TokenTypeDiscriminants::Number, TokenTypeDiscriminants::String]) {
            return Ok(Expr::Literal(Box::new(self.previous())))
        }

        if self.match_tokens(vec![TokenTypeDiscriminants::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenTypeDiscriminants::RightParen, ParseErrorReason::ExpectedParen)?;
            return Ok(Expr::Grouping(Box::new(expr)))
        }

        return Err(ParseError(self.peek(), ParseErrorReason::ExpectedExpr))
    }

    fn match_tokens(&mut self, token_types: Vec<TokenTypeDiscriminants>) -> bool {
        for t in token_types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.pos += 1
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.tokens[self.pos].token_type == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens[self.pos].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.pos-1].clone()
    }

    fn check(&mut self, token_type: TokenTypeDiscriminants) -> bool {
        if self.is_at_end() {
            false
        } else {
            TokenTypeDiscriminants::from(self.tokens[self.pos].token_type.clone()) == token_type
        }
    }

    fn consume(&mut self, token_type: TokenTypeDiscriminants, reason: ParseErrorReason) -> Result<Token, ParseError> {
        if self.check(token_type) {
            return Ok(self.advance())
        }

        Err(ParseError(self.peek(), reason))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if TokenTypeDiscriminants::from(self.previous().token_type) == TokenTypeDiscriminants::Semicolon {
                return
            }

            match self.peek().token_type {
                TokenType::Keyword(k) => {
                    match k {
                        Keyword::Class | Keyword::Fun | Keyword::Var | Keyword::For | 
                            Keyword::If | Keyword::While | Keyword::Print | Keyword::Return => return,
                        _ => (),
                    }
                }
                _ => ()
            }

            self.advance();
        }
    }

    //fn error(&self, token: Token, msg: String) {
    //    if TokenTypeDiscriminants::from(token.token_type) == TokenTypeDiscriminants::Eof {
    //        self.report(token.line, "at end", &msg);
    //    } else {
    //        self.report(token.line, &(" at '".to_owned() + &token.lexeme + "'"), &msg);
    //    }
    //}

    //fn report(&self, line: usize, err_where: &str, msg: &str) {
    //    let s: String = format!("[line {}] Error {}: {}", line, err_where, msg);
    //    stderr().write_all(s.as_bytes());
    //}
}

#[derive(Debug, Clone)]
pub enum ParseErrorReason {
    ExpectedParen,
    ExpectedExpr,
    Other(String),
}

#[derive(Debug, Clone)]
pub struct ParseError(Token, ParseErrorReason);

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

