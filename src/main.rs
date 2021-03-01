use std::io::prelude::*;
use std::io::{stdin, stdout, stderr, Cursor};
use std::env;
use std::fs::File;
use std::path::Path;
use std::iter::Peekable;
mod lexer;
use lexer::*;

use std::fmt;

#[derive(Debug)]
enum Expr {
    Literal(Box<Token>),
    Grouping(Box<Expr>),
    Unary(Box<Token>, Box<Expr>),
    Binary(Box<Expr>, Box<Token>, Box<Expr>),
    Tenary(Box<Expr>, Box<Expr>, Box<Expr>),
    Comma(Vec<Expr>),
    None,
}

struct Parser {
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
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0
        }
    }

    fn parse(&mut self) -> Result<Expr,ParseError> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr,ParseError> {
        self.comma()
    }

    fn comma(&mut self) -> Result<Expr,ParseError> {
        let mut exprs: Vec<Expr> = vec![self.tenary()?];

        while self.match_tokens(vec![TokenTypeDiscriminants::Comma]) {
            exprs.push(self.tenary()?)
        }

        if exprs.len() > 1 {
            Ok(Expr::Comma(exprs))
        } else {
            Ok(exprs.remove(0))
        }
    }

    fn tenary(&mut self) -> Result<Expr,ParseError> {
        let cond = self.equality()?;

        if self.match_tokens(vec![TokenTypeDiscriminants::QuestionMark]) {
            let if_true = self.equality()?;
            if self.match_tokens(vec![TokenTypeDiscriminants::Colon]) {
                let if_false = self.equality()?;
                return Ok(Expr::Tenary(Box::new(cond), Box::new(if_true), Box::new(if_false)))
            } 
            return Err(ParseError(self.peek(), "Expected ':'.".to_string()))
        }

        Ok(cond)
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
        } else if self.match_tokens(vec![TokenTypeDiscriminants::BangEqual, TokenTypeDiscriminants::EqualEqual, 
               TokenTypeDiscriminants::Greater, TokenTypeDiscriminants::GreaterEqual, TokenTypeDiscriminants::Less, 
               TokenTypeDiscriminants::LessEqual, TokenTypeDiscriminants::Minus, TokenTypeDiscriminants::Plus,
               TokenTypeDiscriminants::Slash, TokenTypeDiscriminants::Star]) {
            let op = self.previous();
            let right = self.unary()?;
            println!("{:?}", Expr::Unary(Box::new(op), Box::new(right)));
            return Err(ParseError(self.peek(), "Binary expression without left-hand operand".to_string()))
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr,ParseError> {
        if self.check(TokenTypeDiscriminants::Identifier) {
            let token = self.advance();
            let token_type = token.token_type.clone();
            if let TokenType::Keyword(id) = token_type {
                match id {
                    Keyword::False | Keyword::True | Keyword::Nil => return Ok(Expr::Literal(Box::new(token))),
                    _ => ()
                }
            }
        }

        if self.match_tokens(vec![TokenTypeDiscriminants::Number, TokenTypeDiscriminants::String]) {
            return Ok(Expr::Literal(Box::new(self.previous())))
        }

        if self.match_tokens(vec![TokenTypeDiscriminants::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenTypeDiscriminants::RightParen, "Expected ')' efter expression.".to_string())?;
            return Ok(Expr::Grouping(Box::new(expr)))
        }

        return Err(ParseError(self.peek(), "Expected expression.".to_string()))
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

    fn consume(&mut self, token_type: TokenTypeDiscriminants, msg: String) -> Result<Token, ParseError> {
        if self.check(token_type) {
            return Ok(self.advance())
        }

        Err(ParseError(self.peek(), msg))
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

    fn error(&self, token: Token, msg: String) {
        if TokenTypeDiscriminants::from(token.token_type) == TokenTypeDiscriminants::Eof {
            self.report(token.line, "at end", &msg);
        } else {
            self.report(token.line, &(" at '".to_owned() + &token.lexeme + "'"), &msg);
        }
    }

    fn report(&self, line: usize, err_where: &str, msg: &str) {
        let s: String = format!("[line {}] Error {}: {}", line, err_where, msg);
        stderr().write_all(s.as_bytes());
    }
}

#[derive(Debug, Clone)]
struct ParseError(Token, String);

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

fn run(data: &str) {
    let mut scanner = Scanner::new(data.to_string());
    scanner.scan_tokens();
    let mut parser = Parser::new(scanner.tokens.clone());
    let tree = parser.parse();
    println!("{:?}", scanner.tokens.iter().map(|t| t.token_type.clone()).collect::<Vec<TokenType>>());
    println!("{:#?}", tree);
}

fn run_file(path: &str) -> Result<(), std::io::Error> {
    let mut file = File::open(Path::new(path))?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    run(&contents);
    Ok(())
}

fn run_prompt() -> Result<(), std::io::Error> {
    loop {
        print!("> ");
        stdout().flush()?;
        let mut line = String::new();
        stdin().read_line(&mut line)?;
        run(&line);
    }
}

fn main() -> Result<(), std::io::Error> {
    let expr = Expr::Binary(
        Box::new(Expr::Unary(Box::new(Token::new(TokenType::Minus, "-".to_string(), 0)), 
                 Box::new(Expr::Literal(Box::new(Token::new(TokenType::Number(45.67), "45.67".to_string(), 0)))))),
        Box::new(Token::new(TokenType::Star, "*".to_string(), 0)),
        Box::new(Expr::Grouping(Box::new(Expr::Literal(Box::new(Token::new(TokenType::Number(45.67), "45.67".to_string(), 0)))))),
    );
    println!("{:#}", expr);

    if env::args().len() > 2 {
        println!("Usage: {} script", env::args().nth(0).unwrap());
        Ok(())
    } else if env::args().len() == 2 {
        run_file(&env::args().nth(1).unwrap())
    } else {
        run_prompt()
    }
}
