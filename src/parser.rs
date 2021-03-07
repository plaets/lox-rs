use std::fmt;
use crate::lexer::*;

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    While(Box<Expr>, Box<Stmt>),
    Var(Box<Token>, Option<Expr>), //TODO: first field has to be an identifier, how to avoid having to check the type again in the interpreter?
    //having tokens here is pretty cool as it allows better error handling 
    Block(Vec<Stmt>),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Box<Token>),
    Variable(Box<Token>),
    Grouping(Box<Expr>),
    Unary(Box<Token>, Box<Expr>),
    Logical(Box<Expr>, Box<Keyword>, Box<Expr>),
    Binary(Box<Expr>, Box<Token>, Box<Expr>),
    Assign(Box<Token>, Box<Expr>),
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

macro_rules! sync_on_err {
    ($self:ident,$expr:expr) => {
        $expr.map_err(|e| {
            $self.synchronize();
            e
        })
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>,ParseError> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt,ParseError> {
        if self.match_keyword(Keyword::Var) {
            sync_on_err!(self, self.var_declaration())
        } else {
            sync_on_err!(self, self.statement())
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt,ParseError> {
        let name = self.consume(TokenTypeDiscriminants::Identifier, Some(ParseErrorReason::ExpectedVariableName))?;
        let mut init: Option<Expr> = None;
        if self.match_tokens(vec![TokenTypeDiscriminants::Equal]) {
            init = Some(self.expression()?);
        }
        self.consume(TokenTypeDiscriminants::Semicolon, None)?;
        Ok(Stmt::Var(Box::new(name), init))
    }

    fn statement(&mut self) -> Result<Stmt,ParseError> {
        if self.match_keyword(Keyword::If) {
            self.if_statement()
        } else if self.match_keyword(Keyword::For) {
            self.for_statement()
        } else if self.match_keyword(Keyword::Print) {
            self.print_statement()
        } else if self.match_keyword(Keyword::While) {
            self.while_statement()
        } else if self.match_tokens(vec![TokenTypeDiscriminants::LeftBrace]) {
            self.block()
        } else {
            self.expr_statement()
        }
        //Err(ParseError(self.previous(), ParseErrorReason::NotImplemented))
    }

    fn block(&mut self) -> Result<Stmt,ParseError> {
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.check(TokenTypeDiscriminants::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.consume(TokenTypeDiscriminants::RightBrace, None)?;
        Ok(Stmt::Block(stmts))
    }

    fn if_statement(&mut self) -> Result<Stmt,ParseError> {
        self.consume(TokenTypeDiscriminants::LeftParen, None)?;
        let condition = self.expression()?;
        self.consume(TokenTypeDiscriminants::RightParen, None)?;

        let then_branch = self.statement()?;
        let mut else_branch: Option<Stmt> = None;
        if self.match_keyword(Keyword::Else) {
            else_branch = Some(self.statement()?);
        }

        Ok(Stmt::If(Box::new(condition), Box::new(then_branch), else_branch.map(|v| Box::new(v))))
    }

    fn for_statement(&mut self) -> Result<Stmt,ParseError> {
        self.consume(TokenTypeDiscriminants::LeftParen, None)?;

        let mut init: Option<Stmt> = None;
        if self.match_tokens(vec![TokenTypeDiscriminants::Semicolon]) {
            init = None;
        } else if self.match_keyword(Keyword::Var) {
            init = Some(self.var_declaration()?);
        } else {
            init = Some(self.expr_statement()?);
        }

        let mut cond: Option<Expr> = None; 
        if !self.check(TokenTypeDiscriminants::Semicolon) {
            cond = Some(self.expression()?);
        }
        self.consume(TokenTypeDiscriminants::Semicolon, None)?;

        let mut inc: Option<Expr> = None;
        if !self.check(TokenTypeDiscriminants::RightParen) {
            inc = Some(self.expression()?);
        }

        self.consume(TokenTypeDiscriminants::RightParen, None)?;

        let mut body = self.statement()?;

        if let Some(inc) = inc {
            body = Stmt::Block(vec![
                body,
                Stmt::Expr(inc),
            ]);
        }

        if let Some(cond) = cond {
            body = Stmt::While(Box::new(cond), Box::new(body));
        } else {
            body = Stmt::While(Box::new(Expr::Literal(
                        Box::new(Token::new(TokenType::Keyword(Keyword::True), "true".to_string(), 0))
                    )), Box::new(body));
        }

        if let Some(init) = init {
            body = Stmt::Block(vec![
                init,
                body,
            ])
        }

        Ok(body)
    }

    fn print_statement(&mut self) -> Result<Stmt,ParseError> {
        let expr = self.expression()?;
        self.consume(TokenTypeDiscriminants::Semicolon, None)?;
        Ok(Stmt::Print(expr))
    }

    fn while_statement(&mut self) -> Result<Stmt,ParseError> {
        self.consume(TokenTypeDiscriminants::LeftParen, None)?;
        let cond = self.expression()?;
        self.consume(TokenTypeDiscriminants::RightParen, None)?;
        let body = self.statement()?;
        Ok(Stmt::While(Box::new(cond), Box::new(body)))
    }

    fn expr_statement(&mut self) -> Result<Stmt,ParseError> {
        let value = self.expression()?;
        self.consume(TokenTypeDiscriminants::Semicolon, None)?;
        Ok(Stmt::Expr(value))
    }

    fn expression(&mut self) -> Result<Expr,ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr,ParseError> {
        let expr = self.or()?;
        if self.match_tokens(vec![TokenTypeDiscriminants::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;
            if let Expr::Variable(name) = expr {
                Ok(Expr::Assign(name, Box::new(value)))
            } else {
                Err(ParseError(equals, ParseErrorReason::InvalidAssignmentTarget))
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr,ParseError> {
        let expr = self.and()?;
        if self.match_keyword(Keyword::Or) {
            let op = self.previous();
            let right = self.and()?;
            Ok(Expr::Logical(Box::new(expr), Box::new(Keyword::Or), Box::new(right)))
        } else {
            Ok(expr)
        }
    }

    fn and(&mut self) -> Result<Expr,ParseError> {
        let expr = self.equality()?;
        if self.match_keyword(Keyword::And) {
            let op = self.previous();
            let right = self.equality()?;
            Ok(Expr::Logical(Box::new(expr), Box::new(Keyword::And), Box::new(right)))
        } else {
            Ok(expr)
        }
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

        if self.match_tokens(vec![TokenTypeDiscriminants::Identifier]) {
            return Ok(Expr::Variable(Box::new(self.previous())))
        }

        if self.match_tokens(vec![TokenTypeDiscriminants::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenTypeDiscriminants::Semicolon, None)?;
            return Ok(Expr::Grouping(Box::new(expr)))
        }

        return Err(ParseError(self.peek(), ParseErrorReason::ExpectedExpr))
    }

    fn match_tokens(&mut self, token_types: Vec<TokenTypeDiscriminants>) -> bool {
        for t in token_types.clone() {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn match_keyword(&mut self, keyword: Keyword) -> bool {
        if self.check(TokenTypeDiscriminants::Keyword) {
            if let TokenType::Keyword(k) = self.peek().token_type {
                if k == keyword {
                    self.advance();
                    return true;
                }
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

    fn consume(&mut self, token_type: TokenTypeDiscriminants, reason: Option<ParseErrorReason>) -> Result<Token, ParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(ParseError(self.peek(), reason.or(Some(ParseErrorReason::ExpectedToken(token_type))).unwrap()))
        }
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
    ExpectedToken(TokenTypeDiscriminants),
    ExpectedExpr,
    ExpectedVariableName,
    InvalidAssignmentTarget,
    NotImplemented,
    Other(String),
}

#[derive(Debug, Clone)]
pub struct ParseError(Token, ParseErrorReason);

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}
