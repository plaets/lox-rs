use std::fmt;
use crate::lexer::*;
use gcmodule::{Cc,Trace,Tracer};

#[derive(Debug,Clone)]
pub struct FunctionStmt(pub Box<Token>, pub Vec<Token>, pub Vec<Stmt>);     //name, args, body

impl Trace for FunctionStmt {
    fn trace(&self, _tracer: &mut Tracer) { }
}

#[derive(Debug,Clone)]
pub enum Stmt {
    Expr(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),    //cond, then, else
    Print(Expr),
    Return(Box<Token>, Option<Expr>), 
    While(Expr, Box<Stmt>),                    //cond, body
    Var(Box<Token>, Option<Expr>),                  //name, init
    //TODO: first field has to be an identifier, how to avoid having to check the type again in the interpreter?
    Fun(Cc<FunctionStmt>),
    //having tokens here is pretty cool as it allows better error handling 
    Block(Box<Token>, Vec<Stmt>),
}

impl Stmt {
    pub fn get_token(&self) -> Token {
        match self {
            Stmt::Expr(expr) => expr.get_token(),
            Stmt::If(expr, _, _) => expr.get_token(),
            Stmt::Print(expr) => expr.get_token(),
            Stmt::Return(token, _) => *(token.clone()),
            Stmt::While(expr, _) => expr.get_token(),
            Stmt::Var(token, _) => *(token.clone()),
            Stmt::Fun(fun_stmt) => *(fun_stmt.0.clone()),
            Stmt::Block(token, _) => *(token.clone()),
        }
    }
}

#[derive(Debug,Clone)]
pub enum Expr {
    Assign(Box<Token>, Box<Expr>),                  //name, value
    Logical(Box<Expr>, Box<Keyword>, Box<Expr>),    //left, op (or/and), right
    Binary(Box<Expr>, Box<Token>, Box<Expr>),       //left, op, right
    Call(Box<Expr>, Box<Token>, Vec<Expr>),         //callable, left paren, args
    Unary(Box<Token>, Box<Expr>),                   //op, right
    Literal(Box<Token>),
    Variable(Box<Token>),                           //name
    Grouping(Box<Expr>),
}

impl Expr {
    pub fn get_token(&self) -> Token {
        match self {
            Expr::Assign(token, _) => *(token.clone()),
            Expr::Logical(expr, _, _) => expr.get_token(),
            Expr::Binary(_, op, _) => *(op.clone()),
            Expr::Call(_, paren, _) => *(paren.clone()),
            Expr::Unary(op, _) => *(op.clone()),
            Expr::Literal(token) => *(token.clone()),
            Expr::Variable(token) => *(token.clone()),
            Expr::Grouping(expr) => expr.get_token(),
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    non_critical_erros: Vec<ParseError>,
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
            non_critical_erros: Vec::new(),
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
        } else if self.match_keyword(Keyword::Fun) {
            sync_on_err!(self, self.fun_declaration())
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

    //this could return FunctionStmt i guess... sorta inconsistent
    fn fun_declaration(&mut self) -> Result<Stmt,ParseError> {
        let name = self.consume(TokenTypeDiscriminants::Identifier, Some(ParseErrorReason::ExpectedFunctionName))?;
        self.consume(TokenTypeDiscriminants::LeftParen, None)?;

        let mut parameters: Vec<Token> = Vec::new();
        while let TokenType::Identifier(_) = self.peek().token_type {
            parameters.push(self.peek().clone());
            if parameters.len() == 255 {
                self.non_critical_erros.push(ParseError(self.peek().clone(), ParseErrorReason::TooManyArguments))
            }
            self.advance();
            if self.peek().token_type != TokenType::Comma {
                break;
            } else {
                self.advance();
            }
        }

        self.consume(TokenTypeDiscriminants::RightParen, Some(ParseErrorReason::ExpectedFunctionParameterOrRightParen))?;
        self.consume(TokenTypeDiscriminants::LeftBrace, None)?;
        if let Stmt::Block(_, block) = self.block()? {
            Ok(Stmt::Fun(Cc::new(FunctionStmt(Box::new(name), parameters, block))))
        } else {
            Err(ParseError(self.peek(), ParseErrorReason::Other("internal error: expected a block".to_owned())))
        }
    }

    fn statement(&mut self) -> Result<Stmt,ParseError> {
        if self.match_keyword(Keyword::If) {
            self.if_statement()
        } else if self.match_keyword(Keyword::For) {
            self.for_statement()
        } else if self.match_keyword(Keyword::Print) {
            self.print_statement()
        } else if self.match_keyword(Keyword::Return) {
            self.return_statement()
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
        let start = self.previous();
        while !self.check(TokenTypeDiscriminants::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.consume(TokenTypeDiscriminants::RightBrace, None)?;
        Ok(Stmt::Block(Box::new(start), stmts))
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

        Ok(Stmt::If(condition, Box::new(then_branch), else_branch.map(|v| Box::new(v))))
    }

    fn for_statement(&mut self) -> Result<Stmt,ParseError> {
        let start = Box::new(self.consume(TokenTypeDiscriminants::LeftParen, None)?);

        let init: Option<Stmt>;
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
            body = Stmt::Block(start.clone(), vec![
                body,
                Stmt::Expr(inc),
            ]);
        }

        if let Some(cond) = cond {
            body = Stmt::While(cond, Box::new(body));
        } else {
            body = Stmt::While(Expr::Literal(
                        Box::new(Token::new(TokenType::Keyword(Keyword::True), "true".to_string(), 0))
                    ), Box::new(body));
        }

        if let Some(init) = init {
            body = Stmt::Block(start.clone(), vec![
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

    fn return_statement(&mut self) -> Result<Stmt,ParseError> {
        let keyword = self.previous();
        let mut value: Option<Expr> = None;
        //TODO: implement check as a macro so that discriminants are unecessary
        if !self.check(TokenTypeDiscriminants::Semicolon) {
            value = Some(self.expression()?);
        }
        self.consume(TokenTypeDiscriminants::Semicolon, None)?;
        Ok(Stmt::Return(Box::new(keyword), value))
    }

    fn while_statement(&mut self) -> Result<Stmt,ParseError> {
        self.consume(TokenTypeDiscriminants::LeftParen, None)?;
        let cond = self.expression()?;
        self.consume(TokenTypeDiscriminants::RightParen, None)?;
        let body = self.statement()?;
        Ok(Stmt::While(cond, Box::new(body)))
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
            let _op = self.previous(); //should always be or
            let right = self.and()?;
            Ok(Expr::Logical(Box::new(expr), Box::new(Keyword::Or), Box::new(right)))
        } else {
            Ok(expr)
        }
    }

    fn and(&mut self) -> Result<Expr,ParseError> {
        let expr = self.equality()?;
        if self.match_keyword(Keyword::And) {
            let _op = self.previous(); //should always be and
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

        self.call()
    }

    fn call(&mut self) -> Result<Expr,ParseError> {
        let mut expr = self.primary()?;
        loop {
            if self.match_tokens(vec![TokenTypeDiscriminants::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr,ParseError> {
        let mut args: Vec<Expr> = Vec::new();
        if !self.check(TokenTypeDiscriminants::RightParen) {
            args.push(self.expression()?);
            while self.match_tokens(vec![TokenTypeDiscriminants::Comma]) {
                if args.len() == 255 {
                    self.non_critical_erros.push(ParseError(self.peek().clone(), ParseErrorReason::TooManyArguments))
                }
                args.push(self.expression()?);
            }
        }
        let paren = self.consume(TokenTypeDiscriminants::RightParen, None)?;
        Ok(Expr::Call(Box::new(callee), Box::new(paren), args))
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
            self.consume(TokenTypeDiscriminants::RightParen, None)?;
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
}

#[derive(Debug, Clone)]
pub enum ParseErrorReason {
    ExpectedToken(TokenTypeDiscriminants),
    ExpectedExpr,
    ExpectedVariableName,
    ExpectedFunctionName,
    ExpectedFunctionParameterOrRightParen,
    InvalidAssignmentTarget,
    TooManyArguments,
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
