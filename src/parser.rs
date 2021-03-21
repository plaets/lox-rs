use newtype_enum::Enum;
use crate::ast::*;

pub struct Parser {
    tokens: Vec<Token>,
    non_critical_errors: Vec<ParseError>,
    pos: usize,
}

macro_rules! binary {
    ($name:ident, $tokens:expr, $next:ident) => {
        fn $name(&mut self) -> Result<Expr,ParseError> {
            let mut expr = self.$next()?;

            while self.match_tokens($tokens.into_iter().map(TokenTypeDiscriminants::from).collect()) {
                let op = self.previous();
                let right = self.$next()?;
                expr = Expr::from_variant(ExprVar::Binary{left: Box::new(expr), op: Box::new(op), right: Box::new(right)});
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
            non_critical_errors: Vec::new(),
            pos: 0
        }
    }

    pub fn parse(&mut self) -> (Vec<Stmt>,Vec<ParseError>) { //maybe the first part should be an option? or a complete/incomplete enum
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            let decl = self.declaration();
            match decl {
                Ok(decl) => statements.push(decl),
                Err(err) => {
                    let mut errors = vec![err];
                    errors.append(&mut self.non_critical_errors);
                    return (statements, errors)
                }
            }
        }
        (statements, self.non_critical_errors.drain(0..).collect::<Vec<_>>())
    }

    fn declaration(&mut self) -> Result<Stmt,ParseError> {
        if self.match_keyword(Keyword::Var) {
            sync_on_err!(self, self.var_declaration())
        } else if self.match_keyword(Keyword::Fun) {
            sync_on_err!(self, self.fun_declaration().map(Stmt::from_variant))
        } else if self.match_keyword(Keyword::Class) {
            sync_on_err!(self, self.class_declaration().map(Stmt::from_variant))
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
        Ok(Stmt::from_variant(StmtVar::Var{name: Box::new(name), init}))
    }

    //this could return FunctionStmt i guess... sorta inconsistent
    fn fun_declaration(&mut self) -> Result<StmtVar::Fun,ParseError> {
        let name = self.consume(TokenTypeDiscriminants::Identifier, Some(ParseErrorReason::ExpectedFunctionName))?;
        self.consume(TokenTypeDiscriminants::LeftParen, None)?;

        let mut parameters: Vec<Token> = Vec::new();
        while let TokenType::Identifier(_) = self.peek().token_type {
            parameters.push(self.peek().clone());
            if parameters.len() == 255 {
                self.non_critical_errors.push(ParseError(self.peek().clone(), ParseErrorReason::TooManyArguments))
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
        Ok(StmtVar::Fun{ stmt: CcFunctionStmt::new(FunctionStmt(Box::new(name), parameters, self.block()?))})
    }

    fn class_declaration(&mut self) -> Result<StmtVar::Class,ParseError> {
        let name = self.consume(TokenTypeDiscriminants::Identifier, Some(ParseErrorReason::ExpectedClassName))?;

        let mut superclass: Option<ExprVar::Variable> = None;
        if self.match_tokens(vec![TokenTypeDiscriminants::Less]) {
            superclass = Some(ExprVar::Variable{ name: Box::new(self.consume(TokenTypeDiscriminants::Identifier, 
                                                                Some(ParseErrorReason::ExpectedSuperclassName))?) })
        }

        self.consume(TokenTypeDiscriminants::LeftBrace, None)?;
        
        let mut methods: Vec<StmtVar::Fun> = Vec::new();
        while !self.check(TokenTypeDiscriminants::RightBrace) && !self.is_at_end() {
            methods.push(self.fun_declaration()?);
        }

        self.consume(TokenTypeDiscriminants::RightBrace, None)?;
        Ok(StmtVar::Class{ name: Box::new(name), methods, superclass })
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
            self.block().map(Stmt::from_variant)
        } else {
            self.expr_statement()
        }
        //Err(ParseError(self.previous(), ParseErrorReason::NotImplemented))
    }

    fn block(&mut self) -> Result<StmtVar::Block,ParseError> {
        let mut stmts: Vec<Stmt> = Vec::new();
        let start = self.previous();
        while !self.check(TokenTypeDiscriminants::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.consume(TokenTypeDiscriminants::RightBrace, None)?;
        Ok(StmtVar::Block{ left_brace: Box::new(start), body: stmts })
    }

    fn if_statement(&mut self) -> Result<Stmt,ParseError> {
        self.consume(TokenTypeDiscriminants::LeftParen, None)?;
        let cond = self.expression()?;
        self.consume(TokenTypeDiscriminants::RightParen, None)?;

        let then_branch = self.statement()?;
        let mut else_branch: Option<Stmt> = None;
        if self.match_keyword(Keyword::Else) {
            else_branch = Some(self.statement()?);
        }

        Ok(Stmt::from_variant(StmtVar::If { cond, then: Box::new(then_branch), else_b: else_branch.map(Box::new) }))
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
            body = Stmt::from_variant(StmtVar::Block{ 
                left_brace: start.clone(), 
                body: vec![
                    body,
                    Stmt::from_variant(StmtVar::ExprStmt{ expr: inc }),
                ]
            });
        }

        if let Some(cond) = cond {
            body = Stmt::from_variant(StmtVar::While{ cond, body: Box::new(body) });
        } else {
            body = Stmt::from_variant(StmtVar::While{ 
                cond: Expr::from_variant(ExprVar::Literal {
                        token: Box::new(Token::new(TokenType::Keyword(Keyword::True), "true".to_string(), 0))
                }), 
                body: Box::new(body),
            });
        }

        if let Some(init) = init {
            body = Stmt::from_variant(StmtVar::Block {
                left_brace: start, 
                body: vec![
                    init,
                    body,
                ],
            })
        }

        Ok(body)
    }

    fn print_statement(&mut self) -> Result<Stmt,ParseError> {
        let expr = self.expression()?;
        self.consume(TokenTypeDiscriminants::Semicolon, None)?;
        Ok(Stmt::from_variant(StmtVar::Print{ expr }))
    }

    fn return_statement(&mut self) -> Result<Stmt,ParseError> {
        let keyword = self.previous();
        let mut value: Option<Expr> = None;
        //TODO: implement check as a macro so that discriminants are unecessary
        if !self.check(TokenTypeDiscriminants::Semicolon) {
            value = Some(self.expression()?);
        }
        self.consume(TokenTypeDiscriminants::Semicolon, None)?;
        Ok(Stmt::from_variant(StmtVar::Return{ keyword: Box::new(keyword), value }))
    }

    fn while_statement(&mut self) -> Result<Stmt,ParseError> {
        self.consume(TokenTypeDiscriminants::LeftParen, None)?;
        let cond = self.expression()?;
        self.consume(TokenTypeDiscriminants::RightParen, None)?;
        let body = self.statement()?;
        Ok(Stmt::from_variant(StmtVar::While{ cond, body: Box::new(body) }))
    }

    fn expr_statement(&mut self) -> Result<Stmt,ParseError> {
        let value = self.expression()?;
        self.consume(TokenTypeDiscriminants::Semicolon, None)?;
        Ok(Stmt::from_variant(StmtVar::ExprStmt{ expr: value }))
    }

    fn expression(&mut self) -> Result<Expr,ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr,ParseError> {
        let expr = self.or()?;
        if self.match_tokens(vec![TokenTypeDiscriminants::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;
            if let Expr::Variable(ExprVar::Variable{ name }) = expr {
                Ok(Expr::from_variant(ExprVar::Assign{name, expr: Box::new(value)}))
            } else if let Expr::Get(e) = expr {
                Ok(Expr::from_variant(ExprVar::Set{ object: e.object, name: e.name, value: Box::new(value) }))
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
            Ok(Expr::from_variant(ExprVar::Logical{left: Box::new(expr), op: Box::new(Keyword::Or), right: Box::new(right)}))
        } else {
            Ok(expr)
        }
    }

    fn and(&mut self) -> Result<Expr,ParseError> {
        let expr = self.equality()?;
        if self.match_keyword(Keyword::And) {
            let _op = self.previous(); //should always be and
            let right = self.equality()?;
            Ok(Expr::from_variant(ExprVar::Logical{left: Box::new(expr), op: Box::new(Keyword::And), right: Box::new(right)}))
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
            return Ok(Expr::from_variant(ExprVar::Unary{op: Box::new(op), expr: Box::new(right)}));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr,ParseError> {
        let mut expr = self.primary()?;
        loop {
            if self.match_tokens(vec![TokenTypeDiscriminants::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_tokens(vec![TokenTypeDiscriminants::Dot]) {
                let name = self.consume(TokenTypeDiscriminants::Identifier, Some(ParseErrorReason::ExpectedPropertyName))?;
                expr = Expr::from_variant(ExprVar::Get{ object: Box::new(expr), name: Box::new(name) });
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
                    self.non_critical_errors.push(ParseError(self.peek().clone(), ParseErrorReason::TooManyArguments))
                }
                args.push(self.expression()?);
            }
        }
        let paren = self.consume(TokenTypeDiscriminants::RightParen, None)?;
        Ok(Expr::from_variant(ExprVar::Call{callee: Box::new(callee), left_paren: Box::new(paren), args}))
    }

    fn primary(&mut self) -> Result<Expr,ParseError> {
        if self.check(TokenTypeDiscriminants::Keyword) {
            let token = self.advance();
            let token_type = token.token_type.clone();
            if let TokenType::Keyword(id) = token_type {
                match id {
                    Keyword::False | Keyword::True | Keyword::Nil => 
                        return Ok(Expr::from_variant(ExprVar::Literal{token: Box::new(token)})),
                    Keyword::This => return Ok(Expr::from_variant(ExprVar::This{keyword: Box::new(token)})),
                    Keyword::Super => {
                        self.consume(TokenTypeDiscriminants::Dot, None)?;
                        let method = self.consume(TokenTypeDiscriminants::Identifier,
                                                  Some(ParseErrorReason::ExpectedSuperclassMethodName))?;
                        return Ok(Expr::from_variant(ExprVar::Super{keyword: Box::new(token), method: Box::new(method)}))
                    }
                    _ => ()
                }
            } else {
                return Err(ParseError(token, ParseErrorReason::Other("FATAL Expected a keyword".to_string())))
            }
        }

        if self.match_tokens(vec![TokenTypeDiscriminants::Number, TokenTypeDiscriminants::String]) {
            return Ok(Expr::from_variant(ExprVar::Literal{token: Box::new(self.previous())}))
        }

        if self.match_tokens(vec![TokenTypeDiscriminants::Identifier]) {
            return Ok(Expr::from_variant(ExprVar::Variable{name: Box::new(self.previous())}))
        }

        if self.match_tokens(vec![TokenTypeDiscriminants::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenTypeDiscriminants::RightParen, None)?;
            return Ok(Expr::from_variant(ExprVar::Grouping{expr: Box::new(expr)}))
        }

        Err(ParseError(self.peek(), ParseErrorReason::ExpectedExpr))
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

            if let TokenType::Keyword(k) = self.peek().token_type {
                match k {
                    Keyword::Class | Keyword::Fun | Keyword::Var | Keyword::For | 
                        Keyword::If | Keyword::While | Keyword::Print | Keyword::Return => return,
                    _ => (),
                }
            }

            self.advance();
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseError(Token, ParseErrorReason);

#[derive(Debug, Clone)]
pub enum ParseErrorReason {
    ExpectedToken(TokenTypeDiscriminants),
    ExpectedPropertyName,
    ExpectedExpr,
    ExpectedVariableName,
    ExpectedFunctionName,
    ExpectedClassName,
    ExpectedSuperclassName,
    ExpectedSuperclassMethodName,
    ExpectedFunctionParameterOrRightParen,
    InvalidAssignmentTarget,
    TooManyArguments,
    //NotImplemented,
    Other(String),
}
