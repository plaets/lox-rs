use std::fmt;
use std::collections::HashMap;
use strum_macros::EnumDiscriminants;
use crate::lexer::{Token, TokenType, TokenTypeDiscriminants, Keyword};
use crate::parser::{Expr,Stmt};

#[derive(Debug, Clone, Copy)]
pub enum OperationType {
    Neg,
    Not,
    Add,
    Sub,
    Mul,
    Div,
    Less,
    Leq,
    Greater,
    Geq,
    EqualEqual,
}

#[derive(Debug, Clone, PartialEq, EnumDiscriminants)]
pub enum Object {
    Nil,
    Bool(bool),
    String(String),
    Number(f64),
}

macro_rules! number_bin_op {
    ($fn:ident,$op:tt,$obj:ident,$op_type:path) => {
        pub fn $fn(&self, other: &Self) -> Result<Self,InterpreterErrorReason> {
            match (self, other) {
                (Object::Number(a), Object::Number(b)) => Ok(Object::$obj(a $op b)),
                _ => Err(InterpreterErrorReason::InvalidBinaryOperands(ObjectDiscriminants::from(self), 
                       $op_type, ObjectDiscriminants::from(self))),
            }
        }
    }
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Nil => false,
            Object::Bool(b) => *b,
            Object::String(_) => true,
            Object::Number(n) => *n != 0.0,
        }
    }

    pub fn neg(&self) -> Result<Self,InterpreterErrorReason> {
        match self {
            Object::Number(num) => Ok(Object::Number(-num)),
            _ => Err(InterpreterErrorReason::InvalidUnaryOperand(OperationType::Neg, 
                     ObjectDiscriminants::from(self))),
        }
    }

    pub fn not(&self) -> Result<Self,InterpreterErrorReason> {
        match self {
            Object::Bool(val) => Ok(Object::Bool(!val)),
            _ => Err(InterpreterErrorReason::InvalidUnaryOperand(OperationType::Not, 
                     ObjectDiscriminants::from(self))),
        }
    }

    pub fn equal(&self, other: &Self) -> Result<Self,InterpreterErrorReason> {
        match (self, other) {
            (Object::Number(a), Object::Number(b)) => Ok(Object::Bool(a == b)),
            (Object::String(a), Object::String(b)) => Ok(Object::Bool(a == b)),
            (Object::Bool(a), Object::Bool(b)) => Ok(Object::Bool(a == b)),
            (Object::Nil, Object::Nil) => Ok(Object::Bool(true)),
            _ => Ok(Object::Bool(false))
        }
    }

    pub fn add(&self, other: &Self) -> Result<Self,InterpreterErrorReason> {
        match (self, other) {
            (Object::Number(a), Object::Number(b)) => Ok(Object::Number(a + b)),
            (Object::String(a), Object::String(b)) => Ok(Object::String(a.to_owned() + b)),
            _ => Err(InterpreterErrorReason::InvalidBinaryOperands(ObjectDiscriminants::from(self), 
                                                   OperationType::Add, ObjectDiscriminants::from(self))),
        }
    }

    //number_bin_op!(add, +, Number, OperationType::Add);
    number_bin_op!(mul, *, Number, OperationType::Mul);
    number_bin_op!(sub, -, Number, OperationType::Sub);
    number_bin_op!(div, /, Number, OperationType::Div);
    number_bin_op!(greater, >, Bool, OperationType::Greater);
    number_bin_op!(geq, >=, Bool, OperationType::Geq);
    number_bin_op!(less, <, Bool, OperationType::Less);
    number_bin_op!(leq, <=, Bool, OperationType::Leq);
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Nil => write!(f, "nil"),
            Object::Bool(val) => write!(f, "{}", val),
            Object::String(val) => write!(f, "{}", val),
            Object::Number(val) => write!(f, "{}", val),
        }
    }
}

struct Environment {
    values: Vec<HashMap<String, Object>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: vec![HashMap::new()]
        }
    }

    pub fn define(&mut self, name: String, val: Object) {
        self.values.last_mut().unwrap().insert(name, val);
    }

    pub fn assign(&mut self, name: String, val: Object) -> Option<Object> {
        let found = self.values.iter_mut().find(|e| e.contains_key(&name));
        if let Some(env) = found {
            Some(env.insert(name, val).unwrap()) //dumb
        } else {
            None
        }
    }

    pub fn get(&self, name: &String) -> Option<&Object> {
        self.values.iter().rev().find_map(|e| e.get(name))
    }

    pub fn push(&mut self) {
        self.values.push(HashMap::new());
    }

    pub fn pop(&mut self) -> Option<HashMap<String, Object>> {
        if self.values.len() > 1 {
            self.values.pop()
        } else {
            None
        }
    }
}

pub struct Interpreter {
    env: Environment,
}

macro_rules! map_int_err {
    ($e:expr,$token:ident) => { ($e).map_err(|e| int_err!($token.clone(), e)) }
}

macro_rules! int_err {
    ($t:expr,$e:expr) => { InterpreterStCh::Error(InterpreterError($t,$e)) }
}

impl Interpreter {
    pub fn new() -> Self {
        Self { 
            env: Environment::new(),
        }
    }

    pub fn interpret(&mut self, statements: &Vec<Stmt>) -> Result<Option<Object>,InterpreterStCh> {
        let mut res: Option<Object> = None;
        for stmt in statements {
            res = self.execute(&stmt)?;
        }
        Ok(res)
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<Option<Object>,InterpreterStCh> {
        match stmt {
            Stmt::Expr(expr) => Ok(Some(self.evaluate(expr)?)),
            Stmt::If(cond, thenb, elseb) => Ok(self.exec_if(cond, thenb, elseb.as_ref().map(|v| v.as_ref()))?),
            Stmt::Print(expr) => Ok(self.exec_print(expr)?),
            Stmt::While(cond, body) => Ok(self.exec_while(cond, body)?),
            Stmt::Break(_) => Err(InterpreterStCh::Break),
            Stmt::Var(name, expr) => Ok(self.exec_var(name, expr)?),
            Stmt::Block(stmts) => Ok(self.exec_block(stmts)?),
        }
    }

    fn exec_if(&mut self, cond: &Expr, thenb: &Stmt, elseb: Option<&Stmt>) -> Result<Option<Object>,InterpreterStCh> {
        if self.evaluate(cond)?.is_truthy() {
            self.execute(thenb)
        } else if let Some(elseb) = elseb {
            self.execute(elseb)
        } else {
            Ok(None)
        }
    }

    fn exec_print(&mut self, expr: &Expr) -> Result<Option<Object>,InterpreterStCh> {
        println!("{}", self.evaluate(expr)?);
        Ok(None)
    }

    fn exec_while(&mut self, cond: &Expr, body: &Stmt) -> Result<Option<Object>,InterpreterStCh> {
        while self.evaluate(cond)?.is_truthy() {
            let res = self.execute(body);
            if let Err(InterpreterStCh::Break) = res {
                break;
            } else {
                res?;
            }
        }
        Ok(None)
    }

    fn exec_var(&mut self, name: &Token, val: &Option<Expr>) -> Result<Option<Object>,InterpreterStCh> {
        if let TokenType::Identifier(name) = &name.token_type {
            if let Some(expr) = val {
                let value = self.evaluate(expr)?;
                self.env.define(name.clone(), value);
            } else {
                self.env.define(name.clone(), Object::Nil);
            }
            Ok(None)
        } else {
            Err(int_err!(name.clone(), InterpreterErrorReason::ExpectedToken(TokenTypeDiscriminants::Identifier)))
        }
    }

    fn exec_block(&mut self, stmts: &Vec<Stmt>) -> Result<Option<Object>,InterpreterStCh> {
        self.env.push();
        let mut last_val: Option<Object> = None;
        for stmt in stmts {
            last_val = self.execute(stmt)?;
        }
        self.env.pop().unwrap();
        Ok(last_val)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Object,InterpreterStCh> {
        match expr {
            Expr::Literal(token) => self.get_object(&token),
            Expr::Grouping(expr) => self.evaluate(&expr),
            Expr::Unary(token, expr) => self.evaluate_unary(token, expr),
            Expr::Logical(left, op, right) => self.evaluate_logical(left, op, right),
            Expr::Binary(left, op, right) => self.evaluate_binary(left, op, right),
            Expr::Assign(name, expr) => self.evaluate_assign(name, expr),
            Expr::Variable(name) => self.evaluate_variable(name),
        }
    }

    fn get_object(&mut self, token: &Token) -> Result<Object,InterpreterStCh> {
        match &token.token_type {
            TokenType::Keyword(k) => match k {
                Keyword::True => Ok(Object::Bool(true)),
                Keyword::False => Ok(Object::Bool(false)),
                Keyword::Nil => Ok(Object::Nil),
                _ => Err(int_err!(token.clone(), InterpreterErrorReason::NotALiteral)),
            },
            TokenType::String(obj) => Ok(obj.clone()),
            TokenType::Number(obj) => Ok(obj.clone()),
            _ => Err(int_err!(token.clone(), InterpreterErrorReason::NotALiteral)),
        }
    }

    fn evaluate_unary(&mut self, token: &Token, expr: &Expr) -> Result<Object,InterpreterStCh> {
        let right = self.evaluate(expr)?;

        match &token.token_type {
            TokenType::Minus => map_int_err!(right.neg(), token),
            TokenType::Bang => Ok(Object::Bool(!right.is_truthy())),
            _ => Err(int_err!(token.clone(), InterpreterErrorReason::InvalidOperator(TokenTypeDiscriminants::from(&token.token_type)))),
        }
    }

    fn evaluate_logical(&mut self, left: &Expr, op: &Keyword, right: &Expr) -> Result<Object,InterpreterStCh> {
        let left = self.evaluate(left)?;
        if *op == Keyword::Or {
            if left.is_truthy() {
                return Ok(left)
            }
        } else if *op == Keyword::And {
            if !left.is_truthy() {
                return Ok(left)
            }
        } 

        self.evaluate(right)
    }

    fn evaluate_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Object,InterpreterStCh> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        match &op.token_type {
            TokenType::Minus => map_int_err!(left.sub(&right), op),
            TokenType::Slash => map_int_err!(left.div(&right), op),
            TokenType::Star => map_int_err!(left.mul(&right), op),
            TokenType::Plus => map_int_err!(left.add(&right), op),
            TokenType::LessEqual => map_int_err!(left.leq(&right), op),
            TokenType::Less => map_int_err!(left.less(&right), op),
            TokenType::GreaterEqual => map_int_err!(left.geq(&right), op),
            TokenType::Greater => map_int_err!(left.greater(&right), op),
            TokenType::EqualEqual => map_int_err!(left.equal(&right), op),
            TokenType::BangEqual => map_int_err!(left.equal(&right), op).and_then(|v| map_int_err!(v.not(), op)),
            _ => Err(int_err!(op.clone(), 
                  InterpreterErrorReason::InvalidOperator(TokenTypeDiscriminants::from(&op.token_type)))),
        }
    }

    fn evaluate_variable(&mut self, name: &Token) -> Result<Object,InterpreterStCh> {
        self.env.get(&name.lexeme).map_or(
            Err(int_err!(name.clone(), InterpreterErrorReason::UndefinedVariable)),
            |v| Ok(v.clone())
        )
    }

    fn evaluate_assign(&mut self, name: &Token, expr: &Expr) -> Result<Object,InterpreterStCh> {
        let value = self.evaluate(expr)?;
        if let Some(_) = self.env.assign(name.lexeme.clone(), value.clone()) {
            Ok(value)
        } else {
            Err(int_err!(name.clone(), InterpreterErrorReason::UndefinedVariable))
        }
    }
}

//interpreter state change
#[derive(Debug, Clone)]
pub enum InterpreterStCh {
    Break,
    Error(InterpreterError),
}

#[derive(Debug, Clone)]
pub struct InterpreterError(pub Token, pub InterpreterErrorReason);

#[derive(Debug, Clone)]
pub enum InterpreterErrorReason {
    NotALiteral,
    UndefinedVariable,
    InvalidBinaryOperands(ObjectDiscriminants, OperationType, ObjectDiscriminants),
    InvalidUnaryOperand(OperationType, ObjectDiscriminants),
    InvalidOperator(TokenTypeDiscriminants),
    ExpectedToken(TokenTypeDiscriminants),
}