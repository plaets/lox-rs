use std::fmt;
use strum_macros::EnumDiscriminants;
use crate::lexer::{Token, TokenType, TokenTypeDiscriminants, Keyword};
use crate::parser::Expr;

#[derive(Debug, Clone, Copy)]
pub enum OperationType {
    Neg,
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

#[derive(Debug, Clone)]
pub struct InterpreterError(pub Token, pub InterpreterErrorReason);

#[derive(Debug, Clone)]
pub enum InterpreterErrorReason {
    NotALiteral,
    InvalidBinaryOperands(ObjectDiscriminants, OperationType, ObjectDiscriminants),
    InvalidUnaryOperand(OperationType, ObjectDiscriminants),
    InvalidOperator(TokenTypeDiscriminants)
}

#[derive(Debug, Clone, PartialEq, EnumDiscriminants)]
pub enum Object {
    Nil,
    Bool(bool),
    String(String),
    Number(f64),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Nil => write!(f, "nil"),
            Object::Bool(val) => write!(f, "{}", val),
            Object::String(val) => write!(f, "\"{}\"", val),
            Object::Number(val) => write!(f, "{}", val),
        }
    }
}

pub struct Interpreter;

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

macro_rules! map_int_err {
    ($e:expr,$token:ident) => { ($e).map_err(|e| InterpreterError($token.clone(), e)) }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn evaluate(&self, expr: &Expr) -> Result<Object,InterpreterError> {
        match expr {
            Expr::Literal(token) => self.get_object(&token),
            Expr::Grouping(expr) => self.evaluate(&expr),
            Expr::Unary(token, expr) => self.evaluate_unary(token, expr),
            Expr::Binary(left, op, right) => self.evaluate_binary(left, op, right),
        }
    }

    fn get_object(&self, token: &Token) -> Result<Object,InterpreterError> {
        match &token.token_type {
            TokenType::Keyword(k) => match k {
                Keyword::True => Ok(Object::Bool(true)),
                Keyword::False => Ok(Object::Bool(false)),
                Keyword::Nil => Ok(Object::Nil),
                _ => Err(InterpreterError(token.clone(), InterpreterErrorReason::NotALiteral)),
            },
            TokenType::String(obj) => Ok(obj.clone()),
            TokenType::Number(obj) => Ok(obj.clone()),
            _ => Err(InterpreterError(token.clone(), InterpreterErrorReason::NotALiteral)),
        }
    }

    fn evaluate_unary(&self, token: &Token, expr: &Expr) -> Result<Object,InterpreterError> {
        let right = self.evaluate(expr)?;

        match &token.token_type {
            TokenType::Minus => map_int_err!(right.neg(), token),
            TokenType::Bang => Ok(Object::Bool(!right.is_truthy())),
            _ => Err(InterpreterError(token.clone(), InterpreterErrorReason::InvalidOperator(TokenTypeDiscriminants::from(&token.token_type)))),
        }
    }

    fn evaluate_binary(&self, left: &Expr, op: &Token, right: &Expr) -> Result<Object,InterpreterError> {
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
            TokenType::BangEqual => map_int_err!(map_int_err!(left.equal(&right), op)?.neg(), op),
            _ => Err(InterpreterError(op.clone(), 
                  InterpreterErrorReason::InvalidOperator(TokenTypeDiscriminants::from(&op.token_type)))),
        }
    }
}
