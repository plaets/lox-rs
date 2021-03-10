use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::swap;
use strum_macros::EnumDiscriminants;
use crate::lexer::{Token, TokenType, TokenTypeDiscriminants, Keyword};
use crate::parser::{Expr,Stmt,FunctionStmt};
use crate::function::Function;

//so i been thinking... do i need garbage collection or does rc handle all i need to do
//update: check the examples for an answer

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

pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Object>) -> Result<Option<Object>,StateChange>;
    fn arity(&self) -> u8;
}

#[derive(Clone)]
pub struct CallableObject(Rc<dyn Callable>);

impl CallableObject {
    pub fn new(arg: Rc<dyn Callable>) -> Self {
        Self(arg)
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: &Vec<Object>) -> Result<Option<Object>,StateChange> {
        self.0.call(interpreter, args)
    }

    pub fn arity(&self) -> u8 {
        self.0.arity()
    }
}

impl PartialEq for CallableObject {
    fn eq(&self, other: &Self) -> bool {
        Rc::as_ptr(&self.0) == Rc::as_ptr(&other.0)
    }
}

impl fmt::Debug for CallableObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CallableObject")
    }
}

#[derive(Clone, Debug, PartialEq, EnumDiscriminants)]
pub enum Object {
    Nil,
    Bool(bool),
    String(String),
    Number(f64),
    Callable(CallableObject),
}

macro_rules! number_bin_op {
    ($fn:ident,$op:tt,$obj:ident,$op_type:path) => {
        pub fn $fn(&self, other: &Self) -> Result<Self,ErrReason> {
            match (self, other) {
                (Object::Number(a), Object::Number(b)) => Ok(Object::$obj(a $op b)),
                _ => Err(ErrReason::InvalidBinaryOperands(ObjectDiscriminants::from(self), 
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
            Object::Callable(_) => true, //???
        }
    }

    pub fn neg(&self) -> Result<Self,ErrReason> {
        match self {
            Object::Number(num) => Ok(Object::Number(-num)),
            _ => Err(ErrReason::InvalidUnaryOperand(OperationType::Neg, 
                     ObjectDiscriminants::from(self))),
        }
    }

    pub fn not(&self) -> Result<Self,ErrReason> {
        match self {
            Object::Bool(val) => Ok(Object::Bool(!val)),
            _ => Err(ErrReason::InvalidUnaryOperand(OperationType::Not, 
                     ObjectDiscriminants::from(self))),
        }
    }

    pub fn equal(&self, other: &Self) -> Result<Self,ErrReason> {
        match (self, other) {
            (Object::Number(a), Object::Number(b)) => Ok(Object::Bool(a == b)),
            (Object::String(a), Object::String(b)) => Ok(Object::Bool(a == b)),
            (Object::Bool(a), Object::Bool(b)) => Ok(Object::Bool(a == b)),
            (Object::Nil, Object::Nil) => Ok(Object::Bool(true)),
            _ => Ok(Object::Bool(false))
        }
    }

    pub fn add(&self, other: &Self) -> Result<Self,ErrReason> {
        match (self, other) {
            (Object::Number(a), Object::Number(b)) => Ok(Object::Number(a + b)),
            (Object::String(a), Object::String(b)) => Ok(Object::String(a.to_owned() + b)),
            _ => Err(ErrReason::InvalidBinaryOperands(ObjectDiscriminants::from(self), 
                                                   OperationType::Add, ObjectDiscriminants::from(self))),
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: &Vec<Object>) -> Result<Option<Self>,StateChange> {
        match self {
            Object::Callable(f) => {
                if f.arity() as usize == args.len() {
                    f.call(interpreter, args)
                } else {
                    Err(StateChange::ErrReason(ErrReason::WrongNumberOfArgs(f.arity(), args.len())))
                }
            }
            _ => Err(StateChange::ErrReason(ErrReason::NotACallable(ObjectDiscriminants::from(self)))),
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
            Object::Callable(_) => write!(f, "Callable"),
        }
    }
}


pub type EnvironmentScope = Rc<RefCell<HashMap<String, Object>>>;

#[derive(Debug)]
pub struct Environment {
    values: Vec<EnvironmentScope>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: vec![Rc::new(RefCell::new(HashMap::new()))]
        }
    }

    pub fn new_with(env: Vec<EnvironmentScope>) -> Self {
        Self {
            values: env,
        }
    }

    pub fn get_current(&self) -> Vec<EnvironmentScope> {
        self.values[0..self.values.len()].to_vec()
    }

    pub fn define(&mut self, name: String, val: Object) {
        self.values.last_mut().unwrap().borrow_mut().insert(name, val);
    }

    pub fn assign(&mut self, name: String, val: Object) -> Option<Object> {
        let found = self.values.iter_mut().find(|e| e.borrow().contains_key(&name));
        if let Some(env) = found {
            Some(env.borrow_mut().insert(name, val).unwrap()) //dumb
        } else {
            None
        }
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        self.values.iter().rev().find_map(|e| e.borrow().get(name).map(|v| v.clone()))
    }

    pub fn push(&mut self) {
        self.values.push(Rc::new(RefCell::new(HashMap::new())));
    }
    
    pub fn push_foreign(&mut self, scope: EnvironmentScope) {
        self.values.push(scope)
    }

    pub fn pop(&mut self) -> Option<Rc<RefCell<HashMap<String, Object>>>> {
        if self.values.len() > 1 {
            self.values.pop()
        } else {
            None
        }
    }

    pub fn globals(&mut self) -> Rc<RefCell<HashMap<String, Object>>> {
        self.values.iter().next().unwrap().clone()
    }
}

pub struct Interpreter {
    env: Environment,
}

macro_rules! map_int_err {
    ($e:expr,$token:ident) => { ($e).map_err(|e| int_err!($token.clone(), e)) }
}

macro_rules! st_err {
    ($t:expr,$e:expr) => { StateChange::Err(IntErr($t,$e)) }
}

macro_rules! int_err {
    ($t:expr,$e:expr) => { IntErr($t,$e) }
}

macro_rules! int_to_st {
    ($t:expr) => { ($t).map_err(|e| StateChange::Err(e)) }
}


impl Interpreter {
    pub fn new() -> Self {
        Self { 
            env: Environment::new(),
        }
    }

    pub fn interpret(&mut self, statements: &Vec<Stmt>) -> Result<Option<Object>,IntErr> {
        let mut res: Option<Object> = None;
        for stmt in statements {
            let exec_res = self.execute(&stmt);
            res = match exec_res {
                Ok(val) => Ok(val),
                Err(StateChange::Return(_)) => Err(IntErr(stmt.get_token(), ErrReason::ReturnOutsideOfFunction)),
                Err(StateChange::ErrReason(reason)) => Err(IntErr(stmt.get_token(), reason)),
                Err(StateChange::Err(err)) => Err(err),
            }?;
        }
        Ok(res)
    }

    pub fn get_env(&mut self) -> &mut Environment {
        &mut self.env
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<Option<Object>,StateChange> {
        match stmt {
            Stmt::Expr(expr) => Ok(Some(int_to_st!(self.evaluate(expr))?)),
            Stmt::If(cond, thenb, elseb) => Ok(self.exec_if(cond, thenb, elseb.as_ref().map(|v| v.as_ref()))?),
            Stmt::Print(expr) => Ok(self.exec_print(expr)?),
            Stmt::Return(_, expr) => Ok(self.exec_return(expr)?),
            Stmt::While(cond, body) => Ok(self.exec_while(cond, body)?),
            Stmt::Fun(fun) => Ok(self.exec_fun(fun.clone())?),
            Stmt::Var(name, expr) => Ok(self.exec_var(name, expr)?),
            Stmt::Block(_, stmts) => Ok(self.exec_block(stmts)?),
        }
    }

    fn exec_if(&mut self, cond: &Expr, thenb: &Stmt, elseb: Option<&Stmt>) -> Result<Option<Object>,StateChange> {
        if int_to_st!(self.evaluate(cond))?.is_truthy() {
            self.execute(thenb)
        } else if let Some(elseb) = elseb {
            self.execute(elseb)
        } else {
            Ok(None)
        }
    }

    fn exec_print(&mut self, expr: &Expr) -> Result<Option<Object>,StateChange> {
        println!("{}", int_to_st!(self.evaluate(expr))?);
        Ok(None)
    }

    fn exec_return(&mut self, expr: &Option<Expr>) -> Result<Option<Object>,StateChange> {
        match expr {
            Some(expr) => Err(StateChange::Return(int_to_st!(self.evaluate(expr))?)),
            None => Err(StateChange::Return(Object::Nil))
        }
    }

    fn exec_while(&mut self, cond: &Expr, body: &Stmt) -> Result<Option<Object>,StateChange> {
        while int_to_st!(self.evaluate(cond))?.is_truthy() {
            self.execute(body)?;
        }
        Ok(None)
    }

    fn exec_fun(&mut self, fun: Rc<FunctionStmt>) -> Result<Option<Object>,StateChange> {
        let function = Function::new(fun.clone(), self.env.get_current());
        self.env.define(fun.0.lexeme.to_string(), Object::Callable(CallableObject::new(Rc::new(function))));
        Ok(None)
    }

    fn exec_var(&mut self, name: &Token, val: &Option<Expr>) -> Result<Option<Object>,StateChange> {
        if let TokenType::Identifier(name) = &name.token_type {
            if let Some(expr) = val {
                let value = int_to_st!(self.evaluate(expr))?;
                self.env.define(name.clone(), value);
            } else {
                self.env.define(name.clone(), Object::Nil);
            }
            Ok(None)
        } else {
            Err(st_err!(name.clone(), ErrReason::ExpectedToken(TokenTypeDiscriminants::Identifier)))
        }
    }

    fn exec_block(&mut self, stmts: &Vec<Stmt>) -> Result<Option<Object>,StateChange> {
        self.env.push();
        let mut last_val: Option<Object> = None;
        for stmt in stmts {
            last_val = self.execute(stmt)?;
        }
        self.env.pop().unwrap();
        Ok(last_val)
    }

    pub fn exec_block_in_env(&mut self, stmts: &Vec<Stmt>, env: &mut Environment) -> Result<Option<Object>,StateChange> {
        //this fucking sucks there is no way this works
        //function calling needs to be implemented differently
        //maybe it was a good idea to use linked lists after all
        swap(&mut self.env, env);
        let mut return_val: Option<Object> = None;
        for stmt in stmts {
            let res = self.execute(stmt);
            match res {
                Err(StateChange::Return(val)) => {
                    return_val = Some(val);
                    break;
                }
                _ => { res?; },
            }
        }
        swap(&mut self.env, env);
        Ok(return_val)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Object,IntErr> {
        match expr {
            Expr::Assign(name, expr) => self.evaluate_assign(name, expr),
            Expr::Logical(left, op, right) => self.evaluate_logical(left, op, right),
            Expr::Binary(left, op, right) => self.evaluate_binary(left, op, right),
            Expr::Call(callee, paren, args) => self.evaluate_call(callee, paren, args),
            Expr::Unary(token, expr) => self.evaluate_unary(token, expr),
            Expr::Literal(token) => self.get_object(&token),
            Expr::Variable(name) => self.evaluate_variable(name),
            Expr::Grouping(expr) => self.evaluate(&expr),
        }
    }

    fn get_object(&mut self, token: &Token) -> Result<Object,IntErr> {
        match &token.token_type {
            TokenType::Keyword(k) => match k {
                Keyword::True => Ok(Object::Bool(true)),
                Keyword::False => Ok(Object::Bool(false)),
                Keyword::Nil => Ok(Object::Nil),
                _ => Err(int_err!(token.clone(), ErrReason::NotALiteral)),
            },
            TokenType::String(obj) => Ok(obj.clone()),
            TokenType::Number(obj) => Ok(obj.clone()),
            _ => Err(int_err!(token.clone(), ErrReason::NotALiteral)),
        }
    }

    fn evaluate_unary(&mut self, token: &Token, expr: &Expr) -> Result<Object,IntErr> {
        let right = self.evaluate(expr)?;

        match &token.token_type {
            TokenType::Minus => map_int_err!(right.neg(), token),
            TokenType::Bang => Ok(Object::Bool(!right.is_truthy())),
            _ => Err(int_err!(token.clone(), ErrReason::InvalidOperator(TokenTypeDiscriminants::from(&token.token_type)))),
        }
    }

    fn evaluate_logical(&mut self, left: &Expr, op: &Keyword, right: &Expr) -> Result<Object,IntErr> {
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

    fn evaluate_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Object,IntErr> {
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
                  ErrReason::InvalidOperator(TokenTypeDiscriminants::from(&op.token_type)))),
        }
    }

    fn evaluate_call(&mut self, callee: &Expr, paren: &Token, args: &Vec<Expr>) -> Result<Object,IntErr> {
        let callee = self.evaluate(callee)?;
        let mut eval_args = Vec::new();
        for arg in args {
            eval_args.push(self.evaluate(arg)?);
        }
        match callee.call(self, &eval_args) {
            Ok(Some(val)) => Ok(val),
            Ok(None) => Ok(Object::Nil),
            Err(StateChange::Return(value)) => Ok(value),
            Err(StateChange::ErrReason(reason)) => Err(IntErr(paren.clone(), reason)),
            Err(StateChange::Err(err)) => Err(err),
        }
    }

    fn evaluate_variable(&mut self, name: &Token) -> Result<Object,IntErr> {
        self.env.get(&name.lexeme).map_or(
            Err(int_err!(name.clone(), ErrReason::UndefinedVariable)),
            |v| Ok(v.clone())
        )
    }

    fn evaluate_assign(&mut self, name: &Token, expr: &Expr) -> Result<Object,IntErr> {
        let value = self.evaluate(expr)?;
        if let Some(_) = self.env.assign(name.lexeme.clone(), value.clone()) {
            Ok(value)
        } else {
            Err(int_err!(name.clone(), ErrReason::UndefinedVariable))
        }
    }
}

//todo (not anymore): result should not be used anymore i think, we need a new enum - haha actually
//you cant propagate errors from other enums (yet? i think? without nightly at least)
//i guess i will have to stay with this ugly ass return-in-error
#[derive(Debug, Clone)]
pub enum StateChange {
    Return(Object),
    Err(IntErr),
    ErrReason(ErrReason),
}

#[derive(Debug, Clone)]
//TODO: change to just error
pub struct IntErr(pub Token, pub ErrReason);

#[derive(Debug, Clone)]
pub enum ErrReason {
    NotALiteral,
    UndefinedVariable,
    InvalidBinaryOperands(ObjectDiscriminants, OperationType, ObjectDiscriminants),
    NotACallable(ObjectDiscriminants),
    WrongNumberOfArgs(u8, usize), //expected, given
    InvalidUnaryOperand(OperationType, ObjectDiscriminants),
    InvalidOperator(TokenTypeDiscriminants),
    ExpectedToken(TokenTypeDiscriminants),
    ReturnOutsideOfFunction,
}
