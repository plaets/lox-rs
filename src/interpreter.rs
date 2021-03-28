use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use std::mem::swap;
use gcmodule::{Cc,collect_thread_cycles};
use newtype_enum::Enum;
use crate::ast::*;
use crate::object::*;
use crate::resolver::LocalsMap;

//so i been thinking... do i need garbage collection or does rc handle all i need to do
//update: check the examples for an answer

#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
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
}

pub type EnvironmentScope = Cc<RefCell<HashMap<String, Object>>>;
pub fn new_env_scope() -> EnvironmentScope {
    Cc::new(RefCell::new(HashMap::new()))
}

#[derive(Debug)]
pub struct Environment {
    values: Vec<EnvironmentScope>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: vec![Cc::new(RefCell::new(HashMap::new()))]
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

    pub fn assign_at(&mut self, name: String, val: Object, distance: usize) -> Option<Object> {
        if let Some(env) = self.values.iter_mut().rev().nth(distance) {
            env.borrow_mut().insert(name, val) //dumb
        } else {
            None
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.values.iter().rev().find_map(|e| e.borrow().get(name).cloned())
    }

    pub fn get_at(&self, name: &str, at: usize) -> Option<Object> {
        self.values.iter().rev().nth(at).and_then(|e| e.borrow().get(name).cloned()) //does this actually work
    }

    pub fn push(&mut self) {
        self.values.push(Cc::new(RefCell::new(HashMap::new())));
    }
    
    #[allow(unused)]
    pub fn push_foreign(&mut self, scope: EnvironmentScope) {
        self.values.push(scope)
    }

    pub fn pop(&mut self) -> Option<EnvironmentScope> {
        if self.values.len() > 1 {
            self.values.pop()
        } else {
            None
        }
    }

    pub fn set_globals(&mut self, globals: EnvironmentScope) {
        self.values[0] = globals;
    }

    pub fn globals(&self) -> EnvironmentScope {
        self.values.get(0).unwrap().clone()
    }

    pub fn globals_mut(&mut self) -> EnvironmentScope {
        self.values.get(0).unwrap().clone()
    }
}

pub struct Interpreter {
    env: Environment,
    locals_map: LocalsMap,
}

macro_rules! map_int_err {
    ($e:expr,$token:expr) => { ($e).map_err(|e| int_err!($token.clone(), e)) }
}

macro_rules! st_err {
    ($t:expr,$e:expr) => { StateChange::Err(IntErr($t,$e)) }
}

macro_rules! int_err {
    ($t:expr,$e:expr) => { IntErr($t,$e) }
}

macro_rules! int_to_st {
    ($t:expr) => { ($t).map_err(StateChange::Err) }
}


impl Interpreter {
    pub fn new(locals_map: LocalsMap) -> Self {
        Self { 
            env: Environment::new(),
            locals_map,
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<Option<Object>,IntErr> {
        let mut res: Option<Object> = None;
        for stmt in statements {
            let exec_res = self.execute(&stmt);
            res = match exec_res {
                Ok(val) => Ok(val),
                Err(StateChange::Return(_)) => Err(IntErr(stmt.get_token(), 
                                  ErrReason::Critical(CriticalErrorReason::ReturnOutsideOfFunction))),
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
            Stmt::ExprStmt(expr) => Ok(Some(int_to_st!(self.evaluate(&expr.expr))?)),
            Stmt::If(stmt) => Ok(self.exec_if(stmt)?),
            Stmt::Print(stmt) => Ok(self.exec_print(stmt)?),
            Stmt::Return(stmt) => Ok(self.exec_return(stmt)?),
            Stmt::While(stmt) => Ok(self.exec_while(stmt)?),
            Stmt::Fun(stmt) => Ok(self.exec_fun(stmt)?),
            Stmt::Class(stmt) => Ok(self.exec_class(stmt)?),
            Stmt::Var(stmt) => Ok(self.exec_var(stmt)?),
            Stmt::Block(stmt) => Ok(self.exec_block(stmt)?),
        }
    }

    fn exec_if(&mut self, stmt: &StmtVar::If) -> Result<Option<Object>,StateChange> {
        if int_to_st!(self.evaluate(&stmt.cond))?.is_truthy() {
            self.execute(&stmt.then)
        } else if let Some(else_b) = &stmt.else_b {
            self.execute(&else_b)
        } else {
            Ok(None)
        }
    }

    fn exec_print(&mut self, stmt: &StmtVar::Print) -> Result<Option<Object>,StateChange> {
        println!("{}", int_to_st!(self.evaluate(&stmt.expr))?);
        Ok(None)
    }

    fn exec_return(&mut self, stmt: &StmtVar::Return) -> Result<Option<Object>,StateChange> {
        match &stmt.value {
            Some(expr) => Err(StateChange::Return(int_to_st!(self.evaluate(&expr))?)),
            None => Err(StateChange::Return(Object::Nil))
        }
    }

    fn exec_while(&mut self, stmt: &StmtVar::While) -> Result<Option<Object>,StateChange> {
        while int_to_st!(self.evaluate(&stmt.cond))?.is_truthy() {
            self.execute(&stmt.body)?;
        }
        Ok(None)
    }

    #[allow(clippy::unnecessary_wraps)]
    fn exec_fun(&mut self, stmt: &StmtVar::Fun) -> Result<Option<Object>,StateChange> {
        let fun = &stmt.stmt;
        let function = Box::new(Function::new(fun.clone(), self.env.get_current(), false));
        self.env.define(fun.0.lexeme.to_string(), Object::Callable(CallableObject::new(function)));
        Ok(None)
    }

    fn exec_class(&mut self, stmt: &StmtVar::Class) -> Result<Option<Object>,StateChange> {
        let mut superclass: Option<CcClass> = None;
        if let Some(s) = &stmt.superclass {
            match int_to_st!(self.evaluate(&Expr::from_variant(s.clone())))? {
                Object::Class(c) => superclass = Some(c),
                _ => return Err(StateChange::Err(IntErr(*s.name.clone(),
                                                      ErrReason::SuperclassIsNotAClass)))
            }
        }

        self.env.define(stmt.name.lexeme.clone(), Object::Nil);

        if let Some(ref s) = &superclass {
            self.env.push();
            self.env.define("super".to_string(), Object::Class(s.clone()));
        }
        
        let mut methods: HashMap<String,CallableObject> = HashMap::new();
        for m in stmt.methods.iter() {
            let function = CallableObject::new(Box::new(Function::new(m.stmt.clone(), self.env.get_current(), 
                                                                      m.stmt.0.lexeme == "init")));
            methods.insert(m.stmt.0.lexeme.clone(), function);
        }
        
        let class = Object::Class(CcClass(Cc::new(ClassObject::new(stmt.name.lexeme.clone(), methods, superclass.clone()))));

        if superclass.is_some() {
            self.env.pop();
        }

        self.env.assign(stmt.name.lexeme.clone(), class);
        Ok(None)
    }

    fn exec_var(&mut self, stmt: &StmtVar::Var) -> Result<Option<Object>,StateChange> {
        if let TokenType::Identifier(name) = &stmt.name.token_type {
            if let Some(expr) = &stmt.init {
                let value = int_to_st!(self.evaluate(&expr))?;
                self.env.define(name.clone(), value);
            } else {
                self.env.define(name.clone(), Object::Nil);
            }
            Ok(None)
        } else {
            Err(st_err!(*stmt.name.clone(), ErrReason::ExpectedToken(TokenTypeDiscriminants::Identifier)))
        }
    }

    fn exec_block(&mut self, stmt: &StmtVar::Block) -> Result<Option<Object>,StateChange> {
        self.env.push();
        let mut last_val: Option<Object> = None;
        for stmt in stmt.body.iter() {
            last_val = self.execute(&stmt)?;
        }
        self.env.pop().unwrap();
        collect_thread_cycles(); //TODO: should i collect this shit here?
        Ok(last_val)
    }

    pub fn exec_block_in_env(&mut self, stmts: &StmtVar::Block, env: &mut Environment) -> Result<Option<Object>,StateChange> {
        //this fucking sucks there is no way this works
        //function calling needs to be implemented differently
        //maybe it was a good idea to use linked lists after all
        swap(&mut self.env, env);
        let mut return_val: Option<Object> = None;
        for stmt in &stmts.body {
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

    fn evaluate(&mut self, m_expr: &Expr) -> Result<Object,IntErr> {
        match m_expr {
            Expr::Assign(expr) => self.evaluate_assign(&expr),
            Expr::Logical(expr) => self.evaluate_logical(&expr),
            Expr::Binary(expr) => self.evaluate_binary(&expr),
            Expr::Call(expr) => self.evaluate_call(&expr),
            Expr::Subscr(expr) => self.evaluate_subscr(&expr),
            Expr::Get(expr) => self.evaluate_get(&expr),
            Expr::Set(expr) => self.evaluate_set(&expr),
            Expr::This(expr) => self.evaluate_variable(&m_expr, &*expr.keyword),
            Expr::Super(expr) => self.evaluate_super(&expr),
            Expr::Unary(expr) => self.evaluate_unary(&expr),
            Expr::Literal(expr) => self.get_object(&expr.token),
            Expr::List(expr) => self.evaluate_list(&expr),
            Expr::Variable(expr) => self.evaluate_variable(&m_expr, &*expr.name),
            Expr::Grouping(expr) => self.evaluate(&expr.expr),
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
            TokenType::String(obj) => Ok(Object::String(obj.clone())),
            TokenType::Number(obj) => Ok(Object::Number(
                obj.parse::<f64>().expect("valid number"),
            )),
            _ => Err(int_err!(token.clone(), ErrReason::NotALiteral)),
        }
    }

    fn evaluate_unary(&mut self, expr: &ExprVar::Unary) -> Result<Object,IntErr> {
        let right = self.evaluate(&expr.expr)?;
        let op = &*expr.op;

        match &op.token_type {
            TokenType::Minus => map_int_err!(right.neg(), op.clone()),
            TokenType::Bang => Ok(Object::Bool(!right.is_truthy())),
            _ => Err(int_err!(op.clone(), ErrReason::InvalidOperator(TokenTypeDiscriminants::from(&op.token_type)))),
        }
    }
    
    fn evaluate_list(&mut self, expr: &ExprVar::List) -> Result<Object,IntErr> {
        let mut list = Vec::new();
        for n in &expr.elems {
            list.push(self.evaluate(&n)?);
        }
        Ok(Object::List(CcList::from_vec(list)))
    }

    fn evaluate_logical(&mut self, expr: &ExprVar::Logical) -> Result<Object,IntErr> {
        let left = self.evaluate(&expr.left)?;
        if *expr.op == Keyword::Or {
            if left.is_truthy() {
                return Ok(left)
            }
        } else if *expr.op == Keyword::And && !left.is_truthy() {
            return Ok(left)
        } 

        self.evaluate(&expr.right)
    }

    fn evaluate_binary(&mut self, expr: &ExprVar::Binary) -> Result<Object,IntErr> {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;
        let op = &*expr.op;

        match &expr.op.token_type {
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

    fn evaluate_call(&mut self, expr: &ExprVar::Call) -> Result<Object,IntErr> {
        let callee = self.evaluate(&expr.callee)?;
        let mut eval_args = Vec::new();
        for arg in expr.args.iter() {
            eval_args.push(self.evaluate(arg)?);
        }
        match callee.call(self, &eval_args) {
            Ok(Some(val)) => Ok(val),
            Ok(None) => Ok(Object::Nil),
            Err(StateChange::Return(value)) => Ok(value),
            Err(StateChange::ErrReason(reason)) => Err(IntErr(*expr.left_paren.clone(), reason)),
            Err(StateChange::Err(err)) => Err(err),
        }
    }

    fn evaluate_subscr(&mut self, expr: &ExprVar::Subscr) -> Result<Object,IntErr> {
        let object = self.evaluate(&expr.object)?;
        let arg = self.evaluate(&expr.arg)?;
        match object.subscr(self, &arg) {
            Ok(Some(val)) => Ok(val),
            Ok(None) => Ok(Object::Nil),
            Err(StateChange::Return(value)) => Ok(value),
            Err(StateChange::ErrReason(reason)) => Err(IntErr(*expr.left_paren.clone(), reason)),
            Err(StateChange::Err(err)) => Err(err),
        }
    }

    fn evaluate_get(&mut self, expr: &ExprVar::Get) -> Result<Object,IntErr> {
        match self.evaluate(&expr.object)?.get(&expr.name.lexeme) {
            Ok(v) => Ok(v),
            Err(StateChange::ErrReason(reason)) => Err(IntErr(*expr.name.clone(), reason)),
            Err(StateChange::Return(value)) => Ok(value),
            Err(StateChange::Err(err)) => Err(err),
        }
    }

    fn evaluate_set(&mut self, expr: &ExprVar::Set) -> Result<Object,IntErr> {
        let mut obj = self.evaluate(&expr.object)?;
        let value = self.evaluate(&expr.value)?;
        match obj.set(&*expr.name.lexeme, value) {
            Ok(v) => Ok(v),
            Err(StateChange::ErrReason(reason)) => Err(IntErr(*expr.name.clone(), reason)),
            Err(StateChange::Return(value)) => Ok(value),      //TODO: this will never happen (unless???)
            Err(StateChange::Err(err)) => Err(err),             //same for this
        }
    }

    fn get_global_variable(&self, name: &Token) -> Result<Object,IntErr> {
        self.env.globals().borrow().get(&name.lexeme).map_or(
            Err(int_err!(name.clone(), ErrReason::UndefinedVariable)),
            |v| Ok(v.clone()),
        )
    }

    fn get_local_variable(&self, name: &Token, distance: usize) -> Result<Object,IntErr> {
        self.env.get_at(&name.lexeme, distance).map_or(
            Err(int_err!(name.clone(), ErrReason::Critical(CriticalErrorReason::LocalVariableNotFound))), //should never happen i think?
            Ok,
        )
    }

    //not the most error-proof function i guess
    fn evaluate_variable(&mut self, expr: &Expr, name: &Token) -> Result<Object,IntErr> {
        if let Some(distance) = self.locals_map.0.get(&expr) {
            self.get_local_variable(&name, *distance)
        } else {
            self.get_global_variable(&name)
        }
    }

    fn evaluate_super(&mut self, expr: &ExprVar::Super) -> Result<Object,IntErr> {
        let distance = self.locals_map.0.get(&Expr::from_variant(expr.clone())).unwrap();
        if let Object::Class(superclass) = self.env.get_at("super", *distance).unwrap() {
            let method = superclass.find_method(&expr.method.lexeme);
            if method.is_none() {
                return Err(IntErr(*expr.method.clone(), ErrReason::UndefinedProperty))
            }
            if let Object::Instance(c) = self.env.get_at("this", distance-1).unwrap() {
                Ok(Object::Callable(CallableObject::new(Box::new(BoundCallable{ callable: method.unwrap(), bound: c }))))
            } else {
                Err(int_err!(*expr.keyword.clone(), ErrReason::Critical(CriticalErrorReason::ThisIsNotAnInstance)))
            }
        } else {
            Err(int_err!(*expr.keyword.clone(), ErrReason::Critical(CriticalErrorReason::SuperIsNotAClass)))
        }
    }

    fn evaluate_assign(&mut self, expr: &ExprVar::Assign) -> Result<Object,IntErr> {
        let value = self.evaluate(&expr.expr)?;
       
        if let Some(distance) = self.locals_map.0.get(&Expr::from_variant(expr.clone())) { //this probably too
            if self.env.assign_at(expr.name.lexeme.clone(), value.clone(), *distance).is_some() {
                Ok(value)
            } else {
                Err(int_err!(*expr.name.clone(), ErrReason::Critical(CriticalErrorReason::LocalVariableNotFound)))
            }
        } else if self.env.assign(expr.name.lexeme.clone(), value.clone()).is_some() {
                Ok(value)
        } else {
            Err(int_err!(*expr.name.clone(), ErrReason::UndefinedVariable))
        }
    }
}

//todo (not anymore): result should not be used anymore i think, we need a new enum - haha actually
//you cant propagate errors from other enums (yet? i think? without nightly at least)
//i guess i will have to stay with this ugly ass return-in-error
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum StateChange {
    Return(Object),
    Err(IntErr),
    ErrReason(ErrReason),
}

#[derive(Debug, Clone)]
//TODO: change to just error
pub struct IntErr(pub Token, pub ErrReason);

pub trait NativeError: core::fmt::Debug + core::fmt::Display { }

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum ErrReason {
    NotALiteral,
    UndefinedVariable,
    InvalidBinaryOperands(ObjectDiscriminants, OperationType, ObjectDiscriminants),
    NotACallable(ObjectDiscriminants),
    NotSubscriptable(ObjectDiscriminants),
    OutOfBounds(usize, usize), //expected, given
    UnexpectedType(ObjectDiscriminants, ObjectDiscriminants), //expected, given
    NotAnInstance,
    UndefinedProperty,
    WrongNumberOfArgs(u8, usize), //expected, given
    SuperclassIsNotAClass,
    NativeError(Rc<dyn NativeError>),
    InvalidUnaryOperand(OperationType, ObjectDiscriminants),
    InvalidOperator(TokenTypeDiscriminants),
    ExpectedToken(TokenTypeDiscriminants),
    Critical(CriticalErrorReason),
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum CriticalErrorReason {
    LocalVariableNotFound,
    ReturnOutsideOfFunction,
    SuperIsNotAClass,
    ThisIsNotAnInstance,
}
