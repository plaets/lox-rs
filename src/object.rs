use std::fmt;
use std::collections::HashMap;
use std::cell::RefCell;
use gcmodule::{Cc,Trace,Tracer};
use strum_macros::EnumDiscriminants;
use crate::interpreter::*;
use crate::ast::CcFunctionStmt;

#[derive(Clone, Debug, PartialEq, EnumDiscriminants)]
#[non_exhaustive]
pub enum Object {
    Nil,
    Bool(bool),
    String(String),
    Number(f64),
    Callable(CallableObject),
    Class(CcClass),
    Instance(CcInstanceObject),
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
            Object::Class(_) => true, //???
            Object::Instance(_) => true, //???
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
            (Object::Number(a), Object::Number(b)) => Ok(Object::Bool((a-b).abs() < f64::EPSILON)),
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

    pub fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Self>,StateChange> {
        match self {
            Object::Callable(f) => {
                if f.arity() as usize == args.len() {
                    f.call(interpreter, args)
                } else {
                    Err(StateChange::ErrReason(ErrReason::WrongNumberOfArgs(f.arity(), args.len())))
                }
            },
            Object::Class(c) => {
                if c.arity() as usize == args.len() {
                    c.call(interpreter, args)
                } else {
                    Err(StateChange::ErrReason(ErrReason::WrongNumberOfArgs(c.arity(), args.len())))
                }
            },
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
            Object::Class(c) => write!(f, "Class<{}>", c.name),
            Object::Instance(c) => write!(f, "Instance<{}>", c.borrow().class.name),
        }
    }
}

impl Trace for Object {
    fn trace(&self, tracer: &mut Tracer) {
        match self {
            Object::Callable(o) => o.trace(tracer),
            Object::Class(o) => o.trace(tracer),
            Object::Instance(o) => o.trace(tracer),
            _ => {},
        }
    }
}

/////// FUNCTIONS

//trait for everything that can be called
pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange>;
    fn arity(&self) -> u8;
    fn get_closure(&self) -> Option<&Vec<EnvironmentScope>> {
        None
    }
}

impl Trace for dyn Callable {
    fn trace(&self, tracer: &mut Tracer) {
        if let Some(env) = self.get_closure() {
            env.trace(tracer)
        }
    }
}

//why the fuck is this called "BoxValues"
//i remember that this was needed because dyn
pub struct BoxValues(pub Box<dyn Callable>);

impl Trace for BoxValues {
    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer)
    }
}

impl std::ops::Deref for BoxValues {
    type Target = Box<dyn Callable>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Trace)]
pub struct CallableObject(Cc<BoxValues>);

impl CallableObject {
    pub fn new(arg: Cc<BoxValues>) -> Self {
        Self(arg)
    }
}

impl Callable for CallableObject {
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange> {
        self.0.call(interpreter, args)
    }

    fn arity(&self) -> u8 {
        self.0.arity()
    }

    fn get_closure(&self) -> Option<&Vec<EnvironmentScope>> {
        self.0.get_closure()
    }
}

impl PartialEq for CallableObject {
    fn eq(&self, other: &Self) -> bool {
        let left: *const BoxValues = &*self.0;
        let right: *const BoxValues = &*other.0;
        left == right
    }
}

impl fmt::Debug for CallableObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CallableObject")
    }
}

//struct for functions defined in loc
#[derive(Trace)]
pub struct Function {
    declaration: CcFunctionStmt,
    closure: Vec<EnvironmentScope>,     //maybe env as a linked list was a good idea? im sure i will intrudce so many cool bugs by trying to use slices here
}

impl Function {
    pub fn new(declaration: CcFunctionStmt, closure: Vec<EnvironmentScope>) -> Self {
        Self {
            declaration,
            closure,
        }
    }
}

impl Callable for Function {
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange> {
        let mut env = Environment::new_with(self.closure.clone());
        env.push();
        for (name,val) in self.declaration.1.iter().zip(args.iter()) {
            env.define(name.lexeme.clone(), val.clone());
        }
        interpreter.exec_block_in_env(&self.declaration.2, &mut env)
    }

    fn arity(&self) -> u8 {
        self.declaration.1.len() as u8
    }

    fn get_closure(&self) -> Option<&Vec<EnvironmentScope>> {
        Some(&self.closure)
    }
}

/////// CLASSES

#[derive(Clone, Debug, PartialEq, Eq, Trace)]
pub struct CcClass(pub Cc<ClassObject>);

impl std::ops::Deref for CcClass {
    type Target = Cc<ClassObject>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
pub struct ClassObject {
    name: String,
}

impl ClassObject {
    pub fn new(name: String) -> Self {
        Self {
            name,
        }
    }
}

impl Callable for CcClass {
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange> {
        Ok(Some(Object::Instance(CcInstanceObject::new(self.0.clone()))))
    }

    fn arity(&self) -> u8 {
        0
    }

    fn get_closure(&self) -> Option<&Vec<EnvironmentScope>> {
        None
    }
}

#[derive(Debug, Clone, PartialEq, Trace)]
pub struct CcInstanceObject(pub Cc<RefCell<InstanceObject>>);

impl CcInstanceObject {
    pub fn new(class: Cc<ClassObject>) -> Self {
        Self(Cc::new(RefCell::new(InstanceObject {
            class,
            fields: HashMap::new(),
        })))
    }
}

impl std::ops::Deref for CcInstanceObject {
    type Target = Cc<RefCell<InstanceObject>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Trace)]
pub struct InstanceObject {
    class: Cc<ClassObject>,
    fields: HashMap<String,Object>,
}

impl InstanceObject {
    pub fn new(class: Cc<ClassObject>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.fields.get(name).map(|v| v.clone())
    }

    pub fn set(&mut self, name: &str, value: Object) {
        self.fields.insert(name.to_string(), value);
    }
}
