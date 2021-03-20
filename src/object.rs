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

//all of this is way too complicated tbh
//i will need to come up with something better

//trait for everything that can be called
pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange>;
    fn call_with_bound(&self, interpreter: &mut Interpreter, args: &[Object], _bound: EnvironmentScope) 
        -> Result<Option<Object>,StateChange>;
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

impl Trace for Box<dyn Callable> {
    //i wanted to call trace from dyn Callable but i dnont know how
    fn trace(&self, tracer: &mut Tracer) {
        if let Some(env) = self.get_closure() {
            env.trace(tracer)
        }
    }
}

#[derive(Clone)]
pub struct CallableObject(Cc<Box<dyn Callable>>);

//Cc<BoxValues> -> Cc<Box<dyn Callable>> there are like two layers of indirection, do i really need
//that box? probably but i dont remember why
//all the cc related boilerprate is way too complicated rn 

impl CallableObject {
    pub fn new(arg: Box<dyn Callable>) -> Self {
        Self(Cc::new(arg))
    }
}

impl Trace for CallableObject {
    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer)
    }
}

//why is everything fun forbidden
impl core::ops::Deref for CallableObject {
    type Target = Cc<Box<dyn Callable>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq for CallableObject {
    fn eq(&self, other: &Self) -> bool {
        let left: *const Box<dyn Callable> = &*self.0;
        let right: *const Box<dyn Callable> = &*other.0;
        left == right
    }
}

impl fmt::Debug for CallableObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CallableObject")
    }
}

//struct for functions defined in loc
#[derive(Debug, Clone, PartialEq, Trace)]
pub struct Function {
    declaration: CcFunctionStmt,
    closure: Vec<EnvironmentScope>,     //maybe env as a linked list was a good idea? im sure i will intrudce so many cool bugs by trying to use slices here
    is_init: bool,
}

impl Function {
    pub fn new(declaration: CcFunctionStmt, closure: Vec<EnvironmentScope>, is_init: bool) -> Self {
        Self {
            declaration,
            closure,
            is_init,
        }
    }

    fn finalize_call(&self, interpreter: &mut Interpreter, args: &[Object], env: &mut Environment) -> Result<Option<Object>,StateChange> {
        env.push();
        for (name,val) in self.declaration.1.iter().zip(args.iter()) {
            env.define(name.lexeme.clone(), val.clone());
        }
        interpreter.exec_block_in_env(&self.declaration.2, env)
    }
}

impl Callable for Function {
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange> {
        let mut env = Environment::new_with(self.closure.clone());
        self.finalize_call(interpreter, args, &mut env)
    }

    fn arity(&self) -> u8 {
        self.declaration.1.len() as u8
    }

    fn get_closure(&self) -> Option<&Vec<EnvironmentScope>> {
        Some(&self.closure)
    }

    fn call_with_bound(&self, interpreter: &mut Interpreter, args: &[Object], bound: EnvironmentScope) 
            -> Result<Option<Object>,StateChange> {
        let mut env = Environment::new_with(self.closure.clone());
        env.push_foreign(bound.clone());
        let res = self.finalize_call(interpreter, args, &mut env)?;
        if self.is_init {
            Ok(Some(bound.borrow().get("this").unwrap().clone())) //finalize_call pushes one env, shit is sorta fishy ngl
        } else {
            Ok(res)
        }
    }
}

/////// CLASSES

#[derive(Clone, Debug, PartialEq, Trace)]
pub struct CcClass(pub Cc<ClassObject>);

impl std::ops::Deref for CcClass {
    type Target = Cc<ClassObject>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Trace)]
pub struct ClassObject {
    name: String,
    methods: HashMap<String,CallableObject>,
    superclass: Option<CcClass>,
}

impl ClassObject {
    pub fn new(name: String, methods: HashMap<String,CallableObject>, superclass: Option<CcClass>) -> Self {
        Self {
            name,
            methods,
            superclass,
        }
    }

    pub fn find_method(&self, name: &str) -> Option<CallableObject> {
        match self.methods.get(name).map(|v| v.clone()) {
            Some(m) => Some(m),
            None => {
                if let Some(superclass) = &self.superclass {
                    superclass.find_method(name)
                } else {
                    None
                }
            }
        }
    }
}

impl Callable for CcClass {
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange> {
        let instance = CcInstanceObject::new(self.0.clone());
        if let Some(init) = self.find_method("init") {
            let env = new_env_scope();
            env.borrow_mut().insert("this".to_string(), Object::Instance(instance.clone()));
            init.call_with_bound(interpreter, args, env)?;
        }
        Ok(Some(Object::Instance(instance)))
    }

    fn arity(&self) -> u8 {
        if let Some(init) = self.find_method("init") {
            init.arity()
        } else {
            0
        }
    }

    fn call_with_bound(&self, interpreter: &mut Interpreter, args: &[Object], _bound: EnvironmentScope) -> Result<Option<Object>,StateChange> {
        self.call(interpreter, args)
    }

    fn get_closure(&self) -> Option<&Vec<EnvironmentScope>> {
        None
    }
}

//:__:
pub struct BoundCallable {
    pub callable: CallableObject,
    pub bound: CcInstanceObject,
}

impl Callable for BoundCallable {
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange> {
        let env = new_env_scope();
        env.borrow_mut().insert("this".to_string(), Object::Instance(self.bound.clone()));
        self.callable.call_with_bound(interpreter, args, env)
    }

    //alos this just silently fails if callable does not support it lmao
    fn call_with_bound(&self, interpreter: &mut Interpreter, args: &[Object], bound: EnvironmentScope) 
        -> Result<Option<Object>,StateChange> {
        //wont contain this lol
        self.callable.call_with_bound(interpreter, args, bound)
    }

    fn arity(&self) -> u8 {
        self.callable.arity()
    }

    fn get_closure(&self) -> Option<&Vec<EnvironmentScope>> {
        self.callable.get_closure() //won't contain this tho
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

    pub fn get(&self, name: &str) -> Option<Object> {
        if let Some(v) = (*self.0).borrow().fields.get(name) {
            Some(v.clone())
        } else if let Some(m) = (*self.0).borrow().class.find_method(name) {
            Some(Object::Callable(CallableObject::new(Box::new(BoundCallable{ callable: m, bound: self.clone() }))))
        } else {
            None
        }
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

    pub fn set(&mut self, name: &str, value: Object) {
        self.fields.insert(name.to_string(), value);
    }
}
