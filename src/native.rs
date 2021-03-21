use std::time;
use std::rc::Rc;
use gcmodule::Trace;
use inventory;
use crate::object::{Callable,Object};
use crate::interpreter::{Interpreter,StateChange,EnvironmentScope,ErrReason,NativeError};

pub struct NativeInventoryEntry(pub String, pub Object);
type ReturnType = Result<Option<Object>,StateChange>;

#[derive(Debug,Clone)]
struct SimpleError {
    pub reason: String,
}

impl core::fmt::Display for SimpleError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.reason)
    }
}

impl SimpleError {
    fn new(reason: &str) -> Self {
        Self {
            reason: reason.to_string(),
        }
    }
}

impl NativeError for SimpleError {}

macro_rules! native_err {
    ($body:expr) => { Err(StateChange::ErrReason(ErrReason::NativeError(Rc::new($body)))) }
}

macro_rules! define_native {
    ($name:ident,$body:expr) => { define_native!($name,None,$body); };
    ($name:ident,$arity:expr,$body:expr) => { define_native!($name,std::stringify!($name),{$arity},$body); };
    ($name:ident,$str_name:expr,$arity:expr,$body:expr) => {
        #[derive(Trace)]
        pub struct $name;
        impl Callable for $name {
            #[allow(unused_variables)]
            fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange> {
                $body(interpreter,args)
            }

            fn call_with_bound(&self, interpreter: &mut Interpreter, args: &[Object], _bound: EnvironmentScope) 
                -> Result<Option<Object>,StateChange> {
                self.call(interpreter, args)
            }

            fn arity(&self) -> Option<u8> {
                $arity
            }
        }
        inventory::submit! {
            use crate::object::CallableObject;
            NativeInventoryEntry($str_name.to_owned(), Object::Callable(CallableObject::new(Box::new($name{}))))
        }
    }
}

define_native!(clock,Some(0),|_,_| -> ReturnType {
    Ok(Some(Object::Number(time::SystemTime::now().duration_since(time::SystemTime::UNIX_EPOCH).unwrap().as_nanos() as f64)))
});

define_native!(input,Some(0),|_,_| -> ReturnType {
    let mut buffer = String::new();
    let stdin = std::io::stdin();
    stdin.read_line(&mut buffer);
    Ok(Some(Object::String(buffer.trim_end().to_string())))
});

define_native!(exit,Some(1),|_,args: &[Object]| -> ReturnType {
    if let Some(Object::Number(n)) = args.first() {
        std::process::exit(*n as i32)
    } else {
        native_err!(SimpleError::new("Argument must be a number"))
    }
});
