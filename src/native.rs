use std::time;
use std::rc::Rc;
use gcmodule::Trace;
use inventory;
use paste::paste;
use crate::object::{Callable,Object,CallableObject,CcClass};
use crate::interpreter::{Interpreter,StateChange,EnvironmentScope,ErrReason,NativeError};

#[derive(Debug)]
pub struct NativeInventoryEntry(pub String, pub CallableObject);
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

    fn from_string(reason: String) -> Self {
        Self {
            reason,
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
    ($name:ident,$arity:expr,$body:expr) => { define_native!($name,std::stringify!($name),{$arity},$obj,$body); };
    ($name:ident,$str_name:expr,$arity:expr,$body:expr) => {
        #[derive(Trace)]
        pub struct $name;
        impl Callable for $name {
            #[allow(unused_variables)]
            fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange> {
                $body(interpreter, args)
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
            NativeInventoryEntry($str_name.to_owned(), CallableObject::new(Box::new($name{})))
        }
    }
}

#[derive(Debug)]
pub struct NativeClassInventoryEntry(pub CcClass);

macro_rules! define_native_class {
    ($name:ident,$($method_name:ident,$arity:expr,$body:expr),*) => {
        mod $name {
            use super::*;
            use std::collections::HashMap;
            use gcmodule::{Trace,Cc};
            use inventory;
            use crate::object::*;
            use crate::interpreter::new_env_scope;

            //all classes will be generated without methods if paste is omitted...
            paste! {
            #[derive(Debug)]
            pub struct [<$name NativeMethodInventoryEntry>](pub String, pub CallableObject);

            $(
                #[derive(Trace)]
                pub struct $method_name;
                impl Callable for $method_name {
                    #[allow(unused_variables)]
                    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Option<Object>,StateChange> {
                        self.call_with_bound(interpreter, args, new_env_scope())
                    }

                    //TODO
                    fn call_with_bound(&self, interpreter: &mut Interpreter, args: &[Object], bound: EnvironmentScope) 
                        -> Result<Option<Object>,StateChange> {
                        $body(interpreter, args, bound)
                    }

                    fn arity(&self) -> Option<u8> {
                        $arity
                    }
                }
                inventory::submit! {
                    use crate::object::CallableObject;
                    [<$name NativeMethodInventoryEntry>](std::stringify!($method_name).to_owned(), CallableObject::new(Box::new($method_name{})))
                }
            )*

            inventory::collect!([<$name NativeMethodInventoryEntry>]);
            inventory::submit! {
                use crate::object::CallableObject;
                let mut methods: HashMap<String,CallableObject> = HashMap::new();
                for entry in inventory::iter::<[<$name NativeMethodInventoryEntry>]> {
                    methods.insert(entry.0.clone(), entry.1.clone());
                }
                NativeClassInventoryEntry(
                    CcClass(Cc::new(ClassObject::new(std::stringify!($name).to_string(), methods, None))))
            }
            }
        }
    }
}

define_native_class!(
    CoolClass,
    cool_method, None, |_,_,_| -> ReturnType {
        Ok(Some(Object::String("asd".to_string())))
    },
    a, None, |_,_,_| -> ReturnType {
        Ok(Some(Object::String("zxc".to_string())))
    }
);

define_native_class!(
    CoolerClass,
    cool_method, None, |_,_,_| -> ReturnType {
        Ok(Some(Object::String("asd".to_string())))
    },
    b, None, |_,_,bound: EnvironmentScope| -> ReturnType {
        if let Some(Object::Instance(mut i)) =  bound.borrow_mut().get("this").cloned() {
            i.set("cool_method", Object::String("lol".to_string()));
        }
        Ok(Some(Object::String("zxc".to_string())))
    }
);

define_native!(clock,Some(0),|_,_| -> ReturnType {
    Ok(Some(Object::Number(time::SystemTime::now().duration_since(time::SystemTime::UNIX_EPOCH).unwrap().as_nanos() as f64)))
});

define_native!(input,Some(0),|_,_| -> ReturnType {
    let mut buffer = String::new();
    let stdin = std::io::stdin();
    match stdin.read_line(&mut buffer) {
        Ok(_) => Ok(Some(Object::String(buffer.trim_end().to_string()))),
        Err(err) => native_err!(SimpleError::from_string(format!("Reading error: {:?}", err)))
    }
});

define_native!(exit,Some(1),|_,args: &[Object]| -> ReturnType {
    if let Some(Object::Number(n)) = args.first() {
        std::process::exit(*n as i32)
    } else {
        native_err!(SimpleError::new("Argument must be a number"))
    }
});
