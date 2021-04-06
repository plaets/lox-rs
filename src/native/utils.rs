use std::rc::Rc;
use crate::object::{Object,CallableObject,CcClass,CcInstanceObject};
use crate::interpreter::{StateChange,EnvironmentScope,ErrReason,NativeError};

#[derive(Debug)]
pub struct NativeInventoryEntry(pub String, pub CallableObject);
pub type ReturnType = Result<Option<Object>,StateChange>;

#[derive(Debug,Clone)]
pub struct SimpleError {
    pub reason: String,
}

impl core::fmt::Display for SimpleError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.reason)
    }
}

impl SimpleError {
    pub fn new(reason: &str) -> Self {
        Self {
            reason: reason.to_string(),
        }
    }

    pub fn from_string(reason: String) -> Self {
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
    ($name:ident,$str_name:expr,$arity:expr,$body:expr) => {
        paste! {
            #[derive(Trace)]
            pub struct [<Native $name>];
            impl Callable for [<Native $name>] {
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
                NativeInventoryEntry($str_name.to_owned(), CallableObject::new(Box::new([<Native $name>]{})))
            }
        }
    }
}

#[derive(Debug)]
pub struct NativeClassInventoryEntry(pub CcClass);

macro_rules! sumbit_method {
    ($name:ident,$method_name:ident) => {
        inventory::submit! {
            paste! {
                use crate::object::CallableObject;
                [<$name NativeMethodInventoryEntry>](std::stringify!($method_name).to_owned(), CallableObject::new(Box::new($method_name{})))
            }
        }
    }
}

macro_rules! finish_inventory {
    ($name:ident,$entry:ident) => {
        paste! {
            inventory::collect!([<$name NativeMethodInventoryEntry>]);
            inventory::submit! {
                use std::collections::HashMap;
                use crate::object::{CallableObject,ClassObject};
                let mut methods: HashMap<String,CallableObject> = HashMap::new();
                for entry in inventory::iter::<[<$name NativeMethodInventoryEntry>]> {
                    methods.insert(entry.0.clone(), entry.1.clone());
                }
                $entry(
                    CcClass(Cc::new(ClassObject::new(std::stringify!($name).to_string(), methods, None, vec![]))))
            }
        }
    }
}

macro_rules! define_method {
    ($name:ident,$method_name:ident,$arity:expr,$body:expr) => {
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
    }
}

#[macro_export]
macro_rules! define_native_class {
    ($name:ident $(,$method_name:ident,$arity:expr,$body:expr)*) => {
        define_native_class!($name,NativeClassInventoryEntry $(,$method_name, $arity, $body)*);
    };
    ($name:ident,$entry:ident $(, $method_name:ident,$arity:expr,$body:expr)*) => {
        mod $name {
            use super::*;
            use gcmodule::Trace;
            use crate::interpreter::new_env_scope;
            use paste::paste;

            //all classes will be generated without methods if paste is omitted...
            paste! {
                #[derive(Debug)]
                pub struct [<$name NativeMethodInventoryEntry>](pub String, pub CallableObject);

                $(
                    define_method!($name,$method_name,$arity,$body);
                    sumbit_method!($name,$method_name);
                )*

                finish_inventory!($name,NativeClassInventoryEntry);
            }
        }
    }
}

macro_rules! assert_object {
    ($type:ident,$what:expr) => {
        {
            use crate::object::ObjectDiscriminants;
            if let Object::$type(n) = $what {
                Ok(n)
            } else {
                Err(StateChange::ErrReason(ErrReason::UnexpectedType(ObjectDiscriminants::$type,$what.into()))) 
            }
        }
    }
}

pub fn get_or_err(bound: &EnvironmentScope, name: &str) -> Result<CcInstanceObject,StateChange> {
    if let Some(Object::Instance(i)) = bound.borrow_mut().get(name).cloned() {
        Ok(i)
    } else {
        native_err!(SimpleError::new("This is not an instance"))
    }
}
