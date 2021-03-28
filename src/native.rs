use std::time;
use std::rc::Rc;
use std::cell::RefCell;
use std::any::Any;
use std::io::{Read,Seek,SeekFrom};
use std::fs::File as RsFile;
use gcmodule::Trace;
use paste::paste;
use crate::object::{Callable,Object,CallableObject,CcClass,CcInstanceObject,NativeObject,CcNativeObject};
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
    ($name:ident, $($method_name:ident,$arity:expr,$body:expr),*) => {
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

fn this_or_err(bound: &EnvironmentScope) -> Result<CcInstanceObject,StateChange> {
    if let Some(Object::Instance(i)) = bound.borrow_mut().get("this").cloned() {
        Ok(i)
    } else {
        native_err!(SimpleError::new("This is not an instance"))
    }
}

//so like three of four levels of pointer indirection
#[derive(Debug)]
struct FileHandle(pub RefCell<std::fs::File>);
impl Trace for FileHandle {}
impl NativeObject for FileHandle {
    fn get_any(&self) -> &dyn std::any::Any { self }
}

define_native_class!(
    File,
    init, Some(1), |_,args: &[Object],bound: EnvironmentScope| -> ReturnType {
        let mut this = this_or_err(&bound)?;
        if let Object::String(s) = &args[0] {
            if let Ok(f) = RsFile::open(s) {
                this.set("__handle", Object::Native(CcNativeObject(Cc::new(Box::new(FileHandle(RefCell::new(f)))))));
                Ok(None)
            } else {
                native_err!(SimpleError::new("Failed to open a file"))
            }
        } else {
            native_err!(SimpleError::new("Expected a string"))
        }
    },
    read, None, |_,args: &[Object],bound: EnvironmentScope| -> ReturnType {
        let mut this = this_or_err(&bound)?;
        if let Some(Object::Native(n)) = &this.get("__handle") {
            if let Some(f) = n.0.get_any().downcast_ref::<FileHandle>() {
                let mut buffer = String::new();
                let mut fd = f.0.borrow_mut();
                match args.len() {
                    0 => { fd.read_to_string(&mut buffer); },
                    1 => if let Object::Number(n) = args[0] {
                        fd.by_ref().take(n as u64).read_to_string(&mut buffer);
                    } else {
                        return native_err!(SimpleError::new("Expected a number"))
                    }
                    _ => return native_err!(SimpleError::new("Expected either 0 or 1 arguments"))
                }
                Ok(Some(Object::String(buffer)))
            } else {
                native_err!(SimpleError::new("Invalid handle"))
            }
        } else {
            native_err!(SimpleError::new("Could not find file handle"))
        }
    },
    seek, Some(1), |_,args: &[Object],bound: EnvironmentScope| -> ReturnType {
        let mut this = this_or_err(&bound)?;
        if let Some(Object::Native(n)) = &this.get("__handle") {
            if let Some(f) = n.0.get_any().downcast_ref::<FileHandle>() {
                if let Object::Number(n) = args[0] {
                    f.0.borrow_mut().seek(SeekFrom::Start(n as u64));
                    Ok(None)
                } else {
                    native_err!(SimpleError::new("Expected a number"))
                }
            } else {
                native_err!(SimpleError::new("Invalid handle"))
            }
        } else {
            native_err!(SimpleError::new("Could not find file handle"))
        }
    },
    size, Some(0), |_,args: &[Object],bound: EnvironmentScope| -> ReturnType {
        let mut this = this_or_err(&bound)?;
        if let Some(Object::Native(n)) = &this.get("__handle") {
            if let Some(f) = n.0.get_any().downcast_ref::<FileHandle>() {
                let mut f = f.0.borrow_mut();
                let pos = f.stream_position().unwrap();
                let len = f.seek(SeekFrom::End(0)).unwrap();
                f.seek(SeekFrom::Start(pos));
                Ok(Some(Object::Number(len as f64)))
            } else {
                native_err!(SimpleError::new("Invalid handle"))
            }
        } else {
            native_err!(SimpleError::new("Could not find file handle"))
        }
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
