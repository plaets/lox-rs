use std::time;
use std::rc::Rc;
use std::cell::RefCell;
use std::io::{Read,Seek,SeekFrom};
use std::fs::File as RsFile;
use gcmodule::{Trace,Cc};
use crate::native::utils::*;
use crate::object::{Callable,Object,CallableObject,CcClass,NativeObject,CcNativeObject};
use crate::interpreter::{Interpreter,StateChange,EnvironmentScope,ErrReason};

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
        let mut this = get_or_err(&bound, "this")?;
        let s = assert_object!(String, &args[0])?;
        if let Ok(f) = RsFile::open(s) {
            this.set("__handle", Object::Native(CcNativeObject(Cc::new(Box::new(FileHandle(RefCell::new(f)))))));
            Ok(None)
        } else {
            native_err!(SimpleError::new("Failed to open a file"))
        }
    },
    read, None, |_,args: &[Object],bound: EnvironmentScope| -> ReturnType {
        let this = get_or_err(&bound, "this")?;
        if let Some(Object::Native(n)) = &this.get("__handle") {
            if let Some(f) = n.0.get_any().downcast_ref::<FileHandle>() {
                let mut buffer = String::new();
                let mut fd = f.0.borrow_mut();
                match args.len() {
                    0 => { fd.read_to_string(&mut buffer); },
                    1 => { fd.by_ref().take(*assert_object!(Number, &args[0])? as u64).read_to_string(&mut buffer); },
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
        let this = get_or_err(&bound, "this")?;
        if let Some(Object::Native(n)) = &this.get("__handle") {
            if let Some(f) = n.0.get_any().downcast_ref::<FileHandle>() {
                f.0.borrow_mut().seek(SeekFrom::Start(*assert_object!(Number, &args[0])? as u64));
                Ok(None)
            } else {
                native_err!(SimpleError::new("Invalid handle"))
            }
        } else {
            native_err!(SimpleError::new("Could not find file handle"))
        }
    },
    size, Some(0), |_,_,bound: EnvironmentScope| -> ReturnType {
        let this = get_or_err(&bound, "this")?;
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

use paste::paste;
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
    std::process::exit(*assert_object!(Number, &args[0])? as i32)
});

define_native!(len,Some(1),|_,args: &[Object]| -> ReturnType {
    match &args[0] {
        Object::String(n) => Ok(Some(Object::Number(n.len() as f64))),
        Object::List(n) => Ok(Some(Object::Number(n.0.borrow().len() as f64))),
        _ => native_err!(SimpleError::new("Expected a list or a string")),
    }
});
