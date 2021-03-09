use std::time;
use crate::interpreter::{Callable,Interpreter,InterpreterErrorReason,Object};

pub struct Clock { }
impl Callable for Clock {
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Object>) -> Result<Option<Object>,InterpreterErrorReason> {
        Ok(Some(Object::Number(time::SystemTime::now().duration_since(time::SystemTime::UNIX_EPOCH).unwrap().as_secs() as f64)))
    }

    fn arity(&self) -> u8 {
        0
    }
}
