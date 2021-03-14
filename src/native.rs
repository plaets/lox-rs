use std::time;
use crate::object::{Callable,Object};
use crate::interpreter::{Interpreter,StateChange};

pub struct Clock;
impl Callable for Clock {
    fn call(&self, _interpreter: &mut Interpreter, _args: &[Object]) -> Result<Option<Object>,StateChange> {
        Ok(Some(Object::Number(time::SystemTime::now().duration_since(time::SystemTime::UNIX_EPOCH).unwrap().as_nanos() as f64)))
    }

    fn arity(&self) -> u8 {
        0
    }
}
