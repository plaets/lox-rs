use std::time;
use gcmodule::Trace;
use crate::object::{Callable,Object};
use crate::interpreter::{Interpreter,StateChange,EnvironmentScope};

#[derive(Trace)]
pub struct Clock;
impl Callable for Clock {
    fn call(&self, _interpreter: &mut Interpreter, _args: &[Object]) -> Result<Option<Object>,StateChange> {
        Ok(Some(Object::Number(time::SystemTime::now().duration_since(time::SystemTime::UNIX_EPOCH).unwrap().as_nanos() as f64)))
    }

    fn call_with_bound(&self, interpreter: &mut Interpreter, args: &[Object], _bound: EnvironmentScope) 
        -> Result<Option<Object>,StateChange> {
        self.call(interpreter, args)
    }

    fn is_getter(&self) -> bool {
        false
    }

    fn arity(&self) -> u8 {
        0
    }
}
