use std::rc::Rc;
use crate::interpreter::{Callable,Interpreter,Object,Environment,InterpreterErrorReason};
use crate::parser::FunctionStmt;

pub struct Function {
    declaration: Rc<FunctionStmt>,
}

impl Function {
    pub fn new(declaration: Rc<FunctionStmt>) -> Self {
        Self {
            declaration,
        }
    }
}

impl Callable for Function {
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Object>) -> Result<Option<Object>,InterpreterErrorReason> {
        let mut env = Environment::new();
        env.push_foreign(interpreter.get_env().globals());
        env.push();
        for (name,val) in self.declaration.1.iter().zip(args.iter()) {
            env.define(name.lexeme.clone(), val.clone());
        }
        interpreter.exec_block_in_env(&self.declaration.2, &mut env)
            .map_err(|e| InterpreterErrorReason::UserCallError(Box::new(e)))
    }

    fn arity(&self) -> u8 {
        self.declaration.1.len() as u8
    }
}
