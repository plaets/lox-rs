use std::rc::Rc;
use crate::interpreter::{Callable,Interpreter,Object,Environment,StateChange};
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
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Object>) -> Result<Option<Object>,StateChange> {
        let mut env = Environment::new_with(interpreter.get_env().globals());
        env.push();
        for (name,val) in self.declaration.1.iter().zip(args.iter()) {
            env.define(name.lexeme.clone(), val.clone());
        }
        interpreter.exec_block_in_env(&self.declaration.2, &mut env)
    }

    fn arity(&self) -> u8 {
        self.declaration.1.len() as u8
    }
}
