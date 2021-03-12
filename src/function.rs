use std::rc::Rc;
use crate::interpreter::{Callable,Interpreter,Object,Environment,EnvironmentScope,StateChange};
use crate::parser::FunctionStmt;

pub struct Function {
    declaration: Rc<FunctionStmt>,
    closure: Vec<EnvironmentScope>,     //maybe env as a linked list was a good idea? im sure i will intrudce so many cool bugs by trying to use slices here
}

impl Function {
    pub fn new(declaration: Rc<FunctionStmt>, closure: Vec<EnvironmentScope>) -> Self {
        Self {
            declaration,
            closure,
        }
    }
}

impl Callable for Function {
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Object>) -> Result<Option<Object>,StateChange> {
        let mut env = Environment::new_with(self.closure.clone());
        env.push();
        for (name,val) in self.declaration.1.iter().zip(args.iter()) {
            env.define(name.lexeme.clone(), val.clone());
        }
        interpreter.exec_block_in_env(&self.declaration.2, &mut env)
    }

    fn arity(&self) -> u8 {
        self.declaration.1.len() as u8
    }

    fn get_closure(&self) -> Option<&Vec<EnvironmentScope>> {
        Some(&self.closure)
    }
}
