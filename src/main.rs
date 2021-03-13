use std::io::prelude::*;
use std::io::{stdin, stdout};
use std::env;
use std::fs::File;
use std::path::Path;
use std::cmp::Ordering;
use gcmodule::Cc;

mod lexer;
use lexer::*;
mod parser;
use parser::*;
mod interpreter;
use interpreter::*;
mod native;
mod object;
use object::{Object,BoxValues,CallableObject};
mod ast;

#[cfg(test)]
mod tests;

fn run(data: &str, interpreter: &mut Interpreter) -> Result<Option<Object>,IntErr> {
    let mut scanner = Scanner::new(data.to_string());
    let tokens = scanner.scan_tokens();
    if let Ok(_tokens) = tokens {
        let mut parser = Parser::new(scanner.tokens.clone());
        let tree = parser.parse();
        match tree {
            Ok(t) => {
                return interpreter.interpret(&t);
            },
            Err(e) => println!("parse error: {:#?}", e),
        }
        Ok(None)
    } else {
        println!("scanner error: {:#?}", tokens);
        Ok(None)
    }
}

fn get_default_interpreter() -> Interpreter {
    let mut interpreter = Interpreter::new();
    interpreter.get_env().define("clock".to_owned(), Object::Callable(CallableObject::new(Cc::new(BoxValues(Box::new(native::Clock{}))))));
    interpreter
}

fn run_file(path: &str) -> Result<(), std::io::Error> {
    let mut interpreter = get_default_interpreter();
    let mut file = File::open(Path::new(path))?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let res = run(&contents, &mut interpreter);
    if let Err(r) = res {
        println!("{:?}", r);
    } 
    Ok(())
}

fn run_prompt() -> Result<(), std::io::Error> {
    let mut interpreter = get_default_interpreter();
    loop {
        print!("> ");
        stdout().flush()?;
        let mut line = String::new();
        stdin().read_line(&mut line)?;
        let res = run(&line, &mut interpreter);
        if let Err(r) = res {
            println!("{:?}", r);
        } else if let Ok(Some(r)) = res {
            println!("{}", r);
        }
    }
}

fn main() -> Result<(), std::io::Error> {
    match env::args().len().cmp(&2) {
        Ordering::Greater => {
            println!("Usage: {} script", env::args().next().unwrap());
            Ok(())
        }, 
        Ordering::Equal => run_file(&env::args().nth(1).unwrap()),
        Ordering::Less => run_prompt(),
    }
}
