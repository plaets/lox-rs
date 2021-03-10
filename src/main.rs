use std::io::prelude::*;
use std::io::{stdin, stdout, stderr};
use std::rc::Rc;
use std::env;
use std::fs::File;
use std::path::Path;

mod lexer;
use lexer::*;
mod parser;
use parser::*;
mod interpreter;
use interpreter::*;
mod native;
mod function;

//TODO: main cleanup
fn error(line: usize, msg: &str) {
    report(line, "", msg)
}

fn report(line: usize, err_where: &str, msg: &str) {
    let s: String = format!("[line {}] Error {}: {}", line, err_where, msg);
    stderr().write_all(s.as_bytes()).unwrap();
}

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
    interpreter.get_env().define("clock".to_owned(), interpreter::Object::Callable(interpreter::CallableObject::new(Rc::new(native::Clock{}))));
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
    if env::args().len() > 2 {
        println!("Usage: {} script", env::args().nth(0).unwrap());
        Ok(())
    } else if env::args().len() == 2 {
        run_file(&env::args().nth(1).unwrap())
    } else {
        run_prompt()
    }
}
