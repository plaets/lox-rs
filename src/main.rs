use std::io::prelude::*;
use std::io::{stdin, stdout};
use std::env;
use std::fs::File;
use std::path::Path;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::cell::RefCell;
use gcmodule::Cc;
use inventory;

mod lexer;
use lexer::*;
mod parser;
use parser::*;
mod interpreter;
use interpreter::*;
mod object;
use object::{Object};
mod resolver;
use resolver::*;
#[allow(non_camel_case_types)]
mod native;
use native::*;
mod ast;

#[cfg(test)]
mod tests;

fn run(data: &str, globals: EnvironmentScope) -> Result<Option<Object>,IntErr> {
    let mut scanner = Scanner::new(data.to_string());
    let tokens = scanner.scan_tokens();
    if let Ok(_tokens) = tokens {
        let mut parser = Parser::new(scanner.tokens.clone());
        let (tree, errors) = parser.parse();
        if errors.is_empty() {
            let mut resolver = Resolver::new();
            let (locals_map, errors) = resolver.resolve(&tree);
            if errors.is_empty() {
                let mut interpreter = Interpreter::new(locals_map);
                interpreter.get_env().set_globals(globals);
                interpreter.interpret(&tree)
            } else {
                for e in errors.iter() {
                    println!("resolver error: {:?}", e);
                }
                Ok(None)
            }
        } else {
            for e in errors.iter() {
                println!("parser error: {:?}", e);
            }
            Ok(None)
        }
    } else {
        println!("scanner error: {:#?}", tokens);
        Ok(None)
    }
}

inventory::collect!(NativeInventoryEntry);
inventory::collect!(NativeClassInventoryEntry);
fn get_default_env() -> EnvironmentScope {
    let env = Cc::new(RefCell::new(HashMap::new()));
    for entry in inventory::iter::<NativeInventoryEntry> {
        (*env).borrow_mut().insert(entry.0.clone(), Object::Callable(entry.1.clone()));
    }
    for entry in inventory::iter::<NativeClassInventoryEntry> {
        (*env).borrow_mut().insert(entry.0.name.clone(), Object::Class(entry.0.clone()));
    }
    env
}

fn run_file(path: &str) -> Result<(), std::io::Error> {
    let env = get_default_env();
    let mut file = File::open(Path::new(path))?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let res = run(&contents, env);
    if let Err(r) = res {
        println!("{:?}", r);
    } 
    Ok(())
}

fn run_prompt() -> Result<(), std::io::Error> {
    let env = get_default_env();
    loop {
        print!("> ");
        stdout().flush()?;
        let mut line = String::new();
        stdin().read_line(&mut line)?;
        let res = run(&line, env.clone());
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
