use std::io::prelude::*;
use std::io::{stdin, stdout, stderr};
use std::env;
use std::fs::File;
use std::path::Path;

mod lexer;
use lexer::*;
mod parser;
use parser::*;
mod interpreter;
use interpreter::*;

fn error(line: usize, msg: &str) {
    report(line, "", msg)
}

fn report(line: usize, err_where: &str, msg: &str) {
    let s: String = format!("[line {}] Error {}: {}", line, err_where, msg);
    stderr().write_all(s.as_bytes()).unwrap();
}

fn run(data: &str) {
    let mut scanner = Scanner::new(data.to_string());
    let tokens = scanner.scan_tokens();
    if let Ok(tokens) = tokens {
        let mut parser = Parser::new(scanner.tokens.clone());
        let tree = parser.parse();
        let mut interpreter = Interpreter::new();
        match tree {
            Ok(t) => {
                let res = interpreter.interpret(&t);
                if let Ok(Some(res)) = res {
                    println!("{}", res);
                } else {
                    println!("{:?}", res);
                }
            },
            Err(e) => println!("parse error: {:#?}", e),
        }
    } else {
        println!("scanner error: {:#?}", tokens);
    }
    //println!("{:?}", scanner.tokens.iter().map(|t| t.token_type.clone()).collect::<Vec<TokenType>>());
    //println!("{:#?}", tree);
}

fn run_file(path: &str) -> Result<(), std::io::Error> {
    let mut file = File::open(Path::new(path))?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    run(&contents);
    Ok(())
}

fn run_prompt() -> Result<(), std::io::Error> {
    loop {
        print!("> ");
        stdout().flush()?;
        let mut line = String::new();
        stdin().read_line(&mut line)?;
        run(&line);
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
