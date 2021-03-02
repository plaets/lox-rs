use std::io::prelude::*;
use std::io::{stdin, stdout};
use std::env;
use std::fs::File;
use std::path::Path;

mod lexer;
use lexer::*;
mod parser;
use parser::*;
mod interpreter;
use interpreter::*;

fn run(data: &str) {
    let mut scanner = Scanner::new(data.to_string());
    scanner.scan_tokens();
    let mut parser = Parser::new(scanner.tokens.clone());
    let tree = parser.parse();
    match tree {
        Ok(t) => {
            let res = evaluate(&t);
            println!("{:#?}", res);
        },
        Err(e) => println!("parse error: {:#?}", e),
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
    //let expr = Expr::Binary(
    //    Box::new(Expr::Unary(Box::new(Token::new(TokenType::Minus, "-".to_string(), 0)), 
    //             Box::new(Expr::Literal(Box::new(Token::new(TokenType::Number(Object::Number(45.67)), "45.67".to_string(), 0)))))),
    //    Box::new(Token::new(TokenType::Star, "*".to_string(), 0)),
    //    Box::new(Expr::Grouping(Box::new(Expr::Literal(Box::new(Token::new(TokenType::Number(Object::Number(45.67)), "45.67".to_string(), 0)))))),
    //);
    //println!("{:#}", expr);

    if env::args().len() > 2 {
        println!("Usage: {} script", env::args().nth(0).unwrap());
        Ok(())
    } else if env::args().len() == 2 {
        run_file(&env::args().nth(1).unwrap())
    } else {
        run_prompt()
    }
}
