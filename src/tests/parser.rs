use newtype_enum::Enum;
use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;

fn parse(data: &str) -> (Vec<Stmt>,Vec<ParseError>) {
    let mut scanner = Scanner::new(data.to_string());
    scanner.scan_tokens().unwrap();
    let mut parser = Parser::new(scanner.tokens.clone());
    parser.parse()
}

fn get_first_expr(v: &(Vec<Stmt>,Vec<ParseError>)) -> Expr {
    let v = &v.0;
    assert_eq!(v.len(), 1);
    if let Stmt::ExprStmt(expr) = &v[0] {
        expr.expr.clone()
    } else {
        panic!("not an expression")
    }
}

fn stringify_block(v: &(Vec<Stmt>,Vec<ParseError>)) -> String {
    let v = &v.0;
    format!("[{}]", v.iter().fold(String::new(), |t, v| t + &v.stringify_tree()))
}

fn parse_str_expr(v: &str) -> String {
    get_first_expr(&parse(v)).stringify_tree()
}

fn parse_str_stmts(v: &str) -> String {
    stringify_block(&parse(v))
}

//TODO: how to test gc 

//im not sure if comparing stringified trees is a good idea but bulding the trees by hand is way
//too tedious
//so i came up with this ambigious pretend-lisp syntax instead, should be good enough
//:slight_smile: :thumbsup:
#[test]
fn test_basic_expr() {
    let tree = parse("2+2;");
    let expr = get_first_expr(&tree);
    assert_eq!(expr.stringify_tree(), "(+ 2 2)");
}

#[test]
fn test_grouping() {
    assert_eq!(parse_str_expr("2+(3*4);"), "(+ 2 ((* 3 4)))");
}

#[test]
fn test_basic_precendence() {
    assert_eq!(parse_str_expr("2+3*4;"), "(+ 2 (* 3 4))");
}

#[test]
fn test_logical_expr() { 
    assert_eq!(parse_str_expr("true and false;"), "(And true false)");
    assert_eq!(parse_str_expr("false or true;"), "(Or false true)");
}

#[test]
fn test_if() {
    assert_eq!(parse_str_stmts("if(2 == 4) { 4; }"), "[(if (== 2 4) [4])]");
    assert_eq!(parse_str_stmts("if(2 == 4) { 4; } else { 3; }"), "[(if (== 2 4) [4] [3])]");
}

//this isnt even useful
#[test]
fn test_class() {
    assert_eq!(parse_str_stmts("class asd { zxc() { this.a = 4; print \"a\"; } init(b) { this.b = b; return b; }}
           class b < asd { init() { super.init(2, 3*4/2); } }"), 
        "[(class asd [(fun zxc() [(= this.a 4),(print \"a\")])(fun init(b) [(= this.b b),(return b)])])(class b < asd [(fun init() [(super.init (2, (/ (* 3 4) 2)))])])]")
}
