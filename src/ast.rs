use std::fmt;
use gcmodule::{Cc,Trace,Tracer};
use strum_macros::EnumDiscriminants;

//is this enum necessary
#[enumeration(rename_all = "snake_case")]
#[derive(Debug, Clone, PartialEq, enum_utils::FromStr)]
pub enum Keyword {
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Debug, Clone, PartialEq, EnumDiscriminants)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,

    Bang,
    BangEqual,
    EqualEqual,
    Equal,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,

    String(String),             //TODO: how to handle invlaid variants like TokenType::String(Object::Number)?
    Number(f64),
    Keyword(Keyword),
    Identifier(String),

    Slash,
    Eof,
}

#[derive(Clone,Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

//impl fmt::Debug for Token {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        write!(f, "{:?}", self.token_type)
//    }
//}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            line,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.token_type, self.lexeme)
    }
}

#[derive(Debug,Clone)]
pub struct FunctionStmt(pub Box<Token>, pub Vec<Token>, pub Vec<Stmt>);     //name, args, body

impl Trace for FunctionStmt {
    fn trace(&self, _tracer: &mut Tracer) { }
}

#[derive(Debug,Clone)]
pub enum Stmt {
    Expr(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),    //cond, then, else
    Print(Expr),
    Return(Box<Token>, Option<Expr>), 
    While(Expr, Box<Stmt>),                    //cond, body
    Var(Box<Token>, Option<Expr>),                  //name, init
    //TODO: first field has to be an identifier, how to avoid having to check the type again in the interpreter?
    Fun(Cc<FunctionStmt>),
    //having tokens here is pretty cool as it allows better error handling 
    Block(Box<Token>, Vec<Stmt>),
}

impl Stmt {
    pub fn get_token(&self) -> Token {
        match self {
            Stmt::Expr(expr) => expr.get_token(),
            Stmt::If(expr, _, _) => expr.get_token(),
            Stmt::Print(expr) => expr.get_token(),
            Stmt::Return(token, _) => *(token.clone()),
            Stmt::While(expr, _) => expr.get_token(),
            Stmt::Var(token, _) => *(token.clone()),
            Stmt::Fun(fun_stmt) => *(fun_stmt.0.clone()),
            Stmt::Block(token, _) => *(token.clone()),
        }
    }

    #[cfg(test)]
    pub fn stringify_tree(&self) -> String {
        "(".to_string() + &match self {
            Stmt::Expr(expr) => return expr.stringify_tree(),
            Stmt::If(expr, then, else_b) => format!("if {} {}", expr.stringify_tree(), then.stringify_tree()) + 
                                            &else_b.as_ref().map_or("".to_string(), |v| " ".to_string() + &v.stringify_tree()),
            Stmt::Print(expr) => format!("print {}", expr.stringify_tree()),
            Stmt::Return(_, expr) => format!("return {}", expr.as_ref().map_or("".to_string(), |v| v.stringify_tree())),
            Stmt::While(expr, body) => format!("while {} {}", expr.stringify_tree(), body.stringify_tree()),
            Stmt::Var(name, init) => format!("var {} {}", name.lexeme.clone(), init.as_ref().map_or("".to_string(), |v| v.stringify_tree())),
            Stmt::Fun(f) => format!("fun {} {:?} {:?}", f.0.lexeme.clone(), 
                                    f.1.iter().map(|v| v.lexeme.clone()).collect::<Vec<_>>(), 
                                    format!("[{}]", f.2.iter().fold(String::new(), |t, v| t + &v.stringify_tree()))),
            Stmt::Block(_, stmts) => return format!("[{}]", stmts.iter().fold(String::new(), |t, v| t + &v.stringify_tree())),
        }.to_string() + ")"
    }
}

#[derive(Debug,Clone)]
pub enum Expr {
    Assign(Box<Token>, Box<Expr>),                  //name, value
    Logical(Box<Expr>, Box<Keyword>, Box<Expr>),    //left, op (or/and), right
    Binary(Box<Expr>, Box<Token>, Box<Expr>),       //left, op, right
    Call(Box<Expr>, Box<Token>, Vec<Expr>),         //callable, left paren, args
    Unary(Box<Token>, Box<Expr>),                   //op, right
    Literal(Box<Token>),
    Variable(Box<Token>),                           //name
    Grouping(Box<Expr>),
}

impl Expr {
    pub fn get_token(&self) -> Token {
        match self {
            Expr::Assign(token, _) => *(token.clone()),
            Expr::Logical(expr, _, _) => expr.get_token(),
            Expr::Binary(_, op, _) => *(op.clone()),
            Expr::Call(_, paren, _) => *(paren.clone()),
            Expr::Unary(op, _) => *(op.clone()),
            Expr::Literal(token) => *(token.clone()),
            Expr::Variable(token) => *(token.clone()),
            Expr::Grouping(expr) => expr.get_token(),
        }
    }

    #[cfg(test)]
    pub fn stringify_tree(&self) -> String {
        "(".to_string() + &match self {
            Expr::Assign(name, value) => format!("= {} {}", name.lexeme, value.stringify_tree()),
            Expr::Logical(left, op, right) => format!("{:?} {} {}", op, left.stringify_tree(), right.stringify_tree()),
            Expr::Binary(left, op, right) => format!("{} {} {}", op.lexeme, left.stringify_tree(), right.stringify_tree()),
            Expr::Call(callable, _paren, args) => format!("{} {:?}", callable.stringify_tree(), args),
            Expr::Unary(op, val) => format!("{} {}", op, val.stringify_tree()),
            Expr::Literal(token) => return token.lexeme.clone(),
            Expr::Variable(token) => token.lexeme.clone(),
            Expr::Grouping(expr) => expr.stringify_tree(),
        }.to_string() + ")"
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}
