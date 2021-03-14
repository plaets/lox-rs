use std::fmt;
use gcmodule::{Cc,Trace,Tracer};
use strum_macros::EnumDiscriminants;
use newtype_enum::newtype_enum;

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
pub struct FunctionStmt(pub Box<Token>, pub Vec<Token>, pub StmtVar::Block);     //name, args, body

impl Trace for FunctionStmt {
    fn trace(&self, _tracer: &mut Tracer) { }
}

#[newtype_enum(variants = "pub StmtVar")]
#[derive(Debug,Clone)]
pub enum Stmt {
    ExprStmt { expr: Expr },
    If { cond: Expr, then: Box<Stmt>, else_b: Option<Box<Stmt>> },
    Print { expr: Expr },
    Return { keyword: Box<Token>, value: Option<Expr> }, 
    While { cond: Expr, body: Box<Stmt> },
    Var { name: Box<Token>, init: Option<Expr> },
    //TODO: first field has to be an identifier, how to avoid having to check the type again in the interpreter?
    Fun { stmt: Cc<FunctionStmt> },
    //having tokens here is pretty cool as it allows better error handling 
    Block { left_brace: Box<Token>, body: Vec<Stmt> },
}

impl Stmt {
    pub fn get_token(&self) -> Token {
        match self {
            Stmt::ExprStmt(StmtVar::ExprStmt{expr}) => expr.get_token(),
            Stmt::If(StmtVar::If{cond, ..}) => cond.get_token(),
            Stmt::Print(StmtVar::Print{expr}) => expr.get_token(),
            Stmt::Return(StmtVar::Return{keyword, ..}) => *(keyword.clone()),
            Stmt::While(StmtVar::While{cond, ..}) => cond.get_token(),
            Stmt::Var(StmtVar::Var{name, ..}) => *(name.clone()),
            Stmt::Fun(StmtVar::Fun{stmt, ..}) => *(stmt.0.clone()),
            Stmt::Block(StmtVar::Block{left_brace, ..}) => *(left_brace.clone()),
        }
    }

    #[cfg(test)]
    pub fn stringify_tree(&self) -> String {
        "(".to_string() + &match self {
            Stmt::ExprStmt(StmtVar::ExprStmt{expr}) => return expr.stringify_tree(),
            Stmt::If(StmtVar::If{cond, then, else_b}) => format!("if {} {}", cond.stringify_tree(), then.stringify_tree()) + 
                                            &else_b.as_ref().map_or("".to_string(), |v| " ".to_string() + &v.stringify_tree()),
            Stmt::Print(StmtVar::Print{expr}) => format!("print {}", expr.stringify_tree()),
            Stmt::Return(StmtVar::Return{value, ..}) => format!("return {}", value.as_ref().map_or("".to_string(), |v| v.stringify_tree())),
            Stmt::While(StmtVar::While{cond, body}) => format!("while {} {}", cond.stringify_tree(), body.stringify_tree()),
            Stmt::Var(StmtVar::Var{name, init}) => format!("var {} {}", name.lexeme.clone(), init.as_ref().map_or("".to_string(), |v| v.stringify_tree())),
            Stmt::Fun(StmtVar::Fun{stmt}) => format!("fun {} {:?} {:?}", stmt.0.lexeme.clone(), 
                                    stmt.1.iter().map(|v| v.lexeme.clone()).collect::<Vec<_>>(), 
                                    format!("[{}]", stmt.2.body.iter().fold(String::new(), |t, v| t + &v.stringify_tree()))),
            Stmt::Block(StmtVar::Block{body, ..}) => return format!("[{}]", body.iter().fold(String::new(), |t, v| t + &v.stringify_tree())),
        }.to_string() + ")"
    }
}

#[newtype_enum(variants = "pub ExprVar")]
#[derive(Debug,Clone)]
pub enum Expr {
    Assign { name: Box<Token>, expr: Box<Expr> },
    Logical { left: Box<Expr>, op: Box<Keyword>, right: Box<Expr> },
    Binary { left: Box<Expr>, op: Box<Token>, right: Box<Expr> },
    Call { callee: Box<Expr>, left_paren: Box<Token>, args: Vec<Expr> },
    Unary { op: Box<Token>, expr: Box<Expr> },
    Literal { token: Box<Token> },
    Variable { name: Box<Token> },
    Grouping { expr: Box<Expr> },
}

impl Expr {
    pub fn get_token(&self) -> Token {
        match self {
            Expr::Assign(ExprVar::Assign{name, ..}) => *(name.clone()),
            Expr::Logical(ExprVar::Logical{left, ..}) => left.get_token(),
            Expr::Binary(ExprVar::Binary{op, ..}) => *(op.clone()),
            Expr::Call(ExprVar::Call{left_paren, ..}) => *(left_paren.clone()),
            Expr::Unary(ExprVar::Unary{op, ..}) => *(op.clone()),
            Expr::Literal(ExprVar::Literal{token, ..}) => *(token.clone()),
            Expr::Variable(ExprVar::Variable{name, ..}) => *(name.clone()),
            Expr::Grouping(ExprVar::Grouping{expr, ..}) => expr.get_token(),
        }
    }

    #[cfg(test)]
    pub fn stringify_tree(&self) -> String {
        "(".to_string() + &match self {
            Expr::Assign(ExprVar::Assign{name, expr}) => format!("= {} {}", name.lexeme, expr.stringify_tree()),
            Expr::Logical(ExprVar::Logical{left, op, right}) => format!("{:?} {} {}", op, left.stringify_tree(), right.stringify_tree()),
            Expr::Binary(ExprVar::Binary{left, op, right}) => format!("{} {} {}", op.lexeme, left.stringify_tree(), right.stringify_tree()),
            Expr::Call(ExprVar::Call{callee, args, ..}) => format!("{} {:?}", callee.stringify_tree(), args),
            Expr::Unary(ExprVar::Unary{op, expr}) => format!("{} {}", op, expr.stringify_tree()),
            Expr::Literal(ExprVar::Literal{token}) => return token.lexeme.clone(),
            Expr::Variable(ExprVar::Variable{name}) => name.lexeme.clone(),
            Expr::Grouping(ExprVar::Grouping{expr}) => expr.stringify_tree(),
        }.to_string() + ")"
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}
