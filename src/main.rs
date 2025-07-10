#[derive(Debug, Clone, PartialEq)]

enum Token {
    Num(i64),
    Ident(String),
    Plus, Minus, Star, Slash, Eq, LParen, RParen,
    If, Else,
    Semicolon,
}

fn lexer(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(&c) = chars.peek() {
        match c {
            ch if ch.is_whitespace() => {chars.next(); }
            '0'..='9' => {
                let mut num = 0i64;
                while let Some(&d) = chars.peek() {
                    if let Some(dg) = d.to_digit(10) {
                        num = num * 10 + dg as i64;
                        chars.next();
                    } else { break; }
                }
                tokens.push(Token::Num(num));
            };
            'a'..='z' | 'A'..='Z' => {
                let mut ident = String::new();
                while let Some(&d) = chars.peek() {
                    if d.is_alphanumeric(){
                        ident.push(d);
                        chars.next();
                    } else { break; }
                }
                tokens.push(match ident.as_str() {
                    "if" => Token::If,
                    "else" => Token::Else,
                    _ => Token::Ident(ident),
                });
            }
            '+' => { tokens.push(Token::Plus);  chars.next(); }
            '-' => { tokens.push(Token::Minus); chars.next(); }
            '*' => { tokens.push(Token::Star);  chars.next(); }
            '/' => { tokens.push(Token::Slash); chars.next(); }
            '=' => { tokens.push(Token::Eq);    chars.next(); }
            '(' => { tokens.push(Token::LParen);chars.next(); }
            ')' => { tokens.push(Token::RParen);chars.next(); }
            ';' => { tokens.push(Token::Semicolon); chars.next(); }
            _   => panic!("Unexpected char: {}", c),
        }
    }
    tokens
}

enum Expr {
    Num(i64),
    Var(String),
    BinOp(Box<Expr>, Op, Box<Expr>),
}

enum Op { Add, Sub, Mul, Div }

enum Stmt {
    Assign(String, Expr),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self { Parser { tokens, pos: 0 } }
    fn peek(&self) -> Option<&Token> { self.tokens.get(self.pos) }
    fn bump(&mut self) -> Token { let t=self.tokens[self.pos].clone(); self.pos+=1; t}
    fn parse_program(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.pos < self.tokens.len() {
            stmts.push(self.parse_stmt());
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Stmt { 
        match self.peek().unwrap() {
            Token::If => {
                self.bump(); // if
                self.bump(); // '('
                let cond = self.parse_expr();
                self.bump(); // ')'
                let then_blk = self.parse_block();
                let else_blk = if let Some(Token::Else) = self.peek() {
                    self.bump(); // else
                    self.parse_block()
                } else { Vec::new() };
                Stmt::If(cond, then_blk, else_blk)
            }
            Token::While => {
                self.bump(); // while
                self.bump(); // '('
                let cond = self.parse_expr();
                self.bump(); // ')'
                let body = self.parse_block();
                Stmt::While(cond, body)
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.bump();
                self.bump();
                let expr = self.parse_expr();
                self.bump();
                Stmt::Assign(name, expr)
            }
            other => panic!("found {:?}", other)
        }
    }

    fn parse_expr(&mut self) -> Expr {
        let mut left = match self.bump() {
            Token::Num(n) => Expr::Num(n),
            Token::Ident(s) => Expr::Var(s),
            Token::LParen => {
                let e = self.parse_expr();
                self.bump();
                e
            }
            other => panic!("Unexpected in expr {:?}", other),
        };

        while let Some(op) = math self.peek() {
            Some(Token::Plus) => { self.bump(); Some(Op::Add) }
            Some(Token::Minus) => { self.bump(); Some(Op::Sub) }
            _ => None
        } {
            let right = self.parse_expr();
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        left
    }
}

enum Instr {
    Push(i64),
    Load(String),
    Store(String),
    Add, Sub, Mul, Div,
    JumpIfFalse(usize),
    Jump(usize),
}

fn gen_expr(e: &Expr, code: &mut Vec<Instr>) {
    match e {
        Expr::Num(n) => code.push(Instr::Push(*n)),
        Expr::Var(name) => code.push(Instr::Load(name.clone())),
        Expr::BinOp(a, Op::Add, b) => {
            gen_expr(a, code);
            gen_expr(b, code);
            code.push(Instr::Add);
        }
        Expr::BinOp(a, Op::Div, b) => {
            gen_expr(a, code);
            gen_expr(b, code);
            code.push(Instr::Div);
        }
        Expr::BinOp(a, Op::Sub, b) => {
            gen_expr(a, code);
            gen_expr(b, code);
            code.push(Instr::Sub);
        }
        Expr::BinOp(a, Op::Mul, b) => {
            gen_expr(a, code);
            gen_expr(b, code);
            code.push(Instr::Mul);
        }
        _ => unimplemented!(),
    }
}

fn gen_stmt(s: &Stmt, code: &mut Vec<Instr>) {
    match s {
        Stmt::Assign(name, expr) => {
            gen_expr(expr, code);
            code.push(Instr::Store(name.clone()))
        }
        Stmt::If(cond, then_blk, else_blk) => {
            gen_expr(cond, code);
            let jfalse_pos = code.len(); code.push(Instr::JumpIfFalse(0));
            for st in then_blk { gen_stmt(st, code); }
            let jmp_pos = code.len(); code.push(Instr::Jump(0));
            let else_pos = code.len();
            code[jfalse_pos] = Instr::JumpIfFalse(else_pos);
            for st in else_blk { gen_stmt(st, code); }
            let end_pos = code.len();
            code[jmp_pos] = Instr::Jump(end_pos);
        }
        Stmt::While(cond, body) => {
            let start_pos = code.len();
            gen_expr(cond, code);
            let jfalse_pos = code.len(); code.push(Instr::JumpIfFalse(0));
            for st in body { gen_stmt(st, code); }
            code.push(Instr::Jump(start_pos));
            let end_pos = code.len();
            code[jfalse_pos] = Instr::JumpIfFalse(end_pos);
        }
    }
}

use std::collections::HashMap;

fn run_vm(code: &[Instr]) {
    let mut stack = Vec::new();
    let mut vars  = HashMap::new();
    for instr in code {
        match instr {
            Instr::Push(n)      => stack.push(*n),
            Instr::Load(name)   => {
                let v = *vars.get(name).unwrap_or(&0);
                stack.push(v);
            }
            Instr::Store(name)  => {
                let v = stack.pop().unwrap();
                vars.insert(name.clone(), v);
            }
            Instr::Add => {
                let b=stack.pop().unwrap();
                let a=stack.pop().unwrap();
                stack.push(a + b);
            }
            Instr::Sub => {
                let b=stack.pop().unwrap();
                let a=stack.pop().unwrap();
                stack.push(a - b);
            }
            Instr::Mul => {
                let b=stack.pop().unwrap();
                let a=stack.pop().unwrap();
                stack.push(a * b);
            }
            Instr::Div => {
                let b=stack.pop().unwrap();
                let a=stack.pop().unwrap();
                stack.push(a / b);
            }
            Instr::JumpIfFalse(pos) => {
                if stack.pop().unwrap()==0 { ip = *pos; continue; }
            }
            Instr::Jump(pos)  => { ip = *pos; continue; }
            // handle Sub, Mul, Div similarly…
            _ => unimplemented!(),
        }
        ip += 1;
    }
    println!("Final vars: {:?}", vars);
}

fn main() {
    let src = r#"
        a = 1 + 2;
        b = a * 3;
        if (b - 5) {
            c = b;
        } else {
            c = 0;
        }
        i = 0;
        while (i - 3) {
            i = i + 1;
            a = a + i;
        }
    "#;
    let tokens = lexer(src);
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse_program();
    let mut code = Vec::new();
    for stmt in &stmts {
        gen_stmt(stmt. &mut code);
    }

    run_vm(&code);
}