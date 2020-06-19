#![allow(unused_braces)]
use pomelo::pomelo;

pomelo! {
    //%verbose;
    %include {
        use super::super::ast::*;
        use super::super::token::*;
        use crate::{Arena, Expression, Function, LocalVariable, Module};
    }
    %token #[derive(Debug)] pub enum Token {};
    %extra_argument Module;
    %extra_token TokenMetadata;
    %type Ident String;
    %type Version String;
    %type Number i64;
    %type String String;
    %type expr Expr;
    %type expr_list Vec<Expr>;
    %type stmt Stmt;
    %type block Vec<Stmt>;
    %type stmt_list Vec<Stmt>;
    %type arg_list Vec<String>;
    %type f_decl Function;
    %type v_decl Variable;

    %left Else;
    %right Assign;
    %left Or;
    %left And;
    %nonassoc Equal NotEqual;
    %nonassoc Less LessEq Greater GreaterEq;
    %left Plus Minus;
    %left Mult Div;
    %nonassoc Not;

    input ::= version_pragma decl_list?;

    version_pragma ::= Version Number Ident;

    decl_list ::= decl;
    decl_list ::= decl_list decl;

    decl ::= f_decl(f) {
        extra.functions.append(f);
    }
    decl ::= v_decl(v) { /*extra.add_variable(v);*/ }

    f_decl ::= Fn Ident(name) LParen arg_list?(args) RParen block(code) {
        //Function::new(name.1, args.unwrap_or_else(Vec::new), code)
        Function {
            name: Some(name.1),
            control: spirv::FunctionControl::NONE,
            parameter_types: vec![],
            return_type: None,
            global_usage: vec![],
            local_variables: Arena::<LocalVariable>::new(),
            expressions: Arena::<Expression>::new(),
            body: vec![],
        }
    }

    arg_list ::= Ident(n) { vec![n.1] }
    arg_list ::= arg_list(mut args) Comma Ident(n) { args.push(n.1); args }

    block ::= LBrace stmt_list?(ss) RBrace { ss.unwrap_or_default() }

    v_decl ::= Var Ident(name) Assign expr(ini) Semicolon { Variable::new(name.1, ini) }

    stmt_list ::= stmt(s) { vec![s] }
    stmt_list ::= stmt_list(mut ss) stmt(s) { ss.push(s); ss }

    stmt ::= block(ss) { Stmt::Block(ss) }
    stmt ::= expr(e) Semicolon {Stmt::Expr(e) }
    stmt ::= If LParen expr(e) RParen stmt(s1) [Else] { Stmt::If(e, Box::new((s1, None))) }
    stmt ::= If LParen expr(e) RParen stmt(s1) Else stmt(s2) {Stmt::If(e, Box::new((s1, Some(s2))))  }
    stmt ::= While LParen expr(e) RParen stmt(s) { Stmt::While(e, Box::new(s)) }
    stmt ::= Return expr(e) Semicolon { Stmt::Return(Some(e)) }
    stmt ::= Return Semicolon { Stmt::Return(None) }
    stmt ::= Break Semicolon { Stmt::Break }
    stmt ::= Continue Semicolon {Stmt::Continue }

    expr ::= Number(n) { Expr::Number(n.1) }
    expr ::= String(s) { Expr::String(s.1) }
    expr ::= Ident(n) { Expr::Variable(n.1) }
    expr ::= Ident(n) LParen expr_list?(es) RParen { Expr::Call(n.1, es.unwrap_or_default()) }
    expr ::= LParen expr(e) RParen { e }

    expr ::= expr(a) Plus expr(b) { Expr::BinaryOp(BinOp::Plus, Box::new((a, b))) }
    expr ::= expr(a) Minus expr(b) { Expr::BinaryOp(BinOp::Minus, Box::new((a, b))) }
    expr ::= expr(a) Mult expr(b) { Expr::BinaryOp(BinOp::Mult, Box::new((a, b))) }
    expr ::= expr(a) Div expr(b) { Expr::BinaryOp(BinOp::Div, Box::new((a, b))) }
    expr ::= Minus expr(a) [Not] { Expr::UnaryOp(UnaOp::Neg, Box::new(a)) }

    expr ::= expr(a) Equal expr(b) { Expr::BinaryOp(BinOp::Equal, Box::new((a, b))) }
    expr ::= expr(a) NotEqual expr(b) { Expr::BinaryOp(BinOp::NotEqual, Box::new((a, b))) }

    expr ::= expr(a) And expr(b) { Expr::BinaryOp(BinOp::And, Box::new((a, b))) }
    expr ::= expr(a) Or expr(b) { Expr::BinaryOp(BinOp::Or, Box::new((a, b))) }
    expr ::= Not expr(a) { Expr::UnaryOp(UnaOp::Not, Box::new(a)) }

    expr ::= expr(a) Less expr(b) { Expr::BinaryOp(BinOp::Less, Box::new((a, b))) }
    expr ::= expr(a) Greater expr(b) { Expr::BinaryOp(BinOp::Greater, Box::new((a, b))) }
    expr ::= expr(a) LessEq expr(b) { Expr::BinaryOp(BinOp::LessEq, Box::new((a, b))) }
    expr ::= expr(a) GreaterEq expr(b) { Expr::BinaryOp(BinOp::GreaterEq, Box::new((a, b))) }

    expr ::= expr(a) Assign expr(b) { Expr::BinaryOp(BinOp::Assign, Box::new((a, b))) }

    expr_list ::= expr(e) { vec![e] }
    expr_list ::= expr_list(mut es) Comma expr(e) { es.push(e); es }
}

pub use parser::*;
