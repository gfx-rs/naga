#![allow(clippy::all)]
#![allow(dead_code)]

use super::{Error, ErrorKind};
use std::{
    fmt,
    iter::Peekable,
    ops::{Deref, Range},
    vec::IntoIter,
};

pub mod lex;
#[path = "./preprocessor.rs"]
pub mod preprocessor;

type Tokens = Peekable<IntoIter<TokenMetadata>>;

#[derive(Debug, Clone)]
pub struct TokenMetadata {
    pub token: Token,
    pub line: usize,
    pub chars: Range<usize>,
}

impl Deref for TokenMetadata {
    type Target = Token;

    fn deref(&self) -> &Token {
        &self.token
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Separator(char),
    DoubleColon,
    Paren(char),
    Integral(usize),
    Float(f32),
    Double(f64),
    Word(String),
    Operation(char),
    OpAssign(char),
    LogicalOperation(char),
    ShiftOperation(char),
    Unknown(char),
    LineComment,
    MultiLineCommentOpen,
    MultiLineCommentClose,
    Preprocessor,
    End,
    Selection,
    Sufix(char),
    TokenPasting,
}

impl Token {
    pub fn type_to_string(&self) -> String {
        match self {
            Token::Separator(separator) => separator.to_string(),
            Token::DoubleColon => ":".to_string(),
            Token::Paren(paren) => paren.to_string(),
            Token::Integral(_) => "integer".to_string(),
            Token::Float(_) => "float".to_string(),
            Token::Double(_) => "double".to_string(),
            Token::Word(_) => "word".to_string(),
            Token::Operation(op) => op.to_string(),
            Token::OpAssign(op) => format!("{}=", op),
            Token::LogicalOperation(op) => format!("{}=", op),
            Token::ShiftOperation(op) => format!("{0}{0}", op),
            Token::Unknown(_) => "unknown".to_string(),
            Token::LineComment => "//".to_string(),
            Token::MultiLineCommentOpen => "/*".to_string(),
            Token::MultiLineCommentClose => "*/".to_string(),
            Token::Preprocessor => "#".to_string(),
            Token::End => "EOF".to_string(),
            Token::Selection => "?".to_string(),
            Token::Sufix(op) => format!("{0}{0}", op),
            Token::TokenPasting => "##".to_string(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Separator(sep) => write!(f, "{}", sep),
            Token::DoubleColon => write!(f, ":"),
            Token::Paren(paren) => write!(f, "{}", paren),
            Token::Integral(int) => write!(f, "{}", int),
            Token::Float(float) => write!(f, "{}", float),
            Token::Double(double) => write!(f, "{}", double),
            Token::Word(word) => write!(f, "{}", word),
            Token::Operation(op) => write!(f, "{}", op),
            Token::OpAssign(op) => write!(f, "{}=", op),
            Token::LogicalOperation(op) => write!(f, "{0}=", op),
            Token::ShiftOperation(op) => write!(f, "{0}{0}", op),
            Token::Unknown(unknown) => write!(f, "{}", unknown),
            Token::LineComment => write!(f, "//"),
            Token::MultiLineCommentOpen => write!(f, "/*"),
            Token::MultiLineCommentClose => write!(f, "*/"),
            Token::Preprocessor => write!(f, "#"),
            Token::End => write!(f, ""),
            Token::Selection => write!(f, "?"),
            Token::Sufix(op) => write!(f, "{0}{0}", op),
            Token::TokenPasting => write!(f, "##"),
        }
    }
}

#[derive(Debug)]
pub enum Node {
    Ident(String),
    Const(Literal),
}

#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Double(f64),
    Float(f32),
    Uint(usize),
    Sint(isize),
    Bool(bool),
}

fn parse_primary_expression(tokens: &mut Tokens) -> Result<Node, Error> {
    let token = tokens.next().ok_or(Error {
        kind: ErrorKind::EOF,
    })?;

    match token.token {
        Token::Word(ident) => Ok(match ident.as_str() {
            "true" => Node::Const(Literal::Bool(true)),
            "false" => Node::Const(Literal::Bool(false)),
            _ => Node::Ident(ident),
        }),
        Token::Integral(uint) => Ok(Node::Const(Literal::Uint(uint))),
        Token::Float(float) => Ok(Node::Const(Literal::Float(float))),
        Token::Double(double) => Ok(Node::Const(Literal::Double(double))),
        Token::Paren('(') => todo!(), /* parse_expression */
        _ => Err(Error {
            kind: ErrorKind::UnexpectedToken {
                expected: vec![
                    Token::Word(String::new()),
                    Token::Integral(0),
                    Token::Double(0.0),
                    Token::Float(0.0),
                    Token::Paren('('),
                ],
                got: token,
            },
        }),
    }
}
