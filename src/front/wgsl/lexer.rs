use std::fmt::{Debug, Display};

use crate::Span;
use logos::Logos;

#[derive(Clone)]
pub struct Lexer<'a> {
    inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            inner: logos::Lexer::new(source),
        }
    }

    pub fn eof_span(&self) -> Span {
        Span::new(
            self.inner.source().len() as _,
            self.inner.source().len() as u32 + 1,
        )
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next();
        let span = self.inner.span();
        next.map(|kind| Token {
            kind,
            span: Span::new(span.start as _, span.end as _),
        })
    }
}

#[derive(Copy, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Logos)]
pub enum TokenKind {
    #[regex(r#"(0[iu]?)|([1-9][0-9]*[iu]?)|(0[xX][0-9a-fA-F]+[iu]?)"#)]
    IntLit,
    #[regex(
		r#"(0[fh])|([0-9]*\.[0-9]+([eE][+-]?[0-9]+)?[fh]?)|([0-9]+[eE][+-]?[0-9]+[fh]?)|([0-9]+\.[0-9]*([eE][+-]?[0-9]+)?[fh]?)|([1-9][0-9]*[fh])|(0[xX][0-9a-fA-F]*\.[0-9a-fA-F]+([pP][+-]?[0-9]+[fh]?)?)|(0[xX][0-9a-fA-F]+[pP][+-]?[0-9]+[fh]?)|(0[xX][0-9a-fA-F]+\.[0-9a-fA-F]*([pP][+-]?[0-9]+[fh]?)?)"#
	)]
    FloatLit,
    #[regex(r"[\p{XID_Start}_]\p{XID_Continue}*")]
    Word,
    #[token("&")]
    And,
    #[token("&&")]
    AndAnd,
    #[token("->")]
    Arrow,
    #[token("@")]
    Attr,
    #[token("/")]
    ForwardSlash,
    #[token("!")]
    Bang,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token("<<")]
    ShiftLeft,
    #[token("%")]
    Modulo,
    #[token("-")]
    Minus,
    #[token("--")]
    MinusMinus,
    #[token(".")]
    Period,
    #[token("+")]
    Plus,
    #[token("++")]
    PlusPlus,
    #[token("|")]
    Or,
    #[token("||")]
    OrOr,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(";")]
    Semicolon,
    #[token("*")]
    Star,
    #[token("~")]
    Tilde,
    #[token("_")]
    Underscore,
    #[token("^")]
    Xor,
    #[token("+=")]
    PlusEqual,
    #[token("-=")]
    MinusEqual,
    #[token("*=")]
    TimesEqual,
    #[token("/=")]
    DivideEqual,
    #[token("%=")]
    ModuloEqual,
    #[token("&=")]
    AndEqual,
    #[token("|=")]
    OrEqual,
    #[token("^=")]
    XorEqual,
    #[token(">>=")]
    ShiftRightEqual,
    #[token("<<=")]
    ShiftLeftEqual,
    Eof,
    #[error]
    #[regex(r"[ \t\n\r\f]+|//.*|", logos::skip)]
    #[token("/*", |lex| {
		let len = lex.remainder().find("*/").unwrap_or(lex.remainder().len() - 2);
		lex.bump(len + 2); // include len of `*/`

		logos::Skip
	})]
    Error,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Debug for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;

        write!(
            f,
            "{}",
            match self {
                IntLit => "{integer}",
                FloatLit => "{float}",
                Word => "<word>",
                And => "&",
                AndAnd => "&&",
                Arrow => "->",
                Attr => "@",
                ForwardSlash => "/",
                Bang => "!",
                LBracket => "[",
                RBracket => "]",
                LBrace => "{",
                RBrace => "}",
                Colon => ":",
                Comma => ",",
                Equal => "=",
                EqualEqual => "==",
                NotEqual => "!=",
                Greater => ">",
                GreaterEqual => ">=",
                Less => "<",
                LessEqual => "<=",
                ShiftLeft => "<<",
                Modulo => "%",
                Minus => "-",
                MinusMinus => "--",
                Period => ".",
                Plus => "+",
                PlusPlus => "++",
                Or => "|",
                OrOr => "||",
                LParen => "(",
                RParen => ")",
                Semicolon => ";",
                Star => "*",
                Tilde => "~",
                Underscore => "_",
                Xor => "^",
                PlusEqual => "+=",
                MinusEqual => "-=",
                TimesEqual => "*=",
                DivideEqual => "/=",
                ModuloEqual => "%=",
                AndEqual => "&=",
                OrEqual => "|=",
                XorEqual => "^=",
                ShiftRightEqual => ">>=",
                ShiftLeftEqual => "<<=",
                Eof => "<eof>",
                Error => "<error>",
            }
        )
    }
}
