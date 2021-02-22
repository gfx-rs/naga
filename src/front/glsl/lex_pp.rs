use super::{parser::Token, token::TokenMetadata, types::parse_type};
use crate::FastHashMap;
use pp_rs::{
    pp::Preprocessor,
    token::{Punct, Token as PPToken, TokenValue},
};
use std::collections::VecDeque;

//#[derive(Clone, Debug)]
pub struct LexerPP<'a> {
    pp: Preprocessor<'a>,
    pp_buf: VecDeque<PPToken>,
}

impl<'a> LexerPP<'a> {
    pub fn new(input: &'a str, defines: &'a FastHashMap<String, String>) -> Self {
        let mut pp = Preprocessor::new(input);
        for (define, value) in defines {
            pp.add_define(define, value).unwrap(); //TODO: handle error
        }
        let lexer = LexerPP {
            pp,
            pp_buf: Default::default(),
        };
        lexer
    }

    pub fn next(&mut self) -> Option<Token> {
        let mut meta = TokenMetadata {
            line: 0,
            chars: 0..0,
        };
        let pp_token = self.pp_buf.pop_front().or_else(|| {
            let t = self.pp.next();
            if let Some(t) = t {
                if let Ok(t) = t {
                    Some(t)
                } else {
                    //TODO: handle error
                    None
                }
            } else {
                None
            }
        });

        pp_token.map(|pp_token| {
            meta.line = pp_token.location.line as usize;
            meta.chars.start = pp_token.location.pos as usize;
            //TODO: proper location end
            meta.chars.end = pp_token.location.pos as usize + 1;
            match pp_token.value {
                TokenValue::Ident(ident) => {
                    match ident.as_str() {
                        "layout" => Token::Layout(meta),
                        "in" => Token::In(meta),
                        "out" => Token::Out(meta),
                        "uniform" => Token::Uniform(meta),
                        "flat" => Token::Interpolation((meta, crate::Interpolation::Flat)),
                        "noperspective" => {
                            Token::Interpolation((meta, crate::Interpolation::Linear))
                        }
                        "smooth" => Token::Interpolation((meta, crate::Interpolation::Perspective)),
                        "centroid" => Token::Interpolation((meta, crate::Interpolation::Centroid)),
                        "sample" => Token::Interpolation((meta, crate::Interpolation::Sample)),
                        // values
                        "true" => Token::BoolConstant((meta, true)),
                        "false" => Token::BoolConstant((meta, false)),
                        // jump statements
                        "continue" => Token::Continue(meta),
                        "break" => Token::Break(meta),
                        "return" => Token::Return(meta),
                        "discard" => Token::Discard(meta),
                        // selection statements
                        "if" => Token::If(meta),
                        "else" => Token::Else(meta),
                        "switch" => Token::Switch(meta),
                        "case" => Token::Case(meta),
                        "default" => Token::Default(meta),
                        // iteration statements
                        "while" => Token::While(meta),
                        "do" => Token::Do(meta),
                        "for" => Token::For(meta),
                        // types
                        "void" => Token::Void(meta),
                        "const" => Token::Const(meta),

                        word => {
                            let token = match parse_type(word) {
                                Some(t) => Token::TypeName((meta, t)),
                                None => Token::Identifier((meta, String::from(word))),
                            };
                            token
                        }
                    }
                }
                TokenValue::Integer(integer) => {
                    Token::IntConstant((meta, integer.value as i64)) //TODO: unsigned etc
                }
                TokenValue::Punct(punct) => match punct {
                    Punct::LeftParen => Token::LeftParen(meta),
                    Punct::RightParen => Token::RightParen(meta),
                    Punct::LeftBrace => Token::LeftBrace(meta),
                    Punct::RightBrace => Token::RightBrace(meta),

                    _ => Token::Unknown((meta, format!("{:?}", pp_token.value))),
                },
                TokenValue::Version(version) => {
                    for t in version.tokens {
                        self.pp_buf.push_back(t);
                    }
                    Token::Version(meta)
                }
                _ => Token::Unknown((meta, format!("{:?}", pp_token.value))),
            }
        })
    }
}

impl<'a> Iterator for LexerPP<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

#[cfg(test)]
mod tests {
    use super::{
        super::{parser::Token::*, token::TokenMetadata},
        LexerPP,
    };

    #[test]
    fn lex_tokens() {
        let defines = crate::FastHashMap::default();

        // line comments
        let mut lex = LexerPP::new("#version 450\nvoid main () {}", &defines);
        assert_eq!(
            lex.next().unwrap(),
            Version(TokenMetadata {
                line: 1,
                chars: 1..2 //TODO
            })
        );
        assert_eq!(
            lex.next().unwrap(),
            IntConstant((
                TokenMetadata {
                    line: 1,
                    chars: 9..10 //TODO
                },
                450
            ))
        );
        assert_eq!(
            lex.next().unwrap(),
            Void(TokenMetadata {
                line: 2,
                chars: 0..1 //TODO
            })
        );
        assert_eq!(
            lex.next().unwrap(),
            Identifier((
                TokenMetadata {
                    line: 2,
                    chars: 5..6 //TODO
                },
                "main".into()
            ))
        );
        assert_eq!(
            lex.next().unwrap(),
            LeftParen(TokenMetadata {
                line: 2,
                chars: 10..11 //TODO
            })
        );
        assert_eq!(
            lex.next().unwrap(),
            RightParen(TokenMetadata {
                line: 2,
                chars: 11..12 //TODO
            })
        );
        assert_eq!(
            lex.next().unwrap(),
            LeftBrace(TokenMetadata {
                line: 2,
                chars: 13..14 //TODO
            })
        );
        assert_eq!(
            lex.next().unwrap(),
            RightBrace(TokenMetadata {
                line: 2,
                chars: 14..15 //TODO
            })
        );
    }
}
