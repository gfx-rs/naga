use std::io::{BufRead, Read};
use std::ops::Range;
use std::str::FromStr;

use super::error::{ErrorKind, ParseError};
use super::parser;
use super::token::TokenMetadata;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CharKind {
    Number,
    Alpha,
    Punct,
    String,
    Comment,
    Pragma,
}

pub struct Lexer<R> {
    rdr: R,
    current: Option<(CharKind, Vec<u8>)>,
}

impl CharKind {
    fn of(b: u8) -> Option<CharKind> {
        match b {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => Some(CharKind::Alpha),
            b'0'..=b'9' => Some(CharKind::Number),
            b' ' | b'\r' | b'\n' | b'\t' => None,
            //any other ASCII character is a punct
            x if x < 0x80 => Some(CharKind::Punct),
            //non ASCII characters are considered letters for tokenization purposes
            _ => Some(CharKind::Alpha),
        }
    }
}

impl<R: BufRead> Lexer<R> {
    pub fn new(rdr: R) -> Self {
        Lexer { rdr, current: None }
    }
    fn next_priv(&mut self) -> Result<Option<(CharKind, Vec<u8>)>, ParseError> {
        let mut reply = None;
        while reply.is_none() {
            let b = match self.rdr.by_ref().bytes().next() {
                Some(b) => b?,
                None => {
                    //EOF: if there is a current token, return it now
                    let res = self.current.take();
                    return Ok(res);
                }
            };
            let k2 = CharKind::of(b);
            self.current = match (self.current.take(), k2) {
                (None, None) => None,
                (None, Some(k2)) => Some((k2, vec![b])),
                (Some((k1, mut bs)), t2) => {
                    //extend a Number or Alpha, but not a Punct
                    if (Some(k1) == t2 || k1 == CharKind::Alpha && Some(CharKind::Number) == t2)
                        && k1 != CharKind::Punct
                    {
                        bs.push(b);
                        Some((k1, bs))
                    } else {
                        //check special Puncts
                        if k1 == CharKind::Punct && bs[0] == b'"' {
                            //string
                            let text = if b != b'"' {
                                let mut text = vec![b];
                                self.rdr.by_ref().read_until(b'"', &mut text)?;
                                if text.pop().unwrap() != b'"' {
                                    return Err(ErrorKind::InvalidInput.into());
                                }
                                text
                            } else {
                                vec![]
                            };
                            reply = Some((CharKind::String, text));
                            None
                        } else if k1 == CharKind::Punct && bs[0] == b'/' && b == b'/' {
                            //comment
                            let mut text = vec![];
                            self.rdr.by_ref().read_until(b'\n', &mut text)?;
                            reply = Some((CharKind::Comment, text));
                            None
                        } else if k1 == CharKind::Punct && bs[0] == b'#' {
                            //pragma
                            let pragma = if b != b' ' {
                                let mut pragma = vec![b];
                                self.rdr.by_ref().read_until(b' ', &mut pragma)?;
                                if pragma.pop().unwrap() != b' ' {
                                    return Err(ErrorKind::InvalidInput.into());
                                }
                                pragma
                            } else {
                                vec![]
                            };
                            reply = Some((CharKind::Pragma, pragma));
                            None
                        } else {
                            //The current token is returned and the new lookahead is stored
                            reply = Some((k1, bs));
                            t2.map(|k2| (k2, vec![b]))
                        }
                    }
                }
            }
        }
        Ok(reply)
    }
    fn next_token(&mut self) -> Result<Option<parser::Token>, ParseError> {
        loop {
            let token = self.next_priv()?;
            let meta = TokenMetadata {
                line: 0,
                chars: Range { start: 0, end: 0 },
            };
            log::debug!("token: {:#?}", token);
            let token = match token {
                None => None,
                Some((kind, s)) => {
                    let reply = match kind {
                        CharKind::Comment => continue,
                        CharKind::Number => {
                            let s = String::from_utf8(s).map_err(|_| ErrorKind::InvalidInput)?;
                            let val = i64::from_str(&s).map_err(|_| ErrorKind::InvalidInput)?;
                            parser::Token::Number((meta, val))
                        }
                        CharKind::Alpha => match &s[..] {
                            b"void" => parser::Token::Void(meta),
                            b"vec4" => parser::Token::Vec4(meta),
                            b"and" => parser::Token::And(meta),
                            b"or" => parser::Token::Or(meta),
                            b"not" => parser::Token::Not(meta),
                            _ => {
                                let s =
                                    String::from_utf8(s).map_err(|_| ErrorKind::InvalidInput)?;
                                parser::Token::Identifier((meta, s))
                            }
                        },
                        CharKind::String => {
                            let s = String::from_utf8(s).map_err(|_| ErrorKind::InvalidInput)?;
                            parser::Token::String((meta, s))
                        }
                        CharKind::Punct => {
                            let s = s[0];
                            //First check for two-char Puncts
                            let tt = if let Some((CharKind::Punct, n)) = &self.current {
                                let n = n[0];
                                match (s, n) {
                                    (b'=', b'=') => Some(parser::Token::Equal(meta.clone())),
                                    (b'!', b'=') => Some(parser::Token::NotEqual(meta.clone())),
                                    (b'<', b'=') => Some(parser::Token::LessEq(meta.clone())),
                                    (b'>', b'=') => Some(parser::Token::GreaterEq(meta.clone())),
                                    _ => None,
                                }
                            } else {
                                None
                            };
                            if tt.is_some() {
                                self.current = None;
                                return Ok(tt);
                            }
                            match s {
                                b'(' => parser::Token::LeftParen(meta),
                                b')' => parser::Token::RightParen(meta),
                                b'{' => parser::Token::LeftBrace(meta),
                                b'}' => parser::Token::RightBrace(meta),
                                b',' => parser::Token::Comma(meta),
                                b';' => parser::Token::Semicolon(meta),
                                b'=' => parser::Token::Equal(meta),
                                b'+' => parser::Token::Plus(meta),
                                b'-' => parser::Token::Minus(meta),
                                b'*' => parser::Token::Mult(meta),
                                b'/' => parser::Token::Div(meta),
                                b'<' => parser::Token::Less(meta),
                                b'>' => parser::Token::Greater(meta),
                                b'?' => parser::Token::Question(meta),
                                b':' => parser::Token::Colon(meta),
                                _ => return Err(ErrorKind::InvalidInput.into()),
                            }
                        }
                        CharKind::Pragma => match &s[..] {
                            b"version" => {
                                let s =
                                    String::from_utf8(s).map_err(|_| ErrorKind::InvalidInput)?;
                                log::debug!("found version {}", s);
                                parser::Token::Version((meta, s))
                            }
                            _ => return Err(ErrorKind::InvalidInput.into()),
                        },
                    };
                    Some(reply)
                }
            };
            break Ok(token);
        }
    }
}

impl<R: BufRead> Iterator for Lexer<R> {
    type Item = Result<parser::Token, ParseError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().transpose()
    }
}
