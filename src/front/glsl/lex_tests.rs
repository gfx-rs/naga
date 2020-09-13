use super::lex::Lexer;
use super::parser::Token;
use super::parser::Token::*;
use super::token::TokenMetadata;

#[test]
fn glsl_lex_simple() {
    let source = "void main() {\n}";
    let lex = Lexer::new(source);
    let tokens: Vec<Token> = lex.collect();
    assert_eq!(tokens.len(), 6);

    let mut iter = tokens.iter();
    assert_eq!(
        iter.next().unwrap(),
        &Void(TokenMetadata {
            line: 0,
            chars: 0..4
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &Identifier((
            TokenMetadata {
                line: 0,
                chars: 5..9
            },
            "main".into()
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &LeftParen(TokenMetadata {
            line: 0,
            chars: 9..10
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &RightParen(TokenMetadata {
            line: 0,
            chars: 10..11
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &LeftBrace(TokenMetadata {
            line: 0,
            chars: 12..13
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &RightBrace(TokenMetadata {
            line: 1,
            chars: 0..1
        })
    );
}

#[test]
fn glsl_lex_line_comment() {
    let source = "void main // myfunction\n//()\n{}";
    let lex = Lexer::new(source);
    let tokens: Vec<Token> = lex.collect();
    assert_eq!(tokens.len(), 4);

    let mut iter = tokens.iter();
    assert_eq!(
        iter.next().unwrap(),
        &Void(TokenMetadata {
            line: 0,
            chars: 0..4
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &Identifier((
            TokenMetadata {
                line: 0,
                chars: 5..9
            },
            "main".into()
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &LeftBrace(TokenMetadata {
            line: 2,
            chars: 0..1
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &RightBrace(TokenMetadata {
            line: 2,
            chars: 1..2
        })
    );
}

#[test]
fn glsl_lex_multi_line_comment() {
    let source = "void main /* comment [] {}\n/**\n{}*/{}";
    let lex = Lexer::new(source);
    let tokens: Vec<Token> = lex.collect();
    assert_eq!(tokens.len(), 4);

    let mut iter = tokens.iter();
    assert_eq!(
        iter.next().unwrap(),
        &Void(TokenMetadata {
            line: 0,
            chars: 0..4
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &Identifier((
            TokenMetadata {
                line: 0,
                chars: 5..9
            },
            "main".into()
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &LeftBrace(TokenMetadata {
            line: 2,
            chars: 4..5
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &RightBrace(TokenMetadata {
            line: 2,
            chars: 5..6
        })
    );
}

#[test]
fn glsl_lex_identifier() {
    let source = "id123_OK 92No æNoø No¾ No好";
    let lex = Lexer::new(source);
    let tokens: Vec<Token> = lex.collect();
    assert_eq!(tokens.len(), 10);

    let mut iter = tokens.iter();
    assert_eq!(
        iter.next().unwrap(),
        &Identifier((
            TokenMetadata {
                line: 0,
                chars: 0..8
            },
            "id123_OK".into()
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &IntConstant((
            TokenMetadata {
                line: 0,
                chars: 9..11
            },
            92
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &Identifier((
            TokenMetadata {
                line: 0,
                chars: 11..13
            },
            "No".into()
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &Unknown((
            TokenMetadata {
                line: 0,
                chars: 14..15
            },
            'æ'
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &Identifier((
            TokenMetadata {
                line: 0,
                chars: 15..17
            },
            "No".into()
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &Unknown((
            TokenMetadata {
                line: 0,
                chars: 17..18
            },
            'ø'
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &Identifier((
            TokenMetadata {
                line: 0,
                chars: 19..21
            },
            "No".into()
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &Unknown((
            TokenMetadata {
                line: 0,
                chars: 21..22
            },
            '¾'
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &Identifier((
            TokenMetadata {
                line: 0,
                chars: 23..25
            },
            "No".into()
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &Unknown((
            TokenMetadata {
                line: 0,
                chars: 25..26
            },
            '好'
        ))
    );
}

#[test]
fn glsl_lex_version() {
    let source = "#version 890 core";
    let lex = Lexer::new(source);
    let tokens: Vec<Token> = lex.collect();
    assert_eq!(tokens.len(), 3);

    let mut iter = tokens.iter();
    assert_eq!(
        iter.next().unwrap(),
        &Version(TokenMetadata {
            line: 0,
            chars: 0..8
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &IntConstant((
            TokenMetadata {
                line: 0,
                chars: 9..12
            },
            890
        ))
    );
    assert_eq!(
        iter.next().unwrap(),
        &Identifier((
            TokenMetadata {
                line: 0,
                chars: 13..17
            },
            "core".into()
        ))
    );
}

#[test]
fn glsl_lex_operators() {
    let source = "+ - * | & % / += -= *= |= &= %= /= ++ -- || && ^^";
    let lex = Lexer::new(source);
    let tokens: Vec<Token> = lex.collect();
    assert_eq!(tokens.len(), 19);

    let mut iter = tokens.iter();
    assert_eq!(
        iter.next().unwrap(),
        &Plus(TokenMetadata {
            line: 0,
            chars: 0..1
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &Dash(TokenMetadata {
            line: 0,
            chars: 2..3
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &Star(TokenMetadata {
            line: 0,
            chars: 4..5
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &VerticalBar(TokenMetadata {
            line: 0,
            chars: 6..7
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &Ampersand(TokenMetadata {
            line: 0,
            chars: 8..9
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &Percent(TokenMetadata {
            line: 0,
            chars: 10..11
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &Slash(TokenMetadata {
            line: 0,
            chars: 12..13
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &AddAssign(TokenMetadata {
            line: 0,
            chars: 14..16
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &SubAssign(TokenMetadata {
            line: 0,
            chars: 17..19
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &MulAssign(TokenMetadata {
            line: 0,
            chars: 20..22
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &OrAssign(TokenMetadata {
            line: 0,
            chars: 23..25
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &AndAssign(TokenMetadata {
            line: 0,
            chars: 26..28
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &ModAssign(TokenMetadata {
            line: 0,
            chars: 29..31
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &DivAssign(TokenMetadata {
            line: 0,
            chars: 32..34
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &IncOp(TokenMetadata {
            line: 0,
            chars: 35..37
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &DecOp(TokenMetadata {
            line: 0,
            chars: 38..40
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &OrOp(TokenMetadata {
            line: 0,
            chars: 41..43
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &AndOp(TokenMetadata {
            line: 0,
            chars: 44..46
        })
    );
    assert_eq!(
        iter.next().unwrap(),
        &XorOp(TokenMetadata {
            line: 0,
            chars: 47..49
        })
    );
}
