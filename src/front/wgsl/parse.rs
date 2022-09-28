use std::convert::TryInto;
use std::ops::Range;
use std::{cell::RefCell, fmt::Write, str::FromStr};

use chumsky::{error::SimpleReason, prelude::*, Parser as CParser, Stream};
use half::f16;

use crate::front::wgsl::{
    ast::*,
    lexer::{Lexer, Token, TokenKind},
    text::Interner,
    WgslError,
};
use crate::Span;

impl chumsky::Span for Span {
    type Context = ();
    type Offset = usize;

    fn new(_: Self::Context, range: Range<Self::Offset>) -> Self {
        range.into()
    }

    fn context(&self) -> Self::Context {
        ()
    }

    fn start(&self) -> Self::Offset {
        self.to_range().unwrap().start
    }

    fn end(&self) -> Self::Offset {
        self.to_range().unwrap().end
    }
}

struct Parser<'a> {
    intern: &'a mut Interner,
    diagnostics: &'a mut Vec<WgslError>,
    lexer: Lexer<'a>,
}

pub fn parse(
    source: &str,
    intern: &mut Interner,
    diagnostics: &mut Vec<WgslError>,
) -> TranslationUnit {
    let (tu, errors) = Parser {
        lexer: Lexer::new(source),
        intern,
        diagnostics,
    }
    .parse(source);

    for x in errors.into_iter().map(|x| error_to_diagnostic(x)) {
        diagnostics.push(x);
    }

    tu.unwrap_or_default()
}

impl Parser<'_> {
    fn parse(self, source: &str) -> (Option<TranslationUnit>, Vec<Simple<TokenKind, Span>>) {
        fn get_text(source: &str, span: Span) -> &str {
            &source[span.to_range().unwrap()]
        }

        let intern = RefCell::new(self.intern);
        let diagnostics = RefCell::new(self.diagnostics);

        let kw = |kw: &'static str| {
            filter_map(move |span: Span, kind: TokenKind| {
                if matches!(kind, TokenKind::Word) && get_text(source, span) == kw {
                    Ok(span)
                } else {
                    Err(Simple::custom(span, format!("expected keyword `{}`", kw)))
                }
            })
        };
        let text = just(TokenKind::Word)
            .map_with_span(|_, span: Span| intern.borrow_mut().get(get_text(source, span)));
        let ident = text.map_with_span(|name, span| Ident { name, span });

        let mut expr = Recursive::declare();

        let static_assert = kw("static_assert")
            .ignore_then(expr.clone())
            .map(|expr| StaticAssert { expr });

        let mut block = Recursive::declare();

        let stmt = recursive(|stmt| {
            let break_ = kw("break")
                .ignore_then(kw("if").ignore_then(expr.clone()).or_not())
                .map(|expr| match expr {
                    Some(expr) => StmtKind::BreakIf(expr),
                    None => StmtKind::Break,
                });

            let for_ = kw("for")
                .ignore_then(just(TokenKind::LParen))
                .ignore_then(expr.clone().or_not())
                .then_ignore(just(TokenKind::Semicolon))
                .then(expr.clone().or_not())
                .then_ignore(just(TokenKind::Semicolon))
                .then(expr.clone().or_not())
                .then_ignore(just(TokenKind::RParen))
                .then(block.clone())
                .map(|(((init, cond), update), block)| For {
                    init,
                    cond,
                    update,
                    block,
                })
                .boxed();

            let if_ = kw("if")
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(kw("else").ignore_then(stmt.clone()).or_not())
                .map(|((cond, block), else_)| If {
                    cond,
                    block,
                    else_: else_.map(Box::new),
                })
                .boxed();

            let case_selector = kw("default")
                .to(CaseSelector::Default)
                .or(kw("case").ignore_then(
                    kw("default")
                        .to(CaseSelector::Default)
                        .or(expr.clone().map(CaseSelector::Expr)),
                ));
            let case = case_selector
                .separated_by(just(TokenKind::Comma))
                .allow_trailing()
                .then_ignore(just(TokenKind::Colon).or_not())
                .then(block.clone())
                .map_with_span(|(selectors, block), span| Case {
                    selectors,
                    block,
                    span,
                })
                .boxed();
            let switch_ = kw("switch")
                .ignore_then(expr.clone())
                .then(
                    case.repeated()
                        .delimited_by(just(TokenKind::LBrace), just(TokenKind::RBrace)),
                )
                .map(|(expr, cases)| Switch { expr, cases })
                .boxed();

            let while_ = kw("while")
                .ignore_then(expr.clone())
                .then(block.clone())
                .map(|(cond, block)| While { cond, block });

            let with_semi = choice((
                break_,
                kw("continue").to(StmtKind::Continue),
                kw("discard").to(StmtKind::Discard),
                kw("return")
                    .ignore_then(expr.clone().or_not())
                    .map(StmtKind::Return),
                static_assert.clone().map(StmtKind::StaticAssert),
                expr.clone().map(StmtKind::Expr),
            ))
            .then_ignore(just(TokenKind::Semicolon))
            .boxed();

            choice((
                block.clone().map(StmtKind::Block),
                for_.map(StmtKind::For),
                if_.map(StmtKind::If),
                kw("loop").ignore_then(block.clone()).map(StmtKind::Loop),
                switch_.map(StmtKind::Switch),
                while_.map(StmtKind::While),
                kw("continuing")
                    .ignore_then(block.clone())
                    .map(StmtKind::Continuing),
                just(TokenKind::Semicolon).to(StmtKind::Empty),
            ))
            .or(with_semi)
            .map_with_span(|kind, span| Stmt { kind, span })
            .boxed()
        });

        block.define(
            stmt.repeated()
                .delimited_by(just(TokenKind::LBrace), just(TokenKind::RBrace))
                .map_with_span(|stmts, span| Block { stmts, span }),
        );

        let ty = recursive(|ty: Recursive<_, Type, _>| {
            choice((
                kw("array")
                    .or(kw("binding_array"))
                    .then_ignore(just(TokenKind::Less))
                    .then(ty.clone())
                    .then(just(TokenKind::Comma).ignore_then(expr.clone()).or_not())
                    .then_ignore(just(TokenKind::Greater))
                    .map(|((span, ty), len)| {
                        TypeKind::Array(
                            Ident {
                                name: intern.borrow_mut().get(get_text(source, span)),
                                span,
                            },
                            Box::new(ty),
                            len,
                        )
                    }),
                ident
                    .clone()
                    .then(
                        ty.separated_by(just(TokenKind::Comma))
                            .delimited_by(just(TokenKind::Less), just(TokenKind::Greater))
                            .or_not(),
                    )
                    .map(|(ident, generics)| TypeKind::Ident(ident, generics.unwrap_or_default())),
            ))
            .map_with_span(|kind, span| Type { kind, span })
        });

        let let_ = |k: &'static str| {
            kw(k)
                .ignore_then(ident.clone())
                .then(just(TokenKind::Colon).ignore_then(ty.clone()).or_not())
                .then(just(TokenKind::Equal).ignore_then(expr.clone()))
                .map(|((name, ty), val)| Let { name, ty, val })
                .boxed()
        };
        let const_ = let_("const");
        let let_ = let_("let");

        let var_no_attribs = kw("var")
            .ignore_then(
                just(TokenKind::Less)
                    .ignore_then(ident.clone().or_not())
                    .then(just(TokenKind::Comma).ignore_then(ident.clone()).or_not())
                    .then_ignore(just(TokenKind::Greater))
                    .or_not(),
            )
            .then(ident.clone())
            .then(just(TokenKind::Colon).ignore_then(ty.clone()).or_not())
            .then(just(TokenKind::Equal).ignore_then(expr.clone()).or_not())
            .map(|(((access, name), ty), val)| {
                let (address_space, access_mode) = match access {
                    Some((address_space, access_mode)) => (address_space, access_mode),
                    None => (None, None),
                };
                VarNoAttribs {
                    address_space,
                    access_mode,
                    name,
                    ty,
                    val,
                }
            })
            .boxed();

        let var_decl = choice((
            var_no_attribs.clone().map(VarDecl::Var),
            const_.clone().map(VarDecl::Const),
            let_.clone().map(VarDecl::Let),
        ))
        .map(Box::new);

        let lit = choice((
            kw("true").to(Literal::Bool(true)),
            kw("false").to(Literal::Bool(false)),
            just(TokenKind::IntLit).map_with_span(|_, span| {
                parse_int(get_text(source, span), span, *diagnostics.borrow_mut())
            }),
            just(TokenKind::FloatLit).map_with_span(|_, span| {
                parse_float(get_text(source, span), span, *diagnostics.borrow_mut())
            }),
        ));

        let array = intern.borrow_mut().get_static("array");
        let ident_expr = kw("array")
            .then(
                just(TokenKind::Less)
                    .ignore_then(ty.clone())
                    .then(just(TokenKind::Comma).ignore_then(expr.clone()).or_not())
                    .then_ignore(just(TokenKind::Greater))
                    .or_not(),
            )
            .map(move |(span, ty)| {
                let (generics, array_len) = match ty {
                    Some((generics, len)) => (vec![generics], len.map(Box::new)),
                    None => (Vec::new(), None),
                };
                IdentExpr {
                    name: Ident { name: array, span },
                    generics,
                    array_len,
                }
            })
            .or(ident
                .then(
                    ty.clone()
                        .separated_by(just(TokenKind::Comma))
                        .allow_trailing()
                        .delimited_by(just(TokenKind::Less), just(TokenKind::Greater))
                        .or_not(),
                )
                .map(|(name, generics)| IdentExpr {
                    name,
                    generics: generics.unwrap_or_default(),
                    array_len: None,
                }));

        let atom = choice((
            just(TokenKind::Underscore).to(ExprKind::Underscore),
            lit.map(ExprKind::Literal),
            var_decl.map(ExprKind::VarDecl),
            ident_expr.map(ExprKind::Ident),
        ))
        .map_with_span(|kind, span| Expr { kind, span })
        .or(expr
            .clone()
            .delimited_by(just(TokenKind::LParen), just(TokenKind::RParen)))
        .boxed();

        enum CallIndexAccess {
            Call(Vec<Expr>),
            Index(Expr),
            Access(Ident),
            Postfix(PostfixOp),
        }
        let postfix = atom
            .then(
                choice((
                    expr.clone()
                        .separated_by(just(TokenKind::Comma))
                        .allow_trailing()
                        .delimited_by(just(TokenKind::LParen), just(TokenKind::RParen))
                        .map_with_span(|args, span| (CallIndexAccess::Call(args), span)),
                    expr.clone()
                        .delimited_by(just(TokenKind::LBracket), just(TokenKind::RBracket))
                        .map_with_span(|index, span| (CallIndexAccess::Index(index), span)),
                    just(TokenKind::Period)
                        .ignore_then(ident.clone())
                        .map_with_span(|ident, span| (CallIndexAccess::Access(ident), span)),
                    just(TokenKind::PlusPlus)
                        .to(PostfixOp::Increment)
                        .or(just(TokenKind::MinusMinus).to(PostfixOp::Decrement))
                        .map_with_span(|op, span| (CallIndexAccess::Postfix(op), span)),
                ))
                .repeated(),
            )
            .foldl(|f, access| {
                let span = Span::total_span([f.span, access.1]);
                Expr {
                    kind: match access.0 {
                        CallIndexAccess::Call(args) => ExprKind::Call(CallExpr {
                            target: Box::new(f),
                            args,
                        }),
                        CallIndexAccess::Index(index) => {
                            ExprKind::Index(Box::new(f), Box::new(index))
                        }
                        CallIndexAccess::Access(field) => ExprKind::Member(Box::new(f), field),
                        CallIndexAccess::Postfix(op) => ExprKind::Postfix(PostfixExpr {
                            expr: Box::new(f),
                            op,
                        }),
                    },
                    span,
                }
            })
            .boxed();

        let unary = select! {
            TokenKind::And => UnaryOp::Ref,
            TokenKind::Bang => UnaryOp::Not,
            TokenKind::Minus => UnaryOp::Minus,
            TokenKind::Star => UnaryOp::Deref,
            TokenKind::Tilde => UnaryOp::BitNot,
        }
        .map_with_span(|op, span| (op, span))
        .repeated()
        .then(postfix)
        .foldr(|op, expr| {
            let span = Span::total_span([op.1, expr.span]);
            Expr {
                kind: ExprKind::Unary(UnaryExpr {
                    op: op.0,
                    expr: Box::new(expr),
                }),
                span,
            }
        })
        .boxed();

        fn binary<'a>(
            side: impl CParser<TokenKind, Expr, Error = Simple<TokenKind, Span>> + Clone + 'a,
            op: impl CParser<TokenKind, BinaryOp, Error = Simple<TokenKind, Span>> + Clone + 'a,
        ) -> impl CParser<TokenKind, Expr, Error = Simple<TokenKind, Span>> + Clone + 'a {
            side.clone()
                .then(op.then(side).repeated())
                .foldl(|lhs, (op, rhs)| {
                    let span = Span::total_span([lhs.span, rhs.span]);
                    Expr {
                        kind: ExprKind::Binary(BinaryExpr {
                            lhs: Box::new(lhs),
                            op,
                            rhs: Box::new(rhs),
                        }),
                        span,
                    }
                })
                .boxed()
        }

        let product = binary(
            unary,
            select! {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::ForwardSlash => BinaryOp::Div,
                TokenKind::Modulo => BinaryOp::Mod,
            },
        );
        let sum = binary(
            product,
            select! {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
            },
        );
        let shift = binary(
            sum,
            select! {
                TokenKind::ShiftLeft => BinaryOp::BitShiftLeft,
            }
            .or(just(TokenKind::Greater)
                .map_with_span(|_, span| span)
                .then(just(TokenKind::Greater).map_with_span(|_, span: Span| span))
                .try_map(|(l, r): (Span, Span), span| {
                    if l.end() == r.start() {
                        Ok(BinaryOp::BitShiftRight)
                    } else {
                        Err(Simple::custom(span, "you should not be seeing this"))
                    }
                })),
        );
        let comparison = binary(
            shift,
            select! {
                TokenKind::Greater => BinaryOp::GreaterThan,
                TokenKind::GreaterEqual => BinaryOp::GreaterThanEqual,
                TokenKind::Less => BinaryOp::LessThan,
                TokenKind::LessEqual => BinaryOp::LessThanEqual,
            },
        );
        let equality = binary(
            comparison,
            select! {
                TokenKind::EqualEqual => BinaryOp::Equal,
                TokenKind::NotEqual => BinaryOp::NotEqual,
            },
        );
        let bitand = binary(equality, just(TokenKind::And).to(BinaryOp::BitAnd));
        let bitxor = binary(bitand, just(TokenKind::Xor).to(BinaryOp::BitXor));
        let bitor = binary(bitxor, just(TokenKind::Or).to(BinaryOp::BitOr));
        let and = binary(bitor, just(TokenKind::AndAnd).to(BinaryOp::And));
        let or = binary(and, just(TokenKind::OrOr).to(BinaryOp::Or));

        let assign = select! {
            TokenKind::Equal => AssignOp::Assign,
            TokenKind::PlusEqual => AssignOp::Add,
            TokenKind::MinusEqual => AssignOp::Sub,
            TokenKind::TimesEqual => AssignOp::Mul,
            TokenKind::DivideEqual => AssignOp::Div,
            TokenKind::ModuloEqual => AssignOp::Mod,
            TokenKind::ShiftLeftEqual => AssignOp::ShiftLeft,
            TokenKind::ShiftRightEqual => AssignOp::ShiftRight,
            TokenKind::AndEqual => AssignOp::BitAnd,
            TokenKind::XorEqual => AssignOp::BitXor,
            TokenKind::OrEqual => AssignOp::BitOr,
        };
        let assign = or
            .clone()
            .then(assign.then(or).repeated())
            .foldl(|lhs, (op, rhs)| {
                let span = Span::total_span([lhs.span, rhs.span]);
                Expr {
                    kind: ExprKind::Assign(AssignExpr {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    }),
                    span,
                }
            })
            .boxed();

        expr.define(assign);

        let attrib = just(TokenKind::Attr)
            .ignore_then(ident.clone())
            .then(
                expr.clone()
                    .separated_by(just(TokenKind::Comma))
                    .allow_trailing()
                    .delimited_by(just(TokenKind::LParen), just(TokenKind::RParen))
                    .or_not(),
            )
            .map_with_span(|(name, exprs), span| Attribute {
                name,
                exprs: exprs.unwrap_or_default(),
                span,
            })
            .boxed();
        let attribs = attrib.repeated();

        let arg = attribs
            .clone()
            .then(ident.clone())
            .then(just(TokenKind::Colon).ignore_then(ty.clone()))
            .map_with_span(|((attribs, name), ty), span| Arg {
                attribs,
                name,
                ty,
                span,
            })
            .boxed();

        let fn_ = attribs
            .clone()
            .then_ignore(kw("fn"))
            .then(ident.clone())
            .then(
                arg.clone()
                    .separated_by(just(TokenKind::Comma))
                    .allow_trailing()
                    .delimited_by(just(TokenKind::LParen), just(TokenKind::RParen)),
            )
            .then(
                just(TokenKind::Arrow)
                    .ignore_then(attribs.clone())
                    .then(ty.clone())
                    .or_not(),
            )
            .then(block.clone())
            .map(|((((attribs, name), args), ret), block)| {
                let (ret_attribs, ret) = match ret {
                    Some((ret_attribs, ret)) => (ret_attribs, Some(ret)),
                    None => (Vec::new(), None),
                };

                Fn {
                    attribs,
                    name,
                    args,
                    ret_attribs,
                    ret,
                    block,
                }
            })
            .boxed();

        let over = attribs
            .clone()
            .then_ignore(kw("override"))
            .then(ident.clone())
            .then(just(TokenKind::Colon).ignore_then(ty.clone()).or_not())
            .then(just(TokenKind::Equal).ignore_then(expr.clone()).or_not())
            .then_ignore(just(TokenKind::Semicolon))
            .map(|(((attribs, name), ty), val)| Override {
                attribs,
                name,
                ty,
                val,
            })
            .boxed();

        let var = attribs
            .then(var_no_attribs.clone())
            .map(|(attribs, inner)| Var { attribs, inner })
            .then_ignore(just(TokenKind::Semicolon))
            .boxed();

        let struct_ = kw("struct")
            .ignore_then(ident.clone())
            .then(
                arg.separated_by(just(TokenKind::Comma))
                    .allow_trailing()
                    .delimited_by(just(TokenKind::LBrace), just(TokenKind::RBrace)),
            )
            .map(|(name, fields)| Struct { name, fields })
            .boxed();

        let type_ = kw("type")
            .ignore_then(ident.clone())
            .then(just(TokenKind::Equal).ignore_then(ty.clone()))
            .then_ignore(just(TokenKind::Semicolon))
            .map(|(name, ty)| TypeDecl { name, ty })
            .boxed();

        let global_decl = choice((
            fn_.map(GlobalDeclKind::Fn),
            over.map(GlobalDeclKind::Override),
            var.map(GlobalDeclKind::Var),
            const_
                .map(GlobalDeclKind::Const)
                .then_ignore(just(TokenKind::Semicolon)),
            let_.map(GlobalDeclKind::Let)
                .then_ignore(just(TokenKind::Semicolon)),
            static_assert
                .map(GlobalDeclKind::StaticAssert)
                .then_ignore(just(TokenKind::Semicolon)),
            struct_.map(GlobalDeclKind::Struct),
            type_.map(GlobalDeclKind::Type),
        ))
        .map_with_span(|kind, span| GlobalDecl { kind, span })
        .boxed();

        let enable = kw("enable")
            .ignore_then(ident.clone())
            .then_ignore(just(TokenKind::Semicolon))
            .map_with_span(|name, span| Enable { name, span })
            .boxed();

        let tu = enable
            .repeated()
            .then(global_decl.repeated())
            .then_ignore(end())
            .map(|(enables, decls)| TranslationUnit { enables, decls });

        let mut lexer = self.lexer;
        tu.parse_recovery(Stream::from_iter(
            lexer.eof_span(),
            std::iter::from_fn(|| lexer.next().map(|Token { kind, span }| (kind, span))),
        ))
    }
}

fn parse_int(text: &str, span: Span, diagnostics: &mut Vec<WgslError>) -> Literal {
    let signedness = text.bytes().last().unwrap();
    let ty = if text.len() > 2 { &text[..2] } else { "" };
    let value = if signedness == b'i' || signedness == b'u' {
        &text[..text.len() - 1]
    } else {
        text
    };

    let value = if ty == "0x" {
        let value = &value[2..];
        i64::from_str_radix(value, 16)
    } else {
        i64::from_str(value)
    };

    match value {
        Ok(value) => {
            if signedness == b'i' {
                let i32: Result<i32, _> = value.try_into();
                match i32 {
                    Ok(value) => Literal::I32(value),
                    Err(_) => {
                        diagnostics.push(WgslError {
                            message: "integer literal is too large for i32".to_string(),
                            labels: vec![(span, "".to_string())],
                            notes: vec![],
                        });
                        Literal::I32(0)
                    }
                }
            } else if signedness == b'u' {
                let u32: Result<u32, _> = value.try_into();
                match u32 {
                    Ok(value) => Literal::U32(value),
                    Err(_) => {
                        diagnostics.push(WgslError {
                            message: "integer literal is too large for u32".to_string(),
                            labels: vec![(span, "".to_string())],
                            notes: vec![],
                        });
                        Literal::U32(0)
                    }
                }
            } else {
                Literal::AbstractInt(value)
            }
        }
        Err(_) => {
            diagnostics.push(WgslError {
                message: "integer literal too large".to_string(),
                labels: vec![(span, "".to_string())],
                notes: vec![],
            });
            Literal::AbstractInt(0)
        }
    }
}

fn parse_float(text: &str, span: Span, diagnostics: &mut Vec<WgslError>) -> Literal {
    let width = text.bytes().last().unwrap();
    let ty = if text.len() > 2 { &text[..2] } else { "" };
    let value = if width == b'f' || width == b'h' {
        &text[..text.len() - 1]
    } else {
        text
    };

    let value = if ty == "0x" {
        let value = &value[2..];
        hexf_parse::parse_hexf64(value, false).ok()
    } else {
        f64::from_str(value).ok()
    };

    match value {
        Some(value) => {
            if width == b'f' {
                Literal::F32(value as f32)
            } else if width == b'h' {
                Literal::F16(f16::from_f64(value))
            } else {
                Literal::AbstractFloat(value)
            }
        }
        None => {
            diagnostics.push(WgslError {
                message: "float literal could not be parsed".to_string(),
                labels: vec![(span, "".to_string())],
                notes: vec![],
            });
            Literal::AbstractFloat(0.0)
        }
    }
}

fn error_to_diagnostic(error: Simple<TokenKind, Span>) -> WgslError {
    let span = error.span();

    match error.reason() {
        SimpleReason::Unexpected => {
            let expected = error.expected();
            match error.found() {
                Some(tok) => WgslError {
                    message: format!("unexpected `{}`", tok),
                    labels: vec![(span, {
                        let mut s = "expected one of ".to_string();
                        comma_sep(&mut s, expected);
                        s
                    })],
                    notes: vec![],
                },
                None => WgslError {
                    message: "unexpected end of file".to_string(),
                    labels: vec![(span, {
                        let mut s = "expected one of ".to_string();
                        comma_sep(&mut s, expected);
                        s
                    })],
                    notes: vec![],
                },
            }
        }
        SimpleReason::Unclosed { span, delimiter } => WgslError {
            message: format!("unclosed `{}`", delimiter),
            labels: vec![(*span, "unclosed".to_string())],
            notes: vec![],
        },
        SimpleReason::Custom(message) => WgslError {
            message: message.to_string(),
            labels: vec![(span, "".to_string())],
            notes: vec![],
        },
    }
}

fn comma_sep<'a>(to: &mut String, toks: impl ExactSizeIterator<Item = &'a Option<TokenKind>>) {
    let mut toks = toks.filter_map(|x| x.as_ref().copied());
    match toks.next() {
        Some(x) => {
            write!(to, "`{}`", x).unwrap();
            for x in toks {
                write!(to, ", `{}`", x).unwrap();
            }
        }
        None => return,
    }
}
