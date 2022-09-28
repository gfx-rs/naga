use crate::front::wgsl::WgslError;
use std::convert::TryInto;
use std::fmt::Display;
use std::ops::{Neg, Not};
use wgsl::ast::{Literal, UnaryOp};
use wgsl::resolve::ir::{Expr, ExprKind, TranslationUnit};
use wgsl::text::Interner;

pub struct Evaluator<'a> {
    intern: &'a Interner,
    module: &'a TranslationUnit,
    errors: Vec<WgslError>,
}

impl<'a> Evaluator<'a> {
    pub fn new(module: &'a TranslationUnit, intern: &'a Interner) -> Self {
        Self {
            intern,
            module,
            errors: Vec::new(),
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Option<Value> {
        let unsupported = |this: &mut Self| {
            this.errors.push(WgslError {
                message: format!("this expression is not supported in a const context yet"),
                labels: vec![(expr.span.into(), "".to_string())],
                notes: vec![],
            });
        };

        match expr.kind {
            ExprKind::Error => {
                unreachable!("tried to evaluate errored expr: should've already exited?")
            }
            ExprKind::Literal(ref l) => Some(match l {
                Literal::Bool(b) => Value::Bool(*b),
                Literal::AbstractInt(i) => Value::AbstractInt(*i),
                Literal::AbstractFloat(f) => Value::AbstractFloat(*f),
                Literal::I32(i) => Value::I32(*i),
                Literal::U32(u) => Value::U32(*u),
                Literal::F32(f) => Value::F32(*f),
                Literal::F16(_) => {
                    unsupported(self);
                    return None;
                }
            }),
            ExprKind::Unary(ref u) => {
                let rhs = self.eval(&u.expr)?;
                let ret = match u.op {
                    UnaryOp::Ref | UnaryOp::Deref => {
                        unsupported(self);
                        return None;
                    }
                    UnaryOp::Not => !rhs,
                    UnaryOp::Minus => -rhs,
                    UnaryOp::BitNot => rhs.bit_not(),
                };

                if ret.is_none() {
                    self.errors.push(WgslError {
                        message: format!("invalid type for operand"),
                        labels: vec![(u.expr.span.into(), format!("has type {}", rhs.ty()))],
                        notes: vec![],
                    })
                }

                ret
            }
            ExprKind::Binary(_)
            | ExprKind::Call(_)
            | ExprKind::Index(_, _)
            | ExprKind::Member(_, _)
            | ExprKind::Global(_)
            | ExprKind::Local(_) => {
                unsupported(self);
                None
            }
        }
    }

    pub fn as_positive_int(&mut self, expr: &Expr) -> Option<u32> {
        let value = self.eval(expr)?;

        match value {
            Value::U32(u) => Some(u),
            Value::I32(i) => {
                let x: Result<u32, _> = i.try_into();
                match x {
                    Ok(x) => Some(x),
                    Err(_) => {
                        self.errors.push(WgslError {
                            message: format!("integer value is too large"),
                            labels: vec![(expr.span.into(), format!("has value {}", i))],
                            notes: vec![],
                        });
                        None
                    }
                }
            }
            Value::AbstractInt(i) if i >= 0 => {
                let x: Result<u32, _> = i.try_into();
                match x {
                    Ok(x) => Some(x),
                    Err(_) => {
                        self.errors.push(WgslError {
                            message: format!("integer value is too large"),
                            labels: vec![(expr.span.into(), format!("has value {}", i))],
                            notes: vec![],
                        });
                        None
                    }
                }
            }
            _ => {
                self.errors.push(WgslError {
                    message: "expected a positive integer".to_string(),
                    labels: vec![(expr.span.into(), format!("has type {}", value.ty()))],
                    notes: vec![],
                });
                None
            }
        }
    }

    pub fn as_int(&mut self, expr: &Expr) -> Option<i32> {
        let value = self.eval(expr)?;

        match value {
            Value::U32(u) => {
                let x: Result<i32, _> = u.try_into();
                match x {
                    Ok(x) => Some(x),
                    Err(_) => {
                        self.errors.push(WgslError {
                            message: format!("integer value is too large"),
                            labels: vec![(expr.span.into(), format!("has value {}", u))],
                            notes: vec![],
                        });
                        None
                    }
                }
            }
            Value::I32(i) => Some(i),
            Value::AbstractInt(i) if i >= 0 => {
                let x: Result<i32, _> = i.try_into();
                match x {
                    Ok(x) => Some(x),
                    Err(_) => {
                        self.errors.push(WgslError {
                            message: format!("integer value is too large"),
                            labels: vec![(expr.span.into(), format!("has value {}", i))],
                            notes: vec![],
                        });
                        None
                    }
                }
            }
            _ => {
                self.errors.push(WgslError {
                    message: "expected an integer".to_string(),
                    labels: vec![(expr.span.into(), format!("has type {}", value.ty()))],
                    notes: vec![],
                });
                None
            }
        }
    }

    pub fn as_bool(&mut self, expr: &Expr) -> Option<bool> {
        let value = self.eval(expr)?;

        match value {
            Value::Bool(b) => Some(b),
            _ => {
                self.errors.push(WgslError {
                    message: "expected a boolean".to_string(),
                    labels: vec![(expr.span.into(), format!("has type {}", value.ty()))],
                    notes: vec![],
                });
                None
            }
        }
    }

    pub fn finish(self) -> Vec<WgslError> {
        self.errors
    }
}

#[derive(Copy, Clone)]
pub enum Value {
    Bool(bool),
    AbstractInt(i64),
    I32(i32),
    U32(u32),
    AbstractFloat(f64),
    F32(f32),
    F64(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::AbstractInt(i) => write!(f, "{}", i),
            Value::I32(i) => write!(f, "{}", i),
            Value::U32(u) => write!(f, "{}", u),
            Value::AbstractFloat(x) => write!(f, "{}", x),
            Value::F32(x) => write!(f, "{}", x),
            Value::F64(x) => write!(f, "{}", x),
        }
    }
}

impl Not for Value {
    type Output = Option<Self>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(b) => Some(Value::Bool(!b)),
            _ => None,
        }
    }
}

impl Neg for Value {
    type Output = Option<Self>;

    fn neg(self) -> Self::Output {
        match self {
            Value::AbstractInt(i) => Some(Value::AbstractInt(-i)),
            Value::I32(i) => Some(Value::I32(-i)),
            Value::AbstractFloat(f) => Some(Value::AbstractFloat(-f)),
            Value::F32(f) => Some(Value::F32(-f)),
            Value::F64(f) => Some(Value::F64(-f)),
            _ => None,
        }
    }
}

impl Value {
    fn bit_not(self) -> Option<Self> {
        match self {
            Value::U32(u) => Some(Value::U32(!u)),
            Value::I32(i) => Some(Value::I32(!i)),
            Value::AbstractInt(i) => Some(Value::AbstractInt(!i)),
            _ => None,
        }
    }
}

impl Value {
    fn ty(&self) -> ValueType {
        ValueType { src: self }
    }
}

pub struct ValueType<'a> {
    src: &'a Value,
}

impl Display for ValueType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.src {
            Value::Bool(_) => write!(f, "bool"),
            Value::AbstractInt(_) => write!(f, "{{integer}}"),
            Value::I32(_) => write!(f, "i32"),
            Value::U32(_) => write!(f, "u32"),
            Value::AbstractFloat(_) => write!(f, "{{float}}"),
            Value::F32(_) => write!(f, "f32"),
            Value::F64(_) => write!(f, "f64"),
        }
    }
}
