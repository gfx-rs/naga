use crate::front::wgsl::parse::ast::Literal;
use crate::front::wgsl::resolve::ir::{
    CallExpr, Expr, ExprKind, FnTarget, InbuiltType, TranslationUnit,
};
use crate::front::wgsl::text::Interner;
use crate::front::wgsl::WgslError;
use crate::UnaryOperator;
use std::convert::TryInto;
use std::fmt::Display;
use std::ops::{Neg, Not};

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
            this.errors.push(
                WgslError::new("this operation is not supported in a const context yet")
                    .marker(expr.span),
            );
        };

        match expr.kind {
            ExprKind::Error => return None,
            ExprKind::Literal(ref l) => Some(Value::Scalar(match l {
                Literal::Bool(b) => ScalarValue::Bool(*b),
                Literal::AbstractInt(i) => ScalarValue::AbstractInt(*i),
                Literal::AbstractFloat(f) => ScalarValue::AbstractFloat(*f),
                Literal::I32(i) => ScalarValue::I32(*i),
                Literal::U32(u) => ScalarValue::U32(*u),
                Literal::F32(f) => ScalarValue::F32(*f),
                Literal::F16(_) => {
                    unsupported(self);
                    return None;
                }
            })),
            ExprKind::Unary(ref u) => {
                let rhs = self.eval(&u.expr)?;
                let ty = rhs.ty().to_string();

                let ret = match u.op {
                    UnaryOperator::Not => rhs.map(|x| !x),
                    UnaryOperator::Negate => rhs.map(|x| -x),
                };

                if ret.is_none() {
                    self.errors.push(
                        WgslError::new("invalid type for operand")
                            .label(u.expr.span, format!("has type {}", ty)),
                    )
                }

                ret
            }
            ExprKind::Call(CallExpr {
                target: FnTarget::InbuiltType(ref ty),
                ref args,
            }) => match **ty {
                InbuiltType::Vector { size, .. } => {
                    if args.len() != size as usize && args.len() != 1 {
                        self.errors.push(
                            WgslError::new(format!(
                                "expected {} arguments, got {}",
                                size as usize,
                                args.len()
                            ))
                            .marker(expr.span),
                        );
                        return None;
                    }

                    let mut out = Vec::with_capacity(size as usize);
                    for expr in args {
                        let arg = self.eval(expr)?;
                        match arg {
                            Value::Scalar(scalar) => out.push(scalar),
                            _ => {
                                self.errors.push(
                                    WgslError::new("expected scalar value")
                                        .label(expr.span, format!("has type {}", arg.ty())),
                                );
                                return None;
                            }
                        }
                    }

                    // Splat.
                    if args.len() == 1 {
                        let arg = out[0];
                        for _ in 1..size as usize {
                            out.push(arg);
                        }
                    }

                    Some(Value::Vector(out))
                }
                _ => {
                    unsupported(self);
                    None
                }
            },
            ExprKind::Binary(_)
            | ExprKind::Call(_)
            | ExprKind::Index(_, _)
            | ExprKind::Member(_, _)
            | ExprKind::Global(_)
            | ExprKind::Local(_)
            | ExprKind::AddrOf(_)
            | ExprKind::Deref(_) => {
                unsupported(self);
                None
            }
        }
    }

    pub fn as_positive_int(&mut self, expr: &Expr) -> Option<u32> {
        let value = self.eval(expr)?;

        match value {
            Value::Scalar(ScalarValue::U32(u)) => Some(u),
            Value::Scalar(ScalarValue::I32(i)) => {
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
            Value::Scalar(ScalarValue::AbstractInt(i)) if i >= 0 => {
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
            Value::Scalar(ScalarValue::U32(u)) => {
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
            Value::Scalar(ScalarValue::I32(i)) => Some(i),
            Value::Scalar(ScalarValue::AbstractInt(i)) => {
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
            Value::Scalar(ScalarValue::Bool(b)) => Some(b),
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

#[derive(Clone)]
pub enum Value {
    Scalar(ScalarValue),
    Vector(Vec<ScalarValue>),
}

impl Value {
    fn map<F>(self, mut f: F) -> Option<Self>
    where
        F: FnMut(ScalarValue) -> Option<ScalarValue>,
    {
        match self {
            Value::Scalar(s) => f(s).map(Value::Scalar),
            Value::Vector(v) => v
                .into_iter()
                .map(f)
                .collect::<Option<Vec<_>>>()
                .map(Value::Vector),
        }
    }

    pub fn ty(&self) -> ValueType {
        ValueType { src: self }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Scalar(s) => write!(f, "{}", s),
            Value::Vector(v) => {
                write!(f, "vec{}(", v.len())?;
                for (i, s) in v.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", s)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Copy, Clone)]
pub enum ScalarValue {
    Bool(bool),
    AbstractInt(i64),
    I32(i32),
    U32(u32),
    AbstractFloat(f64),
    F32(f32),
    F64(f64),
}

impl Display for ScalarValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScalarValue::Bool(b) => write!(f, "{}", b),
            ScalarValue::AbstractInt(i) => write!(f, "{}", i),
            ScalarValue::I32(i) => write!(f, "{}", i),
            ScalarValue::U32(u) => write!(f, "{}", u),
            ScalarValue::AbstractFloat(x) => write!(f, "{}", x),
            ScalarValue::F32(x) => write!(f, "{}", x),
            ScalarValue::F64(x) => write!(f, "{}", x),
        }
    }
}

impl Not for ScalarValue {
    type Output = Option<Self>;

    fn not(self) -> Self::Output {
        match self {
            ScalarValue::Bool(b) => Some(ScalarValue::Bool(!b)),
            ScalarValue::U32(u) => Some(ScalarValue::U32(!u)),
            ScalarValue::I32(i) => Some(ScalarValue::I32(!i)),
            ScalarValue::AbstractInt(i) => Some(ScalarValue::AbstractInt(!i)),
            _ => None,
        }
    }
}

impl Neg for ScalarValue {
    type Output = Option<Self>;

    fn neg(self) -> Self::Output {
        match self {
            ScalarValue::AbstractInt(i) => Some(ScalarValue::AbstractInt(-i)),
            ScalarValue::I32(i) => Some(ScalarValue::I32(-i)),
            ScalarValue::AbstractFloat(f) => Some(ScalarValue::AbstractFloat(-f)),
            ScalarValue::F32(f) => Some(ScalarValue::F32(-f)),
            ScalarValue::F64(f) => Some(ScalarValue::F64(-f)),
            _ => None,
        }
    }
}

impl ScalarValue {
    fn ty(&self) -> ScalarValueType {
        ScalarValueType { src: self }
    }
}

pub struct ScalarValueType<'a> {
    src: &'a ScalarValue,
}

impl Display for ScalarValueType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.src {
            ScalarValue::Bool(_) => write!(f, "bool"),
            ScalarValue::AbstractInt(_) => write!(f, "{{integer}}"),
            ScalarValue::I32(_) => write!(f, "i32"),
            ScalarValue::U32(_) => write!(f, "u32"),
            ScalarValue::AbstractFloat(_) => write!(f, "{{float}}"),
            ScalarValue::F32(_) => write!(f, "f32"),
            ScalarValue::F64(_) => write!(f, "f64"),
        }
    }
}

pub struct ValueType<'a> {
    src: &'a Value,
}

impl Display for ValueType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.src {
            Value::Scalar(s) => write!(f, "{}", s.ty()),
            Value::Vector(v) => {
                write!(f, "vec{}<{}>", v.len(), v[0].ty())
            }
        }
    }
}
