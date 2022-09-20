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
            ExprKind::Literal(ref l) => Some(Value::Scalar(match l {
                Literal::Bool(b) => ScalarValue::Bool(*b),
                Literal::AbstractInt(i) => ScalarValue::AbstractInt(*i),
                Literal::AbstractFloat(f) => ScalarValue::Float(FloatValue::AbstractFloat(*f)),
                Literal::I32(i) => ScalarValue::I32(*i),
                Literal::U32(u) => ScalarValue::U32(*u),
                Literal::F32(f) => ScalarValue::Float(FloatValue::F32(*f)),
                Literal::F16(_) => {
                    unsupported(self);
                    return None;
                }
            })),
            ExprKind::Local(_) => unreachable!("local usage found in const expr?"),
            ExprKind::Global(_) => {
                unsupported(self);
                None
            }
            ExprKind::Unary(ref u) => {
                let rhs = self.eval(&u.expr)?;
                let ret = match u.op {
                    UnaryOp::Ref => {
                        unsupported(self);
                        return None;
                    }
                    UnaryOp::RefRef => {
                        unsupported(self);
                        return None;
                    }
                    UnaryOp::Not => rhs.try_map_scalar_like(|value| !value),
                    UnaryOp::Minus => rhs.try_map_scalar_like(|value| -value),
                    UnaryOp::Deref => {
                        unsupported(self);
                        return None;
                    }
                    UnaryOp::BitNot => rhs.try_map_scalar_like(|value| value.bit_not()),
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
            ExprKind::Binary(_) => {
                unsupported(self);
                None
            }
            ExprKind::Call(_) => {
                unsupported(self);
                None
            }
            ExprKind::Index(_, _) => {
                unsupported(self);
                None
            }
            ExprKind::Member(_, _) => {
                unsupported(self);
                None
            }
        }
    }

    pub fn as_positive_int(&mut self, expr: &Expr) -> Option<u32> {
        let value = self.eval(expr)?;

        match value {
            Value::Scalar(ScalarValue::U32(u)) => Some(u),
            Value::Scalar(ScalarValue::I32(i)) if i >= 0 => Some(i as u32),
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

trait HasType<'a> {
    type Type: Display;

    fn ty(&'a self) -> Self::Type;
}

#[derive(Copy, Clone)]
pub enum FloatValue {
    AbstractFloat(f64),
    F32(f32),
    F64(f64),
}

impl Display for FloatValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FloatValue::AbstractFloat(v) => write!(f, "{}", v),
            FloatValue::F32(v) => write!(f, "{}", v),
            FloatValue::F64(v) => write!(f, "{}", v),
        }
    }
}

impl Neg for FloatValue {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::AbstractFloat(f) => Self::AbstractFloat(-f),
            Self::F32(f) => Self::F32(-f),
            Self::F64(f) => Self::F64(-f),
        }
    }
}

impl<'a> HasType<'a> for FloatValue {
    type Type = FloatValueType<'a>;

    fn ty(&self) -> FloatValueType {
        FloatValueType { src: self }
    }
}

pub struct FloatValueType<'a> {
    src: &'a FloatValue,
}

impl Display for FloatValueType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.src {
            FloatValue::AbstractFloat(_) => write!(f, "{{float}}"),
            FloatValue::F32(_) => write!(f, "f32"),
            FloatValue::F64(_) => write!(f, "f64"),
        }
    }
}

#[derive(Copy, Clone)]
pub enum ScalarValue {
    Bool(bool),
    AbstractInt(i64),
    I32(i32),
    U32(u32),
    Float(FloatValue),
}

impl Display for ScalarValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScalarValue::Bool(b) => write!(f, "{}", b),
            ScalarValue::AbstractInt(i) => write!(f, "{}", i),
            ScalarValue::I32(i) => write!(f, "{}", i),
            ScalarValue::U32(u) => write!(f, "{}", u),
            ScalarValue::Float(v) => write!(f, "{}", v),
        }
    }
}

impl Not for ScalarValue {
    type Output = Option<Self>;

    fn not(self) -> Self::Output {
        match self {
            ScalarValue::Bool(b) => Some(ScalarValue::Bool(!b)),
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
            ScalarValue::Float(f) => Some(ScalarValue::Float(-f)),
            _ => None,
        }
    }
}

impl ScalarValue {
    fn bit_not(self) -> Option<Self> {
        match self {
            ScalarValue::U32(u) => Some(ScalarValue::U32(!u)),
            ScalarValue::I32(i) => Some(ScalarValue::I32(!i)),
            ScalarValue::AbstractInt(i) => Some(ScalarValue::AbstractInt(!i)),
            _ => None,
        }
    }
}

impl<'a> HasType<'a> for ScalarValue {
    type Type = ScalarValueType<'a>;

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
            ScalarValue::Float(v) => write!(f, "{}", v.ty()),
        }
    }
}

#[derive(Copy, Clone)]
pub enum VecWidth<T> {
    W2([T; 2]),
    W3([T; 3]),
    W4([T; 4]),
}

impl<T: Copy> VecWidth<T> {
    pub fn get_any(&self) -> &T {
        match self {
            VecWidth::W2(v) => &v[0],
            VecWidth::W3(v) => &v[0],
            VecWidth::W4(v) => &v[0],
        }
    }

    fn map<U>(self, f: impl FnMut(T) -> U) -> VecWidth<U> {
        match self {
            VecWidth::W2(v) => VecWidth::W2(v.map(f)),
            VecWidth::W3(v) => VecWidth::W3(v.map(f)),
            VecWidth::W4(v) => VecWidth::W4(v.map(f)),
        }
    }

    fn try_map<U>(self, mut f: impl FnMut(T) -> Option<U>) -> Option<VecWidth<U>> {
        match self {
            VecWidth::W2(v) => f(v[0]).and_then(|_0| f(v[1]).map(|_1| VecWidth::W2([_0, _1]))),
            VecWidth::W3(v) => f(v[0])
                .and_then(|_0| f(v[1]).and_then(|_1| f(v[2]).map(|_2| VecWidth::W3([_0, _1, _2])))),
            VecWidth::W4(v) => f(v[0]).and_then(|_0| {
                f(v[1]).and_then(|_1| {
                    f(v[2]).and_then(|_2| f(v[3]).map(|_3| VecWidth::W4([_0, _1, _2, _3])))
                })
            }),
        }
    }
}

impl<T: Display> Display for VecWidth<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VecWidth::W2(v) => write!(f, "{}, {}", v[0], v[1]),
            VecWidth::W3(v) => write!(f, "{}, {}, {}", v[0], v[1], v[2]),
            VecWidth::W4(v) => write!(f, "{}, {}, {}, {}", v[0], v[1], v[2], v[3]),
        }
    }
}

impl<'a, T: 'a> HasType<'a> for VecWidth<T> {
    type Type = VecWidthType<'a, T>;

    fn ty(&self) -> VecWidthType<T> {
        VecWidthType { src: self }
    }
}

pub struct VecWidthType<'a, T> {
    src: &'a VecWidth<T>,
}

impl<T> Display for VecWidthType<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.src {
            VecWidth::W2(_) => write!(f, "2"),
            VecWidth::W3(_) => write!(f, "3"),
            VecWidth::W4(_) => write!(f, "4"),
        }
    }
}

#[derive(Copy, Clone)]
pub enum Value {
    Scalar(ScalarValue),
    Vector(VecWidth<ScalarValue>),
}

pub struct ValueType<'a> {
    src: &'a Value,
}

impl Display for ValueType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.src {
            Value::Scalar(s) => write!(f, "{}", s.ty()),
            Value::Vector(v) => write!(f, "vec{}<{}>", v.ty(), v.get_any().ty()),
        }
    }
}

impl<'a> HasType<'a> for Value {
    type Type = ValueType<'a>;

    fn ty(&self) -> ValueType {
        ValueType { src: self }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Scalar(s) => write!(f, "`{}`", s),
            Value::Vector(v) => write!(f, "`vec{}({})`", v.ty(), v),
        }
    }
}

impl Value {
    fn map_scalar_like(self, mut f: impl FnMut(ScalarValue) -> ScalarValue) -> Option<Self> {
        match self {
            Value::Scalar(s) => Some(Value::Scalar(f(s))),
            Value::Vector(v) => Some(Value::Vector(v.map(f))),
        }
    }

    fn try_map_scalar_like(
        self,
        mut f: impl FnMut(ScalarValue) -> Option<ScalarValue>,
    ) -> Option<Self> {
        match self {
            Value::Scalar(s) => f(s).map(Value::Scalar),
            Value::Vector(v) => v.try_map(f).map(Value::Vector),
        }
    }
}
