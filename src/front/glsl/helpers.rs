use crate::{ScalarKind, TypeInner, VectorSize};
use glsl::syntax::{BinaryOp, TypeSpecifierNonArray};

pub fn glsl_to_spirv_binary_op(op: BinaryOp) -> crate::BinaryOperator {
    match op {
        BinaryOp::Or => crate::BinaryOperator::LogicalOr,
        BinaryOp::Xor => todo!(),
        BinaryOp::And => crate::BinaryOperator::LogicalAnd,
        BinaryOp::BitOr => crate::BinaryOperator::InclusiveOr,
        BinaryOp::BitXor => crate::BinaryOperator::ExclusiveOr,
        BinaryOp::BitAnd => crate::BinaryOperator::And,
        BinaryOp::Equal => crate::BinaryOperator::Equal,
        BinaryOp::NonEqual => crate::BinaryOperator::NotEqual,
        BinaryOp::LT => crate::BinaryOperator::Less,
        BinaryOp::GT => crate::BinaryOperator::Greater,
        BinaryOp::LTE => crate::BinaryOperator::LessEqual,
        BinaryOp::GTE => crate::BinaryOperator::GreaterEqual,
        BinaryOp::LShift => crate::BinaryOperator::ShiftLeftLogical,
        BinaryOp::RShift => crate::BinaryOperator::ShiftRightArithmetic,
        BinaryOp::Add => crate::BinaryOperator::Add,
        BinaryOp::Sub => crate::BinaryOperator::Subtract,
        BinaryOp::Mult => crate::BinaryOperator::Multiply,
        BinaryOp::Div => crate::BinaryOperator::Divide,
        BinaryOp::Mod => crate::BinaryOperator::Modulo,
    }
}

pub fn glsl_to_spirv_type(ty: TypeSpecifierNonArray) -> Option<TypeInner> {
    use TypeSpecifierNonArray::*;

    match ty {
        Void => None,
        Bool => Some(TypeInner::Scalar {
            kind: ScalarKind::Bool,
            width: 1,
        }),
        Int => Some(TypeInner::Scalar {
            kind: ScalarKind::Sint,
            width: 32,
        }),
        UInt => Some(TypeInner::Scalar {
            kind: ScalarKind::Uint,
            width: 32,
        }),
        Float => Some(TypeInner::Scalar {
            kind: ScalarKind::Float,
            width: 32,
        }),
        Double => Some(TypeInner::Scalar {
            kind: ScalarKind::Float,
            width: 64,
        }),
        Vec2 => Some(TypeInner::Vector {
            size: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 32,
        }),
        Vec3 => Some(TypeInner::Vector {
            size: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 32,
        }),
        Vec4 => Some(TypeInner::Vector {
            size: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 32,
        }),
        DVec2 => Some(TypeInner::Vector {
            size: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 64,
        }),
        DVec3 => Some(TypeInner::Vector {
            size: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 64,
        }),
        DVec4 => Some(TypeInner::Vector {
            size: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 64,
        }),
        BVec2 => Some(TypeInner::Vector {
            size: VectorSize::Bi,
            kind: ScalarKind::Bool,
            width: 1,
        }),
        BVec3 => Some(TypeInner::Vector {
            size: VectorSize::Tri,
            kind: ScalarKind::Bool,
            width: 1,
        }),
        BVec4 => Some(TypeInner::Vector {
            size: VectorSize::Quad,
            kind: ScalarKind::Bool,
            width: 1,
        }),
        IVec2 => Some(TypeInner::Vector {
            size: VectorSize::Bi,
            kind: ScalarKind::Sint,
            width: 32,
        }),
        IVec3 => Some(TypeInner::Vector {
            size: VectorSize::Tri,
            kind: ScalarKind::Sint,
            width: 32,
        }),
        IVec4 => Some(TypeInner::Vector {
            size: VectorSize::Quad,
            kind: ScalarKind::Sint,
            width: 32,
        }),
        UVec2 => Some(TypeInner::Vector {
            size: VectorSize::Bi,
            kind: ScalarKind::Uint,
            width: 32,
        }),
        UVec3 => Some(TypeInner::Vector {
            size: VectorSize::Tri,
            kind: ScalarKind::Uint,
            width: 32,
        }),
        UVec4 => Some(TypeInner::Vector {
            size: VectorSize::Quad,
            kind: ScalarKind::Uint,
            width: 32,
        }),
        // Float Matrices
        Mat2 => Some(TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 32,
        }),
        Mat3 => Some(TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 32,
        }),
        Mat4 => Some(TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 32,
        }),
        Mat23 => Some(TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 32,
        }),
        Mat24 => Some(TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 32,
        }),
        Mat32 => Some(TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 32,
        }),
        Mat34 => Some(TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 32,
        }),
        Mat42 => Some(TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 32,
        }),
        Mat43 => Some(TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 32,
        }),
        // Double Matrices
        DMat2 => Some(TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 64,
        }),
        DMat3 => Some(TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 64,
        }),
        DMat4 => Some(TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 64,
        }),
        DMat23 => Some(TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 64,
        }),
        DMat24 => Some(TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 64,
        }),
        DMat32 => Some(TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 64,
        }),
        DMat34 => Some(TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 64,
        }),
        DMat42 => Some(TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 64,
        }),
        DMat43 => Some(TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 64,
        }),
        _ => unimplemented!(),
    }
}
