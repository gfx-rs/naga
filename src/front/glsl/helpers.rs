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

    Some(match ty {
        Void => return None,
        Bool => TypeInner::Scalar {
            kind: ScalarKind::Bool,
            width: 1,
        },
        Int => TypeInner::Scalar {
            kind: ScalarKind::Sint,
            width: 32,
        },
        UInt => TypeInner::Scalar {
            kind: ScalarKind::Uint,
            width: 32,
        },
        Float => TypeInner::Scalar {
            kind: ScalarKind::Float,
            width: 32,
        },
        Double => TypeInner::Scalar {
            kind: ScalarKind::Float,
            width: 64,
        },
        Vec2 => TypeInner::Vector {
            size: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 32,
        },
        Vec3 => TypeInner::Vector {
            size: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 32,
        },
        Vec4 => TypeInner::Vector {
            size: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 32,
        },
        DVec2 => TypeInner::Vector {
            size: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 64,
        },
        DVec3 => TypeInner::Vector {
            size: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 64,
        },
        DVec4 => TypeInner::Vector {
            size: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 64,
        },
        BVec2 => TypeInner::Vector {
            size: VectorSize::Bi,
            kind: ScalarKind::Bool,
            width: 1,
        },
        BVec3 => TypeInner::Vector {
            size: VectorSize::Tri,
            kind: ScalarKind::Bool,
            width: 1,
        },
        BVec4 => TypeInner::Vector {
            size: VectorSize::Quad,
            kind: ScalarKind::Bool,
            width: 1,
        },
        IVec2 => TypeInner::Vector {
            size: VectorSize::Bi,
            kind: ScalarKind::Sint,
            width: 32,
        },
        IVec3 => TypeInner::Vector {
            size: VectorSize::Tri,
            kind: ScalarKind::Sint,
            width: 32,
        },
        IVec4 => TypeInner::Vector {
            size: VectorSize::Quad,
            kind: ScalarKind::Sint,
            width: 32,
        },
        UVec2 => TypeInner::Vector {
            size: VectorSize::Bi,
            kind: ScalarKind::Uint,
            width: 32,
        },
        UVec3 => TypeInner::Vector {
            size: VectorSize::Tri,
            kind: ScalarKind::Uint,
            width: 32,
        },
        UVec4 => TypeInner::Vector {
            size: VectorSize::Quad,
            kind: ScalarKind::Uint,
            width: 32,
        },
        // Float Matrices
        Mat2 => TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 32,
        },
        Mat3 => TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 32,
        },
        Mat4 => TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 32,
        },
        Mat23 => TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 32,
        },
        Mat24 => TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 32,
        },
        Mat32 => TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 32,
        },
        Mat34 => TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 32,
        },
        Mat42 => TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 32,
        },
        Mat43 => TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 32,
        },
        // Double Matrices
        DMat2 => TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 64,
        },
        DMat3 => TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 64,
        },
        DMat4 => TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 64,
        },
        DMat23 => TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 64,
        },
        DMat24 => TypeInner::Matrix {
            columns: VectorSize::Bi,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 64,
        },
        DMat32 => TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 64,
        },
        DMat34 => TypeInner::Matrix {
            columns: VectorSize::Tri,
            rows: VectorSize::Quad,
            kind: ScalarKind::Float,
            width: 64,
        },
        DMat42 => TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Bi,
            kind: ScalarKind::Float,
            width: 64,
        },
        DMat43 => TypeInner::Matrix {
            columns: VectorSize::Quad,
            rows: VectorSize::Tri,
            kind: ScalarKind::Float,
            width: 64,
        },
        _ => unimplemented!(),
    })
}
