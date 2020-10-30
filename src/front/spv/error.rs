use super::ModuleState;
use crate::arena::Handle;

#[derive(Debug)]
pub enum Error {
    InvalidHeader,
    InvalidWordCount,
    UnknownInstruction(u16),
    UnknownCapability(spirv::Word),
    UnsupportedInstruction(ModuleState, spirv::Op),
    UnsupportedCapability(spirv::Capability),
    UnsupportedExtension(String),
    UnsupportedExtSet(String),
    UnsupportedExtInstSet(spirv::Word),
    UnsupportedExtInst(spirv::Word),
    UnsupportedType(Handle<crate::Type>),
    UnsupportedExecutionModel(spirv::Word),
    UnsupportedExecutionMode(spirv::Word),
    UnsupportedStorageClass(spirv::Word),
    UnsupportedImageDim(spirv::Word),
    UnsupportedImageFormat(spirv::Word),
    UnsupportedBuiltIn(spirv::Word),
    UnsupportedControlFlow(spirv::Word),
    UnsupportedBinaryOperator(spirv::Word),
    InvalidParameter(spirv::Op),
    InvalidOperandCount(spirv::Op, u16),
    InvalidOperand,
    InvalidId(spirv::Word),
    InvalidDecoration(spirv::Word),
    InvalidTypeWidth(spirv::Word),
    InvalidSign(spirv::Word),
    InvalidInnerType(spirv::Word),
    InvalidVectorSize(spirv::Word),
    InvalidVariableClass(spirv::StorageClass),
    InvalidAccessType(spirv::Word),
    InvalidAccess(Handle<crate::Expression>),
    InvalidAccessIndex(spirv::Word),
    InvalidBinding(spirv::Word),
    InvalidImageExpression(Handle<crate::Expression>),
    InvalidImageBaseType(Handle<crate::Type>),
    InvalidSamplerExpression(Handle<crate::Expression>),
    InvalidSampleImage(Handle<crate::Type>),
    InvalidSampleSampler(Handle<crate::Type>),
    InvalidSampleCoordinates(Handle<crate::Type>),
    InvalidDepthReference(Handle<crate::Type>),
    InvalidAsType(Handle<crate::Type>),
    InconsistentComparisonSampling(Handle<crate::Type>),
    WrongFunctionResultType(spirv::Word),
    WrongFunctionParameterType(spirv::Word),
    MissingDecoration(spirv::Decoration),
    BadString,
    IncompleteData,
    InvalidTerminator,
    InvalidEdgeClassification,
    UnexpectedComparisonType(Handle<crate::Type>),
}
