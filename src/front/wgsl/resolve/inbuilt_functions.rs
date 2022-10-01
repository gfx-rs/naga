use std::fmt::Display;

use strum::EnumIter;

use crate::front::wgsl::resolve::inbuilt::ToStaticString;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum InbuiltFunction {
    Bitcast,
    All,
    Any,
    Select,
    ArrayLength,
    Abs,
    Acos,
    Acosh,
    Asin,
    Asinh,
    Atan,
    Atanh,
    Atan2,
    Ceil,
    Clamp,
    Cos,
    Cosh,
    CountLeadingZeros,
    CountOneBits,
    CountTrailingZeros,
    Cross,
    Degrees,
    Determinant,
    Distance,
    Dot,
    Exp,
    Exp2,
    ExtractBits,
    FaceForward,
    FirstLeadingBit,
    FirstTrailingBit,
    Floor,
    Fma,
    Fract,
    Frexp,
    InsertBits,
    InverseSqrt,
    Ldexp,
    Length,
    Log,
    Log2,
    Max,
    Min,
    Mix,
    Modf,
    Normalize,
    Pow,
    QuantizeToF16,
    Radians,
    Reflect,
    Refract,
    ReverseBits,
    Round,
    Saturate,
    Sign,
    Sin,
    Sinh,
    Smoothstep,
    Sqrt,
    Step,
    Tan,
    Tanh,
    Transpose,
    Trunc,
    Dpdx,
    DpdxCoarse,
    DpdxFine,
    Dpdy,
    DpdyCoarse,
    DpdyFine,
    Fwidth,
    FwidthCoarse,
    FwidthFine,
    TextureDimensions,
    TextureGather,
    TextureGatherCompare,
    TextureLoad,
    TextureNumLayers,
    TextureNumLevels,
    TextureNumSamples,
    TextureSample,
    TextureSampleBias,
    TextureSampleCompare,
    TextureSampleCompareLevel,
    TextureSampleGrad,
    TextureSampleLevel,
    TextureSampleBaseClampToEdge,
    TextureStore,
    AtomicLoad,
    AtomicStore,
    AtomicAdd,
    AtomicSub,
    AtomicMax,
    AtomicMin,
    AtomicAnd,
    AtomicOr,
    AtomicXor,
    AtomicExchange,
    AtomicCompareExchangeWeak,
    Pack4x8Snorm,
    Pack4x8Unorm,
    Pack2x16Snorm,
    Pack2x16Unorm,
    Pack2x16Float,
    Unpack4x8Snorm,
    Unpack4x8Unorm,
    Unpack2x16Snorm,
    Unpack2x16Unorm,
    Unpack2x16Float,
    StorageBarrier,
    WorkgroupBarrier,
}

impl Display for InbuiltFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl ToStaticString for InbuiltFunction {
    fn to_static_str(&self) -> &'static str {
        match *self {
            InbuiltFunction::Abs => "abs",
            InbuiltFunction::Acos => "acos",
            InbuiltFunction::All => "all",
            InbuiltFunction::Any => "any",
            InbuiltFunction::Asin => "asin",
            InbuiltFunction::Atan => "atan",
            InbuiltFunction::Atan2 => "atan2",
            InbuiltFunction::Ceil => "ceil",
            InbuiltFunction::Clamp => "clamp",
            InbuiltFunction::Cos => "cos",
            InbuiltFunction::Cosh => "cosh",
            InbuiltFunction::Cross => "cross",
            InbuiltFunction::Determinant => "determinant",
            InbuiltFunction::Distance => "distance",
            InbuiltFunction::Dot => "dot",
            InbuiltFunction::Exp => "exp",
            InbuiltFunction::Exp2 => "exp2",
            InbuiltFunction::FaceForward => "faceForward",
            InbuiltFunction::Floor => "floor",
            InbuiltFunction::Fma => "fma",
            InbuiltFunction::Fract => "fract",
            InbuiltFunction::Frexp => "frexp",
            InbuiltFunction::InverseSqrt => "inverseSqrt",
            InbuiltFunction::Length => "length",
            InbuiltFunction::Log => "log",
            InbuiltFunction::Log2 => "log2",
            InbuiltFunction::Max => "max",
            InbuiltFunction::Min => "min",
            InbuiltFunction::Mix => "mix",
            InbuiltFunction::Modf => "modf",
            InbuiltFunction::Normalize => "normalize",
            InbuiltFunction::Pow => "pow",
            InbuiltFunction::QuantizeToF16 => "quantizeToF16",
            InbuiltFunction::Radians => "radians",
            InbuiltFunction::Reflect => "reflect",
            InbuiltFunction::Refract => "refract",
            InbuiltFunction::ReverseBits => "reverseBits",
            InbuiltFunction::Round => "round",
            InbuiltFunction::Saturate => "saturate",
            InbuiltFunction::Sign => "sign",
            InbuiltFunction::Sin => "sin",
            InbuiltFunction::Sinh => "sinh",
            InbuiltFunction::Smoothstep => "smoothStep",
            InbuiltFunction::Sqrt => "sqrt",
            InbuiltFunction::Step => "step",
            InbuiltFunction::Tan => "tan",
            InbuiltFunction::Tanh => "tanh",
            InbuiltFunction::Trunc => "trunc",
            InbuiltFunction::Transpose => "transpose",
            InbuiltFunction::TextureLoad => "textureLoad",
            InbuiltFunction::TextureSample => "textureSample",
            InbuiltFunction::TextureSampleBias => "textureSampleBias",
            InbuiltFunction::TextureSampleCompare => "textureSampleCompare",
            InbuiltFunction::TextureSampleGrad => "textureSampleGrad",
            InbuiltFunction::TextureSampleLevel => "textureSampleLevel",
            InbuiltFunction::TextureStore => "textureStore",
            InbuiltFunction::AtomicLoad => "atomicLoad",
            InbuiltFunction::AtomicStore => "atomicStore",
            InbuiltFunction::AtomicAdd => "atomicAdd",
            InbuiltFunction::AtomicSub => "atomicSub",
            InbuiltFunction::AtomicMax => "atomicMax",
            InbuiltFunction::AtomicMin => "atomicMin",
            InbuiltFunction::AtomicAnd => "atomicAnd",
            InbuiltFunction::AtomicOr => "atomicOr",
            InbuiltFunction::AtomicXor => "atomicXor",
            InbuiltFunction::AtomicExchange => "atomicExchange",
            InbuiltFunction::AtomicCompareExchangeWeak => "atomicCompareExchangeWeak",
            InbuiltFunction::Pack4x8Snorm => "pack4x8snorm",
            InbuiltFunction::Pack4x8Unorm => "pack4x8unorm",
            InbuiltFunction::Pack2x16Snorm => "pack2x16snorm",
            InbuiltFunction::Pack2x16Unorm => "pack2x16unorm",
            InbuiltFunction::Pack2x16Float => "pack2x16float",
            InbuiltFunction::Unpack4x8Snorm => "unpack4x8snorm",
            InbuiltFunction::Unpack4x8Unorm => "unpack4x8unorm",
            InbuiltFunction::Unpack2x16Snorm => "unpack2x16snorm",
            InbuiltFunction::Unpack2x16Unorm => "unpack2x16unorm",
            InbuiltFunction::Unpack2x16Float => "unpack2x16float",
            InbuiltFunction::StorageBarrier => "storageBarrier",
            InbuiltFunction::WorkgroupBarrier => "workgroupBarrier",
            InbuiltFunction::Bitcast => "bitcast",
            InbuiltFunction::Select => "select",
            InbuiltFunction::ArrayLength => "arrayLength",
            InbuiltFunction::Acosh => "acosh",
            InbuiltFunction::Asinh => "asinh",
            InbuiltFunction::Atanh => "atanh",
            InbuiltFunction::CountLeadingZeros => "countLeadingZeros",
            InbuiltFunction::CountOneBits => "countOneBits",
            InbuiltFunction::CountTrailingZeros => "countTrailingZeros",
            InbuiltFunction::Degrees => "degrees",
            InbuiltFunction::ExtractBits => "extractBits",
            InbuiltFunction::FirstLeadingBit => "firstLeadingBit",
            InbuiltFunction::FirstTrailingBit => "firstTrailingBit",
            InbuiltFunction::InsertBits => "insertBits",
            InbuiltFunction::Ldexp => "ldexp",
            InbuiltFunction::Dpdx => "dpdx",
            InbuiltFunction::DpdxCoarse => "dpdxCoarse",
            InbuiltFunction::DpdxFine => "dpdxFine",
            InbuiltFunction::Dpdy => "dpdy",
            InbuiltFunction::DpdyCoarse => "dpdyCoarse",
            InbuiltFunction::DpdyFine => "dpdyFine",
            InbuiltFunction::Fwidth => "fwidth",
            InbuiltFunction::FwidthCoarse => "fwidthCoarse",
            InbuiltFunction::FwidthFine => "fwidthFine",
            InbuiltFunction::TextureDimensions => "textureDimensions",
            InbuiltFunction::TextureGather => "textureGather",
            InbuiltFunction::TextureGatherCompare => "textureGatherCompare",
            InbuiltFunction::TextureNumLayers => "textureNumLayers",
            InbuiltFunction::TextureNumLevels => "textureNumLevels",
            InbuiltFunction::TextureNumSamples => "textureNumSamples",
            InbuiltFunction::TextureSampleCompareLevel => "textureSampleCompareLevel",
            InbuiltFunction::TextureSampleBaseClampToEdge => "textureSampleBaseClampToEdge",
        }
    }
}
