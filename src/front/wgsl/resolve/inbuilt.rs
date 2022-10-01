use std::fmt::Display;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder};
use rustc_hash::FxHashMap;
use strum::{EnumIter, IntoEnumIterator};

use crate::front::wgsl::parse::ast::Expr;
use crate::{
    front::wgsl::text::{Interner, Text},
    BuiltIn, Bytes, ImageDimension, Interpolation, Sampling, ScalarKind, Span, StorageFormat,
    VectorSize,
};

pub trait ToStaticString {
    fn to_static_str(&self) -> &'static str;
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum AccessMode {
    Read,
    Write,
    ReadWrite,
}

impl ToStaticString for AccessMode {
    fn to_static_str(&self) -> &'static str {
        match *self {
            AccessMode::Read => "read",
            AccessMode::Write => "write",
            AccessMode::ReadWrite => "read_write",
        }
    }
}

impl Display for AccessMode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<AccessMode> for crate::StorageAccess {
    fn from(access: AccessMode) -> Self {
        match access {
            AccessMode::Read => Self::LOAD,
            AccessMode::Write => Self::STORE,
            AccessMode::ReadWrite => Self::LOAD | Self::STORE,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum AddressSpace {
    Function,
    Private,
    Storage,
    Uniform,
    Workgroup,
    Handle,
    PushConstant,
}

impl ToStaticString for AddressSpace {
    fn to_static_str(&self) -> &'static str {
        match *self {
            AddressSpace::Function => "function",
            AddressSpace::Private => "private",
            AddressSpace::Storage => "storage",
            AddressSpace::Uniform => "uniform",
            AddressSpace::Workgroup => "workgroup",
            AddressSpace::Handle => "handle",
            AddressSpace::PushConstant => "push_constant",
        }
    }
}

impl Display for AddressSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<AddressSpace> for crate::AddressSpace {
    fn from(space: AddressSpace) -> Self {
        match space {
            AddressSpace::Function => Self::Function,
            AddressSpace::Private => Self::Private,
            AddressSpace::Storage => Self::Storage {
                access: crate::StorageAccess::LOAD,
            },
            AddressSpace::Uniform => Self::Uniform,
            AddressSpace::Workgroup => Self::WorkGroup,
            AddressSpace::Handle => Self::Handle,
            AddressSpace::PushConstant => Self::PushConstant,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Attribute {
    pub ty: AttributeType,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum AttributeType {
    Align(Expr),
    Binding(Expr),
    Builtin(Builtin),
    Compute,
    Const,
    Fragment,
    Group(Expr),
    Id(Expr),
    Interpolate(InterpolationType, Option<InterpolationSample>),
    Invariant,
    Location(Expr),
    Size(Expr),
    Vertex,
    WorkgroupSize(Expr, Option<Expr>, Option<Expr>),
    ConservativeDepth(Option<ConservativeDepth>),
}

impl Display for AttributeType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            AttributeType::Align(_) => write!(f, "align"),
            AttributeType::Binding(_) => write!(f, "binding"),
            AttributeType::Builtin(_) => write!(f, "builtin"),
            AttributeType::Compute => write!(f, "compute"),
            AttributeType::Const => write!(f, "const"),
            AttributeType::Fragment => write!(f, "fragment"),
            AttributeType::Group(_) => write!(f, "group"),
            AttributeType::Id(_) => write!(f, "id"),
            AttributeType::Interpolate(..) => write!(f, "interpolate"),
            AttributeType::Invariant => write!(f, "invariant"),
            AttributeType::Location(_) => write!(f, "location"),
            AttributeType::Size(_) => write!(f, "size"),
            AttributeType::Vertex => write!(f, "vertex"),
            AttributeType::WorkgroupSize(..) => write!(f, "workgroup_size"),
            AttributeType::ConservativeDepth(_) => write!(f, "early_depth_test"),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum Builtin {
    FragDepth,
    FrontFacing,
    GlobalInvocationId,
    InstanceIndex,
    LocalInvocationId,
    LocalInvocationIndex,
    NumWorkgroups,
    Position,
    SampleIndex,
    SampleMask,
    VertexIndex,
    WorkgroupId,
    PrimitiveIndex,
    ViewIndex,
}

impl ToStaticString for Builtin {
    fn to_static_str(&self) -> &'static str {
        match *self {
            Builtin::FragDepth => "frag_depth",
            Builtin::FrontFacing => "front_facing",
            Builtin::GlobalInvocationId => "global_invocation_id",
            Builtin::InstanceIndex => "instance_index",
            Builtin::LocalInvocationId => "local_invocation_id",
            Builtin::LocalInvocationIndex => "local_invocation_index",
            Builtin::NumWorkgroups => "num_workgroups",
            Builtin::Position => "position",
            Builtin::SampleIndex => "sample_index",
            Builtin::SampleMask => "sample_mask",
            Builtin::VertexIndex => "vertex_index",
            Builtin::WorkgroupId => "workgroup_id",
            Builtin::PrimitiveIndex => "primitive_index",
            Builtin::ViewIndex => "view_index",
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<Builtin> for BuiltIn {
    fn from(bt: Builtin) -> Self {
        match bt {
            Builtin::FragDepth => Self::FragDepth,
            Builtin::FrontFacing => Self::FrontFacing,
            Builtin::GlobalInvocationId => Self::GlobalInvocationId,
            Builtin::InstanceIndex => Self::InstanceIndex,
            Builtin::LocalInvocationId => Self::LocalInvocationId,
            Builtin::LocalInvocationIndex => Self::LocalInvocationIndex,
            Builtin::NumWorkgroups => Self::NumWorkGroups,
            Builtin::Position => Self::Position { invariant: false },
            Builtin::SampleIndex => Self::SampleIndex,
            Builtin::SampleMask => Self::SampleMask,
            Builtin::VertexIndex => Self::VertexIndex,
            Builtin::WorkgroupId => Self::WorkGroupId,
            Builtin::PrimitiveIndex => Self::PrimitiveIndex,
            Builtin::ViewIndex => Self::ViewIndex,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum InterpolationSample {
    Center,
    Centroid,
    Sample,
}

impl ToStaticString for InterpolationSample {
    fn to_static_str(&self) -> &'static str {
        match *self {
            InterpolationSample::Center => "center",
            InterpolationSample::Centroid => "centroid",
            InterpolationSample::Sample => "sample",
        }
    }
}

impl Display for InterpolationSample {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<InterpolationSample> for Sampling {
    fn from(sample: InterpolationSample) -> Self {
        match sample {
            InterpolationSample::Center => Self::Center,
            InterpolationSample::Centroid => Self::Centroid,
            InterpolationSample::Sample => Self::Sample,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum InterpolationType {
    Flat,
    Linear,
    Perspective,
}

impl ToStaticString for InterpolationType {
    fn to_static_str(&self) -> &'static str {
        match *self {
            InterpolationType::Flat => "flat",
            InterpolationType::Linear => "linear",
            InterpolationType::Perspective => "perspective",
        }
    }
}

impl Display for InterpolationType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<InterpolationType> for Interpolation {
    fn from(it: InterpolationType) -> Self {
        match it {
            InterpolationType::Flat => Self::Flat,
            InterpolationType::Linear => Self::Linear,
            InterpolationType::Perspective => Self::Perspective,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum PrimitiveType {
    I32,
    U32,
    F64,
    F32,
    F16,
    Bool,
}

impl ToStaticString for PrimitiveType {
    fn to_static_str(&self) -> &'static str {
        match *self {
            PrimitiveType::I32 => "i32",
            PrimitiveType::U32 => "u32",
            PrimitiveType::F64 => "f64",
            PrimitiveType::F32 => "f32",
            PrimitiveType::F16 => "f16",
            PrimitiveType::Bool => "bool",
        }
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<PrimitiveType> for (ScalarKind, Bytes) {
    fn from(ty: PrimitiveType) -> (ScalarKind, Bytes) {
        match ty {
            PrimitiveType::I32 => (ScalarKind::Sint, 4),
            PrimitiveType::U32 => (ScalarKind::Uint, 4),
            PrimitiveType::F64 => (ScalarKind::Float, 8),
            PrimitiveType::F32 => (ScalarKind::Float, 4),
            PrimitiveType::F16 => (ScalarKind::Float, 2),
            PrimitiveType::Bool => (ScalarKind::Bool, 1),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum VecType {
    Vec2,
    Vec3,
    Vec4,
}

impl ToStaticString for VecType {
    fn to_static_str(&self) -> &'static str {
        match *self {
            VecType::Vec2 => "vec2",
            VecType::Vec3 => "vec3",
            VecType::Vec4 => "vec4",
        }
    }
}

impl Display for VecType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<VecType> for VectorSize {
    fn from(ty: VecType) -> Self {
        match ty {
            VecType::Vec2 => Self::Bi,
            VecType::Vec3 => Self::Tri,
            VecType::Vec4 => Self::Quad,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum MatType {
    Mat2x2,
    Mat2x3,
    Mat2x4,
    Mat3x2,
    Mat3x3,
    Mat3x4,
    Mat4x2,
    Mat4x3,
    Mat4x4,
}

impl ToStaticString for MatType {
    fn to_static_str(&self) -> &'static str {
        match *self {
            MatType::Mat2x2 => "mat2x2",
            MatType::Mat2x3 => "mat2x3",
            MatType::Mat2x4 => "mat2x4",
            MatType::Mat3x2 => "mat3x2",
            MatType::Mat3x3 => "mat3x3",
            MatType::Mat3x4 => "mat3x4",
            MatType::Mat4x2 => "mat4x2",
            MatType::Mat4x3 => "mat4x3",
            MatType::Mat4x4 => "mat4x4",
        }
    }
}

impl Display for MatType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<MatType> for (VectorSize, VectorSize) {
    fn from(ty: MatType) -> (VectorSize, VectorSize) {
        match ty {
            MatType::Mat2x2 => (VectorSize::Bi, VectorSize::Bi),
            MatType::Mat2x3 => (VectorSize::Bi, VectorSize::Tri),
            MatType::Mat2x4 => (VectorSize::Bi, VectorSize::Quad),
            MatType::Mat3x2 => (VectorSize::Tri, VectorSize::Bi),
            MatType::Mat3x3 => (VectorSize::Tri, VectorSize::Tri),
            MatType::Mat3x4 => (VectorSize::Tri, VectorSize::Quad),
            MatType::Mat4x2 => (VectorSize::Quad, VectorSize::Bi),
            MatType::Mat4x3 => (VectorSize::Quad, VectorSize::Tri),
            MatType::Mat4x4 => (VectorSize::Quad, VectorSize::Quad),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum SampledTextureType {
    Texture1d,
    Texture1dArray,
    Texture2d,
    TextureMultisampled2d,
    Texture2dArray,
    Texture3d,
    TextureCube,
    TextureCubeArray,
}

impl ToStaticString for SampledTextureType {
    fn to_static_str(&self) -> &'static str {
        match *self {
            SampledTextureType::Texture1d => "texture_1d",
            SampledTextureType::Texture1dArray => "texture_1d_array",
            SampledTextureType::Texture2d => "texture_2d",
            SampledTextureType::TextureMultisampled2d => "texture_multisampled_2d",
            SampledTextureType::Texture2dArray => "texture_2d_array",
            SampledTextureType::Texture3d => "texture_3d",
            SampledTextureType::TextureCube => "texture_cube",
            SampledTextureType::TextureCubeArray => "texture_cube_array",
        }
    }
}

impl Display for SampledTextureType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<SampledTextureType> for (ImageDimension, bool, bool) {
    fn from(ty: SampledTextureType) -> (ImageDimension, bool, bool) {
        match ty {
            SampledTextureType::Texture1d => (ImageDimension::D1, false, false),
            SampledTextureType::Texture1dArray => (ImageDimension::D1, true, false),
            SampledTextureType::Texture2d => (ImageDimension::D2, false, false),
            SampledTextureType::TextureMultisampled2d => (ImageDimension::D2, false, true),
            SampledTextureType::Texture2dArray => (ImageDimension::D2, true, false),
            SampledTextureType::Texture3d => (ImageDimension::D3, false, false),
            SampledTextureType::TextureCube => (ImageDimension::Cube, false, false),
            SampledTextureType::TextureCubeArray => (ImageDimension::Cube, true, false),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum DepthTextureType {
    Depth2d,
    Depth2dArray,
    DepthCube,
    DepthCubeArray,
    DepthMultisampled2d,
}

impl ToStaticString for DepthTextureType {
    fn to_static_str(&self) -> &'static str {
        match *self {
            DepthTextureType::Depth2d => "texture_depth_2d",
            DepthTextureType::Depth2dArray => "texture_depth_2d_array",
            DepthTextureType::DepthCube => "texture_depth_cube",
            DepthTextureType::DepthCubeArray => "texture_depth_cube_array",
            DepthTextureType::DepthMultisampled2d => "texture_depth_multisampled_2d",
        }
    }
}

impl Display for DepthTextureType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<DepthTextureType> for (ImageDimension, bool, bool) {
    fn from(ty: DepthTextureType) -> (ImageDimension, bool, bool) {
        match ty {
            DepthTextureType::Depth2d => (ImageDimension::D2, false, false),
            DepthTextureType::Depth2dArray => (ImageDimension::D2, true, false),
            DepthTextureType::DepthCube => (ImageDimension::Cube, false, false),
            DepthTextureType::DepthCubeArray => (ImageDimension::Cube, true, false),
            DepthTextureType::DepthMultisampled2d => (ImageDimension::D2, false, true),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum SamplerType {
    Sampler,
    SamplerComparison,
}

impl ToStaticString for SamplerType {
    fn to_static_str(&self) -> &'static str {
        match *self {
            SamplerType::Sampler => "sampler",
            SamplerType::SamplerComparison => "sampler_comparison",
        }
    }
}

impl Display for SamplerType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<SamplerType> for bool {
    fn from(ty: SamplerType) -> bool {
        match ty {
            SamplerType::Sampler => false,
            SamplerType::SamplerComparison => true,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum StorageTextureType {
    Storage1d,
    Storage1dArray,
    Storage2d,
    Storage2dArray,
    Storage3d,
}

impl ToStaticString for StorageTextureType {
    fn to_static_str(&self) -> &'static str {
        match *self {
            StorageTextureType::Storage1d => "texture_storage_1d",
            StorageTextureType::Storage1dArray => "texture_storage_1d_array",
            StorageTextureType::Storage2d => "texture_storage_2d",
            StorageTextureType::Storage2dArray => "texture_storage_2d_array",
            StorageTextureType::Storage3d => "texture_storage_3d",
        }
    }
}

impl Display for StorageTextureType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<StorageTextureType> for (ImageDimension, bool) {
    fn from(ty: StorageTextureType) -> (ImageDimension, bool) {
        match ty {
            StorageTextureType::Storage1d => (ImageDimension::D1, false),
            StorageTextureType::Storage1dArray => (ImageDimension::D1, true),
            StorageTextureType::Storage2d => (ImageDimension::D2, false),
            StorageTextureType::Storage2dArray => (ImageDimension::D2, true),
            StorageTextureType::Storage3d => (ImageDimension::D3, false),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum TexelFormat {
    R32Float,
    R32Sint,
    R32Uint,
    Rg32Float,
    Rg32Sint,
    Rg32Uint,
    Rgba16Float,
    Rgba16Sint,
    Rgba16Uint,
    Rgba32Float,
    Rgba32Sint,
    Rgba32Uint,
    Rgba8Sint,
    Rgba8Uint,
    Rgba8Unorm,
    Rgba8Snorm,
}

impl ToStaticString for TexelFormat {
    fn to_static_str(&self) -> &'static str {
        match *self {
            TexelFormat::R32Float => "r32float",
            TexelFormat::R32Sint => "r32sint",
            TexelFormat::R32Uint => "r32uint",
            TexelFormat::Rg32Float => "rg32float",
            TexelFormat::Rg32Sint => "rg32sint",
            TexelFormat::Rg32Uint => "rg32uint",
            TexelFormat::Rgba16Float => "rgba16float",
            TexelFormat::Rgba16Sint => "rgba16sint",
            TexelFormat::Rgba16Uint => "rgba16uint",
            TexelFormat::Rgba32Float => "rgba32float",
            TexelFormat::Rgba32Sint => "rgba32sint",
            TexelFormat::Rgba32Uint => "rgba32uint",
            TexelFormat::Rgba8Sint => "rgba8sint",
            TexelFormat::Rgba8Uint => "rgba8uint",
            TexelFormat::Rgba8Unorm => "rgba8unorm",
            TexelFormat::Rgba8Snorm => "rgba8snorm",
        }
    }
}

impl Display for TexelFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<TexelFormat> for StorageFormat {
    fn from(fmt: TexelFormat) -> Self {
        match fmt {
            TexelFormat::R32Float => Self::R32Float,
            TexelFormat::R32Sint => Self::R32Sint,
            TexelFormat::R32Uint => Self::R32Uint,
            TexelFormat::Rg32Float => Self::Rg32Float,
            TexelFormat::Rg32Sint => Self::Rg32Sint,
            TexelFormat::Rg32Uint => Self::Rg32Uint,
            TexelFormat::Rgba16Float => Self::Rgba16Float,
            TexelFormat::Rgba16Sint => Self::Rgba16Sint,
            TexelFormat::Rgba16Uint => Self::Rgba16Uint,
            TexelFormat::Rgba32Float => Self::Rgba32Float,
            TexelFormat::Rgba32Sint => Self::Rgba32Sint,
            TexelFormat::Rgba32Uint => Self::Rgba32Uint,
            TexelFormat::Rgba8Sint => Self::Rgba8Sint,
            TexelFormat::Rgba8Uint => Self::Rgba8Uint,
            TexelFormat::Rgba8Unorm => Self::Rgba8Unorm,
            TexelFormat::Rgba8Snorm => Self::Rgba8Snorm,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum ConservativeDepth {
    GreaterEqual,
    LessEqual,
    Unchanged,
}

impl ToStaticString for ConservativeDepth {
    fn to_static_str(&self) -> &'static str {
        match *self {
            ConservativeDepth::GreaterEqual => "greater_equal",
            ConservativeDepth::LessEqual => "less_equal",
            ConservativeDepth::Unchanged => "unchanged",
        }
    }
}

impl Display for ConservativeDepth {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl From<ConservativeDepth> for crate::ConservativeDepth {
    fn from(depth: ConservativeDepth) -> Self {
        match depth {
            ConservativeDepth::GreaterEqual => Self::GreaterEqual,
            ConservativeDepth::LessEqual => Self::LessEqual,
            ConservativeDepth::Unchanged => Self::Unchanged,
        }
    }
}

#[derive(Clone)]
pub struct Matcher<T> {
    map: FxHashMap<Text, T>,
}

impl<T: ToStaticString + IntoEnumIterator + Copy> Matcher<T> {
    pub fn new(intern: &mut Interner) -> Self {
        let mut map = FxHashMap::default();

        for variant in T::iter() {
            map.insert(intern.get_static(variant.to_static_str()), variant);
        }

        Self { map }
    }

    pub fn get(&self, text: Text) -> Option<T> {
        self.map.get(&text).copied()
    }
}

pub fn reserved_matcher() -> AhoCorasick {
    AhoCorasickBuilder::new()
        .anchored(true)
        .build(crate::keywords::wgsl::RESERVED)
}
