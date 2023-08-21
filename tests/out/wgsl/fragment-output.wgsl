struct FragmentOutputVec4Vec3_ {
    @location(0) @size(16) vec4f: vec4<f32>,
    @location(1) @interpolate(flat) @size(16) vec4i: vec4<i32>,
    @location(2) @interpolate(flat) @size(16) vec4u: vec4<u32>,
    @location(3) @size(16) vec3f: vec3<f32>,
    @location(4) @interpolate(flat) @size(16) vec3i: vec3<i32>,
    @location(5) @interpolate(flat) @size(16) vec3u: vec3<u32>,
}

struct FragmentOutputVec2Scalar {
    @location(0) @size(8) vec2f: vec2<f32>,
    @location(1) @interpolate(flat) @size(8) vec2i: vec2<i32>,
    @location(2) @interpolate(flat) @size(8) vec2u: vec2<u32>,
    @location(3) @size(4) scalarf: f32,
    @location(4) @interpolate(flat) @size(4) scalari: i32,
    @location(5) @interpolate(flat) @size(8) scalaru: u32,
}

@fragment 
fn main_vec4vec3_() -> FragmentOutputVec4Vec3_ {
    var output: FragmentOutputVec4Vec3_;

    output.vec4f = vec4<f32>(0.0);
    output.vec4i = vec4<i32>(0);
    output.vec4u = vec4<u32>(0u);
    output.vec3f = vec3<f32>(0.0);
    output.vec3i = vec3<i32>(0);
    output.vec3u = vec3<u32>(0u);
    let _e19 = output;
    return _e19;
}

@fragment 
fn main_vec2scalar() -> FragmentOutputVec2Scalar {
    var output_1: FragmentOutputVec2Scalar;

    output_1.vec2f = vec2<f32>(0.0);
    output_1.vec2i = vec2<i32>(0);
    output_1.vec2u = vec2<u32>(0u);
    output_1.scalarf = 0.0;
    output_1.scalari = 0;
    output_1.scalaru = 0u;
    let _e16 = output_1;
    return _e16;
}
