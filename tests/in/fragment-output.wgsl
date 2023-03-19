struct FragmentOutput {
    @location(0) vec4f: vec4<f32>,
    @location(1) vec4i: vec4<i32>,
    @location(2) vec4u: vec4<u32>,
    @location(3) vec3f: vec3<f32>,
    @location(4) vec3i: vec3<i32>,
    @location(5) vec3u: vec3<u32>,
    @location(6) vec2f: vec2<f32>,
    @location(7) vec2i: vec2<i32>,
    @location(8) vec2u: vec2<u32>,
    @location(9) scalarf: f32,
    @location(10) scalari: i32,
    @location(11) scalaru: u32,
}

@fragment
fn main() -> FragmentOutput {
    var output: FragmentOutput;
    output.vec4f = vec4<f32>(0.0);
    output.vec4i = vec4<i32>(0);
    output.vec4u = vec4<u32>(0u);
    output.vec3f = vec3<f32>(0.0);
    output.vec3i = vec3<i32>(0);
    output.vec3u = vec3<u32>(0u);
    output.vec2f = vec2<f32>(0.0);
    output.vec2i = vec2<i32>(0);
    output.vec2u = vec2<u32>(0u);
    output.scalarf = 0.0;
    output.scalari = 0;
    output.scalaru = 0u;
    return output;
}
