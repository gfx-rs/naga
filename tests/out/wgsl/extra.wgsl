struct PushConstants {
    @size(16) index: u32,
    @size(16) double: vec2<f64>,
}

struct FragmentIn {
    @location(0) @size(16) color: vec4<f32>,
    @builtin(primitive_index) @size(16) primitive_index: u32,
}

var<push_constant> pc: PushConstants;

@fragment 
fn main(in: FragmentIn) -> @location(0) vec4<f32> {
    let _e4 = pc.index;
    if (in.primitive_index == _e4) {
        return in.color;
    } else {
        return vec4<f32>((vec3<f32>(1.0) - in.color.xyz), in.color.w);
    }
}
