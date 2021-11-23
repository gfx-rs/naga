[[block]]
struct Globals {
    view_matrix: mat4x4<f32>;
};

[[block]]
struct VertexPushConstants {
    world_matrix: mat4x4<f32>;
};

struct VertexOutput {
    [[location(0)]] frag_color: vec4<f32>;
    [[builtin(position)]] member: vec4<f32>;
};

[[group(0), binding(0)]]
var<uniform> global: Globals;
var<push_constant> global_1: VertexPushConstants;
var<private> position_1: vec2<f32>;
var<private> color_1: vec4<f32>;
var<private> frag_color: vec4<f32>;
var<private> gl_Position: vec4<f32>;

fn main_1() {
    let _e7: vec4<f32> = color_1;
    frag_color = _e7;
    let _e9: mat4x4<f32> = global.view_matrix;
    let _e10: mat4x4<f32> = global_1.world_matrix;
    let _e12: vec2<f32> = position_1;
    gl_Position = ((_e9 * _e10) * vec4<f32>(_e12, 0.0, 1.0));
    let _e18: vec4<f32> = gl_Position;
    let _e20: vec4<f32> = gl_Position;
    gl_Position.z = ((_e18.z + _e20.w) / 2.0);
    return;
}

[[stage(vertex)]]
fn main([[location(0)]] position: vec2<f32>, [[location(1)]] color: vec4<f32>) -> VertexOutput {
    position_1 = position;
    color_1 = color;
    main_1();
    let _e15: vec4<f32> = frag_color;
    let _e17: vec4<f32> = gl_Position;
    return VertexOutput(_e15, _e17);
}
