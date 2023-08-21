struct gl_PerVertex {
    @builtin(position) @size(16) gl_Position: vec4<f32>,
    @size(4) gl_PointSize: f32,
    @size(4) gl_ClipDistance: array<f32, 1>,
    @size(8) gl_CullDistance: array<f32, 1>,
}

struct VertexOutput {
    @location(0) @size(0) member: vec2<f32>,
    @builtin(position) @size(65535) gl_Position: vec4<f32>,
}

var<private> v_uv: vec2<f32>;
var<private> a_uv_1: vec2<f32>;
var<private> perVertexStruct: gl_PerVertex = gl_PerVertex(vec4<f32>(0.0, 0.0, 0.0, 1.0), 1.0, array<f32, 1>(), array<f32, 1>());
var<private> a_pos_1: vec2<f32>;

fn main_1() {
    let _e8 = a_uv_1;
    v_uv = _e8;
    let _e9 = a_pos_1;
    perVertexStruct.gl_Position = vec4<f32>(_e9.x, _e9.y, 0.0, 1.0);
    return;
}

@vertex 
fn main(@location(1) a_uv: vec2<f32>, @location(0) a_pos: vec2<f32>) -> VertexOutput {
    a_uv_1 = a_uv;
    a_pos_1 = a_pos;
    main_1();
    let _e7 = v_uv;
    let _e8 = perVertexStruct.gl_Position;
    return VertexOutput(_e7, _e8);
}
