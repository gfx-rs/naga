[[block]]
struct Camera {
    ViewProj: mat4x4<f32>;
};

[[block]]
struct Transform {
    Model: mat4x4<f32>;
};

struct VertexOutput {
    [[location(0), interpolate(perspective)]] member: vec3<f32>;
    [[location(1), interpolate(perspective)]] member1: vec3<f32>;
    [[location(2), interpolate(perspective)]] member2: vec2<f32>;
    [[builtin(position)]] member3: vec4<f32>;
};

var<private> Vertex_Position: vec3<f32>;
var<private> Vertex_Normal: vec3<f32>;
var<private> Vertex_Uv: vec2<f32>;
var<private> v_Position: vec3<f32>;
var<private> v_Normal: vec3<f32>;
var<private> v_Uv: vec2<f32>;
[[group(0), binding(0)]]
var<uniform> global: Camera;
[[group(2), binding(0)]]
var<uniform> global1: Transform;
var<private> gl_Position: vec4<f32>;

fn main() {
    let _e3: vec3<f32> = Vertex_Normal;
    let _e17: mat4x4<f32> = global1.Model;
    let _e20: vec4<f32> = (_e17 * vec4<f32>(_e3, 1.0));
    v_Normal = vec3<f32>(_e20.x, _e20.y, _e20.z);
    v_Normal = (mat3x3<f32>(_e17[0].xyz, _e17[1].xyz, _e17[2].xyz) * _e3);
    let _e35: vec4<f32> = (_e17 * vec4<f32>(Vertex_Position, 1.0));
    v_Position = vec3<f32>(_e35.x, _e35.y, _e35.z);
    v_Uv = Vertex_Uv;
    gl_Position = (global.ViewProj * vec4<f32>(v_Position, 1.0));
    return;
}

[[stage(vertex)]]
fn main1([[location(0), interpolate(perspective)]] param: vec3<f32>, [[location(1), interpolate(perspective)]] param1: vec3<f32>, [[location(2), interpolate(perspective)]] param2: vec2<f32>) -> VertexOutput {
    Vertex_Position = param;
    Vertex_Normal = param1;
    Vertex_Uv = param2;
    main();
    return VertexOutput(v_Position, v_Normal, v_Uv, gl_Position);
}
