struct LightScatteringParams {
    BetaRay: f32,
    BetaMie: array<f32, 3>,
    HGg: f32,
    DistanceMul: array<f32, 4>,
    BlendCoeff: f32,
    SunDirection: vec3<f32>,
    SunColor: vec3<f32>,
}

struct VertexOutput {
    @builtin(position) member: vec4<f32>,
}

var<private> gl_Position: vec4<f32>;

fn main_1() {
    gl_Position = vec4<f32>(1.0, 1.0, 1.0, 1.0);
    return;
}

@vertex 
fn main() -> VertexOutput {
    main_1();
    let _e1 = gl_Position;
    return VertexOutput(_e1);
}
