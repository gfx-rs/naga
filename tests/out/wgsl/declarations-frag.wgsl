struct VertexData {
    @size(8) position: vec2<f32>,
    @size(8) a: vec2<f32>,
}

struct FragmentData {
    @size(8) position: vec2<f32>,
    @size(8) a: vec2<f32>,
}

struct TestStruct {
    @size(4) a: f32,
    @size(4) b: f32,
}

struct LightScatteringParams {
    @size(16) BetaRay: f32,
    @size(48) BetaMie: array<f32, 3>,
    @size(16) HGg: f32,
    @size(64) DistanceMul: array<f32, 4>,
    @size(16) BlendCoeff: f32,
    @size(16) SunDirection: vec3<f32>,
    @size(16) SunColor: vec3<f32>,
}

struct FragmentOutput {
    @location(0) @size(8) position: vec2<f32>,
    @location(1) @size(8) a: vec2<f32>,
    @location(2) @size(16) out_array: vec4<f32>,
    @location(3) @size(16) out_array_1: vec4<f32>,
}

var<private> vert: VertexData;
var<private> frag: FragmentData;
var<private> in_array_2: array<vec4<f32>, 2>;
var<private> out_array: array<vec4<f32>, 2>;
var<private> array_2d: array<array<f32, 2>, 2>;
var<private> array_toomanyd: array<array<array<array<array<array<array<f32, 2>, 2>, 2>, 2>, 2>, 2>, 2>;

fn main_1() {
    var positions: array<vec3<f32>, 2>;
    var strct: TestStruct;
    var from_input_array: vec4<f32>;
    var a_1: f32;
    var b: f32;

    positions = array<vec3<f32>, 2>(vec3<f32>(-(1.0), 1.0, 0.0), vec3<f32>(-(1.0), -(1.0), 0.0));
    strct = TestStruct(f32(1), f32(2));
    let _e35 = in_array_2[1];
    from_input_array = _e35;
    let _e41 = array_2d[0][0];
    a_1 = _e41;
    let _e57 = array_toomanyd[0][0][0][0][0][0][0];
    b = _e57;
    out_array[0] = vec4<f32>(2.0);
    return;
}

@fragment 
fn main(@location(0) position: vec2<f32>, @location(1) a: vec2<f32>, @location(2) in_array: vec4<f32>, @location(3) in_array_1: vec4<f32>) -> FragmentOutput {
    vert.position = position;
    vert.a = a;
    in_array_2[0] = in_array;
    in_array_2[1] = in_array_1;
    main_1();
    let _e30 = frag.position;
    let _e32 = frag.a;
    let _e35 = out_array[0];
    let _e37 = out_array[1];
    return FragmentOutput(_e30, _e32, _e35, _e37);
}
