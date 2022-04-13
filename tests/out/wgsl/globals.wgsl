struct Foo {
    v3_: vec3<f32>,
    v1_: f32,
}

let Foo_2: bool = true;

var<workgroup> wg: array<f32,10u>;
var<workgroup> at_1: atomic<u32>;
@group(0) @binding(1) 
var<storage, read_write> alignment: Foo;
@group(0) @binding(2) 
var<storage> dummy: array<vec2<f32>>;
@group(0) @binding(3) 
var<uniform> float_vecs: array<vec4<f32>,20>;
@group(0) @binding(4) 
var<uniform> global_vec: vec4<f32>;

fn test_msl_packed_vec3_as_arg(arg: vec3<f32>) {
    return;
}

fn test_msl_packed_vec3_() {
    var idx: i32 = 1;

    alignment.v3_ = vec3<f32>(1.0);
    alignment.v3_.x = 1.0;
    alignment.v3_.x = 2.0;
    let _e20 = idx;
    alignment.v3_[_e20] = 3.0;
    let data = alignment;
    let unnamed = data.v3_;
    let unnamed_1 = data.v3_.zx;
    test_msl_packed_vec3_as_arg(data.v3_);
    let unnamed_2 = (data.v3_ * mat3x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)));
    let unnamed_3 = (mat3x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)) * data.v3_);
    let unnamed_4 = (data.v3_ * 2.0);
    let unnamed_5 = (2.0 * data.v3_);
}

@stage(compute) @workgroup_size(1, 1, 1) 
fn main() {
    var Foo_1: f32 = 1.0;
    var at: bool = true;

    test_msl_packed_vec3_();
    let _e10 = global_vec.x;
    wg[6] = _e10;
    let _e16 = dummy[1].y;
    wg[5] = _e16;
    let _e22 = float_vecs[0].w;
    wg[4] = _e22;
    let _e26 = alignment.v1_;
    wg[3] = _e26;
    let _e31 = alignment.v3_.x;
    wg[2] = _e31;
    alignment.v1_ = 4.0;
    wg[1] = f32(arrayLength((&dummy)));
    atomicStore((&at_1), 2u);
    return;
}
