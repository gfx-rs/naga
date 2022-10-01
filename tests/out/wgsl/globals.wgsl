struct Foo {
    v3_: vec3<f32>,
    v1_: f32,
}

const FooConst: bool = true;

var<workgroup> wg: array<f32,10u>;
var<workgroup> at_1: atomic<u32>;
@group(0) @binding(1) 
var<storage, read_write> alignment: Foo;
@group(0) @binding(2) 
var<storage> dummy: array<vec2<f32>>;
@group(0) @binding(3) 
var<uniform> float_vecs: array<vec4<f32>,20u>;
@group(0) @binding(4) 
var<uniform> global_vec: vec3<f32>;
@group(0) @binding(5) 
var<uniform> global_mat: mat3x2<f32>;
@group(0) @binding(6) 
var<uniform> global_nested_arrays_of_matrices_2x4_: array<array<mat2x4<f32>,2u>,2u>;
@group(0) @binding(7) 
var<uniform> global_nested_arrays_of_matrices_4x2_: array<array<mat4x2<f32>,2u>,2u>;

fn test_msl_packed_vec3_as_arg(arg: vec3<f32>) {
    return;
}

fn test_msl_packed_vec3_() {
    var idx: i32;

    alignment.v3_ = vec3<f32>(1.0);
    idx = 1;
    alignment.v3_.x = 1.0;
    alignment.v3_.x = 2.0;
    let _e18 = idx;
    alignment.v3_[_e18] = 3.0;
    let data = alignment;
    _ = data.v3_;
    _ = data.v3_.zx;
    test_msl_packed_vec3_as_arg(data.v3_);
    _ = (data.v3_ * mat3x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)));
    _ = (mat3x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)) * data.v3_);
    _ = (data.v3_ * 2.0);
    _ = (2.0 * data.v3_);
}

@compute @workgroup_size(1, 1, 1) 
fn main() {
    var Foo_1: f32;
    var at: bool;

    test_msl_packed_vec3_();
    let _e5 = global_nested_arrays_of_matrices_4x2_[0][0];
    let _e13 = global_nested_arrays_of_matrices_2x4_[0][0][0];
    wg[7] = (_e5 * _e13).x;
    let _e20 = global_mat;
    let _e22 = global_vec;
    wg[6] = (_e20 * _e22).x;
    let _e32 = dummy[1].y;
    wg[5] = _e32;
    let _e40 = float_vecs[0].w;
    wg[4] = _e40;
    let _e46 = alignment.v1_;
    wg[3] = _e46;
    let _e53 = alignment.v3_.x;
    wg[2] = _e53;
    alignment.v1_ = 4.0;
    wg[1] = f32(arrayLength((&dummy)));
    atomicStore((&at_1), 2u);
    Foo_1 = 1.0;
    at = true;
    return;
}
