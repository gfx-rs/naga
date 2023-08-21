struct S {
    @size(16) a: vec3<f32>,
}

struct Test {
    @size(16) a: S,
    @size(16) b: f32,
}

struct Test2_ {
    @size(32) a: array<vec3<f32>, 2>,
    @size(16) b: f32,
}

struct Test3_ {
    @size(64) a: mat4x3<f32>,
    @size(16) b: f32,
}

@group(0) @binding(0) 
var<uniform> input1_: Test;
@group(0) @binding(1) 
var<uniform> input2_: Test2_;
@group(0) @binding(2) 
var<uniform> input3_: Test3_;

@vertex 
fn vertex() -> @builtin(position) vec4<f32> {
    let _e4 = input1_.b;
    let _e8 = input2_.b;
    let _e12 = input3_.b;
    return (((vec4<f32>(1.0) * _e4) * _e8) * _e12);
}
