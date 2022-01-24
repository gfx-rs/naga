fn test_fma() -> vec2<f32> {
    let a = vec2<f32>(2.0, 2.0);
    let b = vec2<f32>(0.5, 0.5);
    let c = vec2<f32>(0.5, 0.5);

    // Hazard: HLSL needs a different intrinsic function for f32 and f64
    // See: https://github.com/gfx-rs/naga/issues/1579
    return fma(a, b, c);
}

fn test_integer_dot_product() {
    let a_2 = vec2<i32>(1);
    let b_2 = vec2<i32>(1);
    let c_2 = dot(a_2, b_2);

    let a_3 = vec3<u32>(1u);
    let b_3 = vec3<u32>(1u);
    let c_3 = dot(a_2, b_2);

    let a_4 = vec4<i32>(1);
    let b_4 = vec4<u32>(1u);
    let c_4 = dot(a_2, b_2);
}

@stage(compute) @workgroup_size(1)
fn main() {
    let a = test_fma();
    test_integer_dot_product();
}
