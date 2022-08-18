struct Foo {
    a: vec4<f32>,
    b: i32,
}

let v_f32_one: vec4<f32> = vec4<f32>(1.0, 1.0, 1.0, 1.0);
let v_f32_zero: vec4<f32> = vec4<f32>(0.0, 0.0, 0.0, 0.0);
let v_f32_half: vec4<f32> = vec4<f32>(0.5, 0.5, 0.5, 0.5);
let v_i32_one: vec4<i32> = vec4<i32>(1, 1, 1, 1);
fn builtins() -> vec4<f32> {
    let s1_ = select(0, 1, true);
    let s2_ = select(vec4<f32>(0.0, 0.0, 0.0, 0.0), vec4<f32>(1.0, 1.0, 1.0, 1.0), true);
    let s3_ = select(vec4<f32>(1.0, 1.0, 1.0, 1.0), vec4<f32>(0.0, 0.0, 0.0, 0.0), vec4<bool>(false, false, false, false));
    let m1_ = mix(vec4<f32>(0.0, 0.0, 0.0, 0.0), vec4<f32>(1.0, 1.0, 1.0, 1.0), vec4<f32>(0.5, 0.5, 0.5, 0.5));
    let m2_ = mix(vec4<f32>(0.0, 0.0, 0.0, 0.0), vec4<f32>(1.0, 1.0, 1.0, 1.0), 0.10000000149011612);
    let b1_ = bitcast<f32>(vec4<i32>(1, 1, 1, 1).x);
    let b2_ = bitcast<vec4<f32>>(vec4<i32>(1, 1, 1, 1));
    let v_i32_zero = vec4<i32>(vec4<f32>(0.0, 0.0, 0.0, 0.0));
    return (((((vec4<f32>((vec4<i32>(s1_) + v_i32_zero)) + s2_) + m1_) + m2_) + vec4<f32>(b1_)) + b2_);
}

fn splat() -> vec4<f32> {
    let a_1 = (((vec2<f32>(1.0) + vec2<f32>(2.0)) - vec2<f32>(3.0)) / vec2<f32>(4.0));
    let b = (vec4<i32>(5) % vec4<i32>(2));
    return (a_1.xyxy + vec4<f32>(b));
}

fn bool_cast(x: vec3<f32>) -> vec3<f32> {
    let y = vec3<bool>(x);
    return vec3<f32>(y);
}

fn constructors() -> f32 {
    var foo: Foo;

    foo = Foo(vec4<f32>(1.0), 1);
    let mat2comp = mat2x2<f32>(vec2<f32>(1.0, 0.0), vec2<f32>(0.0, 1.0));
    let mat4comp = mat4x4<f32>(vec4<f32>(1.0, 0.0, 0.0, 0.0), vec4<f32>(0.0, 1.0, 0.0, 0.0), vec4<f32>(0.0, 0.0, 1.0, 0.0), vec4<f32>(0.0, 0.0, 0.0, 1.0));
    _ = vec2<u32>(0u);
    _ = mat2x2<f32>(vec2<f32>(0.0), vec2<f32>(0.0));
    _ = array<i32,4u>(0, 1, 2, 3);
    _ = bool(false);
    _ = i32(0);
    _ = u32(0u);
    _ = f32(0.0);
    _ = vec2<u32>(vec2<u32>(0u, 0u));
    _ = mat2x3<f32>(mat2x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)));
    _ = bitcast<vec2<u32>>(vec2<u32>(0u, 0u));
    _ = mat2x3<f32>(mat2x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)));
    let _e75 = foo.a.x;
    return _e75;
}

fn logical() {
    var local: bool;
    var local_1: bool;

    _ = !(true);
    _ = !(vec2<bool>(true));
    if true {
        local = true;
    } else {
        local = false;
    }
    _ = local;
    if true {
        local_1 = false;
    } else {
        local_1 = false;
    }
    _ = local_1;
    _ = (true | false);
    _ = (vec3<bool>(true) | vec3<bool>(false));
    _ = (true & false);
    _ = (vec4<bool>(true) & vec4<bool>(false));
}

fn arithmetic() {
    _ = -(vec2<i32>(1));
    _ = -(vec2<f32>(1.0));
    _ = (2 + 1);
    _ = (2u + 1u);
    _ = (2.0 + 1.0);
    _ = (vec2<i32>(2) + vec2<i32>(1));
    _ = (vec3<u32>(2u) + vec3<u32>(1u));
    _ = (vec4<f32>(2.0) + vec4<f32>(1.0));
    _ = (2 - 1);
    _ = (2u - 1u);
    _ = (2.0 - 1.0);
    _ = (vec2<i32>(2) - vec2<i32>(1));
    _ = (vec3<u32>(2u) - vec3<u32>(1u));
    _ = (vec4<f32>(2.0) - vec4<f32>(1.0));
    _ = (2 * 1);
    _ = (2u * 1u);
    _ = (2.0 * 1.0);
    _ = (vec2<i32>(2) * vec2<i32>(1));
    _ = (vec3<u32>(2u) * vec3<u32>(1u));
    _ = (vec4<f32>(2.0) * vec4<f32>(1.0));
    _ = (2 / 1);
    _ = (2u / 1u);
    _ = (2.0 / 1.0);
    _ = (vec2<i32>(2) / vec2<i32>(1));
    _ = (vec3<u32>(2u) / vec3<u32>(1u));
    _ = (vec4<f32>(2.0) / vec4<f32>(1.0));
    _ = (2 % 1);
    _ = (2u % 1u);
    _ = (2.0 % 1.0);
    _ = (vec2<i32>(2) % vec2<i32>(1));
    _ = (vec3<u32>(2u) % vec3<u32>(1u));
    _ = (vec4<f32>(2.0) % vec4<f32>(1.0));
    _ = (vec2<i32>(2) + vec2<i32>(1));
    _ = (vec2<i32>(2) + vec2<i32>(1));
    _ = (vec2<u32>(2u) + vec2<u32>(1u));
    _ = (vec2<u32>(2u) + vec2<u32>(1u));
    _ = (vec2<f32>(2.0) + vec2<f32>(1.0));
    _ = (vec2<f32>(2.0) + vec2<f32>(1.0));
    _ = (vec2<i32>(2) - vec2<i32>(1));
    _ = (vec2<i32>(2) - vec2<i32>(1));
    _ = (vec2<u32>(2u) - vec2<u32>(1u));
    _ = (vec2<u32>(2u) - vec2<u32>(1u));
    _ = (vec2<f32>(2.0) - vec2<f32>(1.0));
    _ = (vec2<f32>(2.0) - vec2<f32>(1.0));
    _ = (vec2<i32>(2) * 1);
    _ = (2 * vec2<i32>(1));
    _ = (vec2<u32>(2u) * 1u);
    _ = (2u * vec2<u32>(1u));
    _ = (vec2<f32>(2.0) * 1.0);
    _ = (2.0 * vec2<f32>(1.0));
    _ = (vec2<i32>(2) / vec2<i32>(1));
    _ = (vec2<i32>(2) / vec2<i32>(1));
    _ = (vec2<u32>(2u) / vec2<u32>(1u));
    _ = (vec2<u32>(2u) / vec2<u32>(1u));
    _ = (vec2<f32>(2.0) / vec2<f32>(1.0));
    _ = (vec2<f32>(2.0) / vec2<f32>(1.0));
    _ = (vec2<i32>(2) % vec2<i32>(1));
    _ = (vec2<i32>(2) % vec2<i32>(1));
    _ = (vec2<u32>(2u) % vec2<u32>(1u));
    _ = (vec2<u32>(2u) % vec2<u32>(1u));
    _ = (vec2<f32>(2.0) % vec2<f32>(1.0));
    _ = (vec2<f32>(2.0) % vec2<f32>(1.0));
    _ = (mat3x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)) + mat3x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)));
    _ = (mat3x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)) - mat3x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)));
    _ = (mat3x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)) * 1.0);
    _ = (2.0 * mat3x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)));
    _ = (mat4x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)) * vec4<f32>(1.0));
    _ = (vec3<f32>(2.0) * mat4x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)));
    _ = (mat4x3<f32>(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 0.0, 0.0)) * mat3x4<f32>(vec4<f32>(0.0, 0.0, 0.0, 0.0), vec4<f32>(0.0, 0.0, 0.0, 0.0), vec4<f32>(0.0, 0.0, 0.0, 0.0)));
}

fn bit() {
    _ = ~(1);
    _ = ~(1u);
    _ = !(vec2<i32>(1));
    _ = !(vec3<u32>(1u));
    _ = (2 | 1);
    _ = (2u | 1u);
    _ = (vec2<i32>(2) | vec2<i32>(1));
    _ = (vec3<u32>(2u) | vec3<u32>(1u));
    _ = (2 & 1);
    _ = (2u & 1u);
    _ = (vec2<i32>(2) & vec2<i32>(1));
    _ = (vec3<u32>(2u) & vec3<u32>(1u));
    _ = (2 ^ 1);
    _ = (2u ^ 1u);
    _ = (vec2<i32>(2) ^ vec2<i32>(1));
    _ = (vec3<u32>(2u) ^ vec3<u32>(1u));
    _ = (2 << 1u);
    _ = (2u << 1u);
    _ = (vec2<i32>(2) << vec2<u32>(1u));
    _ = (vec3<u32>(2u) << vec3<u32>(1u));
    _ = (2 >> 1u);
    _ = (2u >> 1u);
    _ = (vec2<i32>(2) >> vec2<u32>(1u));
    _ = (vec3<u32>(2u) >> vec3<u32>(1u));
}

fn comparison() {
    _ = (2 == 1);
    _ = (2u == 1u);
    _ = (2.0 == 1.0);
    _ = (vec2<i32>(2) == vec2<i32>(1));
    _ = (vec3<u32>(2u) == vec3<u32>(1u));
    _ = (vec4<f32>(2.0) == vec4<f32>(1.0));
    _ = (2 != 1);
    _ = (2u != 1u);
    _ = (2.0 != 1.0);
    _ = (vec2<i32>(2) != vec2<i32>(1));
    _ = (vec3<u32>(2u) != vec3<u32>(1u));
    _ = (vec4<f32>(2.0) != vec4<f32>(1.0));
    _ = (2 < 1);
    _ = (2u < 1u);
    _ = (2.0 < 1.0);
    _ = (vec2<i32>(2) < vec2<i32>(1));
    _ = (vec3<u32>(2u) < vec3<u32>(1u));
    _ = (vec4<f32>(2.0) < vec4<f32>(1.0));
    _ = (2 <= 1);
    _ = (2u <= 1u);
    _ = (2.0 <= 1.0);
    _ = (vec2<i32>(2) <= vec2<i32>(1));
    _ = (vec3<u32>(2u) <= vec3<u32>(1u));
    _ = (vec4<f32>(2.0) <= vec4<f32>(1.0));
    _ = (2 > 1);
    _ = (2u > 1u);
    _ = (2.0 > 1.0);
    _ = (vec2<i32>(2) > vec2<i32>(1));
    _ = (vec3<u32>(2u) > vec3<u32>(1u));
    _ = (vec4<f32>(2.0) > vec4<f32>(1.0));
    _ = (2 >= 1);
    _ = (2u >= 1u);
    _ = (2.0 >= 1.0);
    _ = (vec2<i32>(2) >= vec2<i32>(1));
    _ = (vec3<u32>(2u) >= vec3<u32>(1u));
    _ = (vec4<f32>(2.0) >= vec4<f32>(1.0));
}

fn assignment() {
    var a: i32 = 1;
    var vec0_: vec3<i32> = vec3<i32>(0, 0, 0);

    let _e6 = a;
    a = (_e6 + 1);
    let _e9 = a;
    a = (_e9 - 1);
    let _e12 = a;
    let _e13 = a;
    a = (_e12 * _e13);
    let _e15 = a;
    let _e16 = a;
    a = (_e15 / _e16);
    let _e18 = a;
    a = (_e18 % 1);
    let _e21 = a;
    a = (_e21 & 0);
    let _e24 = a;
    a = (_e24 | 0);
    let _e27 = a;
    a = (_e27 ^ 0);
    let _e30 = a;
    a = (_e30 << 2u);
    let _e33 = a;
    a = (_e33 >> 1u);
    let _e36 = a;
    a = (_e36 + 1);
    let _e39 = a;
    a = (_e39 - 1);
    let _e46 = vec0_.y;
    vec0_.y = (_e46 + 1);
    let _e51 = vec0_.y;
    vec0_.y = (_e51 - 1);
    return;
}

@compute @workgroup_size(1, 1, 1) 
fn main() {
    let _e4 = builtins();
    let _e5 = splat();
    let _e7 = bool_cast(vec4<f32>(1.0, 1.0, 1.0, 1.0).xyz);
    let _e8 = constructors();
    logical();
    arithmetic();
    bit();
    comparison();
    assignment();
    return;
}
