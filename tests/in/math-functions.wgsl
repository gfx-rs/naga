@vertex
fn main() {
    let f = 1.0;
    let v = vec4<f32>(0.0);
    let a = degrees(f);
    let b = radians(f);
    let c = degrees(v);
    let d = radians(v);
    let e = saturate(v);
    let g = refract(v, v, f);
    let const_dot = dot(vec2<i32>(), vec2<i32>());
    let first_leading_bit_abs = firstLeadingBit(abs(0u));
    let ctz_a = countTrailingZeros(0u);
    let ctz_b = countTrailingZeros(0);
    let ctz_c = countTrailingZeros(0xFFFFFFFFu);
    let ctz_d = countTrailingZeros(-1);
    let ctz_e = countTrailingZeros(vec2(0u));
    let ctz_f = countTrailingZeros(vec2(0));
    let ctz_g = countTrailingZeros(vec2(1u));
    let ctz_h = countTrailingZeros(vec2(1));
    let clz_a = countLeadingZeros(-1);
    let clz_b = countLeadingZeros(1u);
    let clz_c = countLeadingZeros(vec2(-1));
    let clz_d = countLeadingZeros(vec2(1u));
}
