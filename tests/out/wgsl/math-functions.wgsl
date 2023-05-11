@fragment 
fn main() {
    let v = vec4<f32>(0.0);
    let a = degrees(1.0);
    let b = radians(1.0);
    let c = degrees(v);
    let d = radians(v);
    let e = saturate(v);
    let g = refract(v, v, 1.0);
    let const_dot = dot(vec2<i32>(), vec2<i32>());
    let first_leading_bit_abs = firstLeadingBit(abs(0u));
    let flb_a = firstLeadingBit(-1);
    let flb_b = firstLeadingBit(vec2<i32>(-1));
    let flb_c = firstLeadingBit(vec2<u32>(1u));
    let ftb_a = firstTrailingBit(-1);
    let ftb_b = firstTrailingBit(1u);
    let ftb_c = firstTrailingBit(vec2<i32>(-1));
    let ftb_d = firstTrailingBit(vec2<u32>(1u));
    let ctz_a = countTrailingZeros(0u);
    let ctz_b = countTrailingZeros(0);
    let ctz_c = countTrailingZeros(4294967295u);
    let ctz_d = countTrailingZeros(-1);
    let ctz_e = countTrailingZeros(vec2<u32>(0u));
    let ctz_f = countTrailingZeros(vec2<i32>(0));
    let ctz_g = countTrailingZeros(vec2<u32>(1u));
    let ctz_h = countTrailingZeros(vec2<i32>(1));
    let clz_a = countLeadingZeros(-1);
    let clz_b = countLeadingZeros(1u);
    let clz_c = countLeadingZeros(vec2<i32>(-1));
    let clz_d = countLeadingZeros(vec2<u32>(1u));
}
