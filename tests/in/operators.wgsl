// Regexes for the literals are taken from the working draft at
// https://www.w3.org/TR/2021/WD-WGSL-20210806/#literals

// matches for /^(-?[0-9]*\.[0-9]+|-?[0-9]+\.[0-9]*)((e|E)(\+|-)?[0-9]+)?$/
let dec_float_lit_0 : f32 = -1.;
let dec_float_lit_1 : f32 = -.1;
let dec_float_lit_2 : f32 = 42.1234;
let dec_float_lit_3 : f32 = -1.E3;
let dec_float_lit_4 : f32 = -.1e-5;
let dec_float_lit_5 : f32 = 2.3e+55;

// matches for /^-?0x([0-9a-fA-F]*\.?[0-9a-fA-F]+|[0-9a-fA-F]+\.[0-9a-fA-F]*)(p|P)(\+|-)?[0-9]+$/
let hex_float_lit_0 : f32 = -0xa.p1;
let hex_float_lit_1 : f32 = -0x.fp9;
let hex_float_lit_2 : f32 = 0x2a.4D2P4;
let hex_float_lit_3 : f32 = -0x.1p-5;
let hex_float_lit_4 : f32 = 0xC.8p+55;
let hex_float_lit_5 : f32 = 0x1p1;

// matches for /^-?0x[0-9a-fA-F]+|0|-?[1-9][0-9]*$/
let int_lit_0 : i32 = -0x0;
let int_lit_1 : i32 = 0;
let int_lit_2 : i32 = 0x2a4D2;
let int_lit_3 : i32 = 1092;
let int_lit_4 : i32 = -9923;

// matches for /^0x[0-9a-fA-F]+u|0u|[1-9][0-9]*u$/
let uint_lit_0 : u32 = 0x0u;
let uint_lit_1 : u32 = 0u;
let uint_lit_2 : u32 = 0x2a4D2u;
let uint_lit_3 : u32 = 1092u;

//TODO: support splatting constructors for globals?
let v_f32_one: vec4<f32> = vec4<f32>(1.0, 1.0, 1.0, 1.0);
let v_f32_zero: vec4<f32> = vec4<f32>(0.0, 0.0, 0.0, 0.0);
let v_f32_half: vec4<f32> = vec4<f32>(0.5, 0.5, 0.5, 0.5);
let v_i32_one: vec4<i32> = vec4<i32>(1, 1, 1, 1);

fn builtins() -> vec4<f32> {
    // select()
    let condition = true;
    let s1 = select(0, 1, condition);
    let s2 = select(v_f32_zero, v_f32_one, condition);
    let s3 = select(v_f32_one, v_f32_zero, vec4<bool>(false, false, false, false));
    // mix()
    let m1 = mix(v_f32_zero, v_f32_one, v_f32_half);
    let m2 = mix(v_f32_zero, v_f32_one, 0.1);
    // bitcast()
    let b1 = bitcast<f32>(v_i32_one.x);
    let b2 = bitcast<vec4<f32>>(v_i32_one);
    // done
    return vec4<f32>(vec4<i32>(s1)) + s2 + m1 + m2 + b1 + b2;
}

fn splat() -> vec4<f32> {
    let a = (1.0 + vec2<f32>(2.0) - 3.0) / 4.0;
    let b = vec4<i32>(5) % 2;
    return a.xyxy + vec4<f32>(b);
}

fn unary() -> i32 {
    let a = 1;
    if (!true) { return a; } else { return ~a; };
}

[[stage(compute), workgroup_size(1)]]
fn main() {
    let a = builtins();
    let b = splat();
    let c = unary();
}
