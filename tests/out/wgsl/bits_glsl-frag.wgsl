fn main1() {
    var i: i32 = 0;
    var i2_: vec2<i32> = vec2<i32>(0, 0);
    var i3_: vec3<i32> = vec3<i32>(0, 0, 0);
    var i4_: vec4<i32> = vec4<i32>(0, 0, 0, 0);
    var u: u32 = 0u;
    var u2_: vec2<u32> = vec2<u32>(0u, 0u);
    var u3_: vec3<u32> = vec3<u32>(0u, 0u, 0u);
    var u4_: vec4<u32> = vec4<u32>(0u, 0u, 0u, 0u);
    var f2_: vec2<f32> = vec2<f32>(0.0, 0.0);
    var f4_: vec4<f32> = vec4<f32>(0.0, 0.0, 0.0, 0.0);

    let e33: vec4<f32> = f4_;
    u = pack4x8snorm(e33);
    let e36: vec4<f32> = f4_;
    u = pack4x8unorm(e36);
    let e39: vec2<f32> = f2_;
    u = pack2x16unorm(e39);
    let e42: vec2<f32> = f2_;
    u = pack2x16snorm(e42);
    let e45: vec2<f32> = f2_;
    u = pack2x16float(e45);
    let e48: u32 = u;
    f4_ = unpack4x8snorm(e48);
    let e51: u32 = u;
    f4_ = unpack4x8unorm(e51);
    let e54: u32 = u;
    f2_ = unpack2x16snorm(e54);
    let e57: u32 = u;
    f2_ = unpack2x16unorm(e57);
    let e60: u32 = u;
    f2_ = unpack2x16float(e60);
    let e66: i32 = i;
    let e67: i32 = i;
    i = insertBits(e66, e67, u32(5), u32(10));
    let e77: vec2<i32> = i2_;
    let e78: vec2<i32> = i2_;
    i2_ = insertBits(e77, e78, u32(5), u32(10));
    let e88: vec3<i32> = i3_;
    let e89: vec3<i32> = i3_;
    i3_ = insertBits(e88, e89, u32(5), u32(10));
    let e99: vec4<i32> = i4_;
    let e100: vec4<i32> = i4_;
    i4_ = insertBits(e99, e100, u32(5), u32(10));
    let e110: u32 = u;
    let e111: u32 = u;
    u = insertBits(e110, e111, u32(5), u32(10));
    let e121: vec2<u32> = u2_;
    let e122: vec2<u32> = u2_;
    u2_ = insertBits(e121, e122, u32(5), u32(10));
    let e132: vec3<u32> = u3_;
    let e133: vec3<u32> = u3_;
    u3_ = insertBits(e132, e133, u32(5), u32(10));
    let e143: vec4<u32> = u4_;
    let e144: vec4<u32> = u4_;
    u4_ = insertBits(e143, e144, u32(5), u32(10));
    let e153: i32 = i;
    i = extractBits(e153, u32(5), u32(10));
    let e162: vec2<i32> = i2_;
    i2_ = extractBits(e162, u32(5), u32(10));
    let e171: vec3<i32> = i3_;
    i3_ = extractBits(e171, u32(5), u32(10));
    let e180: vec4<i32> = i4_;
    i4_ = extractBits(e180, u32(5), u32(10));
    let e189: u32 = u;
    u = extractBits(e189, u32(5), u32(10));
    let e198: vec2<u32> = u2_;
    u2_ = extractBits(e198, u32(5), u32(10));
    let e207: vec3<u32> = u3_;
    u3_ = extractBits(e207, u32(5), u32(10));
    let e216: vec4<u32> = u4_;
    u4_ = extractBits(e216, u32(5), u32(10));
    return;
}

[[stage(fragment)]]
fn main() {
    main1();
    return;
}
