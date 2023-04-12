#version 310 es

precision highp float;
precision highp int;


void main() {
    vec4 v = vec4(0.0);
    float a = degrees(1.0);
    float b = radians(1.0);
    vec4 c = degrees(v);
    vec4 d = radians(v);
    vec4 e = clamp(v, vec4(0.0), vec4(1.0));
    vec4 g = refract(v, v, 1.0);
    int const_dot = ( + ivec2(0, 0).x * ivec2(0, 0).x + ivec2(0, 0).y * ivec2(0, 0).y);
    uint first_leading_bit_abs = uint(findMSB(uint(abs(int(0u)))));
    uint ctz_a = min(uint(findLSB(0u)), 32u);
    int ctz_b = int(min(uint(findLSB(0)), 32u));
    uint ctz_c = min(uint(findLSB(4294967295u)), 32u);
    int ctz_d = int(min(uint(findLSB(-1)), 32u));
    uvec2 ctz_e = min(uvec2(findLSB(uvec2(0u))), uvec2(32u));
    ivec2 ctz_f = ivec2(min(uvec2(findLSB(ivec2(0))), uvec2(32u)));
    uvec2 ctz_g = min(uvec2(findLSB(uvec2(1u))), uvec2(32u));
    ivec2 ctz_h = ivec2(min(uvec2(findLSB(ivec2(1))), uvec2(32u)));
    int clz_a = (-1 < 0 ? 0 : 31 - findMSB(-1));
    uint clz_b = uint(31 - findMSB(1u));
    ivec2 _e40 = ivec2(-1);
    ivec2 clz_c = mix(ivec2(31) - findMSB(_e40), ivec2(0), lessThan(_e40, ivec2(0)));
    uvec2 clz_d = uvec2(ivec2(31) - findMSB(uvec2(1u)));
}

