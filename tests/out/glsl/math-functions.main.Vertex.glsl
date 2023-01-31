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
    int clz_a = (-1 <= 0 ? (-1 == 0 ? 32 : 0) : int(31.0 - floor(log(float(-1) + 0.5) * 1.442695)));
    uint clz_b = (1u <= 0u ? (1u == 0u ? 32u : 0u) : uint(31.0 - floor(log(float(1u) + 0.5) * 1.442695)));
    ivec2 _e20 = ivec2(-1);
    ivec2 clz_c = mix(ivec2(vec2(31.0) - floor(log(vec2(_e20) + 0.5) * 1.442695)), mix(ivec2(0u), ivec2(32u), equal(_e20, ivec2(0))), lessThanEqual(_e20, ivec2(0)));
    uvec2 _e23 = uvec2(1u);
    uvec2 clz_d = mix(uvec2(vec2(31.0) - floor(log(vec2(_e23) + 0.5) * 1.442695)), mix(uvec2(0u), uvec2(32u), equal(_e23, uvec2(0u))), lessThanEqual(_e23, uvec2(0u)));
}

