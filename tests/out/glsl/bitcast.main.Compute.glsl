#version 310 es

precision highp float;
precision highp int;

layout(local_size_x = 1, local_size_y = 1, local_size_z = 1) in;


void main() {
    ivec2 i2_ = ivec2(0);
    ivec3 i3_ = ivec3(0);
    ivec4 i4_ = ivec4(0);
    uvec2 u2_ = uvec2(0u);
    uvec3 u3_ = uvec3(0u);
    uvec4 u4_ = uvec4(0u);
    vec2 f2_ = vec2(0.0);
    vec3 f3_ = vec3(0.0);
    vec4 f4_ = vec4(0.0);
    ivec2 i2_1 = ivec2(0);
    i2_ = i2_1;
    ivec3 i3_1 = ivec3(0);
    i3_ = i3_1;
    ivec4 i4_1 = ivec4(0);
    i4_ = i4_1;
    uvec2 u2_1 = uvec2(0u);
    u2_ = u2_1;
    uvec3 u3_1 = uvec3(0u);
    u3_ = u3_1;
    uvec4 u4_1 = uvec4(0u);
    u4_ = u4_1;
    vec2 f2_1 = vec2(0.0);
    f2_ = f2_1;
    vec3 f3_1 = vec3(0.0);
    f3_ = f3_1;
    vec4 f4_1 = vec4(0.0);
    f4_ = f4_1;
    ivec2 _e28 = i2_;
    u2_ = uvec2(_e28);
    ivec3 _e32 = i3_;
    u3_ = uvec3(_e32);
    ivec4 _e36 = i4_;
    u4_ = uvec4(_e36);
    uvec2 _e40 = u2_;
    i2_ = ivec2(_e40);
    uvec3 _e44 = u3_;
    i3_ = ivec3(_e44);
    uvec4 _e48 = u4_;
    i4_ = ivec4(_e48);
    ivec2 _e52 = i2_;
    f2_ = intBitsToFloat(_e52);
    ivec3 _e56 = i3_;
    f3_ = intBitsToFloat(_e56);
    ivec4 _e60 = i4_;
    f4_ = intBitsToFloat(_e60);
    return;
}

