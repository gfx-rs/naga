#version 310 es

precision highp float;
precision highp int;

layout(local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

struct Foo {
    vec3 v3_;
    float v1_;
};
shared float wg[10];

shared uint at_1;

layout(std430) buffer Foo_block_0Compute { Foo _group_0_binding_1_cs; };

layout(std430) readonly buffer type_6_block_1Compute { vec2 _group_0_binding_2_cs[]; };

uniform type_8_block_2Compute { vec4 _group_0_binding_3_cs[20]; };

uniform type_4_block_3Compute { vec3 _group_0_binding_4_cs; };

uniform type_9_block_4Compute { mat3x2 _group_0_binding_5_cs; };

uniform type_12_block_5Compute { mat2x4 _group_0_binding_6_cs[2][2]; };

uniform type_15_block_6Compute { mat4x2 _group_0_binding_7_cs[2][2]; };


void test_msl_packed_vec3_as_arg(vec3 arg) {
    return;
}

void test_msl_packed_vec3_() {
    int idx = 1;
    _group_0_binding_1_cs.v3_ = vec3(1.0);
    _group_0_binding_1_cs.v3_.x = 1.0;
    _group_0_binding_1_cs.v3_.x = 2.0;
    int _e23 = idx;
    _group_0_binding_1_cs.v3_[_e23] = 3.0;
    Foo data = _group_0_binding_1_cs;
    vec3 unnamed = data.v3_;
    vec2 unnamed_1 = data.v3_.zx;
    test_msl_packed_vec3_as_arg(data.v3_);
    vec3 unnamed_2 = (data.v3_ * mat3x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)));
    vec3 unnamed_3 = (mat3x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)) * data.v3_);
    vec3 unnamed_4 = (data.v3_ * 2.0);
    vec3 unnamed_5 = (2.0 * data.v3_);
}

void main() {
    float Foo_1 = 1.0;
    bool at = true;
    test_msl_packed_vec3_();
    mat4x2 _e16 = _group_0_binding_7_cs[0][0];
    vec4 _e23 = _group_0_binding_6_cs[0][0][0];
    wg[7] = (_e16 * _e23).x;
    mat3x2 _e28 = _group_0_binding_5_cs;
    vec3 _e29 = _group_0_binding_4_cs;
    wg[6] = (_e28 * _e29).x;
    float _e37 = _group_0_binding_2_cs[1].y;
    wg[5] = _e37;
    float _e43 = _group_0_binding_3_cs[0].w;
    wg[4] = _e43;
    float _e47 = _group_0_binding_1_cs.v1_;
    wg[3] = _e47;
    float _e52 = _group_0_binding_1_cs.v3_.x;
    wg[2] = _e52;
    _group_0_binding_1_cs.v1_ = 4.0;
    wg[1] = float(uint(_group_0_binding_2_cs.length()));
    at_1 = 2u;
    return;
}

