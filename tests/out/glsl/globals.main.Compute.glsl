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

layout(std430) readonly buffer arrayvec2f32__block_1Compute { vec2 _group_0_binding_2_cs[]; };

uniform arrayvec4f3220__block_2Compute { vec4 _group_0_binding_3_cs[20]; };

uniform vec3f32__block_3Compute { vec3 _group_0_binding_4_cs; };

uniform mat3x2f32__block_4Compute { mat3x2 _group_0_binding_5_cs; };

uniform arrayarraymat2x4f3222__block_5Compute { mat2x4 _group_0_binding_6_cs[2][2]; };

uniform arrayarraymat4x2f3222__block_6Compute { mat4x2 _group_0_binding_7_cs[2][2]; };


void test_msl_packed_vec3_as_arg(vec3 arg) {
    return;
}

void test_msl_packed_vec3_() {
    int idx = 0;
    _group_0_binding_1_cs.v3_ = vec3(1.0);
    idx = 1;
    _group_0_binding_1_cs.v3_.x = 1.0;
    _group_0_binding_1_cs.v3_.x = 2.0;
    int _e18 = idx;
    _group_0_binding_1_cs.v3_[_e18] = 3.0;
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
    float Foo_1 = 0.0;
    bool at = false;
    test_msl_packed_vec3_();
    mat4x2 _e5 = _group_0_binding_7_cs[0][0];
    vec4 _e13 = _group_0_binding_6_cs[0][0][0];
    wg[7] = (_e5 * _e13).x;
    mat3x2 _e20 = _group_0_binding_5_cs;
    vec3 _e22 = _group_0_binding_4_cs;
    wg[6] = (_e20 * _e22).x;
    float _e32 = _group_0_binding_2_cs[1].y;
    wg[5] = _e32;
    float _e40 = _group_0_binding_3_cs[0].w;
    wg[4] = _e40;
    float _e46 = _group_0_binding_1_cs.v1_;
    wg[3] = _e46;
    float _e53 = _group_0_binding_1_cs.v3_.x;
    wg[2] = _e53;
    _group_0_binding_1_cs.v1_ = 4.0;
    wg[1] = float(uint(_group_0_binding_2_cs.length()));
    at_1 = 2u;
    Foo_1 = 1.0;
    at = true;
    return;
}

