#version 310 es

precision highp float;
precision highp int;

layout(local_size_x = 2, local_size_y = 3, local_size_z = 1) in;

const uint TWO = 2u;
const int THREE = 3;
const int FOUR = 4;
const int FOUR_ALIAS = 4;
const int TEST_CONSTANT_ADDITION = 8;
const int TEST_CONSTANT_ALIAS_ADDITION = 8;
const float PI = 3.141;
const float phi_sun = 6.282;
const vec4 DIV = vec4(0.44444445, 0.0, 0.0, 0.0);
const int TEXTURE_KIND_REGULAR = 0;
const int TEXTURE_KIND_WARP = 1;
const int TEXTURE_KIND_SKY = 2;

layout(std430) buffer type_block_0Compute { ivec4 _group_0_binding_0_cs; };

layout(std430) buffer type_1_block_1Compute { int _group_0_binding_1_cs; };


void swizzle_of_compose() {
    _group_0_binding_0_cs = ivec4(4, 3, 2, 1);
    return;
}

void index_of_compose() {
    int _e8 = _group_0_binding_1_cs;
    _group_0_binding_1_cs = (_e8 + 2);
    return;
}

void compose_three_deep() {
    int _e8 = _group_0_binding_1_cs;
    _group_0_binding_1_cs = (_e8 + 6);
    return;
}

void non_constant_initializers() {
    int w = 30;
    int x = 0;
    int y = 0;
    int z = 70;
    int _e2 = w;
    x = _e2;
    int _e4 = x;
    y = _e4;
    int _e9 = w;
    int _e10 = x;
    int _e11 = y;
    int _e12 = z;
    ivec4 _e14 = _group_0_binding_0_cs;
    _group_0_binding_0_cs = (_e14 + ivec4(_e9, _e10, _e11, _e12));
    return;
}

void splat_of_constant() {
    _group_0_binding_0_cs = ivec4(-4, -4, -4, -4);
    return;
}

void compose_of_constant() {
    _group_0_binding_0_cs = ivec4(-4, -4, -4, -4);
    return;
}

uint map_texture_kind(int texture_kind) {
    switch(texture_kind) {
        case 0: {
            return 10u;
        }
        case 1: {
            return 20u;
        }
        case 2: {
            return 30u;
        }
        default: {
            return 0u;
        }
    }
}

void main() {
    swizzle_of_compose();
    index_of_compose();
    compose_three_deep();
    non_constant_initializers();
    splat_of_constant();
    compose_of_constant();
    return;
}

