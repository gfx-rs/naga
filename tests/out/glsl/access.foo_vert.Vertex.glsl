#version 310 es

precision highp float;
precision highp int;

struct AlignedWrapper {
    int value;
};
struct Baz {
    mat3x2 m;
};
layout(std430) buffer Bar_block_0Vertex {
    mat4x3 matrix;
    mat2x2 matrix_array[2];
    int atom;
    uvec2 arr[2];
    AlignedWrapper data[];
} _group_0_binding_0_vs;

uniform Baz_block_1Vertex { Baz _group_0_binding_1_vs; };


void test_matrix_within_struct_accesses() {
    int idx = 9;
    Baz t = Baz(mat3x2(0.0));
    int _e4 = idx;
    idx = (_e4 - 1);
    mat3x2 m = _group_0_binding_1_vs.m;
    vec2 vs = _group_0_binding_1_vs.m[0];
    int _e14 = idx;
    vec2 vd = _group_0_binding_1_vs.m[_e14];
    float sss = _group_0_binding_1_vs.m[0][1];
    int _e26 = idx;
    float ssd = _group_0_binding_1_vs.m[0][_e26];
    int _e30 = idx;
    float sds = _group_0_binding_1_vs.m[_e30][1];
    int _e36 = idx;
    int _e38 = idx;
    float sdd = _group_0_binding_1_vs.m[_e36][_e38];
    t = Baz(mat3x2(vec2(1.0), vec2(2.0), vec2(3.0)));
    int _e50 = idx;
    idx = (_e50 + 1);
    t.m = mat3x2(vec2(6.0), vec2(5.0), vec2(4.0));
    t.m[0] = vec2(9.0);
    int _e67 = idx;
    t.m[_e67] = vec2(90.0);
    t.m[0][1] = 10.0;
    int _e80 = idx;
    t.m[0][_e80] = 20.0;
    int _e84 = idx;
    t.m[_e84][1] = 30.0;
    int _e90 = idx;
    int _e92 = idx;
    t.m[_e90][_e92] = 40.0;
    return;
}

float read_from_private(inout float foo_1) {
    float _e3 = foo_1;
    return _e3;
}

void main() {
    uint vi = uint(gl_VertexID);
    float foo = 0.0;
    int c[5] = int[5](0, 0, 0, 0, 0);
    float baz_1 = foo;
    foo = 1.0;
    test_matrix_within_struct_accesses();
    mat4x3 matrix = _group_0_binding_0_vs.matrix;
    uvec2 arr[2] = _group_0_binding_0_vs.arr;
    float b = _group_0_binding_0_vs.matrix[3][0];
    int a = _group_0_binding_0_vs.data[(uint(_group_0_binding_0_vs.data.length()) - 2u)].value;
    float _e28 = read_from_private(foo);
    c = int[5](a, int(b), 3, 4, 5);
    c[(vi + 1u)] = 42;
    int value = c[vi];
    gl_Position = vec4((matrix * vec4(ivec4(value))), 2.0);
    gl_Position.yz = vec2(-gl_Position.y, gl_Position.z * 2.0 - gl_Position.w);
    return;
}

