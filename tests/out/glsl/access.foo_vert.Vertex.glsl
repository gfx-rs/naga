#version 310 es

precision highp float;
precision highp int;

struct GlobalConst {
    uint a;
    uvec3 b;
    int c;
};
struct AlignedWrapper {
    int value;
};
struct Baz {
    mat3x2 m;
};
struct MatCx2InArray {
    mat4x2 am[2];
};
layout(std430) buffer Bar_block_0Vertex {
    mat4x3 _matrix;
    mat2x2 matrix_array[2];
    int atom;
    uvec2 arr[2];
    AlignedWrapper data[];
} _group_0_binding_0_vs;

uniform Baz_block_1Vertex { Baz _group_0_binding_1_vs; };

layout(std430) buffer type_11_block_2Vertex { ivec2 _group_0_binding_2_vs; };

uniform MatCx2InArray_block_3Vertex { MatCx2InArray _group_0_binding_3_vs; };


void test_matrix_within_struct_accesses() {
    int idx = 1;
    Baz t = Baz(mat3x2(0.0));
    int _e6 = idx;
    idx = (_e6 - 1);
    mat3x2 unnamed = _group_0_binding_1_vs.m;
    vec2 unnamed_1 = _group_0_binding_1_vs.m[0];
    int _e16 = idx;
    vec2 unnamed_2 = _group_0_binding_1_vs.m[_e16];
    float unnamed_3 = _group_0_binding_1_vs.m[0][1];
    int _e28 = idx;
    float unnamed_4 = _group_0_binding_1_vs.m[0][_e28];
    int _e32 = idx;
    float unnamed_5 = _group_0_binding_1_vs.m[_e32][1];
    int _e38 = idx;
    int _e40 = idx;
    float unnamed_6 = _group_0_binding_1_vs.m[_e38][_e40];
    t = Baz(mat3x2(vec2(1.0), vec2(2.0), vec2(3.0)));
    int _e52 = idx;
    idx = (_e52 + 1);
    t.m = mat3x2(vec2(6.0), vec2(5.0), vec2(4.0));
    t.m[0] = vec2(9.0);
    int _e69 = idx;
    t.m[_e69] = vec2(90.0);
    t.m[0][1] = 10.0;
    int _e82 = idx;
    t.m[0][_e82] = 20.0;
    int _e86 = idx;
    t.m[_e86][1] = 30.0;
    int _e92 = idx;
    int _e94 = idx;
    t.m[_e92][_e94] = 40.0;
    return;
}

void test_matrix_within_array_within_struct_accesses() {
    int idx_1 = 1;
    MatCx2InArray t_1 = MatCx2InArray(mat4x2[2](mat4x2(0.0), mat4x2(0.0)));
    int _e7 = idx_1;
    idx_1 = (_e7 - 1);
    mat4x2 unnamed_7[2] = _group_0_binding_3_vs.am;
    mat4x2 unnamed_8 = _group_0_binding_3_vs.am[0];
    vec2 unnamed_9 = _group_0_binding_3_vs.am[0][0];
    int _e25 = idx_1;
    vec2 unnamed_10 = _group_0_binding_3_vs.am[0][_e25];
    float unnamed_11 = _group_0_binding_3_vs.am[0][0][1];
    int _e41 = idx_1;
    float unnamed_12 = _group_0_binding_3_vs.am[0][0][_e41];
    int _e47 = idx_1;
    float unnamed_13 = _group_0_binding_3_vs.am[0][_e47][1];
    int _e55 = idx_1;
    int _e57 = idx_1;
    float unnamed_14 = _group_0_binding_3_vs.am[0][_e55][_e57];
    t_1 = MatCx2InArray(mat4x2[2](mat4x2(vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0)), mat4x2(vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0))));
    int _e63 = idx_1;
    idx_1 = (_e63 + 1);
    t_1.am = mat4x2[2](mat4x2(vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0)), mat4x2(vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0)));
    t_1.am[0] = mat4x2(vec2(8.0), vec2(7.0), vec2(6.0), vec2(5.0));
    t_1.am[0][0] = vec2(9.0);
    int _e90 = idx_1;
    t_1.am[0][_e90] = vec2(90.0);
    t_1.am[0][0][1] = 10.0;
    int _e107 = idx_1;
    t_1.am[0][0][_e107] = 20.0;
    int _e113 = idx_1;
    t_1.am[0][_e113][1] = 30.0;
    int _e121 = idx_1;
    int _e123 = idx_1;
    t_1.am[0][_e121][_e123] = 40.0;
    return;
}

float read_from_private(inout float foo_1) {
    float _e6 = foo_1;
    return _e6;
}

float test_arr_as_arg(float a[5][10]) {
    return a[4][9];
}

void assign_through_ptr_fn(inout uint p) {
    p = 42u;
    return;
}

void main() {
    uint vi = uint(gl_VertexID);
    float foo = 0.0;
    int c[5] = int[5](0, 0, 0, 0, 0);
    float baz_1 = foo;
    foo = 1.0;
    test_matrix_within_struct_accesses();
    test_matrix_within_array_within_struct_accesses();
    mat4x3 _matrix = _group_0_binding_0_vs._matrix;
    uvec2 arr[2] = _group_0_binding_0_vs.arr;
    float b = _group_0_binding_0_vs._matrix[3][0];
    int a_1 = _group_0_binding_0_vs.data[(uint(_group_0_binding_0_vs.data.length()) - 2u)].value;
    ivec2 c_1 = _group_0_binding_2_vs;
    float _e32 = read_from_private(foo);
    c = int[5](a_1, int(b), 3, 4, 5);
    c[(vi + 1u)] = 42;
    int value = c[vi];
    float _e46 = test_arr_as_arg(float[5][10](float[10](0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), float[10](0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), float[10](0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), float[10](0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), float[10](0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)));
    gl_Position = vec4((_matrix * vec4(ivec4(value))), 2.0);
    gl_Position.yz = vec2(-gl_Position.y, gl_Position.z * 2.0 - gl_Position.w);
    return;
}

