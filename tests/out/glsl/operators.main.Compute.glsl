#version 310 es

precision highp float;
precision highp int;

layout(local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

struct Foo {
    vec4 a;
    int b;
};

vec4 builtins() {
    int s1_ = (true ? 1 : 0);
    vec4 s2_ = (true ? vec4(1.0, 1.0, 1.0, 1.0) : vec4(0.0, 0.0, 0.0, 0.0));
    vec4 s3_ = mix(vec4(1.0, 1.0, 1.0, 1.0), vec4(0.0, 0.0, 0.0, 0.0), bvec4(false, false, false, false));
    vec4 m1_ = mix(vec4(0.0, 0.0, 0.0, 0.0), vec4(1.0, 1.0, 1.0, 1.0), vec4(0.5, 0.5, 0.5, 0.5));
    vec4 m2_ = mix(vec4(0.0, 0.0, 0.0, 0.0), vec4(1.0, 1.0, 1.0, 1.0), 0.1);
    float b1_ = intBitsToFloat(ivec4(1, 1, 1, 1).x);
    vec4 b2_ = intBitsToFloat(ivec4(1, 1, 1, 1));
    ivec4 v_i32_zero = ivec4(vec4(0.0, 0.0, 0.0, 0.0));
    return (((((vec4((ivec4(s1_) + v_i32_zero)) + s2_) + m1_) + m2_) + vec4(b1_)) + b2_);
}

vec4 splat() {
    vec2 a_2 = (((vec2(1.0) + vec2(2.0)) - vec2(3.0)) / vec2(4.0));
    ivec4 b = (ivec4(5) % ivec4(2));
    return (a_2.xyxy + vec4(b));
}

vec2 splat_assignment() {
    vec2 a = vec2(0.0);
    vec2 a_3 = vec2(2.0);
    a = a_3;
    vec2 _e4 = a;
    a = (_e4 + vec2(1.0));
    vec2 _e9 = a;
    a = (_e9 - vec2(3.0));
    vec2 _e14 = a;
    a = (_e14 / vec2(4.0));
    vec2 _e19 = a;
    return _e19;
}

vec3 bool_cast(vec3 x) {
    bvec3 y = bvec3(x);
    return vec3(y);
}

float constructors() {
    Foo foo = Foo(vec4(0.0), 0);
    foo = Foo(vec4(1.0), 1);
    mat2x2 mat2comp = mat2x2(vec2(1.0, 0.0), vec2(0.0, 1.0));
    mat4x4 mat4comp = mat4x4(vec4(1.0, 0.0, 0.0, 0.0), vec4(0.0, 1.0, 0.0, 0.0), vec4(0.0, 0.0, 1.0, 0.0), vec4(0.0, 0.0, 0.0, 1.0));
    uvec2 unnamed = uvec2(0u);
    mat2x2 unnamed_1 = mat2x2(vec2(0.0), vec2(0.0));
    int unnamed_2[4] = int[4](0, 1, 2, 3);
    bool unnamed_3 = bool(false);
    int unnamed_4 = int(0);
    uint unnamed_5 = uint(0u);
    float unnamed_6 = float(0.0);
    uvec2 unnamed_7 = uvec2(uvec2(0u, 0u));
    mat2x3 unnamed_8 = mat2x3(mat2x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)));
    uvec2 unnamed_9 = uvec2(uvec2(0u, 0u));
    mat2x3 unnamed_10 = mat2x3(mat2x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)));
    float _e72 = foo.a.x;
    return _e72;
}

void logical() {
    bool unnamed_11 = (!true);
    bvec2 unnamed_12 = not(bvec2(true));
    bool unnamed_13 = (true || false);
    bool unnamed_14 = (true && false);
    bool unnamed_15 = (true || false);
    bvec3 unnamed_16 = bvec3(bvec3(true).x || bvec3(false).x, bvec3(true).y || bvec3(false).y, bvec3(true).z || bvec3(false).z);
    bool unnamed_17 = (true && false);
    bvec4 unnamed_18 = bvec4(bvec4(true).x && bvec4(false).x, bvec4(true).y && bvec4(false).y, bvec4(true).z && bvec4(false).z, bvec4(true).w && bvec4(false).w);
}

void arithmetic() {
    float unnamed_19 = (-1.0);
    ivec2 unnamed_20 = (-ivec2(1));
    vec2 unnamed_21 = (-vec2(1.0));
    int unnamed_22 = (2 + 1);
    uint unnamed_23 = (2u + 1u);
    float unnamed_24 = (2.0 + 1.0);
    ivec2 unnamed_25 = (ivec2(2) + ivec2(1));
    uvec3 unnamed_26 = (uvec3(2u) + uvec3(1u));
    vec4 unnamed_27 = (vec4(2.0) + vec4(1.0));
    int unnamed_28 = (2 - 1);
    uint unnamed_29 = (2u - 1u);
    float unnamed_30 = (2.0 - 1.0);
    ivec2 unnamed_31 = (ivec2(2) - ivec2(1));
    uvec3 unnamed_32 = (uvec3(2u) - uvec3(1u));
    vec4 unnamed_33 = (vec4(2.0) - vec4(1.0));
    int unnamed_34 = (2 * 1);
    uint unnamed_35 = (2u * 1u);
    float unnamed_36 = (2.0 * 1.0);
    ivec2 unnamed_37 = (ivec2(2) * ivec2(1));
    uvec3 unnamed_38 = (uvec3(2u) * uvec3(1u));
    vec4 unnamed_39 = (vec4(2.0) * vec4(1.0));
    int unnamed_40 = (2 / 1);
    uint unnamed_41 = (2u / 1u);
    float unnamed_42 = (2.0 / 1.0);
    ivec2 unnamed_43 = (ivec2(2) / ivec2(1));
    uvec3 unnamed_44 = (uvec3(2u) / uvec3(1u));
    vec4 unnamed_45 = (vec4(2.0) / vec4(1.0));
    int unnamed_46 = (2 % 1);
    uint unnamed_47 = (2u % 1u);
    float unnamed_48 = (2.0 - 1.0 * trunc(2.0 / 1.0));
    ivec2 unnamed_49 = (ivec2(2) % ivec2(1));
    uvec3 unnamed_50 = (uvec3(2u) % uvec3(1u));
    vec4 unnamed_51 = (vec4(2.0) - vec4(1.0) * trunc(vec4(2.0) / vec4(1.0)));
    ivec2 unnamed_52 = (ivec2(2) + ivec2(1));
    ivec2 unnamed_53 = (ivec2(2) + ivec2(1));
    uvec2 unnamed_54 = (uvec2(2u) + uvec2(1u));
    uvec2 unnamed_55 = (uvec2(2u) + uvec2(1u));
    vec2 unnamed_56 = (vec2(2.0) + vec2(1.0));
    vec2 unnamed_57 = (vec2(2.0) + vec2(1.0));
    ivec2 unnamed_58 = (ivec2(2) - ivec2(1));
    ivec2 unnamed_59 = (ivec2(2) - ivec2(1));
    uvec2 unnamed_60 = (uvec2(2u) - uvec2(1u));
    uvec2 unnamed_61 = (uvec2(2u) - uvec2(1u));
    vec2 unnamed_62 = (vec2(2.0) - vec2(1.0));
    vec2 unnamed_63 = (vec2(2.0) - vec2(1.0));
    ivec2 unnamed_64 = (ivec2(2) * 1);
    ivec2 unnamed_65 = (2 * ivec2(1));
    uvec2 unnamed_66 = (uvec2(2u) * 1u);
    uvec2 unnamed_67 = (2u * uvec2(1u));
    vec2 unnamed_68 = (vec2(2.0) * 1.0);
    vec2 unnamed_69 = (2.0 * vec2(1.0));
    ivec2 unnamed_70 = (ivec2(2) / ivec2(1));
    ivec2 unnamed_71 = (ivec2(2) / ivec2(1));
    uvec2 unnamed_72 = (uvec2(2u) / uvec2(1u));
    uvec2 unnamed_73 = (uvec2(2u) / uvec2(1u));
    vec2 unnamed_74 = (vec2(2.0) / vec2(1.0));
    vec2 unnamed_75 = (vec2(2.0) / vec2(1.0));
    ivec2 unnamed_76 = (ivec2(2) % ivec2(1));
    ivec2 unnamed_77 = (ivec2(2) % ivec2(1));
    uvec2 unnamed_78 = (uvec2(2u) % uvec2(1u));
    uvec2 unnamed_79 = (uvec2(2u) % uvec2(1u));
    vec2 unnamed_80 = (vec2(2.0) - vec2(1.0) * trunc(vec2(2.0) / vec2(1.0)));
    vec2 unnamed_81 = (vec2(2.0) - vec2(1.0) * trunc(vec2(2.0) / vec2(1.0)));
    mat3x3 unnamed_82 = (mat3x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)) + mat3x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)));
    mat3x3 unnamed_83 = (mat3x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)) - mat3x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)));
    mat3x3 unnamed_84 = (mat3x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)) * 1.0);
    mat3x3 unnamed_85 = (2.0 * mat3x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)));
    vec3 unnamed_86 = (mat4x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)) * vec4(1.0));
    vec4 unnamed_87 = (vec3(2.0) * mat4x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)));
    mat3x3 unnamed_88 = (mat4x3(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 0.0)) * mat3x4(vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0), vec4(0.0, 0.0, 0.0, 0.0)));
}

void bit() {
    int unnamed_89 = (~1);
    uint unnamed_90 = (~1u);
    ivec2 unnamed_91 = (~ivec2(1));
    uvec3 unnamed_92 = (~uvec3(1u));
    int unnamed_93 = (2 | 1);
    uint unnamed_94 = (2u | 1u);
    ivec2 unnamed_95 = (ivec2(2) | ivec2(1));
    uvec3 unnamed_96 = (uvec3(2u) | uvec3(1u));
    int unnamed_97 = (2 & 1);
    uint unnamed_98 = (2u & 1u);
    ivec2 unnamed_99 = (ivec2(2) & ivec2(1));
    uvec3 unnamed_100 = (uvec3(2u) & uvec3(1u));
    int unnamed_101 = (2 ^ 1);
    uint unnamed_102 = (2u ^ 1u);
    ivec2 unnamed_103 = (ivec2(2) ^ ivec2(1));
    uvec3 unnamed_104 = (uvec3(2u) ^ uvec3(1u));
    int unnamed_105 = (2 << 1u);
    uint unnamed_106 = (2u << 1u);
    ivec2 unnamed_107 = (ivec2(2) << uvec2(1u));
    uvec3 unnamed_108 = (uvec3(2u) << uvec3(1u));
    int unnamed_109 = (2 >> 1u);
    uint unnamed_110 = (2u >> 1u);
    ivec2 unnamed_111 = (ivec2(2) >> uvec2(1u));
    uvec3 unnamed_112 = (uvec3(2u) >> uvec3(1u));
}

void comparison() {
    bool unnamed_113 = (2 == 1);
    bool unnamed_114 = (2u == 1u);
    bool unnamed_115 = (2.0 == 1.0);
    bvec2 unnamed_116 = equal(ivec2(2), ivec2(1));
    bvec3 unnamed_117 = equal(uvec3(2u), uvec3(1u));
    bvec4 unnamed_118 = equal(vec4(2.0), vec4(1.0));
    bool unnamed_119 = (2 != 1);
    bool unnamed_120 = (2u != 1u);
    bool unnamed_121 = (2.0 != 1.0);
    bvec2 unnamed_122 = notEqual(ivec2(2), ivec2(1));
    bvec3 unnamed_123 = notEqual(uvec3(2u), uvec3(1u));
    bvec4 unnamed_124 = notEqual(vec4(2.0), vec4(1.0));
    bool unnamed_125 = (2 < 1);
    bool unnamed_126 = (2u < 1u);
    bool unnamed_127 = (2.0 < 1.0);
    bvec2 unnamed_128 = lessThan(ivec2(2), ivec2(1));
    bvec3 unnamed_129 = lessThan(uvec3(2u), uvec3(1u));
    bvec4 unnamed_130 = lessThan(vec4(2.0), vec4(1.0));
    bool unnamed_131 = (2 <= 1);
    bool unnamed_132 = (2u <= 1u);
    bool unnamed_133 = (2.0 <= 1.0);
    bvec2 unnamed_134 = lessThanEqual(ivec2(2), ivec2(1));
    bvec3 unnamed_135 = lessThanEqual(uvec3(2u), uvec3(1u));
    bvec4 unnamed_136 = lessThanEqual(vec4(2.0), vec4(1.0));
    bool unnamed_137 = (2 > 1);
    bool unnamed_138 = (2u > 1u);
    bool unnamed_139 = (2.0 > 1.0);
    bvec2 unnamed_140 = greaterThan(ivec2(2), ivec2(1));
    bvec3 unnamed_141 = greaterThan(uvec3(2u), uvec3(1u));
    bvec4 unnamed_142 = greaterThan(vec4(2.0), vec4(1.0));
    bool unnamed_143 = (2 >= 1);
    bool unnamed_144 = (2u >= 1u);
    bool unnamed_145 = (2.0 >= 1.0);
    bvec2 unnamed_146 = greaterThanEqual(ivec2(2), ivec2(1));
    bvec3 unnamed_147 = greaterThanEqual(uvec3(2u), uvec3(1u));
    bvec4 unnamed_148 = greaterThanEqual(vec4(2.0), vec4(1.0));
}

void assignment() {
    int a_1 = 0;
    ivec3 vec0_ = ivec3(0);
    a_1 = 1;
    int _e3 = a_1;
    a_1 = (_e3 + 1);
    int _e7 = a_1;
    a_1 = (_e7 - 1);
    int _e11 = a_1;
    int _e13 = a_1;
    a_1 = (_e13 * _e11);
    int _e16 = a_1;
    int _e18 = a_1;
    a_1 = (_e18 / _e16);
    int _e21 = a_1;
    a_1 = (_e21 % 1);
    int _e25 = a_1;
    a_1 = (_e25 & 0);
    int _e29 = a_1;
    a_1 = (_e29 | 0);
    int _e33 = a_1;
    a_1 = (_e33 ^ 0);
    int _e38 = a_1;
    a_1 = (_e38 << 2u);
    int _e42 = a_1;
    a_1 = (_e42 >> 1u);
    int _e45 = a_1;
    a_1 = (_e45 + 1);
    int _e49 = a_1;
    a_1 = (_e49 - 1);
    vec0_ = ivec3(0, 0, 0);
    int _e57 = vec0_.y;
    vec0_.y = (_e57 + 1);
    int _e63 = vec0_.y;
    vec0_.y = (_e63 - 1);
    return;
}

void main() {
    vec4 _e0 = builtins();
    vec4 _e1 = splat();
    vec3 _e2 = bool_cast(vec4(1.0, 1.0, 1.0, 1.0).xyz);
    float _e5 = constructors();
    logical();
    arithmetic();
    bit();
    comparison();
    assignment();
    return;
}

