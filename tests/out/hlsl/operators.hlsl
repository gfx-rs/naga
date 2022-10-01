static const float4 v_f32_one = float4(1.0, 1.0, 1.0, 1.0);
static const float4 v_f32_zero = float4(0.0, 0.0, 0.0, 0.0);
static const float4 v_f32_half = float4(0.5, 0.5, 0.5, 0.5);
static const int4 v_i32_one = int4(1, 1, 1, 1);

struct Foo {
    float4 a;
    int b;
    int _end_pad_0;
    int _end_pad_1;
    int _end_pad_2;
};

Foo ConstructFoo(float4 arg0, int arg1) {
    Foo ret = (Foo)0;
    ret.a = arg0;
    ret.b = arg1;
    return ret;
}

typedef Foo ret_Constructarray3_Foo_[3];
ret_Constructarray3_Foo_ Constructarray3_Foo_(Foo arg0, Foo arg1, Foo arg2) {
    Foo ret[3] = { arg0, arg1, arg2 };
    return ret;
}

float4 builtins()
{
    int s1_ = (true ? 1 : 0);
    float4 s2_ = (true ? float4(1.0, 1.0, 1.0, 1.0) : float4(0.0, 0.0, 0.0, 0.0));
    float4 s3_ = (bool4(false, false, false, false) ? float4(0.0, 0.0, 0.0, 0.0) : float4(1.0, 1.0, 1.0, 1.0));
    float4 m1_ = lerp(float4(0.0, 0.0, 0.0, 0.0), float4(1.0, 1.0, 1.0, 1.0), float4(0.5, 0.5, 0.5, 0.5));
    float4 m2_ = lerp(float4(0.0, 0.0, 0.0, 0.0), float4(1.0, 1.0, 1.0, 1.0), 0.1);
    float b1_ = float(int4(1, 1, 1, 1).x);
    float4 b2_ = float4(int4(1, 1, 1, 1));
    int4 v_i32_zero = int4(float4(0.0, 0.0, 0.0, 0.0));
    return (((((float4(((s1_).xxxx + v_i32_zero)) + s2_) + m1_) + m2_) + (b1_).xxxx) + b2_);
}

float4 splat()
{
    float2 a_2 = ((((1.0).xx + (2.0).xx) - (3.0).xx) / (4.0).xx);
    int4 b = ((5).xxxx % (2).xxxx);
    return (a_2.xyxy + float4(b));
}

float2 splat_assignment()
{
    float2 a = (float2)0;

    float2 a_3 = (2.0).xx;
    a = a_3;
    float2 _expr4 = a;
    a = (_expr4 + (1.0).xx);
    float2 _expr9 = a;
    a = (_expr9 - (3.0).xx);
    float2 _expr14 = a;
    a = (_expr14 / (4.0).xx);
    float2 _expr19 = a;
    return _expr19;
}

float3 bool_cast(float3 x)
{
    bool3 y = bool3(x);
    return float3(y);
}

typedef int ret_Constructarray4_int_[4];
ret_Constructarray4_int_ Constructarray4_int_(int arg0, int arg1, int arg2, int arg3) {
    int ret[4] = { arg0, arg1, arg2, arg3 };
    return ret;
}

float constructors()
{
    Foo foo = (Foo)0;

    foo = ConstructFoo((1.0).xxxx, 1);
    float2x2 mat2comp = float2x2(float2(1.0, 0.0), float2(0.0, 1.0));
    float4x4 mat4comp = float4x4(float4(1.0, 0.0, 0.0, 0.0), float4(0.0, 1.0, 0.0, 0.0), float4(0.0, 0.0, 1.0, 0.0), float4(0.0, 0.0, 0.0, 1.0));
    uint2 unnamed = (0u).xx;
    float2x2 unnamed_1 = float2x2((0.0).xx, (0.0).xx);
    int unnamed_2[4] = Constructarray4_int_(0, 1, 2, 3);
    bool unnamed_3 = bool(false);
    int unnamed_4 = int(0);
    uint unnamed_5 = uint(0u);
    float unnamed_6 = float(0.0);
    uint2 unnamed_7 = uint2(uint2(0u, 0u));
    float2x3 unnamed_8 = float2x3(float2x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)));
    uint2 unnamed_9 = uint2(uint2(0u, 0u));
    float2x3 unnamed_10 = float2x3(float2x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)));
    float _expr72 = foo.a.x;
    return _expr72;
}

void logical()
{
    bool unnamed_11 = !true;
    bool2 unnamed_12 = !(true).xx;
    bool unnamed_13 = (true | false);
    bool unnamed_14 = (true & false);
    bool unnamed_15 = (true | false);
    bool3 unnamed_16 = ((true).xxx | (false).xxx);
    bool unnamed_17 = (true & false);
    bool4 unnamed_18 = ((true).xxxx & (false).xxxx);
}

void arithmetic()
{
    float unnamed_19 = -1.0;
    int2 unnamed_20 = -(1).xx;
    float2 unnamed_21 = -(1.0).xx;
    int unnamed_22 = (2 + 1);
    uint unnamed_23 = (2u + 1u);
    float unnamed_24 = (2.0 + 1.0);
    int2 unnamed_25 = ((2).xx + (1).xx);
    uint3 unnamed_26 = ((2u).xxx + (1u).xxx);
    float4 unnamed_27 = ((2.0).xxxx + (1.0).xxxx);
    int unnamed_28 = (2 - 1);
    uint unnamed_29 = (2u - 1u);
    float unnamed_30 = (2.0 - 1.0);
    int2 unnamed_31 = ((2).xx - (1).xx);
    uint3 unnamed_32 = ((2u).xxx - (1u).xxx);
    float4 unnamed_33 = ((2.0).xxxx - (1.0).xxxx);
    int unnamed_34 = (2 * 1);
    uint unnamed_35 = (2u * 1u);
    float unnamed_36 = (2.0 * 1.0);
    int2 unnamed_37 = ((2).xx * (1).xx);
    uint3 unnamed_38 = ((2u).xxx * (1u).xxx);
    float4 unnamed_39 = ((2.0).xxxx * (1.0).xxxx);
    int unnamed_40 = (2 / 1);
    uint unnamed_41 = (2u / 1u);
    float unnamed_42 = (2.0 / 1.0);
    int2 unnamed_43 = ((2).xx / (1).xx);
    uint3 unnamed_44 = ((2u).xxx / (1u).xxx);
    float4 unnamed_45 = ((2.0).xxxx / (1.0).xxxx);
    int unnamed_46 = (2 % 1);
    uint unnamed_47 = (2u % 1u);
    float unnamed_48 = fmod(2.0, 1.0);
    int2 unnamed_49 = ((2).xx % (1).xx);
    uint3 unnamed_50 = ((2u).xxx % (1u).xxx);
    float4 unnamed_51 = fmod((2.0).xxxx, (1.0).xxxx);
    int2 unnamed_52 = ((2).xx + (1).xx);
    int2 unnamed_53 = ((2).xx + (1).xx);
    uint2 unnamed_54 = ((2u).xx + (1u).xx);
    uint2 unnamed_55 = ((2u).xx + (1u).xx);
    float2 unnamed_56 = ((2.0).xx + (1.0).xx);
    float2 unnamed_57 = ((2.0).xx + (1.0).xx);
    int2 unnamed_58 = ((2).xx - (1).xx);
    int2 unnamed_59 = ((2).xx - (1).xx);
    uint2 unnamed_60 = ((2u).xx - (1u).xx);
    uint2 unnamed_61 = ((2u).xx - (1u).xx);
    float2 unnamed_62 = ((2.0).xx - (1.0).xx);
    float2 unnamed_63 = ((2.0).xx - (1.0).xx);
    int2 unnamed_64 = ((2).xx * 1);
    int2 unnamed_65 = (2 * (1).xx);
    uint2 unnamed_66 = ((2u).xx * 1u);
    uint2 unnamed_67 = (2u * (1u).xx);
    float2 unnamed_68 = ((2.0).xx * 1.0);
    float2 unnamed_69 = (2.0 * (1.0).xx);
    int2 unnamed_70 = ((2).xx / (1).xx);
    int2 unnamed_71 = ((2).xx / (1).xx);
    uint2 unnamed_72 = ((2u).xx / (1u).xx);
    uint2 unnamed_73 = ((2u).xx / (1u).xx);
    float2 unnamed_74 = ((2.0).xx / (1.0).xx);
    float2 unnamed_75 = ((2.0).xx / (1.0).xx);
    int2 unnamed_76 = ((2).xx % (1).xx);
    int2 unnamed_77 = ((2).xx % (1).xx);
    uint2 unnamed_78 = ((2u).xx % (1u).xx);
    uint2 unnamed_79 = ((2u).xx % (1u).xx);
    float2 unnamed_80 = fmod((2.0).xx, (1.0).xx);
    float2 unnamed_81 = fmod((2.0).xx, (1.0).xx);
    float3x3 unnamed_82 = (float3x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)) + float3x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)));
    float3x3 unnamed_83 = (float3x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)) - float3x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)));
    float3x3 unnamed_84 = mul(1.0, float3x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)));
    float3x3 unnamed_85 = mul(float3x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)), 2.0);
    float3 unnamed_86 = mul((1.0).xxxx, float4x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)));
    float4 unnamed_87 = mul(float4x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)), (2.0).xxx);
    float3x3 unnamed_88 = mul(float3x4(float4(0.0, 0.0, 0.0, 0.0), float4(0.0, 0.0, 0.0, 0.0), float4(0.0, 0.0, 0.0, 0.0)), float4x3(float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0)));
}

void bit()
{
    int unnamed_89 = ~1;
    uint unnamed_90 = ~1u;
    int2 unnamed_91 = ~(1).xx;
    uint3 unnamed_92 = ~(1u).xxx;
    int unnamed_93 = (2 | 1);
    uint unnamed_94 = (2u | 1u);
    int2 unnamed_95 = ((2).xx | (1).xx);
    uint3 unnamed_96 = ((2u).xxx | (1u).xxx);
    int unnamed_97 = (2 & 1);
    uint unnamed_98 = (2u & 1u);
    int2 unnamed_99 = ((2).xx & (1).xx);
    uint3 unnamed_100 = ((2u).xxx & (1u).xxx);
    int unnamed_101 = (2 ^ 1);
    uint unnamed_102 = (2u ^ 1u);
    int2 unnamed_103 = ((2).xx ^ (1).xx);
    uint3 unnamed_104 = ((2u).xxx ^ (1u).xxx);
    int unnamed_105 = (2 << 1u);
    uint unnamed_106 = (2u << 1u);
    int2 unnamed_107 = ((2).xx << (1u).xx);
    uint3 unnamed_108 = ((2u).xxx << (1u).xxx);
    int unnamed_109 = (2 >> 1u);
    uint unnamed_110 = (2u >> 1u);
    int2 unnamed_111 = ((2).xx >> (1u).xx);
    uint3 unnamed_112 = ((2u).xxx >> (1u).xxx);
}

void comparison()
{
    bool unnamed_113 = (2 == 1);
    bool unnamed_114 = (2u == 1u);
    bool unnamed_115 = (2.0 == 1.0);
    bool2 unnamed_116 = ((2).xx == (1).xx);
    bool3 unnamed_117 = ((2u).xxx == (1u).xxx);
    bool4 unnamed_118 = ((2.0).xxxx == (1.0).xxxx);
    bool unnamed_119 = (2 != 1);
    bool unnamed_120 = (2u != 1u);
    bool unnamed_121 = (2.0 != 1.0);
    bool2 unnamed_122 = ((2).xx != (1).xx);
    bool3 unnamed_123 = ((2u).xxx != (1u).xxx);
    bool4 unnamed_124 = ((2.0).xxxx != (1.0).xxxx);
    bool unnamed_125 = (2 < 1);
    bool unnamed_126 = (2u < 1u);
    bool unnamed_127 = (2.0 < 1.0);
    bool2 unnamed_128 = ((2).xx < (1).xx);
    bool3 unnamed_129 = ((2u).xxx < (1u).xxx);
    bool4 unnamed_130 = ((2.0).xxxx < (1.0).xxxx);
    bool unnamed_131 = (2 <= 1);
    bool unnamed_132 = (2u <= 1u);
    bool unnamed_133 = (2.0 <= 1.0);
    bool2 unnamed_134 = ((2).xx <= (1).xx);
    bool3 unnamed_135 = ((2u).xxx <= (1u).xxx);
    bool4 unnamed_136 = ((2.0).xxxx <= (1.0).xxxx);
    bool unnamed_137 = (2 > 1);
    bool unnamed_138 = (2u > 1u);
    bool unnamed_139 = (2.0 > 1.0);
    bool2 unnamed_140 = ((2).xx > (1).xx);
    bool3 unnamed_141 = ((2u).xxx > (1u).xxx);
    bool4 unnamed_142 = ((2.0).xxxx > (1.0).xxxx);
    bool unnamed_143 = (2 >= 1);
    bool unnamed_144 = (2u >= 1u);
    bool unnamed_145 = (2.0 >= 1.0);
    bool2 unnamed_146 = ((2).xx >= (1).xx);
    bool3 unnamed_147 = ((2u).xxx >= (1u).xxx);
    bool4 unnamed_148 = ((2.0).xxxx >= (1.0).xxxx);
}

void assignment()
{
    int a_1 = (int)0;
    int3 vec0_ = (int3)0;

    a_1 = 1;
    int _expr3 = a_1;
    a_1 = (_expr3 + 1);
    int _expr7 = a_1;
    a_1 = (_expr7 - 1);
    int _expr11 = a_1;
    int _expr13 = a_1;
    a_1 = (_expr13 * _expr11);
    int _expr16 = a_1;
    int _expr18 = a_1;
    a_1 = (_expr18 / _expr16);
    int _expr21 = a_1;
    a_1 = (_expr21 % 1);
    int _expr25 = a_1;
    a_1 = (_expr25 & 0);
    int _expr29 = a_1;
    a_1 = (_expr29 | 0);
    int _expr33 = a_1;
    a_1 = (_expr33 ^ 0);
    int _expr38 = a_1;
    a_1 = (_expr38 << 2u);
    int _expr42 = a_1;
    a_1 = (_expr42 >> 1u);
    int _expr45 = a_1;
    a_1 = (_expr45 + 1);
    int _expr49 = a_1;
    a_1 = (_expr49 - 1);
    vec0_ = int3(0, 0, 0);
    int _expr57 = vec0_.y;
    vec0_.y = (_expr57 + 1);
    int _expr63 = vec0_.y;
    vec0_.y = (_expr63 - 1);
    return;
}

[numthreads(1, 1, 1)]
void main()
{
    const float4 _e0 = builtins();
    const float4 _e1 = splat();
    const float3 _e2 = bool_cast(float4(1.0, 1.0, 1.0, 1.0).xyz);
    const float _e5 = constructors();
    logical();
    arithmetic();
    bit();
    comparison();
    assignment();
    return;
}
