
typedef struct { float2 _0; float2 _1; } __mat2x2;
float2 __get_col_of_mat2x2(__mat2x2 mat, uint idx) {
    switch(idx) {
    case 0: { return mat._0; }
    case 1: { return mat._1; }
    default: { return (float2)0; }
    }
}
void __set_col_of_mat2x2(__mat2x2 mat, uint idx, float2 value) {
    switch(idx) {
    case 0: { mat._0 = value; break; }
    case 1: { mat._1 = value; break; }
    }
}
void __set_el_of_mat2x2(__mat2x2 mat, uint idx, uint vec_idx, float value) {
    switch(idx) {
    case 0: { mat._0[vec_idx] = value; break; }
    case 1: { mat._1[vec_idx] = value; break; }
    }
}

typedef struct { float2 _0; float2 _1; float2 _2; float2 _3; } __mat4x2;
float2 __get_col_of_mat4x2(__mat4x2 mat, uint idx) {
    switch(idx) {
    case 0: { return mat._0; }
    case 1: { return mat._1; }
    case 2: { return mat._2; }
    case 3: { return mat._3; }
    default: { return (float2)0; }
    }
}
void __set_col_of_mat4x2(__mat4x2 mat, uint idx, float2 value) {
    switch(idx) {
    case 0: { mat._0 = value; break; }
    case 1: { mat._1 = value; break; }
    case 2: { mat._2 = value; break; }
    case 3: { mat._3 = value; break; }
    }
}
void __set_el_of_mat4x2(__mat4x2 mat, uint idx, uint vec_idx, float value) {
    switch(idx) {
    case 0: { mat._0[vec_idx] = value; break; }
    case 1: { mat._1[vec_idx] = value; break; }
    case 2: { mat._2[vec_idx] = value; break; }
    case 3: { mat._3[vec_idx] = value; break; }
    }
}

struct GlobalConst {
    uint a;
    int _pad1_0;
    int _pad1_1;
    int _pad1_2;
    uint3 b;
    int c;
};

struct AlignedWrapper {
    int value;
    int _end_pad_0;
};

struct Baz {
    float2 m_0; float2 m_1; float2 m_2;
};

struct MatCx2InArray {
    __mat4x2 am[2];
};

GlobalConst ConstructGlobalConst(uint arg0, uint3 arg1, int arg2) {
    GlobalConst ret = (GlobalConst)0;
    ret.a = arg0;
    ret.b = arg1;
    ret.c = arg2;
    return ret;
}

typedef float4x2 ret_Constructarray2_float4x2_[2];
ret_Constructarray2_float4x2_ Constructarray2_float4x2_(float4x2 arg0, float4x2 arg1) {
    float4x2 ret[2] = { arg0, arg1 };
    return ret;
}

typedef float ret_Constructarray10_float_[10];
ret_Constructarray10_float_ Constructarray10_float_(float arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6, float arg7, float arg8, float arg9) {
    float ret[10] = { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 };
    return ret;
}

typedef float ret_Constructarray5_array10_float__[5][10];
ret_Constructarray5_array10_float__ Constructarray5_array10_float__(float arg0[10], float arg1[10], float arg2[10], float arg3[10], float arg4[10]) {
    float ret[5][10] = { arg0, arg1, arg2, arg3, arg4 };
    return ret;
}

static GlobalConst global_const = ConstructGlobalConst(0u, uint3(0u, 0u, 0u), 0);
RWByteAddressBuffer bar : register(u0);
cbuffer baz : register(b1) { Baz baz; }
RWByteAddressBuffer qux : register(u2);
cbuffer nested_mat_cx2_ : register(b3) { MatCx2InArray nested_mat_cx2_; }
groupshared uint val;

float3x2 GetMatmOnBaz(Baz obj) {
    return float3x2(obj.m_0, obj.m_1, obj.m_2);
}

void SetMatmOnBaz(Baz obj, float3x2 mat) {
    obj.m_0 = mat[0];
    obj.m_1 = mat[1];
    obj.m_2 = mat[2];
}

void SetMatVecmOnBaz(Baz obj, float2 vec, uint mat_idx) {
    switch(mat_idx) {
    case 0: { obj.m_0 = vec; break; }
    case 1: { obj.m_1 = vec; break; }
    case 2: { obj.m_2 = vec; break; }
    }
}

void SetMatScalarmOnBaz(Baz obj, float scalar, uint mat_idx, uint vec_idx) {
    switch(mat_idx) {
    case 0: { obj.m_0[vec_idx] = scalar; break; }
    case 1: { obj.m_1[vec_idx] = scalar; break; }
    case 2: { obj.m_2[vec_idx] = scalar; break; }
    }
}

Baz ConstructBaz(float3x2 arg0) {
    Baz ret = (Baz)0;
    ret.m_0 = arg0[0];
    ret.m_1 = arg0[1];
    ret.m_2 = arg0[2];
    return ret;
}

void test_matrix_within_struct_accesses()
{
    int idx = (int)0;
    Baz t = (Baz)0;

    idx = 1;
    int _expr3 = idx;
    idx = (_expr3 - 1);
    float3x2 unnamed = GetMatmOnBaz(baz);
    float2 unnamed_1 = GetMatmOnBaz(baz)[0];
    int _expr17 = idx;
    float2 unnamed_2 = GetMatmOnBaz(baz)[_expr17];
    float unnamed_3 = GetMatmOnBaz(baz)[0][1];
    int _expr32 = idx;
    float unnamed_4 = GetMatmOnBaz(baz)[0][_expr32];
    int _expr38 = idx;
    float unnamed_5 = GetMatmOnBaz(baz)[_expr38][1];
    int _expr46 = idx;
    int _expr49 = idx;
    float unnamed_6 = GetMatmOnBaz(baz)[_expr46][_expr49];
    Baz t_2 = ConstructBaz(float3x2((1.0).xx, (2.0).xx, (3.0).xx));
    t = t_2;
    int _expr62 = idx;
    idx = (_expr62 + 1);
    SetMatmOnBaz(t, float3x2((6.0).xx, (5.0).xx, (4.0).xx));
    t.m_0 = (9.0).xx;
    int _expr85 = idx;
    SetMatVecmOnBaz(t, (90.0).xx, _expr85);
    t.m_0[1] = 10.0;
    int _expr99 = idx;
    t.m_0[_expr99] = 20.0;
    int _expr105 = idx;
    SetMatScalarmOnBaz(t, 30.0, _expr105, 1);
    int _expr113 = idx;
    int _expr116 = idx;
    SetMatScalarmOnBaz(t, 40.0, _expr113, _expr116);
    return;
}

MatCx2InArray ConstructMatCx2InArray(float4x2 arg0[2]) {
    MatCx2InArray ret = (MatCx2InArray)0;
    ret.am = (__mat4x2[2])arg0;
    return ret;
}

void test_matrix_within_array_within_struct_accesses()
{
    int idx_1 = (int)0;
    MatCx2InArray t_1 = (MatCx2InArray)0;

    idx_1 = 1;
    int _expr3 = idx_1;
    idx_1 = (_expr3 - 1);
    float4x2 unnamed_7[2] = ((float4x2[2])nested_mat_cx2_.am);
    float4x2 unnamed_8 = ((float4x2)nested_mat_cx2_.am[0]);
    float2 unnamed_9 = nested_mat_cx2_.am[0]._0;
    int _expr26 = idx_1;
    float2 unnamed_10 = __get_col_of_mat4x2(nested_mat_cx2_.am[0], _expr26);
    float unnamed_11 = nested_mat_cx2_.am[0]._0[1];
    int _expr45 = idx_1;
    float unnamed_12 = nested_mat_cx2_.am[0]._0[_expr45];
    int _expr53 = idx_1;
    float unnamed_13 = __get_col_of_mat4x2(nested_mat_cx2_.am[0], _expr53)[1];
    int _expr63 = idx_1;
    int _expr66 = idx_1;
    float unnamed_14 = __get_col_of_mat4x2(nested_mat_cx2_.am[0], _expr63)[_expr66];
    MatCx2InArray t_3 = ConstructMatCx2InArray(Constructarray2_float4x2_(float4x2(float2(0.0, 0.0), float2(0.0, 0.0), float2(0.0, 0.0), float2(0.0, 0.0)), float4x2(float2(0.0, 0.0), float2(0.0, 0.0), float2(0.0, 0.0), float2(0.0, 0.0))));
    t_1 = t_3;
    int _expr73 = idx_1;
    idx_1 = (_expr73 + 1);
    t_1.am = (__mat4x2[2])Constructarray2_float4x2_(float4x2(float2(0.0, 0.0), float2(0.0, 0.0), float2(0.0, 0.0), float2(0.0, 0.0)), float4x2(float2(0.0, 0.0), float2(0.0, 0.0), float2(0.0, 0.0), float2(0.0, 0.0)));
    t_1.am[0] = (__mat4x2)float4x2((8.0).xx, (7.0).xx, (6.0).xx, (5.0).xx);
    t_1.am[0]._0 = (9.0).xx;
    int _expr107 = idx_1;
    __set_col_of_mat4x2(t_1.am[0], _expr107, (90.0).xx);
    t_1.am[0]._0[1] = 10.0;
    int _expr125 = idx_1;
    t_1.am[0]._0[_expr125] = 20.0;
    int _expr133 = idx_1;
    __set_el_of_mat4x2(t_1.am[0], _expr133, 1, 30.0);
    int _expr143 = idx_1;
    int _expr146 = idx_1;
    __set_el_of_mat4x2(t_1.am[0], _expr143, _expr146, 40.0);
    return;
}

float read_from_private(inout float foo_1)
{
    float _expr1 = foo_1;
    return _expr1;
}

float test_arr_as_arg(float a[5][10])
{
    return a[4][9];
}

void assign_through_ptr_fn(inout uint p)
{
    p = 42u;
    return;
}

uint NagaBufferLengthRW(RWByteAddressBuffer buffer)
{
    uint ret;
    buffer.GetDimensions(ret);
    return ret;
}

typedef int ret_Constructarray5_int_[5];
ret_Constructarray5_int_ Constructarray5_int_(int arg0, int arg1, int arg2, int arg3, int arg4) {
    int ret[5] = { arg0, arg1, arg2, arg3, arg4 };
    return ret;
}

float4 foo_vert(uint vi : SV_VertexID) : SV_Position
{
    float foo = (float)0;
    int d[5] = {(int)0,(int)0,(int)0,(int)0,(int)0};

    foo = 0.0;
    float baz_1 = foo;
    foo = 1.0;
    test_matrix_within_struct_accesses();
    test_matrix_within_array_within_struct_accesses();
    float4x3 _matrix = float4x3(asfloat(bar.Load3(0+0)), asfloat(bar.Load3(0+16)), asfloat(bar.Load3(0+32)), asfloat(bar.Load3(0+48)));
    uint2 arr[2] = {asuint(bar.Load2(104+0)), asuint(bar.Load2(104+8))};
    float b = asfloat(bar.Load(0+48+0));
    int a_1 = asint(bar.Load(0+(((NagaBufferLengthRW(bar) - 120) / 8) - 2u)*8+120));
    int2 c = asint(qux.Load2(0));
    const float _e35 = read_from_private(foo);
    int d_1[5] = Constructarray5_int_(a_1, int(b), 3, 4, 5);
    d = d_1;
    d[(vi + 1u)] = 42;
    int value = d[vi];
    const float _e53 = test_arr_as_arg(Constructarray5_array10_float__(Constructarray10_float_(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), Constructarray10_float_(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), Constructarray10_float_(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), Constructarray10_float_(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), Constructarray10_float_(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)));
    return float4(mul(float4((value).xxxx), _matrix), 2.0);
}

typedef uint2 ret_Constructarray2_uint2_[2];
ret_Constructarray2_uint2_ Constructarray2_uint2_(uint2 arg0, uint2 arg1) {
    uint2 ret[2] = { arg0, arg1 };
    return ret;
}

float4 foo_frag() : SV_Target0
{
    bar.Store(8+16+0, asuint(1.0));
    {
        float4x3 _value2 = float4x3((0.0).xxx, (1.0).xxx, (2.0).xxx, (3.0).xxx);
        bar.Store3(0+0, asuint(_value2[0]));
        bar.Store3(0+16, asuint(_value2[1]));
        bar.Store3(0+32, asuint(_value2[2]));
        bar.Store3(0+48, asuint(_value2[3]));
    }
    {
        uint2 _value2[2] = Constructarray2_uint2_((0u).xx, (1u).xx);
        bar.Store2(104+0, asuint(_value2[0]));
        bar.Store2(104+8, asuint(_value2[1]));
    }
    bar.Store(0+8+120, asuint(1));
    qux.Store2(0, asuint(int2(0, 0)));
    return (0.0).xxxx;
}

[numthreads(1, 1, 1)]
void atomics()
{
    int tmp = (int)0;

    int value_1 = asint(bar.Load(96));
    int _e6; bar.InterlockedAdd(96, 5, _e6);
    tmp = _e6;
    int _e11; bar.InterlockedAdd(96, -5, _e11);
    tmp = _e11;
    int _e16; bar.InterlockedAnd(96, 5, _e16);
    tmp = _e16;
    int _e21; bar.InterlockedOr(96, 5, _e21);
    tmp = _e21;
    int _e26; bar.InterlockedXor(96, 5, _e26);
    tmp = _e26;
    int _e31; bar.InterlockedMin(96, 5, _e31);
    tmp = _e31;
    int _e36; bar.InterlockedMax(96, 5, _e36);
    tmp = _e36;
    int _e41; bar.InterlockedExchange(96, 5, _e41);
    tmp = _e41;
    bar.Store(96, asuint(value_1));
    return;
}

[numthreads(1, 1, 1)]
void assign_through_ptr()
{
    assign_through_ptr_fn(val);
    return;
}
