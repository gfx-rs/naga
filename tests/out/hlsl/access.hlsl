
struct AlignedWrapper {
    int value;
    int _end_pad_0;
};

struct Baz {
    float2 m_0; float2 m_1; float2 m_2;
};

RWByteAddressBuffer bar : register(u0);
cbuffer baz : register(b1) { Baz baz; }

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
    case 0: obj.m_0 = vec;
    case 1: obj.m_1 = vec;
    case 2: obj.m_2 = vec;
    }
}

void SetMatScalarmOnBaz(Baz obj, float scalar, uint mat_idx, uint vec_idx) {
    switch(mat_idx) {
    case 0: obj.m_0[vec_idx] = scalar;
    case 1: obj.m_1[vec_idx] = scalar;
    case 2: obj.m_2[vec_idx] = scalar;
    }
}

Baz ConstructBaz(float3x2 arg0) {
    Baz ret;
    ret.m_0 = arg0[0];
    ret.m_1 = arg0[1];
    ret.m_2 = arg0[2];
    return ret;
}

void test_matrix_within_struct_accesses()
{
    int idx = 9;
    float3x2 unnamed_1 = (float3x2)0;
    float2 unnamed_2 = (float2)0;
    float2 unnamed_3 = (float2)0;
    float unnamed_4 = (float)0;
    float unnamed_5 = (float)0;
    float unnamed_6 = (float)0;
    float unnamed_7 = (float)0;
    Baz t = (Baz)0;

    int _expr4 = idx;
    idx = (_expr4 - 1);
    float3x2 _expr8 = GetMatmOnBaz(baz);
    unnamed_1 = _expr8;
    float2 _expr13 = GetMatmOnBaz(baz)[0];
    unnamed_2 = _expr13;
    int _expr16 = idx;
    float2 _expr18 = GetMatmOnBaz(baz)[_expr16];
    unnamed_3 = _expr18;
    float _expr25 = GetMatmOnBaz(baz)[0][1];
    unnamed_4 = _expr25;
    int _expr30 = idx;
    float _expr32 = GetMatmOnBaz(baz)[0][_expr30];
    unnamed_5 = _expr32;
    int _expr35 = idx;
    float _expr39 = GetMatmOnBaz(baz)[_expr35][1];
    unnamed_6 = _expr39;
    int _expr42 = idx;
    int _expr44 = idx;
    float _expr46 = GetMatmOnBaz(baz)[_expr42][_expr44];
    unnamed_7 = _expr46;
    t = ConstructBaz(float3x2(float2(1.0.xx), float2(2.0.xx), float2(3.0.xx)));
    int _expr57 = idx;
    idx = (_expr57 + 1);
    SetMatmOnBaz(t, float3x2(float2(6.0.xx), float2(5.0.xx), float2(4.0.xx)));
    t.m_0 = float2(9.0.xx);
    int _expr74 = idx;
    SetMatVecmOnBaz(t, float2(90.0.xx), _expr74);
    t.m_0[1] = 10.0;
    int _expr87 = idx;
    t.m_0[_expr87] = 20.0;
    int _expr91 = idx;
    SetMatScalarmOnBaz(t, 30.0, _expr91, 1);
    int _expr97 = idx;
    int _expr99 = idx;
    SetMatScalarmOnBaz(t, 40.0, _expr97, _expr99);
    return;
}

float read_from_private(inout float foo_1)
{
    float _expr3 = foo_1;
    return _expr3;
}

float test_arr_as_arg(float a[1])
{
    return a[0];
}

uint NagaBufferLengthRW(RWByteAddressBuffer buffer)
{
    uint ret;
    buffer.GetDimensions(ret);
    return ret;
}

float4 foo_vert(uint vi : SV_VertexID) : SV_Position
{
    float foo = 0.0;
    int c[5] = {(int)0,(int)0,(int)0,(int)0,(int)0};
    float unnamed = (float)0;

    float baz_1 = foo;
    foo = 1.0;
    test_matrix_within_struct_accesses();
    float4x3 matrix_ = float4x3(asfloat(bar.Load3(0+0)), asfloat(bar.Load3(0+16)), asfloat(bar.Load3(0+32)), asfloat(bar.Load3(0+48)));
    uint2 arr[2] = {asuint(bar.Load2(104+0)), asuint(bar.Load2(104+8))};
    float b = asfloat(bar.Load(0+48+0));
    int a_1 = asint(bar.Load(0+(((NagaBufferLengthRW(bar) - 120) / 8) - 2u)*8+120));
    const float _e28 = read_from_private(foo);
    {
        int _result[5]={ a_1, int(b), 3, 4, 5 };
        for(int _i=0; _i<5; ++_i) c[_i] = _result[_i];
    }
    c[(vi + 1u)] = 42;
    int value = c[vi];
    const float _e42 = test_arr_as_arg({ 0.0 });
    unnamed = _e42;
    return float4(mul(float4(int4(value.xxxx)), matrix_), 2.0);
}

float4 foo_frag() : SV_Target0
{
    bar.Store(8+16+0, asuint(1.0));
    {
        float4x3 _value2 = float4x3(float3(0.0.xxx), float3(1.0.xxx), float3(2.0.xxx), float3(3.0.xxx));
        bar.Store3(0+0, asuint(_value2[0]));
        bar.Store3(0+16, asuint(_value2[1]));
        bar.Store3(0+32, asuint(_value2[2]));
        bar.Store3(0+48, asuint(_value2[3]));
    }
    {
        uint2 _value2[2] = { uint2(0u.xx), uint2(1u.xx) };
        bar.Store2(104+0, asuint(_value2[0]));
        bar.Store2(104+8, asuint(_value2[1]));
    }
    bar.Store(0+8+120, asuint(1));
    return float4(0.0.xxxx);
}

[numthreads(1, 1, 1)]
void atomics()
{
    int tmp = (int)0;

    int value_1 = asint(bar.Load(96));
    int _e7; bar.InterlockedAdd(96, 5, _e7);
    tmp = _e7;
    int _e10; bar.InterlockedAdd(96, -5, _e10);
    tmp = _e10;
    int _e13; bar.InterlockedAnd(96, 5, _e13);
    tmp = _e13;
    int _e16; bar.InterlockedOr(96, 5, _e16);
    tmp = _e16;
    int _e19; bar.InterlockedXor(96, 5, _e19);
    tmp = _e19;
    int _e22; bar.InterlockedMin(96, 5, _e22);
    tmp = _e22;
    int _e25; bar.InterlockedMax(96, 5, _e25);
    tmp = _e25;
    int _e28; bar.InterlockedExchange(96, 5, _e28);
    tmp = _e28;
    bar.Store(96, asuint(value_1));
    return;
}
