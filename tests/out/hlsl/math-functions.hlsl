
void main()
{
    float4 v = (0.0).xxxx;
    float a = degrees(1.0);
    float b = radians(1.0);
    float4 c = degrees(v);
    float4 d = radians(v);
    float4 e = saturate(v);
    float4 g = refract(v, v, 1.0);
    int const_dot = dot(int2(0, 0), int2(0, 0));
    uint first_leading_bit_abs = firstbithigh(abs(0u));
    int ctz_a = (-1 == 0 ? -1 : firstbitlow(-1) - 1);
    uint ctz_b = asuint(firstbitlow(1u) - 1);
    int2 _expr20 = (-1).xx;
    int2 ctz_c = (_expr20 == (0).xx ? (-1).xx : firstbitlow(_expr20) - (1).xx);
    uint2 ctz_d = asuint(firstbitlow((1u).xx) - (1).xx);
    int ctz_e = (0 == 0 ? -1 : firstbitlow(0) - 1);
    uint ctz_f = asuint(firstbitlow(0u) - 1);
    int clz_a = (-1 < 0 ? 0 : 31 - firstbithigh(-1));
    uint clz_b = asuint(31 - firstbithigh(1u));
    int2 _expr34 = (-1).xx;
    int2 clz_c = (_expr34 < (0).xx ? (0).xx : (31).xx - firstbithigh(_expr34));
    uint2 clz_d = asuint((31).xx - firstbithigh((1u).xx));
}
