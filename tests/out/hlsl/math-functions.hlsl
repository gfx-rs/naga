struct naga_frexp_result {
    float fract;
    int exp;
};

naga_frexp_result naga_frexp(in float arg) {
    float exp;
    float fract = frexp(arg, exp);
    naga_frexp_result result;
    result.exp = exp;
    result.fract = fract;
    return result;
}

struct naga_modf_result {
    float fract;
    float whole;
};

naga_modf_result naga_modf(in float arg) {
    float whole;
    float fract = modf(arg, whole);
    naga_modf_result result;
    result.whole = whole;
    result.fract = fract;
    return result;
}

void main()
{
    float4 v = (0.0).xxxx;
    float a = degrees(1.0);
    float b = radians(1.0);
    float4 c = degrees(v);
    float4 d = radians(v);
    float4 e = saturate(v);
    float4 g = refract(v, v, 1.0);
    int const_dot = dot((int2)0, (int2)0);
    uint first_leading_bit_abs = firstbithigh(abs(0u));
    int flb_a = asint(firstbithigh(-1));
    int2 flb_b = asint(firstbithigh((-1).xx));
    uint2 flb_c = firstbithigh((1u).xx);
    int ftb_a = asint(firstbitlow(-1));
    uint ftb_b = firstbitlow(1u);
    int2 ftb_c = asint(firstbitlow((-1).xx));
    uint2 ftb_d = firstbitlow((1u).xx);
    uint ctz_a = min(32u, firstbitlow(0u));
    int ctz_b = asint(min(32u, firstbitlow(0)));
    uint ctz_c = min(32u, firstbitlow(4294967295u));
    int ctz_d = asint(min(32u, firstbitlow(-1)));
    uint2 ctz_e = min((32u).xx, firstbitlow((0u).xx));
    int2 ctz_f = asint(min((32u).xx, firstbitlow((0).xx)));
    uint2 ctz_g = min((32u).xx, firstbitlow((1u).xx));
    int2 ctz_h = asint(min((32u).xx, firstbitlow((1).xx)));
    int clz_a = (-1 < 0 ? 0 : 31 - asint(firstbithigh(-1)));
    uint clz_b = (31u - firstbithigh(1u));
    int2 _expr58 = (-1).xx;
    int2 clz_c = (_expr58 < (0).xx ? (0).xx : (31).xx - asint(firstbithigh(_expr58)));
    uint2 clz_d = ((31u).xx - firstbithigh((1u).xx));
    struct naga_modf_result modf_a = naga_modf(1.5);
    float modf_b = naga_modf(1.5).fract;
    float modf_c = naga_modf(1.5).whole;
    struct naga_frexp_result frexp_a = naga_frexp(1.5);
    float frexp_b = naga_frexp(1.5).fract;
    int frexp_c = naga_frexp(1.5).exp;
}
