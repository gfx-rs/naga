
struct Ah {
    float inner[2];
};

ByteAddressBuffer ah : register(t0);

Ah ConstructAh(float arg0[2]) {
    Ah ret = (Ah)0;
    ret.inner = arg0;
    return ret;
}

[numthreads(1, 1, 1)]
void cs_main()
{
    Ah unnamed = ConstructAh({asfloat(ah.Load(0+0)), asfloat(ah.Load(0+4))});
}
