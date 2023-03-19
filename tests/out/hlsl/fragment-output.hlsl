
struct FragmentOutput {
    float4 vec4f : SV_Target0;
    nointerpolation int4 vec4i : SV_Target1;
    nointerpolation uint4 vec4u : SV_Target2;
    float3 vec3f : SV_Target3;
    nointerpolation int3 vec3i : SV_Target4;
    nointerpolation uint3 vec3u : SV_Target5;
    float2 vec2f : SV_Target6;
    nointerpolation int2 vec2i : SV_Target7;
    nointerpolation uint2 vec2u : SV_Target8;
    float scalarf : SV_Target9;
    nointerpolation int scalari : SV_Target10;
    nointerpolation uint scalaru : SV_Target11;
};

FragmentOutput main()
{
    FragmentOutput output = (FragmentOutput)0;

    output.vec4f = (0.0).xxxx;
    output.vec4i = (0).xxxx;
    output.vec4u = (0u).xxxx;
    output.vec3f = (0.0).xxx;
    output.vec3i = (0).xxx;
    output.vec3u = (0u).xxx;
    output.vec2f = (0.0).xx;
    output.vec2i = (0).xx;
    output.vec2u = (0u).xx;
    output.scalarf = 0.0;
    output.scalari = 0;
    output.scalaru = 0u;
    FragmentOutput _expr34 = output;
    const FragmentOutput fragmentoutput = _expr34;
    return fragmentoutput;
}
