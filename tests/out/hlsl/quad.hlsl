static const float c_scale = 1.2;

struct VertexOutput {
    linear float2 uv : LOC0;
    float4 position : SV_Position;
};

Texture2D<float4> u_texture : register(t0);
SamplerState u_sampler : register(s1);

struct VertexInput_main {
    float2 pos1 : LOC0;
    float2 uv2 : LOC1;
};

struct FragmentInput_main {
    float2 uv3 : LOC0;
};

VertexOutput main(VertexInput_main vertexinput_main)
{
    float2 pos = vertexinput_main.pos1;
    float2 uv = vertexinput_main.uv2;
    const VertexOutput vertexoutput1 = { uv, float4((c_scale * pos), 0.0, 1.0) };
    return vertexoutput1;
}

float4 main1(FragmentInput_main fragmentinput_main) : SV_Target0
{
    float2 uv1 = fragmentinput_main.uv3;
    float4 color = u_texture.Sample(u_sampler, uv1);
    if ((color.w == 0.0)) {
        discard;
    }
    float4 premultiplied = (color.w * color);
    return premultiplied;
}

float4 fs_extra() : SV_Target0
{
    return float4(0.0, 0.5, 0.0, 0.5);
}
