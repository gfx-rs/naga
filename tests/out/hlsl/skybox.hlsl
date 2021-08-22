struct NagaConstants {
    int base_vertex;
    int base_instance;
    uint other;
};
ConstantBuffer<NagaConstants> _NagaConstants: register(b1);

struct VertexOutput {
    linear float3 uv : LOC0;
    float4 position : SV_Position;
};

struct Data {
    row_major float4x4 proj_inv;
    row_major float4x4 view;
};

cbuffer r_data : register(b0) { Data r_data; }
TextureCube<float4> r_texture : register(t0);
SamplerState r_sampler : register(s0, space1);

struct VertexInput_vs_main {
    uint vertex_index1 : SV_VertexID;
};

struct FragmentInput_fs_main {
    float3 uv : LOC0;
    float4 position : SV_Position;
};

VertexOutput vs_main(VertexInput_vs_main vertexinput_vs_main)
{
    uint vertex_index = vertexinput_vs_main.vertex_index1;
    int tmp1_ = (int)0;
    int tmp2_ = (int)0;

    tmp1_ = (int((_NagaConstants.base_vertex + vertex_index)) / 2);
    tmp2_ = (int((_NagaConstants.base_vertex + vertex_index)) & 1);
    int _expr10 = tmp1_;
    int _expr16 = tmp2_;
    float4 pos = float4(((float(_expr10) * 4.0) - 1.0), ((float(_expr16) * 4.0) - 1.0), 0.0, 1.0);
    float4 _expr27 = r_data.view[0];
    float4 _expr31 = r_data.view[1];
    float4 _expr35 = r_data.view[2];
    float3x3 inv_model_view = transpose(float3x3(_expr27.xyz, _expr31.xyz, _expr35.xyz));
    float4x4 _expr40 = r_data.proj_inv;
    float4 unprojected = mul(pos, _expr40);
    const VertexOutput vertexoutput1 = { mul(unprojected.xyz, inv_model_view), pos };
    return vertexoutput1;
}

float4 fs_main(FragmentInput_fs_main fragmentinput_fs_main) : SV_Target0
{
    VertexOutput in1 = { fragmentinput_fs_main.uv, fragmentinput_fs_main.position };
    float4 _expr5 = r_texture.Sample(r_sampler, in1.uv);
    return _expr5;
}
