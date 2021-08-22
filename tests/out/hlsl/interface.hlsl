struct NagaConstants {
    int base_vertex;
    int base_instance;
    uint other;
};
ConstantBuffer<NagaConstants> _NagaConstants: register(b0, space1);

struct VertexOutput {
    linear float varying : LOC1;
    float4 position : SV_Position;
};

struct FragmentOutput {
    float color : SV_Target0;
    float depth : SV_Depth;
    uint sample_mask : SV_Coverage;
};

groupshared uint output[1];

struct VertexInput_vertex {
    uint color1 : LOC10;
    uint instance_index1 : SV_InstanceID;
    uint vertex_index1 : SV_VertexID;
};

struct FragmentInput_fragment {
    float varying : LOC1;
    float4 position : SV_Position;
    bool front_facing1 : SV_IsFrontFace;
    uint sample_index1 : SV_SampleIndex;
    uint sample_mask1 : SV_Coverage;
};

struct ComputeInput_compute {
    uint3 global_id1 : SV_DispatchThreadID;
    uint3 local_id1 : SV_GroupThreadID;
    uint local_index1 : SV_GroupIndex;
    uint3 wg_id1 : SV_GroupID;
    uint3 num_wgs1 : SV_GroupID;
};

VertexOutput vertex(VertexInput_vertex vertexinput_vertex)
{
    uint vertex_index = vertexinput_vertex.vertex_index1;
    uint instance_index = vertexinput_vertex.instance_index1;
    uint color = vertexinput_vertex.color1;
    uint tmp = (((_NagaConstants.base_vertex + vertex_index) + (_NagaConstants.base_instance + instance_index)) + color);
    const VertexOutput vertexoutput1 = { float(tmp), float4(1.0.xxxx) };
    return vertexoutput1;
}

FragmentOutput fragment(FragmentInput_fragment fragmentinput_fragment)
{
    VertexOutput in1 = { fragmentinput_fragment.varying, fragmentinput_fragment.position };
    bool front_facing = fragmentinput_fragment.front_facing1;
    uint sample_index = fragmentinput_fragment.sample_index1;
    uint sample_mask = fragmentinput_fragment.sample_mask1;
    uint mask = (sample_mask & (1u << sample_index));
    float color2 = (front_facing ? 1.0 : 0.0);
    const FragmentOutput fragmentoutput1 = { color2, in1.varying, mask };
    return fragmentoutput1;
}

[numthreads(1, 1, 1)]
void compute(ComputeInput_compute computeinput_compute)
{
    uint3 global_id = computeinput_compute.global_id1;
    uint3 local_id = computeinput_compute.local_id1;
    uint local_index = computeinput_compute.local_index1;
    uint3 wg_id = computeinput_compute.wg_id1;
    uint3 num_wgs = computeinput_compute.num_wgs1;
    output[0] = ((((global_id.x + local_id.x) + local_index) + wg_id.x) + uint3(_NagaConstants.base_vertex, _NagaConstants.base_instance, _NagaConstants.other).x);
    return;
}
