#version 320 es

precision highp float;
precision highp int;

struct VertexOutput {
    vec4 position;
    vec3 uv;
};
struct Data {
    mat4x4 proj_inv;
    mat4x4 view;
};
layout(std140, binding = 0) uniform Data_block_0Vertex { Data _group_0_binding_0_vs; };

layout(location = 0) smooth out vec3 _vs2fs_location0;

void main() {
    uint vertex_index = uint(gl_VertexID);
    int tmp1_ = 0;
    int tmp2_ = 0;
    int tmp1_1 = (int(vertex_index) / 2);
    tmp1_ = tmp1_1;
    int tmp2_1 = (int(vertex_index) & 1);
    tmp2_ = tmp2_1;
    int _e11 = tmp1_;
    int _e18 = tmp2_;
    vec4 pos = vec4(((float(_e11) * 4.0) - 1.0), ((float(_e18) * 4.0) - 1.0), 0.0, 1.0);
    vec4 _e31 = _group_0_binding_0_vs.view[0];
    vec4 _e37 = _group_0_binding_0_vs.view[1];
    vec4 _e43 = _group_0_binding_0_vs.view[2];
    mat3x3 inv_model_view = transpose(mat3x3(_e31.xyz, _e37.xyz, _e43.xyz));
    mat4x4 _e49 = _group_0_binding_0_vs.proj_inv;
    vec4 unprojected = (_e49 * pos);
    VertexOutput _tmp_return = VertexOutput(pos, (inv_model_view * unprojected.xyz));
    gl_Position = _tmp_return.position;
    _vs2fs_location0 = _tmp_return.uv;
    return;
}

