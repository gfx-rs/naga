struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec3<f32>,
}

struct Data {
    proj_inv: mat4x4<f32>,
    view: mat4x4<f32>,
}

@group(0) @binding(0) 
var<uniform> r_data: Data;
@group(0) @binding(1) 
var r_texture: texture_cube<f32>;
@group(0) @binding(2) 
var r_sampler: sampler;

@vertex 
fn vs_main(@builtin(vertex_index) vertex_index: u32) -> VertexOutput {
    var tmp1_: i32;
    var tmp2_: i32;

    let tmp1_1 = (i32(vertex_index) / 2);
    tmp1_ = tmp1_1;
    let tmp2_1 = (i32(vertex_index) & 1);
    tmp2_ = tmp2_1;
    let _e11 = tmp1_;
    let _e18 = tmp2_;
    let pos = vec4<f32>(((f32(_e11) * 4.0) - 1.0), ((f32(_e18) * 4.0) - 1.0), 0.0, 1.0);
    let _e31 = r_data.view[0];
    let _e37 = r_data.view[1];
    let _e43 = r_data.view[2];
    let inv_model_view = transpose(mat3x3<f32>(_e31.xyz, _e37.xyz, _e43.xyz));
    let _e49 = r_data.proj_inv;
    let unprojected = (_e49 * pos);
    return VertexOutput(pos, (inv_model_view * unprojected.xyz));
}

@fragment 
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let _e4 = textureSample(r_texture, r_sampler, in.uv);
    return _e4;
}
