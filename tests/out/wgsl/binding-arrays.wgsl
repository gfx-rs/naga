struct UniformIndex {
    index: u32,
}

struct FragmentIn {
    @location(0) index: u32,
}

@group(0) @binding(0) 
var texture_array_unbounded: binding_array<texture_2d<f32>>;
@group(0) @binding(1) 
var texture_array_bounded: binding_array<texture_2d<f32>,5u>;
@group(0) @binding(2) 
var texture_array_2darray: binding_array<texture_2d_array<f32>,5u>;
@group(0) @binding(3) 
var texture_array_multisampled: binding_array<texture_multisampled_2d<f32>,5u>;
@group(0) @binding(4) 
var texture_array_depth: binding_array<texture_depth_2d,5u>;
@group(0) @binding(5) 
var texture_array_storage: binding_array<texture_storage_2d<rgba32float,write>,5u>;
@group(0) @binding(6) 
var samp: binding_array<sampler,5u>;
@group(0) @binding(7) 
var samp_comp: binding_array<sampler_comparison,5u>;
@group(0) @binding(8) 
var<uniform> uni: UniformIndex;

@fragment 
fn main(fragment_in: FragmentIn) -> @location(0) vec4<f32> {
    var i1_: i32;
    var i2_: vec2<i32>;
    var v1_: f32;
    var v4_: vec4<f32>;

    let uniform_index = uni.index;
    let non_uniform_index = fragment_in.index;
    i1_ = 0;
    let i2_1 = vec2<i32>(0);
    i2_ = i2_1;
    v1_ = 0.0;
    let v4_1 = vec4<f32>(0.0);
    v4_ = v4_1;
    let uv = vec2<f32>(0.0);
    let pix = vec2<i32>(0);
    let _e22 = textureDimensions(texture_array_unbounded[0]);
    let _e24 = i2_;
    i2_ = (_e24 + _e22);
    let _e28 = textureDimensions(texture_array_unbounded[uniform_index]);
    let _e30 = i2_;
    i2_ = (_e30 + _e28);
    let _e34 = textureDimensions(texture_array_unbounded[non_uniform_index]);
    let _e36 = i2_;
    i2_ = (_e36 + _e34);
    let _e44 = textureGather(0, texture_array_bounded[0], samp[0], uv);
    let _e46 = v4_;
    v4_ = (_e46 + _e44);
    let _e52 = textureGather(0, texture_array_bounded[uniform_index], samp[uniform_index], uv);
    let _e54 = v4_;
    v4_ = (_e54 + _e52);
    let _e60 = textureGather(0, texture_array_bounded[non_uniform_index], samp[non_uniform_index], uv);
    let _e62 = v4_;
    v4_ = (_e62 + _e60);
    let _e71 = textureGatherCompare(texture_array_depth[0], samp_comp[0], uv, 0.0);
    let _e73 = v4_;
    v4_ = (_e73 + _e71);
    let _e80 = textureGatherCompare(texture_array_depth[uniform_index], samp_comp[uniform_index], uv, 0.0);
    let _e82 = v4_;
    v4_ = (_e82 + _e80);
    let _e89 = textureGatherCompare(texture_array_depth[non_uniform_index], samp_comp[non_uniform_index], uv, 0.0);
    let _e91 = v4_;
    v4_ = (_e91 + _e89);
    let _e97 = textureLoad(texture_array_unbounded[0], pix, 0);
    let _e99 = v4_;
    v4_ = (_e99 + _e97);
    let _e104 = textureLoad(texture_array_unbounded[uniform_index], pix, 0);
    let _e106 = v4_;
    v4_ = (_e106 + _e104);
    let _e111 = textureLoad(texture_array_unbounded[non_uniform_index], pix, 0);
    let _e113 = v4_;
    v4_ = (_e113 + _e111);
    let _e118 = textureNumLayers(texture_array_2darray[0]);
    let _e120 = i1_;
    i1_ = (_e120 + _e118);
    let _e124 = textureNumLayers(texture_array_2darray[uniform_index]);
    let _e126 = i1_;
    i1_ = (_e126 + _e124);
    let _e130 = textureNumLayers(texture_array_2darray[non_uniform_index]);
    let _e132 = i1_;
    i1_ = (_e132 + _e130);
    let _e137 = textureNumLevels(texture_array_bounded[0]);
    let _e139 = i1_;
    i1_ = (_e139 + _e137);
    let _e143 = textureNumLevels(texture_array_bounded[uniform_index]);
    let _e145 = i1_;
    i1_ = (_e145 + _e143);
    let _e149 = textureNumLevels(texture_array_bounded[non_uniform_index]);
    let _e151 = i1_;
    i1_ = (_e151 + _e149);
    let _e156 = textureNumSamples(texture_array_multisampled[0]);
    let _e158 = i1_;
    i1_ = (_e158 + _e156);
    let _e162 = textureNumSamples(texture_array_multisampled[uniform_index]);
    let _e164 = i1_;
    i1_ = (_e164 + _e162);
    let _e168 = textureNumSamples(texture_array_multisampled[non_uniform_index]);
    let _e170 = i1_;
    i1_ = (_e170 + _e168);
    let _e178 = textureSample(texture_array_bounded[0], samp[0], uv);
    let _e180 = v4_;
    v4_ = (_e180 + _e178);
    let _e186 = textureSample(texture_array_bounded[uniform_index], samp[uniform_index], uv);
    let _e188 = v4_;
    v4_ = (_e188 + _e186);
    let _e194 = textureSample(texture_array_bounded[non_uniform_index], samp[non_uniform_index], uv);
    let _e196 = v4_;
    v4_ = (_e196 + _e194);
    let _e205 = textureSampleBias(texture_array_bounded[0], samp[0], uv, 0.0);
    let _e207 = v4_;
    v4_ = (_e207 + _e205);
    let _e214 = textureSampleBias(texture_array_bounded[uniform_index], samp[uniform_index], uv, 0.0);
    let _e216 = v4_;
    v4_ = (_e216 + _e214);
    let _e223 = textureSampleBias(texture_array_bounded[non_uniform_index], samp[non_uniform_index], uv, 0.0);
    let _e225 = v4_;
    v4_ = (_e225 + _e223);
    let _e234 = textureSampleCompare(texture_array_depth[0], samp_comp[0], uv, 0.0);
    let _e236 = v1_;
    v1_ = (_e236 + _e234);
    let _e243 = textureSampleCompare(texture_array_depth[uniform_index], samp_comp[uniform_index], uv, 0.0);
    let _e245 = v1_;
    v1_ = (_e245 + _e243);
    let _e252 = textureSampleCompare(texture_array_depth[non_uniform_index], samp_comp[non_uniform_index], uv, 0.0);
    let _e254 = v1_;
    v1_ = (_e254 + _e252);
    let _e263 = textureSampleCompareLevel(texture_array_depth[0], samp_comp[0], uv, 0.0);
    let _e265 = v1_;
    v1_ = (_e265 + _e263);
    let _e272 = textureSampleCompareLevel(texture_array_depth[uniform_index], samp_comp[uniform_index], uv, 0.0);
    let _e274 = v1_;
    v1_ = (_e274 + _e272);
    let _e281 = textureSampleCompareLevel(texture_array_depth[non_uniform_index], samp_comp[non_uniform_index], uv, 0.0);
    let _e283 = v1_;
    v1_ = (_e283 + _e281);
    let _e291 = textureSampleGrad(texture_array_bounded[0], samp[0], uv, uv, uv);
    let _e293 = v4_;
    v4_ = (_e293 + _e291);
    let _e299 = textureSampleGrad(texture_array_bounded[uniform_index], samp[uniform_index], uv, uv, uv);
    let _e301 = v4_;
    v4_ = (_e301 + _e299);
    let _e307 = textureSampleGrad(texture_array_bounded[non_uniform_index], samp[non_uniform_index], uv, uv, uv);
    let _e309 = v4_;
    v4_ = (_e309 + _e307);
    let _e318 = textureSampleLevel(texture_array_bounded[0], samp[0], uv, 0.0);
    let _e320 = v4_;
    v4_ = (_e320 + _e318);
    let _e327 = textureSampleLevel(texture_array_bounded[uniform_index], samp[uniform_index], uv, 0.0);
    let _e329 = v4_;
    v4_ = (_e329 + _e327);
    let _e336 = textureSampleLevel(texture_array_bounded[non_uniform_index], samp[non_uniform_index], uv, 0.0);
    let _e338 = v4_;
    v4_ = (_e338 + _e336);
    let _e344 = v4_;
    textureStore(texture_array_storage[0], pix, _e344);
    let _e348 = v4_;
    textureStore(texture_array_storage[uniform_index], pix, _e348);
    let _e352 = v4_;
    textureStore(texture_array_storage[non_uniform_index], pix, _e352);
    let _e354 = i2_;
    let _e356 = i1_;
    let v2_ = vec2<f32>((_e354 + vec2<i32>(_e356)));
    let _e361 = v4_;
    let _e369 = v1_;
    return ((_e361 + vec4<f32>(v2_.x, v2_.y, v2_.x, v2_.y)) + vec4<f32>(_e369));
}
