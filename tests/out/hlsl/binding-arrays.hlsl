
struct UniformIndex {
    uint index;
};

struct FragmentIn {
    nointerpolation uint index : LOC0;
};

Texture2D<float4> texture_array_unbounded[10] : register(t0);
Texture2D<float4> texture_array_bounded[5] : register(t0, space1);
Texture2DArray<float4> texture_array_2darray[5] : register(t0, space2);
Texture2DMS<float4> texture_array_multisampled[5] : register(t0, space3);
Texture2D<float> texture_array_depth[5] : register(t0, space4);
RWTexture2D<float4> texture_array_storage[5] : register(u0, space5);
SamplerState samp[5] : register(s0, space6);
SamplerComparisonState samp_comp[5] : register(s0, space7);
cbuffer uni : register(b0, space8) { UniformIndex uni; }

struct FragmentInput_main {
    nointerpolation uint index : LOC0;
};

int2 NagaDimensions2D(Texture2D<float4> tex)
{
    uint4 ret;
    tex.GetDimensions(0, ret.x, ret.y, ret.z);
    return ret.xy;
}

int NagaNumLayers2DArray(Texture2DArray<float4> tex)
{
    uint4 ret;
    tex.GetDimensions(0, ret.x, ret.y, ret.z, ret.w);
    return ret.w;
}

int NagaNumLevels2D(Texture2D<float4> tex)
{
    uint4 ret;
    tex.GetDimensions(0, ret.x, ret.y, ret.z);
    return ret.z;
}

int NagaMSNumSamples2D(Texture2DMS<float4> tex)
{
    uint4 ret;
    tex.GetDimensions(ret.x, ret.y, ret.z);
    return ret.z;
}

float4 main(FragmentInput_main fragmentinput_main) : SV_Target0
{
    FragmentIn fragment_in = { fragmentinput_main.index };
    int i1_ = (int)0;
    int2 i2_ = (int2)0;
    float v1_ = (float)0;
    float4 v4_ = (float4)0;

    uint uniform_index = uni.index;
    uint non_uniform_index = fragment_in.index;
    i1_ = 0;
    int2 i2_1 = (0).xx;
    i2_ = i2_1;
    v1_ = 0.0;
    float4 v4_1 = (0.0).xxxx;
    v4_ = v4_1;
    float2 uv = (0.0).xx;
    int2 pix = (0).xx;
    int2 _expr24 = i2_;
    i2_ = (_expr24 + NagaDimensions2D(texture_array_unbounded[0]));
    int2 _expr30 = i2_;
    i2_ = (_expr30 + NagaDimensions2D(texture_array_unbounded[uniform_index]));
    int2 _expr36 = i2_;
    i2_ = (_expr36 + NagaDimensions2D(texture_array_unbounded[NonUniformResourceIndex(non_uniform_index)]));
    float4 _expr44 = texture_array_bounded[0].Gather(samp[0], uv);
    float4 _expr46 = v4_;
    v4_ = (_expr46 + _expr44);
    float4 _expr52 = texture_array_bounded[uniform_index].Gather(samp[uniform_index], uv);
    float4 _expr54 = v4_;
    v4_ = (_expr54 + _expr52);
    float4 _expr60 = texture_array_bounded[NonUniformResourceIndex(non_uniform_index)].Gather(samp[NonUniformResourceIndex(non_uniform_index)], uv);
    float4 _expr62 = v4_;
    v4_ = (_expr62 + _expr60);
    float4 _expr71 = texture_array_depth[0].GatherCmp(samp_comp[0], uv, 0.0);
    float4 _expr73 = v4_;
    v4_ = (_expr73 + _expr71);
    float4 _expr80 = texture_array_depth[uniform_index].GatherCmp(samp_comp[uniform_index], uv, 0.0);
    float4 _expr82 = v4_;
    v4_ = (_expr82 + _expr80);
    float4 _expr89 = texture_array_depth[NonUniformResourceIndex(non_uniform_index)].GatherCmp(samp_comp[NonUniformResourceIndex(non_uniform_index)], uv, 0.0);
    float4 _expr91 = v4_;
    v4_ = (_expr91 + _expr89);
    float4 _expr97 = texture_array_unbounded[0].Load(int3(pix, 0));
    float4 _expr99 = v4_;
    v4_ = (_expr99 + _expr97);
    float4 _expr104 = texture_array_unbounded[uniform_index].Load(int3(pix, 0));
    float4 _expr106 = v4_;
    v4_ = (_expr106 + _expr104);
    float4 _expr111 = texture_array_unbounded[NonUniformResourceIndex(non_uniform_index)].Load(int3(pix, 0));
    float4 _expr113 = v4_;
    v4_ = (_expr113 + _expr111);
    int _expr120 = i1_;
    i1_ = (_expr120 + NagaNumLayers2DArray(texture_array_2darray[0]));
    int _expr126 = i1_;
    i1_ = (_expr126 + NagaNumLayers2DArray(texture_array_2darray[uniform_index]));
    int _expr132 = i1_;
    i1_ = (_expr132 + NagaNumLayers2DArray(texture_array_2darray[NonUniformResourceIndex(non_uniform_index)]));
    int _expr139 = i1_;
    i1_ = (_expr139 + NagaNumLevels2D(texture_array_bounded[0]));
    int _expr145 = i1_;
    i1_ = (_expr145 + NagaNumLevels2D(texture_array_bounded[uniform_index]));
    int _expr151 = i1_;
    i1_ = (_expr151 + NagaNumLevels2D(texture_array_bounded[NonUniformResourceIndex(non_uniform_index)]));
    int _expr158 = i1_;
    i1_ = (_expr158 + NagaMSNumSamples2D(texture_array_multisampled[0]));
    int _expr164 = i1_;
    i1_ = (_expr164 + NagaMSNumSamples2D(texture_array_multisampled[uniform_index]));
    int _expr170 = i1_;
    i1_ = (_expr170 + NagaMSNumSamples2D(texture_array_multisampled[NonUniformResourceIndex(non_uniform_index)]));
    float4 _expr178 = texture_array_bounded[0].Sample(samp[0], uv);
    float4 _expr180 = v4_;
    v4_ = (_expr180 + _expr178);
    float4 _expr186 = texture_array_bounded[uniform_index].Sample(samp[uniform_index], uv);
    float4 _expr188 = v4_;
    v4_ = (_expr188 + _expr186);
    float4 _expr194 = texture_array_bounded[NonUniformResourceIndex(non_uniform_index)].Sample(samp[NonUniformResourceIndex(non_uniform_index)], uv);
    float4 _expr196 = v4_;
    v4_ = (_expr196 + _expr194);
    float4 _expr205 = texture_array_bounded[0].SampleBias(samp[0], uv, 0.0);
    float4 _expr207 = v4_;
    v4_ = (_expr207 + _expr205);
    float4 _expr214 = texture_array_bounded[uniform_index].SampleBias(samp[uniform_index], uv, 0.0);
    float4 _expr216 = v4_;
    v4_ = (_expr216 + _expr214);
    float4 _expr223 = texture_array_bounded[NonUniformResourceIndex(non_uniform_index)].SampleBias(samp[NonUniformResourceIndex(non_uniform_index)], uv, 0.0);
    float4 _expr225 = v4_;
    v4_ = (_expr225 + _expr223);
    float _expr234 = texture_array_depth[0].SampleCmp(samp_comp[0], uv, 0.0);
    float _expr236 = v1_;
    v1_ = (_expr236 + _expr234);
    float _expr243 = texture_array_depth[uniform_index].SampleCmp(samp_comp[uniform_index], uv, 0.0);
    float _expr245 = v1_;
    v1_ = (_expr245 + _expr243);
    float _expr252 = texture_array_depth[NonUniformResourceIndex(non_uniform_index)].SampleCmp(samp_comp[NonUniformResourceIndex(non_uniform_index)], uv, 0.0);
    float _expr254 = v1_;
    v1_ = (_expr254 + _expr252);
    float _expr263 = texture_array_depth[0].SampleCmpLevelZero(samp_comp[0], uv, 0.0);
    float _expr265 = v1_;
    v1_ = (_expr265 + _expr263);
    float _expr272 = texture_array_depth[uniform_index].SampleCmpLevelZero(samp_comp[uniform_index], uv, 0.0);
    float _expr274 = v1_;
    v1_ = (_expr274 + _expr272);
    float _expr281 = texture_array_depth[NonUniformResourceIndex(non_uniform_index)].SampleCmpLevelZero(samp_comp[NonUniformResourceIndex(non_uniform_index)], uv, 0.0);
    float _expr283 = v1_;
    v1_ = (_expr283 + _expr281);
    float4 _expr291 = texture_array_bounded[0].SampleGrad(samp[0], uv, uv, uv);
    float4 _expr293 = v4_;
    v4_ = (_expr293 + _expr291);
    float4 _expr299 = texture_array_bounded[uniform_index].SampleGrad(samp[uniform_index], uv, uv, uv);
    float4 _expr301 = v4_;
    v4_ = (_expr301 + _expr299);
    float4 _expr307 = texture_array_bounded[NonUniformResourceIndex(non_uniform_index)].SampleGrad(samp[NonUniformResourceIndex(non_uniform_index)], uv, uv, uv);
    float4 _expr309 = v4_;
    v4_ = (_expr309 + _expr307);
    float4 _expr318 = texture_array_bounded[0].SampleLevel(samp[0], uv, 0.0);
    float4 _expr320 = v4_;
    v4_ = (_expr320 + _expr318);
    float4 _expr327 = texture_array_bounded[uniform_index].SampleLevel(samp[uniform_index], uv, 0.0);
    float4 _expr329 = v4_;
    v4_ = (_expr329 + _expr327);
    float4 _expr336 = texture_array_bounded[NonUniformResourceIndex(non_uniform_index)].SampleLevel(samp[NonUniformResourceIndex(non_uniform_index)], uv, 0.0);
    float4 _expr338 = v4_;
    v4_ = (_expr338 + _expr336);
    float4 _expr344 = v4_;
    texture_array_storage[0][pix] = _expr344;
    float4 _expr348 = v4_;
    texture_array_storage[uniform_index][pix] = _expr348;
    float4 _expr352 = v4_;
    texture_array_storage[NonUniformResourceIndex(non_uniform_index)][pix] = _expr352;
    int2 _expr354 = i2_;
    int _expr356 = i1_;
    float2 v2_ = float2((_expr354 + (_expr356).xx));
    float4 _expr361 = v4_;
    float _expr369 = v1_;
    return ((_expr361 + float4(v2_.x, v2_.y, v2_.x, v2_.y)) + (_expr369).xxxx);
}
