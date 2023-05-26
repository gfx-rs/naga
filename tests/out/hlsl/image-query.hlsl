
RWTexture2D<float4> output_tex : register(u0);
static uint3 global = (uint3)0;

uint2 NagaRWDimensions2D(RWTexture2D<float4> tex)
{
    uint4 ret;
    tex.GetDimensions(ret.x, ret.y);
    return ret.xy;
}

void shader_1()
{
    uint3 _expr7 = global;
    switch(asint(0u)) {
        default: {
            if ((_expr7.x < NagaRWDimensions2D(output_tex).x)) {
                break;
            }
            uint2 _expr15 = _expr7.xy;
            float4 _expr16 = output_tex.Load(_expr15);
            float unnamed = _expr16.x;
            output_tex[_expr15] = float4(float(_expr7.z), _expr16.y, _expr16.z, _expr16.w);
            break;
        }
    }
    return;
}

[numthreads(8, 8, 1)]
void shader(uint3 param : SV_DispatchThreadID)
{
    global = param;
    shader_1();
}
