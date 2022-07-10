
struct PushConstants {
    float multiplier;
};

struct FragmentIn {
    float4 color : LOC0;
};

ConstantBuffer<PushConstants> pc: register(b0);

struct FragmentInput_main {
    float4 color : LOC0;
};

float4 main(FragmentInput_main fragmentinput_main) : SV_Target0
{
    FragmentIn in_ = { fragmentinput_main.color };
    float _expr4 = pc.multiplier;
    return (in_.color * _expr4);
}
