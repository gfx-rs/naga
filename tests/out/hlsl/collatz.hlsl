
RWByteAddressBuffer v_indices : register(u0);

uint collatz_iterations(uint n_base)
{
    uint n = (uint)0;
    uint i = (uint)0;

    n = n_base;
    i = 0u;
    while(true) {
        uint _expr5 = n;
        if ((_expr5 > 1u)) {
        } else {
            break;
        }
        {
            uint _expr9 = n;
            if (((_expr9 % 2u) == 0u)) {
                uint _expr15 = n;
                n = (_expr15 / 2u);
            } else {
                {
                    uint _expr21 = n;
                    n = ((3u * _expr21) + 1u);
                }
            }
            uint _expr27 = i;
            i = (_expr27 + 1u);
        }
    }
    uint _expr32 = i;
    return _expr32;
}

[numthreads(1, 1, 1)]
void main(uint3 global_id : SV_DispatchThreadID)
{
    uint _expr6 = asuint(v_indices.Load(global_id.x*4+0));
    const uint _e0 = collatz_iterations(_expr6);
    v_indices.Store(global_id.x*4+0, asuint(_e0));
    return;
}
