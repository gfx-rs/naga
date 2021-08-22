
[numthreads(1, 1, 1)]
void main(uint3 global_id : SV_DispatchThreadID)
{
    DeviceMemoryBarrierWithGroupSync();
    GroupMemoryBarrierWithGroupSync();
    return;
}
