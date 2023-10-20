[numthreads(1, 1, 1)]
void main(uint3 __local_invocation_id : SV_GroupThreadID)
{
    if (all(__local_invocation_id == uint3(0u, 0u, 0u))) {
    }
    GroupMemoryBarrierWithGroupSync();
    const uint num_subgroups = (1u + WaveGetLaneCount() - 1u) / WaveGetLaneCount();
    const uint subgroup_id = (__local_invocation_id.x * 1u + __local_invocation_id.y * 1u + __local_invocation_id.z) / WaveGetLaneCount();
    const uint subgroup_size = WaveGetLaneCount();
    const uint subgroup_invocation_id = WaveGetLaneIndex();
    const uint4 _e8 = WaveActiveBallot(((subgroup_invocation_id & 1u) == 1u));
    const bool _e11 = WaveActiveAllTrue((subgroup_invocation_id != 0u));
    const bool _e14 = WaveActiveAnyTrue((subgroup_invocation_id == 0u));
    const uint _e15 = WaveActiveSum(subgroup_invocation_id);
    const uint _e16 = WaveActiveProduct(subgroup_invocation_id);
    const uint _e17 = WaveActiveMin(subgroup_invocation_id);
    const uint _e18 = WaveActiveMax(subgroup_invocation_id);
    const uint _e19 = WaveActiveBitAnd(subgroup_invocation_id);
    const uint _e20 = WaveActiveBitOr(subgroup_invocation_id);
    const uint _e21 = WaveActiveBitXor(subgroup_invocation_id);
    const uint _e22 = WavePrefixSum(subgroup_invocation_id);
    const uint _e23 = WavePrefixProduct(subgroup_invocation_id);
    const uint _e24 = subgroup_invocation_id + WavePrefixSum(subgroup_invocation_id);
    const uint _e25 = subgroup_invocation_id * WavePrefixProduct(subgroup_invocation_id);
    const uint _e26 = WaveReadLaneFirst(subgroup_invocation_id);
    const uint _e28 = WaveReadLaneAt(subgroup_invocation_id, 4u);
    const uint _e32 = WaveReadLaneAt(subgroup_invocation_id, ((subgroup_size - 1u) - subgroup_invocation_id));
    const uint _e34 = WaveReadLaneAt(subgroup_invocation_id, WaveGetLaneIndex() + 1u);
    const uint _e36 = WaveReadLaneAt(subgroup_invocation_id, WaveGetLaneIndex() - 1u);
    const uint _e39 = WaveReadLaneAt(subgroup_invocation_id, WaveGetLaneIndex() ^ (subgroup_size - 1u));
    return;
}
