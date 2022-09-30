static const uint NUM_PARTICLES = 1500;

struct Particle {
    float2 pos;
    float2 vel;
};

struct SimParams {
    float deltaT;
    float rule1Distance;
    float rule2Distance;
    float rule3Distance;
    float rule1Scale;
    float rule2Scale;
    float rule3Scale;
};

cbuffer params : register(b0) { SimParams params; }
ByteAddressBuffer particlesSrc : register(t1);
RWByteAddressBuffer particlesDst : register(u2);

[numthreads(64, 1, 1)]
void main(uint3 global_invocation_id : SV_DispatchThreadID)
{
    float2 vPos = (float2)0;
    float2 vVel = (float2)0;
    float2 cMass = (float2)0;
    float2 cVel = (float2)0;
    float2 colVel = (float2)0;
    int cMassCount = (int)0;
    int cVelCount = (int)0;
    float2 pos = (float2)0;
    float2 vel = (float2)0;
    uint i = (uint)0;

    uint index = global_invocation_id.x;
    if ((index >= NUM_PARTICLES)) {
        return;
    }
    float2 vPos_1 = asfloat(particlesSrc.Load2(0+index*16+0));
    vPos = vPos_1;
    float2 vVel_1 = asfloat(particlesSrc.Load2(8+index*16+0));
    vVel = vVel_1;
    float2 cMass_1 = float2(0.0, 0.0);
    cMass = cMass_1;
    float2 cVel_1 = float2(0.0, 0.0);
    cVel = cVel_1;
    float2 colVel_1 = float2(0.0, 0.0);
    colVel = colVel_1;
    cMassCount = 0;
    cVelCount = 0;
    i = 0u;
    bool loop_init = true;
    while(true) {
        if (!loop_init) {
            uint _expr116 = i;
            i = (_expr116 + 1u);
        }
        loop_init = false;
        uint _expr35 = i;
        if ((_expr35 >= NUM_PARTICLES)) {
            break;
        }
        uint _expr39 = i;
        if ((_expr39 == index)) {
            continue;
        }
        uint _expr44 = i;
        float2 _expr47 = asfloat(particlesSrc.Load2(0+_expr44*16+0));
        pos = _expr47;
        uint _expr52 = i;
        float2 _expr55 = asfloat(particlesSrc.Load2(8+_expr52*16+0));
        vel = _expr55;
        float2 _expr58 = pos;
        float2 _expr60 = vPos;
        float _expr64 = params.rule1Distance;
        if ((distance(_expr58, _expr60) < _expr64)) {
            float2 _expr67 = cMass;
            float2 _expr69 = pos;
            cMass = (_expr67 + _expr69);
            int _expr73 = cMassCount;
            cMassCount = (_expr73 + 1);
        }
        float2 _expr78 = pos;
        float2 _expr80 = vPos;
        float _expr84 = params.rule2Distance;
        if ((distance(_expr78, _expr80) < _expr84)) {
            float2 _expr87 = colVel;
            float2 _expr89 = pos;
            float2 _expr91 = vPos;
            colVel = (_expr87 - (_expr89 - _expr91));
        }
        float2 _expr96 = pos;
        float2 _expr98 = vPos;
        float _expr102 = params.rule3Distance;
        if ((distance(_expr96, _expr98) < _expr102)) {
            float2 _expr105 = cVel;
            float2 _expr107 = vel;
            cVel = (_expr105 + _expr107);
            int _expr111 = cVelCount;
            cVelCount = (_expr111 + 1);
        }
    }
    int _expr121 = cMassCount;
    if ((_expr121 > 0)) {
        float2 _expr125 = cMass;
        int _expr127 = cMassCount;
        float2 _expr132 = vPos;
        cMass = ((_expr125 / (float(_expr127)).xx) - _expr132);
    }
    int _expr136 = cVelCount;
    if ((_expr136 > 0)) {
        float2 _expr140 = cVel;
        int _expr142 = cVelCount;
        cVel = (_expr140 / (float(_expr142)).xx);
    }
    float2 _expr148 = vVel;
    float2 _expr150 = cMass;
    float _expr153 = params.rule1Scale;
    float2 _expr157 = colVel;
    float _expr160 = params.rule2Scale;
    float2 _expr164 = cVel;
    float _expr167 = params.rule3Scale;
    vVel = (((_expr148 + (_expr150 * _expr153)) + (_expr157 * _expr160)) + (_expr164 * _expr167));
    float2 _expr172 = vVel;
    float2 _expr175 = vVel;
    vVel = (normalize(_expr172) * clamp(length(_expr175), 0.0, 0.1));
    float2 _expr183 = vPos;
    float2 _expr185 = vVel;
    float _expr188 = params.deltaT;
    vPos = (_expr183 + (_expr185 * _expr188));
    float _expr194 = vPos.x;
    if ((_expr194 < -1.0)) {
        vPos.x = 1.0;
    }
    float _expr203 = vPos.x;
    if ((_expr203 > 1.0)) {
        vPos.x = -1.0;
    }
    float _expr212 = vPos.y;
    if ((_expr212 < -1.0)) {
        vPos.y = 1.0;
    }
    float _expr221 = vPos.y;
    if ((_expr221 > 1.0)) {
        vPos.y = -1.0;
    }
    float2 _expr229 = vPos;
    particlesDst.Store2(0+index*16+0, asuint(_expr229));
    float2 _expr235 = vVel;
    particlesDst.Store2(8+index*16+0, asuint(_expr235));
    return;
}
