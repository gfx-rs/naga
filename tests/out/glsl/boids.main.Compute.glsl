#version 310 es

precision highp float;
precision highp int;

layout(local_size_x = 64, local_size_y = 1, local_size_z = 1) in;

struct Particle {
    vec2 pos;
    vec2 vel;
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
uniform SimParams_block_0Compute { SimParams _group_0_binding_0_cs; };

layout(std430) readonly buffer Particles_block_1Compute {
    Particle particles[];
} _group_0_binding_1_cs;

layout(std430) buffer Particles_block_2Compute {
    Particle particles[];
} _group_0_binding_2_cs;


void main() {
    uvec3 global_invocation_id = gl_GlobalInvocationID;
    vec2 vPos = vec2(0.0);
    vec2 vVel = vec2(0.0);
    vec2 cMass = vec2(0.0);
    vec2 cVel = vec2(0.0);
    vec2 colVel = vec2(0.0);
    int cMassCount = 0;
    int cVelCount = 0;
    vec2 pos = vec2(0.0);
    vec2 vel = vec2(0.0);
    uint i = 0u;
    uint index = global_invocation_id.x;
    if ((index >= 1500u)) {
        return;
    }
    vec2 vPos_1 = _group_0_binding_1_cs.particles[index].pos;
    vPos = vPos_1;
    vec2 vVel_1 = _group_0_binding_1_cs.particles[index].vel;
    vVel = vVel_1;
    vec2 cMass_1 = vec2(0.0, 0.0);
    cMass = cMass_1;
    vec2 cVel_1 = vec2(0.0, 0.0);
    cVel = cVel_1;
    vec2 colVel_1 = vec2(0.0, 0.0);
    colVel = colVel_1;
    cMassCount = 0;
    cVelCount = 0;
    i = 0u;
    bool loop_init = true;
    while(true) {
        if (!loop_init) {
            uint _e116 = i;
            i = (_e116 + 1u);
        }
        loop_init = false;
        uint _e35 = i;
        if ((_e35 >= 1500u)) {
            break;
        }
        uint _e39 = i;
        if ((_e39 == index)) {
            continue;
        }
        uint _e44 = i;
        vec2 _e47 = _group_0_binding_1_cs.particles[_e44].pos;
        pos = _e47;
        uint _e52 = i;
        vec2 _e55 = _group_0_binding_1_cs.particles[_e52].vel;
        vel = _e55;
        vec2 _e58 = pos;
        vec2 _e60 = vPos;
        float _e64 = _group_0_binding_0_cs.rule1Distance;
        if ((distance(_e58, _e60) < _e64)) {
            vec2 _e67 = cMass;
            vec2 _e69 = pos;
            cMass = (_e67 + _e69);
            int _e73 = cMassCount;
            cMassCount = (_e73 + 1);
        }
        vec2 _e78 = pos;
        vec2 _e80 = vPos;
        float _e84 = _group_0_binding_0_cs.rule2Distance;
        if ((distance(_e78, _e80) < _e84)) {
            vec2 _e87 = colVel;
            vec2 _e89 = pos;
            vec2 _e91 = vPos;
            colVel = (_e87 - (_e89 - _e91));
        }
        vec2 _e96 = pos;
        vec2 _e98 = vPos;
        float _e102 = _group_0_binding_0_cs.rule3Distance;
        if ((distance(_e96, _e98) < _e102)) {
            vec2 _e105 = cVel;
            vec2 _e107 = vel;
            cVel = (_e105 + _e107);
            int _e111 = cVelCount;
            cVelCount = (_e111 + 1);
        }
    }
    int _e121 = cMassCount;
    if ((_e121 > 0)) {
        vec2 _e125 = cMass;
        int _e127 = cMassCount;
        vec2 _e132 = vPos;
        cMass = ((_e125 / vec2(float(_e127))) - _e132);
    }
    int _e136 = cVelCount;
    if ((_e136 > 0)) {
        vec2 _e140 = cVel;
        int _e142 = cVelCount;
        cVel = (_e140 / vec2(float(_e142)));
    }
    vec2 _e148 = vVel;
    vec2 _e150 = cMass;
    float _e153 = _group_0_binding_0_cs.rule1Scale;
    vec2 _e157 = colVel;
    float _e160 = _group_0_binding_0_cs.rule2Scale;
    vec2 _e164 = cVel;
    float _e167 = _group_0_binding_0_cs.rule3Scale;
    vVel = (((_e148 + (_e150 * _e153)) + (_e157 * _e160)) + (_e164 * _e167));
    vec2 _e172 = vVel;
    vec2 _e175 = vVel;
    vVel = (normalize(_e172) * clamp(length(_e175), 0.0, 0.1));
    vec2 _e183 = vPos;
    vec2 _e185 = vVel;
    float _e188 = _group_0_binding_0_cs.deltaT;
    vPos = (_e183 + (_e185 * _e188));
    float _e194 = vPos.x;
    if ((_e194 < (-1.0))) {
        vPos.x = 1.0;
    }
    float _e203 = vPos.x;
    if ((_e203 > 1.0)) {
        vPos.x = (-1.0);
    }
    float _e212 = vPos.y;
    if ((_e212 < (-1.0))) {
        vPos.y = 1.0;
    }
    float _e221 = vPos.y;
    if ((_e221 > 1.0)) {
        vPos.y = (-1.0);
    }
    vec2 _e229 = vPos;
    _group_0_binding_2_cs.particles[index].pos = _e229;
    vec2 _e235 = vVel;
    _group_0_binding_2_cs.particles[index].vel = _e235;
    return;
}

