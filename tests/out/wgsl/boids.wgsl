struct Particle {
    pos: vec2<f32>,
    vel: vec2<f32>,
}

struct SimParams {
    deltaT: f32,
    rule1Distance: f32,
    rule2Distance: f32,
    rule3Distance: f32,
    rule1Scale: f32,
    rule2Scale: f32,
    rule3Scale: f32,
}

struct Particles {
    particles: array<Particle>,
}

const NUM_PARTICLES: u32 = 1500u;

@group(0) @binding(0) 
var<uniform> params: SimParams;
@group(0) @binding(1) 
var<storage> particlesSrc: Particles;
@group(0) @binding(2) 
var<storage, read_write> particlesDst: Particles;

@compute @workgroup_size(64, 1, 1) 
fn main(@builtin(global_invocation_id) global_invocation_id: vec3<u32>) {
    var vPos: vec2<f32>;
    var vVel: vec2<f32>;
    var cMass: vec2<f32>;
    var cVel: vec2<f32>;
    var colVel: vec2<f32>;
    var cMassCount: i32;
    var cVelCount: i32;
    var pos: vec2<f32>;
    var vel: vec2<f32>;
    var i: u32;

    let index = global_invocation_id.x;
    if (index >= NUM_PARTICLES) {
        return;
    }
    let vPos_1 = particlesSrc.particles[index].pos;
    vPos = vPos_1;
    let vVel_1 = particlesSrc.particles[index].vel;
    vVel = vVel_1;
    let cMass_1 = vec2<f32>(0.0, 0.0);
    cMass = cMass_1;
    let cVel_1 = vec2<f32>(0.0, 0.0);
    cVel = cVel_1;
    let colVel_1 = vec2<f32>(0.0, 0.0);
    colVel = colVel_1;
    cMassCount = 0;
    cVelCount = 0;
    i = 0u;
    loop {
        let _e35 = i;
        if (_e35 >= NUM_PARTICLES) {
            break;
        }
        let _e39 = i;
        if (_e39 == index) {
            continue;
        }
        let _e44 = i;
        let _e47 = particlesSrc.particles[_e44].pos;
        pos = _e47;
        let _e52 = i;
        let _e55 = particlesSrc.particles[_e52].vel;
        vel = _e55;
        let _e58 = pos;
        let _e60 = vPos;
        let _e64 = params.rule1Distance;
        if (distance(_e58, _e60) < _e64) {
            let _e67 = cMass;
            let _e69 = pos;
            cMass = (_e67 + _e69);
            let _e73 = cMassCount;
            cMassCount = (_e73 + 1);
        }
        let _e78 = pos;
        let _e80 = vPos;
        let _e84 = params.rule2Distance;
        if (distance(_e78, _e80) < _e84) {
            let _e87 = colVel;
            let _e89 = pos;
            let _e91 = vPos;
            colVel = (_e87 - (_e89 - _e91));
        }
        let _e96 = pos;
        let _e98 = vPos;
        let _e102 = params.rule3Distance;
        if (distance(_e96, _e98) < _e102) {
            let _e105 = cVel;
            let _e107 = vel;
            cVel = (_e105 + _e107);
            let _e111 = cVelCount;
            cVelCount = (_e111 + 1);
        }
        continuing {
            let _e116 = i;
            i = (_e116 + 1u);
        }
    }
    let _e121 = cMassCount;
    if (_e121 > 0) {
        let _e125 = cMass;
        let _e127 = cMassCount;
        let _e132 = vPos;
        cMass = ((_e125 / vec2<f32>(f32(_e127))) - _e132);
    }
    let _e136 = cVelCount;
    if (_e136 > 0) {
        let _e140 = cVel;
        let _e142 = cVelCount;
        cVel = (_e140 / vec2<f32>(f32(_e142)));
    }
    let _e148 = vVel;
    let _e150 = cMass;
    let _e153 = params.rule1Scale;
    let _e157 = colVel;
    let _e160 = params.rule2Scale;
    let _e164 = cVel;
    let _e167 = params.rule3Scale;
    vVel = (((_e148 + (_e150 * _e153)) + (_e157 * _e160)) + (_e164 * _e167));
    let _e172 = vVel;
    let _e175 = vVel;
    vVel = (normalize(_e172) * clamp(length(_e175), 0.0, 0.1));
    let _e183 = vPos;
    let _e185 = vVel;
    let _e188 = params.deltaT;
    vPos = (_e183 + (_e185 * _e188));
    let _e194 = vPos.x;
    if (_e194 < -(1.0)) {
        vPos.x = 1.0;
    }
    let _e203 = vPos.x;
    if (_e203 > 1.0) {
        vPos.x = -(1.0);
    }
    let _e212 = vPos.y;
    if (_e212 < -(1.0)) {
        vPos.y = 1.0;
    }
    let _e221 = vPos.y;
    if (_e221 > 1.0) {
        vPos.y = -(1.0);
    }
    let _e229 = vPos;
    particlesDst.particles[index].pos = _e229;
    let _e235 = vVel;
    particlesDst.particles[index].vel = _e235;
    return;
}
