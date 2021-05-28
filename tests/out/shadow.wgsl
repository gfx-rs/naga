[[block]]
struct Globals {
    num_lights: vec4<u32>;
};

struct Light {
    proj: mat4x4<f32>;
    pos: vec4<f32>;
    color: vec4<f32>;
};

[[block]]
struct Lights {
    data: [[stride(96)]] array<Light>;
};

let c_ambient: vec3<f32> = vec3<f32>(0.05, 0.05, 0.05);
let c_max_lights: u32 = 10u;

[[group(0), binding(0)]]
var<uniform> u_globals: Globals;
[[group(0), binding(1)]]
var<storage> s_lights: [[access(read)]] Lights;
[[group(0), binding(2)]]
var t_shadow: texture_depth_2d_array;
[[group(0), binding(3)]]
var sampler_shadow: sampler_comparison;

fn fetch_shadow(light_id: u32, homogeneous_coords: vec4<f32>) -> f32 {
    if ((homogeneous_coords.w <= 0.0)) {
        return 1.0;
    }
    let _e26: f32 = textureSampleCompare(t_shadow, sampler_shadow, (((homogeneous_coords.xy * vec2<f32>(0.5, -0.5)) / vec2<f32>(homogeneous_coords.w)) + vec2<f32>(0.5, 0.5)), i32(light_id), (homogeneous_coords.z / homogeneous_coords.w));
    return _e26;
}

[[stage(fragment)]]
fn fs_main([[location(0), interpolate(perspective)]] raw_normal: vec3<f32>, [[location(1), interpolate(perspective)]] position: vec4<f32>) -> [[location(0)]] vec4<f32> {
    var color1: vec3<f32> = vec3<f32>(0.05, 0.05, 0.05);
    var i: u32 = 0u;

    loop {
        let _e12: u32 = i;
        let _e14: vec4<u32> = u_globals.num_lights;
        if ((_e12 >= min(_e14.x, c_max_lights))) {
            break;
        }
        let _e19: u32 = i;
        let _e21: Light = s_lights.data[_e19];
        let _e22: u32 = i;
        let _e25: f32 = fetch_shadow(_e22, (_e21.proj * position));
        let _e34: vec3<f32> = color1;
        color1 = (_e34 + ((_e25 * max(0.0, dot(normalize(raw_normal), normalize((_e21.pos.xyz - position.xyz))))) * _e21.color.xyz));
        continuing {
            let _e40: u32 = i;
            i = (_e40 + 1u);
        }
    }
    let _e43: vec3<f32> = color1;
    return vec4<f32>(_e43, 1.0);
}
