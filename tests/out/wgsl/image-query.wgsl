@group(0) @binding(0) 
var output_tex: texture_storage_2d<rgba16float,read_write>;
var<private> global: vec3<u32>;

fn shader_1() {
    let _e7 = global;
    switch bitcast<i32>(0u) {
        default: {
            let _e9 = textureDimensions(output_tex);
            if (_e7.x < _e9.x) {
                break;
            }
            let _e15 = _e7.xy;
            let _e16 = textureLoad(output_tex, _e15);
            _ = _e16.x;
            textureStore(output_tex, _e15, vec4<f32>(f32(_e7.z), _e16.y, _e16.z, _e16.w));
            break;
        }
    }
    return;
}

@compute @workgroup_size(8, 8, 1) 
fn shader(@builtin(global_invocation_id) param: vec3<u32>) {
    global = param;
    shader_1();
}
