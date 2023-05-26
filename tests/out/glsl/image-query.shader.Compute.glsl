#version 310 es

precision highp float;
precision highp int;

layout(local_size_x = 8, local_size_y = 8, local_size_z = 1) in;

layout(rgba16f) uniform highp image2D _group_0_binding_0_cs;

uvec3 global = uvec3(0u);


void shader_1() {
    uvec3 _e7 = global;
    switch(int(0u)) {
        default: {
            if ((_e7.x < uvec2(imageSize(_group_0_binding_0_cs).xy).x)) {
                break;
            }
            uvec2 _e15 = _e7.xy;
            vec4 _e16 = imageLoad(_group_0_binding_0_cs, ivec2(_e15));
            float unnamed = _e16.x;
            imageStore(_group_0_binding_0_cs, ivec2(_e15), vec4(float(_e7.z), _e16.y, _e16.z, _e16.w));
            break;
        }
    }
    return;
}

void main() {
    uvec3 param = gl_GlobalInvocationID;
    global = param;
    shader_1();
}

