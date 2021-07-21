#version 300 es

precision highp float;
precision highp int;

uniform highp sampler2D _group_0_binding_0;

smooth in vec2 _vs2fs_location0;
layout(location = 0) out vec4 _fs2p_location0;

void main() {
    vec2 uv1 = _vs2fs_location0;
    vec4 color = texture(_group_0_binding_0, vec2(uv1));
    if ((color.w == 0.0)) {
        discard;
    }
    vec4 premultiplied = (color.w * color);
    _fs2p_location0 = premultiplied;
    return;
}

