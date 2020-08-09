#version 450 core
layout(location=0) in vec2 a_pos;
layout(location=1) in vec2 a_uv;
layout(location=0) out vec2 v_uv;
layout(location=0) in vec2 a_uv;
layout(location=0) out vec4 o_color;
void main_vert() {
gl_position = vec4((1.2 * a_pos),0,1);
return  ;
}
void main_frag() {
o_color = vec4(1,0,0,1);
return  ;
}
