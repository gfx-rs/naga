#version 310 es

precision highp float;
precision highp int;


void main() {
    float _e0 = 1.0;
    vec4 v = vec4(0.0);
    float a = degrees(_e0);
    float b = radians(_e0);
    vec4 c = degrees(v);
    vec4 d = radians(v);
}

