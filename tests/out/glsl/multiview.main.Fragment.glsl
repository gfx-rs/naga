#version 310 es
#extension GL_EXT_multiview : require

precision highp float;
precision highp int;

int gen_gl_ViewIndex_1 = 0;


void main_1() {
    int view_index = 0;
    int _e6 = gen_gl_ViewIndex_1;
    view_index = _e6;
    return;
}

void main() {
    int gen_gl_ViewIndex = gl_ViewIndex;
    gen_gl_ViewIndex_1 = gen_gl_ViewIndex;
    main_1();
}

