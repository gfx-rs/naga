#version 300 es
#extension GL_OVR_multiview2 : require

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
    int gen_gl_ViewIndex = int(gl_ViewID_OVR);
    gen_gl_ViewIndex_1 = gen_gl_ViewIndex;
    main_1();
}

