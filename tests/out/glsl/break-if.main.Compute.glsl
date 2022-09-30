#version 310 es

precision highp float;
precision highp int;

layout(local_size_x = 1, local_size_y = 1, local_size_z = 1) in;


void breakIfEmpty() {
    bool loop_init = true;
    while(true) {
        if (!loop_init) {
            if (true) {
                break;
            }
        }
        loop_init = false;
    }
    return;
}

void breakIfEmptyBody(bool a) {
    bool b = false;
    bool c = false;
    bool _e9 = c;
    bool unnamed = (a == _e9);
    bool loop_init_1 = true;
    while(true) {
        if (!loop_init_1) {
            b = a;
            bool _e4 = b;
            bool c_1 = (a != _e4);
            c = c_1;
            if (unnamed) {
                break;
            }
        }
        loop_init_1 = false;
    }
    return;
}

void breakIf(bool a_1) {
    bool d = false;
    bool e = false;
    bool _e9 = e;
    bool unnamed_1 = (a_1 == _e9);
    bool loop_init_2 = true;
    while(true) {
        if (!loop_init_2) {
            if (unnamed_1) {
                break;
            }
        }
        loop_init_2 = false;
        d = a_1;
        bool _e4 = d;
        bool e_1 = (a_1 != _e4);
        e = e_1;
    }
    return;
}

void main() {
    return;
}

