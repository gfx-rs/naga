#version 310 es

precision highp float;
precision highp int;

layout(local_size_x = 1, local_size_y = 1, local_size_z = 1) in;


bool a() {
    return true;
}

bool b() {
    return true;
}

bool c() {
    return true;
}

bool d() {
    return true;
}

void main() {
    bool local = false;
    bool local_1 = false;
    bool local_2 = false;
    bool local_3 = false;
    bool local_4 = false;
    bool local_5 = false;
    bool local_6 = false;
    bool _e0 = a();
    if (_e0) {
        local = true;
    } else {
        bool _e1 = b();
        local = _e1;
    }
    bool _e4 = local;
    if (_e4) {
        local_1 = true;
    } else {
        bool _e5 = c();
        local_1 = _e5;
    }
    bool unnamed = local_1;
    bool _e9 = a();
    if (_e9) {
        bool _e10 = b();
        local_2 = _e10;
    } else {
        local_2 = false;
    }
    bool _e13 = local_2;
    if (_e13) {
        bool _e14 = c();
        local_3 = _e14;
    } else {
        local_3 = false;
    }
    bool unnamed_1 = local_3;
    bool _e18 = a();
    if (_e18) {
        local_4 = true;
    } else {
        bool _e19 = b();
        local_4 = _e19;
    }
    bool _e22 = local_4;
    if (_e22) {
        bool _e23 = c();
        if (_e23) {
            local_5 = true;
        } else {
            bool _e24 = d();
            local_5 = _e24;
        }
        bool _e27 = local_5;
        local_6 = _e27;
    } else {
        local_6 = false;
    }
    bool unnamed_2 = local_6;
}

