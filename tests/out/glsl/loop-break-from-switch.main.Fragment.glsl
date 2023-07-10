#version 310 es

precision highp float;
precision highp int;

int global = 0;

int global_1 = 0;

layout(location = 0) flat in int _vs2fs_location0;
layout(location = 0) out int _fs2p_location0;

void function() {
    bool local = false;
    int _e8 = global;
    while(true) {
        switch(_e8) {
            case 0: {
                global_1 = 0;
                local = true;
                break;
            }
            default: {
                break;
            }
        }
        bool _e11 = local;
        if (_e11) {
            break;
        }
        global_1 = -9;
        break;
    }
    return;
}

void main() {
    int param = _vs2fs_location0;
    global = param;
    function();
    int _e3 = global_1;
    _fs2p_location0 = _e3;
    return;
}

