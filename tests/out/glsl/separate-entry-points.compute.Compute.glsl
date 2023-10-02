#version 310 es

precision highp float;
precision highp int;

layout(local_size_x = 1, local_size_y = 1, local_size_z = 1) in;


void derivatives() {
    float x = dFdx(0.0);
    float y = dFdy(0.0);
    float width = fwidth(0.0);
}

void barriers() {
    memoryBarrierBuffer();
    barrier();
    memoryBarrierShared();
    barrier();
    return;
}

void main() {
    barriers();
    return;
}

