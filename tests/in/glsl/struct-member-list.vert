struct LightScatteringParams {
    float BetaRay, BetaMie[3], HGg, DistanceMul[4], BlendCoeff;
    vec3 SunDirection, SunColor;
};

void main() {
   gl_Position = vec4(1.0, 1.0, 1.0, 1.0);
}

