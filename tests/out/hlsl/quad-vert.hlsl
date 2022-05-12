
struct gl_PerVertex {
    float4 gl_Position : SV_Position;
    float gl_PointSize : PSIZE;
    float gl_ClipDistance[1] : SV_ClipDistance;
    float gl_CullDistance[1] : SV_CullDistance;
};

struct type_9 {
    float2 member : LOC0;
    float4 gl_Position : SV_Position;
};

typedef float ret_Constructarray1_float_[1];
ret_Constructarray1_float_ Constructarray1_float_(float arg0) {
    float ret[1] = { arg0 };
    return ret;
}

gl_PerVertex Constructgl_PerVertex(float4 arg0, float arg1, float arg2[1], float arg3[1]) {
    gl_PerVertex ret = (gl_PerVertex)0;
    ret.gl_Position = arg0;
    ret.gl_PointSize = arg1;
    ret.gl_ClipDistance = arg2;
    ret.gl_CullDistance = arg3;
    return ret;
}

static float2 v_uv = (float2)0;
static float2 a_uv_1 = (float2)0;
static gl_PerVertex perVertexStruct = Constructgl_PerVertex(float4(0.0, 0.0, 0.0, 1.0), 1.0, Constructarray1_float_(0.0), Constructarray1_float_(0.0));
static float2 a_pos_1 = (float2)0;

struct VertexOutput_main {
    float2 member : LOC0;
    float4 gl_Position : SV_Position;
};

void main_1()
{
    float2 _expr12 = a_uv_1;
    v_uv = _expr12;
    float2 _expr13 = a_pos_1;
    perVertexStruct.gl_Position = float4(_expr13.x, _expr13.y, 0.0, 1.0);
    return;
}

type_9 Constructtype_9(float2 arg0, float4 arg1) {
    type_9 ret = (type_9)0;
    ret.member = arg0;
    ret.gl_Position = arg1;
    return ret;
}

VertexOutput_main main(float2 a_uv : LOC1, float2 a_pos : LOC0)
{
    a_uv_1 = a_uv;
    a_pos_1 = a_pos;
    main_1();
    float2 _expr7 = v_uv;
    float4 _expr8 = perVertexStruct.gl_Position;
    const type_9 type_9_ = Constructtype_9(_expr7, _expr8);
    const VertexOutput_main type_9_1 = { type_9_.member, type_9_.gl_Position };
    return type_9_1;
}
