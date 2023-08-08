struct OtherStruct {
    data: vec2<f32>,
}

struct MyBuffer {
    data: vec4<f32>,
    sub_buffer: ptr<buffer, OtherStruct, 8>, // Custom alignment = 8
}

// TODO: recursive references
// struct MyBuffer2 {
//     data: f32,
//     next: ptr<buffer, MyBuffer2>,
// }

struct PushConstants {
    buf1: ptr<buffer, MyBuffer>,    // Default alignment = 16
    four_bytes: u32,                // Test struct member offset
    buf2: ptr<buffer, MyBuffer, 4>, // Custom alignment = 4
}
var<push_constant> pc: PushConstants;

@fragment
fn main() -> @location(0) vec4<f32> {
    var d1 = (*pc.buf1).data;
    var d2 = (*(*pc.buf2).sub_buffer).data;
    return d1 + vec4<f32>(d2, 0.0, 0.0);
}
