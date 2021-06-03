[[block]]
struct PushConstants {
    example: f32;
};

struct VertexOutput {
};

var<push_constant> c: PushConstants;

fn main() {
}

[[stage(vertex)]]
fn main1() -> VertexOutput {
    main();
    return VertexOutput();
}
