struct VertexOutput {
};

fn main() {
    var a: f32 = 1.0;

    a = f32(1);
    return;
}

[[stage(vertex)]]
fn main1() -> VertexOutput {
    main();
    return VertexOutput();
}
