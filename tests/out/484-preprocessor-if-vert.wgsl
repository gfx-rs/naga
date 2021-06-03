struct VertexOutput {
};

fn main() {
    return;
}

[[stage(vertex)]]
fn main1() -> VertexOutput {
    main();
    return VertexOutput();
}
