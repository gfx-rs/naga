struct VertexOutput {
};

fn main() {
    var i: i32 = 0;

    let _e2: i32 = i;
    i = 0;
    loop {
        if (!((_e2 < 1))) {
            break;
        }
        {
        }
        continuing {
            i = (_e2 + 1);
        }
    }
    return;
}

[[stage(vertex)]]
fn main1() -> VertexOutput {
    main();
    return VertexOutput();
}
