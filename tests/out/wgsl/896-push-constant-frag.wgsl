struct PushConstants {
    @size(4) example: f32,
}

var<push_constant> c: PushConstants;

fn main_1() {
    return;
}

@fragment 
fn main() {
    main_1();
    return;
}
