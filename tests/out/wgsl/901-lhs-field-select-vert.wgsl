fn main_1() {
    var a: vec4<f32> = vec4<f32>(1.0, 1.0, 1.0, 1.0);

    _ = vec4<f32>(1.0);
    a.x = 2.0;
    return;
}

@vertex 
fn main() {
    main_1();
    return;
}
