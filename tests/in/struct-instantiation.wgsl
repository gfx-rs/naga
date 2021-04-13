
struct Foo {
    a: f32;
    b: vec3<f32>;
};

[[stage(fragment)]]
fn fs_main() {
    var foo: Foo = Foo(0.0, vec3<f32>(0.0, 1.0, 42.0));
}
