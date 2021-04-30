[[block]]
struct Foo {
    data: [[stride(4)]] array<u32>;
};

[[group(0), binding(0)]]
var<storage> foo: [[access(read_write)]] Foo;

[[stage(compute), workgroup_size(1)]]
fn main() {
    foo.data[arrayLength(&foo.data) - 1u] = 0u;
}
