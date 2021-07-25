// This snapshot tests accessing various containers, dereferencing pointers.

[[block]]
struct Bar {
	matrix: mat4x4<f32>;
	arr: [[stride(8)]] array<vec2<u32>, 2>;
	data: [[stride(4)]] array<i32>;
};

[[group(0), binding(0)]]
var<storage> bar: [[access(read_write)]] Bar;

[[stage(vertex)]]
fn foo([[builtin(vertex_index)]] vi: u32) -> [[builtin(position)]] vec4<f32> {
    var foo: f32 = 0.0;
    // We should check that backed doesn't skip this expression
    let baz: f32 = foo;
    foo = 1.0;

	let matrix = bar.matrix;
	let arr = bar.arr;

	let index = 3u;
	let b = bar.matrix[index].x;

	let a = bar.data[arrayLength(&bar.data) - 2u];

	var c = array<i32, 5>(a, i32(b), 3, 4, 5);
	c[vi + 1u] = 42;
	let value = c[vi];

	return matrix * vec4<f32>(vec4<i32>(value));
}
