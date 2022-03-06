struct AlignedWrapper {
    value: i32;
};

struct Bar {
    matrix: mat4x4<f32>;
    matrix_array: array<mat2x2<f32>,2>;
    atom: atomic<i32>;
    arr: array<vec2<u32>,2>;
    data: array<AlignedWrapper>;
};

@group(0) @binding(0) 
var<storage, read_write> bar: Bar;

fn read_from_private(foo_1: ptr<function, f32>) -> f32 {
    let _e2 = (*foo_1);
    return _e2;
}

@stage(vertex) 
fn foo_vert(@builtin(vertex_index) vi: u32) -> @builtin(position) vec4<f32> {
    var foo: f32 = 0.0;
    var c: array<i32,5>;

    let baz = foo;
    foo = 1.0;
    let matrix = bar.matrix;
    let arr = bar.arr;
    let b = bar.matrix[3][0];
    let a = bar.data[(arrayLength((&bar.data)) - 2u)].value;
    let data_pointer = (&bar.data[0].value);
    let _e27 = read_from_private((&foo));
    c = array<i32,5>(a, i32(b), 3, 4, 5);
    c[(vi + 1u)] = 42;
    let value = c[vi];
    return (matrix * vec4<f32>(vec4<i32>(value)));
}

@stage(fragment) 
fn foo_frag() -> @location(0) vec4<f32> {
    bar.matrix[1][2] = 1.0;
    bar.matrix = mat4x4<f32>(vec4<f32>(0.0), vec4<f32>(1.0), vec4<f32>(2.0), vec4<f32>(3.0));
    bar.arr = array<vec2<u32>,2>(vec2<u32>(0u), vec2<u32>(1u));
    bar.data[1].value = 1;
    return vec4<f32>(0.0);
}

@stage(compute) @workgroup_size(1, 1, 1) 
fn atomics() {
    var tmp: i32;

    let value_1 = atomicLoad((&bar.atom));
    let _e6 = atomicAdd((&bar.atom), 5);
    tmp = _e6;
    let _e9 = atomicSub((&bar.atom), 5);
    tmp = _e9;
    let _e12 = atomicAnd((&bar.atom), 5);
    tmp = _e12;
    let _e15 = atomicOr((&bar.atom), 5);
    tmp = _e15;
    let _e18 = atomicXor((&bar.atom), 5);
    tmp = _e18;
    let _e21 = atomicMin((&bar.atom), 5);
    tmp = _e21;
    let _e24 = atomicMax((&bar.atom), 5);
    tmp = _e24;
    let _e27 = atomicExchange((&bar.atom), 5);
    tmp = _e27;
    atomicStore((&bar.atom), value_1);
    return;
}
