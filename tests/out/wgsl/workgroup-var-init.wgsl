struct WStruct {
    @size(2048) arr: array<u32, 512>,
    @size(4) atom: atomic<i32>,
    @size(256) atom_arr: array<array<atomic<i32>, 8>, 8>,
}

var<workgroup> w_mem: WStruct;
@group(0) @binding(0) 
var<storage, read_write> output: array<u32, 512>;

@compute @workgroup_size(1, 1, 1) 
fn main() {
    let _e3 = w_mem.arr;
    output = _e3;
    return;
}
