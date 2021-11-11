[[block]]
struct DynamicArray {
    array_: [[stride(4)]] array<u32>;
};

[[group(0), binding(0)]]
var<storage, read_write> dynamic_array: DynamicArray;

fn f() {
    var v: vec2<i32>;

    let px: ptr<function, i32> = (&v.x);
    (*px) = 10;
    return;
}

fn index_unsized(i: i32, v_1: u32) {
    let val: u32 = dynamic_array.array_[i];
    dynamic_array.array_[i] = (val + v_1);
    return;
}

fn index_dynamic_array(i_1: i32, v_2: u32) {
    let p: ptr<storage, array<u32>, read_write> = (&dynamic_array.array_);
    let val_1: u32 = (*p)[i_1];
    (*p)[i_1] = (val_1 + v_2);
    return;
}

