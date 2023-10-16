fn main_1() {
    debugPrintf("%d",42);
    return;
}

@compute @workgroup_size(1, 1, 1) 
fn main() {
    main_1();
}
