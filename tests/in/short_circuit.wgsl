fn a() -> bool { return true; }
fn b() -> bool { return true; }
fn c() -> bool { return true; }
fn d() -> bool { return true; }

@compute @workgroup_size(1)
fn main() {
    _ = a() || b() || c();
    _ = a() && b() && c();
    _ = (a() || b()) && (c() || d());
}
