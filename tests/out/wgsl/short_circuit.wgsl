fn a() -> bool {
    return true;
}

fn b() -> bool {
    return true;
}

fn c() -> bool {
    return true;
}

fn d() -> bool {
    return true;
}

@compute @workgroup_size(1, 1, 1) 
fn main() {
    var local: bool;
    var local_1: bool;
    var local_2: bool;
    var local_3: bool;
    var local_4: bool;
    var local_5: bool;
    var local_6: bool;

    let _e0 = a();
    if _e0 {
        local = true;
    } else {
        let _e1 = b();
        local = _e1;
    }
    let _e4 = local;
    if _e4 {
        local_1 = true;
    } else {
        let _e5 = c();
        local_1 = _e5;
    }
    _ = local_1;
    let _e9 = a();
    if _e9 {
        let _e10 = b();
        local_2 = _e10;
    } else {
        local_2 = false;
    }
    let _e13 = local_2;
    if _e13 {
        let _e14 = c();
        local_3 = _e14;
    } else {
        local_3 = false;
    }
    _ = local_3;
    let _e18 = a();
    if _e18 {
        local_4 = true;
    } else {
        let _e19 = b();
        local_4 = _e19;
    }
    let _e22 = local_4;
    if _e22 {
        let _e23 = c();
        if _e23 {
            local_5 = true;
        } else {
            let _e24 = d();
            local_5 = _e24;
        }
        let _e27 = local_5;
        local_6 = _e27;
    } else {
        local_6 = false;
    }
    _ = local_6;
}
