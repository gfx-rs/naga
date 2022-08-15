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

fn main_1() {
    var local: bool;
    var local_1: bool;
    var out1_: bool;
    var local_2: bool;
    var local_3: bool;
    var out2_: bool;
    var local_4: bool;
    var local_5: bool;
    var local_6: bool;
    var out3_: bool;

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
    let _e8 = local_1;
    out1_ = _e8;
    let _e10 = a();
    if _e10 {
        let _e11 = b();
        local_2 = _e11;
    } else {
        local_2 = false;
    }
    let _e14 = local_2;
    if _e14 {
        let _e15 = c();
        local_3 = _e15;
    } else {
        local_3 = false;
    }
    let _e18 = local_3;
    out2_ = _e18;
    let _e20 = a();
    if _e20 {
        local_4 = true;
    } else {
        let _e21 = b();
        local_4 = _e21;
    }
    let _e24 = local_4;
    if _e24 {
        let _e25 = c();
        if _e25 {
            local_5 = true;
        } else {
            let _e26 = d();
            local_5 = _e26;
        }
        let _e29 = local_5;
        local_6 = _e29;
    } else {
        local_6 = false;
    }
    let _e32 = local_6;
    out3_ = _e32;
    return;
}

@fragment 
fn main() {
    main_1();
    return;
}
