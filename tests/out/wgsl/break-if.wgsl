fn breakIfEmpty() {
    loop {
        continuing {
            break if true;
        }
    }
    return;
}

fn breakIfEmptyBody(a: bool) {
    var b: bool;
    var c: bool;

    let _e9 = c;
    _ = (a == _e9);
    loop {
        continuing {
            b = a;
            let _e4 = b;
            let c_1 = (a != _e4);
            c = c_1;
            break if (a == _e9);
        }
    }
    return;
}

fn breakIf(a_1: bool) {
    var d: bool;
    var e: bool;

    let _e9 = e;
    _ = (a_1 == _e9);
    loop {
        d = a_1;
        let _e4 = d;
        let e_1 = (a_1 != _e4);
        e = e_1;
        continuing {
            break if (a_1 == _e9);
        }
    }
    return;
}

@compute @workgroup_size(1, 1, 1) 
fn main() {
    return;
}
