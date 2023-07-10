var<private> global: i32;
var<private> global_1: i32;

fn function() {
    var local: bool = false;

    let _e8 = global;
    loop {
        switch _e8 {
            case 0: {
                global_1 = 0;
                local = true;
                break;
            }
            default: {
                break;
            }
        }
        let _e11 = local;
        if _e11 {
            break;
        }
        global_1 = -9;
        break;
    }
    return;
}

@fragment 
fn main(@location(0) @interpolate(flat) param: i32) -> @location(0) i32 {
    global = param;
    function();
    let _e3 = global_1;
    return _e3;
}
