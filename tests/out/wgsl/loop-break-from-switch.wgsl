var<private> global: i32;
var<private> global_1: i32;

fn function() {
    let _e8 = global;
    loop {
        switch _e8 {
            case 0: {
                global_1 = 0;
                break;
            }
            default: {
                break;
            }
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
