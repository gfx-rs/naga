var<private> gl_ViewIndex_1: i32;

fn main_1() {
    var view_index: i32;

    let _e6 = gl_ViewIndex_1;
    view_index = _e6;
    return;
}

@fragment 
fn main(@builtin(view_index) gl_ViewIndex: i32) {
    gl_ViewIndex_1 = gl_ViewIndex;
    main_1();
}
