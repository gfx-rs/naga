fn blockLexicalScope(a: bool) {
    let b = 1.0;
    {
        let a = 2;
        {
            let a = true;
        }
        let test = a == 3;
    }
    let test = b == 2.0;
}

fn ifLexicalScope(a: bool) {
    let b = 1.0;
    if (b == 1.0) {
        let a = true;
    }
    let test = b == 2.0;
}


fn loopLexicalScope(a: bool) {
    let b = 1.0;
    loop {
        let a = true;
    }
    let test = b == 2.0;
}

fn forLexicalScope(a: f32) {
    let b = false;
    for (var a = 0; a < 1; a++) {
        let b = 3.0;
    }
    let test = b == true;
}

fn whileLexicalScope(a: i32) {
    while (a > 2) {
        let b = false;
    }
    let test = a == 1;
}

fn switchLexicalScope(a: i32) {
    switch (a) {
        case 0 {
            let a = false;
        }
        case 1 {
            let a = 2.0;
        }
        default {
            let a = true;
        }
    }
    let test = a == 2;
}
