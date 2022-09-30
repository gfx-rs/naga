struct PrimeIndices {
    data: array<u32>,
}

@group(0) @binding(0) 
var<storage, read_write> v_indices: PrimeIndices;

fn collatz_iterations(n_base: u32) -> u32 {
    var n: u32;
    var i: u32;

    n = n_base;
    i = 0u;
    loop {
        let _e5 = n;
        if (_e5 > 1u) {
        } else {
            break;
        }
        {
            let _e9 = n;
            if ((_e9 % 2u) == 0u) {
                let _e15 = n;
                n = (_e15 / 2u);
            } else {
                {
                    let _e21 = n;
                    n = ((3u * _e21) + 1u);
                }
            }
            let _e27 = i;
            i = (_e27 + 1u);
        }
    }
    let _e32 = i;
    return _e32;
}

@compute @workgroup_size(1, 1, 1) 
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let _e6 = v_indices.data[global_id.x];
    let _e0 = collatz_iterations(_e6);
    v_indices.data[global_id.x] = _e0;
    return;
}
