use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

pub fn set_panic_hook() {
    // When the `console_error_panic_hook` feature is enabled, we can call the
    // `set_panic_hook` function at least once during initialization, and then
    // we will get better error messages if our code ever panics.
    //
    // For more details see
    // https://github.com/rustwasm/console_error_panic_hook#readme
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn wgsl2msl(input: &str) -> String {
    let module = super::front::wgsl::parse_str(&input).unwrap();

    println!("Compiled, header {:?}", module.header);

    use super::back::msl;
    let mut binding_map = msl::BindingMap::default();
    binding_map.insert(
        msl::BindSource { set: 0, binding: 0 },
        msl::BindTarget { buffer: None, texture: Some(1), sampler: None, mutable: false },
    );
    binding_map.insert(
        msl::BindSource { set: 0, binding: 1 },
        msl::BindTarget { buffer: None, texture: None, sampler: Some(1), mutable: false },
    );
    let options = msl::Options {
        binding_map: &binding_map,
    };
    msl::write_string(&module, options).unwrap()
}
