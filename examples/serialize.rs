use std::{env, fs};

#[path = "common.rs"]
mod common;

#[cfg(not(feature = "serialize"))]
compile_error!("this example only works when the \"serialize\" feature is enabled");

fn main() {
    env_logger::init();

    let args = env::args().collect::<Vec<_>>();

    if args.len() <= 1 || !args[2].ends_with(".ron") {
        println!("Call with <input> <output.ron>");
        return;
    }

    let module = common::load_shader_as_module(&args[1]);

    let ron = ron::ser::to_string_pretty(&module, Default::default())
        .expect("unable to serialize module");

    fs::write(&args[2], &ron).expect("unable to write to output");

    println!("module serialized to {:?}", &args[2]);
}
