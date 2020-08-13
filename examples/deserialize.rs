use std::{env, fs, path::Path};

#[cfg(not(feature = "deserialize"))]
compile_error!("this example only works when the \"deserialize\" feature is enabled");

fn module_to_shader(dest: impl AsRef<Path>, module: naga::Module) {
    let dest = dest.as_ref();
    match dest
        .extension()
        .expect("Output has no extension?")
        .to_str()
        .unwrap()
    {
        "metal" => {
            use naga::back::msl;
            let binding_map = msl::BindingMap::default();
            let options = msl::Options {
                binding_map: &binding_map,
            };

            let msl = msl::write_string(&module, options).unwrap();
            fs::write(dest, msl).unwrap();
        }
        #[cfg(feature = "spirv")]
        "spv" => {
            use naga::back::spv;

            let spv = spv::Writer::new(&module.header, spv::WriterFlags::NONE).write(&module);

            let bytes = spv
                .iter()
                .fold(Vec::with_capacity(spv.len() * 4), |mut v, w| {
                    v.extend_from_slice(&w.to_le_bytes());
                    v
                });

            fs::write(dest, bytes.as_slice()).unwrap();
        }
        #[cfg(feature = "glsl-out")]
        "vert" | "frag" => {
            use naga::back::glsl;

            let mut file = fs::OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open(dest)
                .unwrap();

            glsl::write(&module, &mut file).unwrap();
        }
        other => {
            panic!("Unknown output extension: {}", other);
        }
    }
}

fn main() {
    env_logger::init();

    let args = env::args().collect::<Vec<_>>();

    if args.len() < 2 || !args[1].ends_with(".ron") {
        println!("Call with <input.ron> <output>");
        return;
    }

    let module = ron::de::from_str(&fs::read_to_string(&args[1]).expect("unable to read input"))
        .expect("unable to deserialize input");

    module_to_shader(&args[2], module);

    println!("module deserialized to {:?}", &args[2]);
}
