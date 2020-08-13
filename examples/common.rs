use naga::Module;
#[cfg(feature = "glsl")]
use std::env;
use std::{fs, path::Path};

pub fn load_shader_as_module(source: impl AsRef<Path>) -> Module {
    #[cfg(any(feature = "glsl", feature = "glsl-new"))]
    let prefer_glsl_new =
        !cfg!(feature = "glsl") || env::var("PREFER_GLSL_NEW").unwrap_or_default() == "1";

    let source = source.as_ref();

    match source
        .extension()
        .expect("Input has no extension?")
        .to_str()
        .unwrap()
    {
        #[cfg(feature = "spirv")]
        "spv" => {
            let input = fs::read(&args[1]).unwrap();
            naga::front::spv::parse_u8_slice(&input).unwrap()
        }
        "wgsl" => {
            let input = fs::read_to_string(source).unwrap();
            naga::front::wgsl::parse_str(&input).unwrap()
        }
        #[cfg(any(feature = "glsl", feature = "glsl-new"))]
        "vert" => {
            let input = fs::read_to_string(source).unwrap();
            let mut module: Option<naga::Module> = None;
            if prefer_glsl_new {
                #[cfg(feature = "glsl-new")]
                {
                    module = Some(
                        naga::front::glsl_new::parse_str(
                            &input,
                            "main".to_string(),
                            naga::ShaderStage::Vertex,
                        )
                        .unwrap(),
                    )
                }
            }
            if module.is_none() {
                #[cfg(feature = "glsl")]
                {
                    module = Some(
                        naga::front::glsl::parse_str(
                            &input,
                            "main".to_string(),
                            naga::ShaderStage::Vertex,
                        )
                        .unwrap(),
                    )
                }
            }
            module.unwrap()
        }
        #[cfg(feature = "glsl")]
        "frag" => {
            let input = fs::read_to_string(source).unwrap();
            naga::front::glsl::parse_str(&input, "main".to_string(), naga::ShaderStage::Fragment)
                .unwrap()
        }
        #[cfg(feature = "glsl")]
        "comp" => {
            let input = fs::read_to_string(source).unwrap();
            naga::front::glsl::parse_str(&input, "main".to_string(), naga::ShaderStage::Compute)
                .unwrap()
        }
        other => panic!("Unknown input extension: {}", other),
    }
}

// This allows treating the framework as a standalone example,
// thus avoiding listing the example names in `Cargo.toml`.
#[allow(dead_code)]
fn main() {}
