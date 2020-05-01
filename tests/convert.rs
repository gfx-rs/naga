fn load_wgsl(name: &str) -> naga::Module {
    let path = format!("{}/test-data/{}.wgsl", env!("CARGO_MANIFEST_DIR"), name);
    let input = std::fs::read_to_string(path).unwrap();
    naga::front::wgsl::parse_str(&input).unwrap()
}

#[test]
fn convert_quad() {
    let module = load_wgsl("quad");
    {
        use naga::back::msl;
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
        msl::write_string(&module, options).unwrap();
    }
}

#[test]
fn convert_boids() {
    let module = load_wgsl("boids");
    {
        use naga::back::msl;
        let mut binding_map = msl::BindingMap::default();
        binding_map.insert(
            msl::BindSource { set: 0, binding: 0 },
            msl::BindTarget { buffer: Some(0), texture: None, sampler: None, mutable: false },
        );
        binding_map.insert(
            msl::BindSource { set: 0, binding: 1 },
            msl::BindTarget { buffer: Some(1), texture: None, sampler: Some(1), mutable: false },
        );
        binding_map.insert(
            msl::BindSource { set: 0, binding: 2 },
            msl::BindTarget { buffer: Some(2), texture: None, sampler: Some(1), mutable: false },
        );
        let options = msl::Options {
            binding_map: &binding_map,
        };
        msl::write_string(&module, options).unwrap();
    }
}

#[test]
#[cfg(all(feature = "serialize", feature = "deserialize"))]
fn boids_serde_round_trip() {
    let module = load_wgsl("boids");
    let module_ser = ron::ser::to_string(&module).unwrap();
    let module_de = ron::de::from_str::<naga::Module>(&module_ser).unwrap();
    let module_ser_ser = ron::ser::to_string(&module_de).unwrap();
    assert_eq!(module_ser, module_ser_ser);
}
