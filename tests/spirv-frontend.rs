//! Tests for the spv frontend.

#[test]
fn parse_spv_with_atomics() {
    env_logger::Builder::default()
        .is_test(true)
        .filter_level(log::LevelFilter::Trace)
        .init();
    let source = include_bytes!("in/compute_atomics.spv");
    let opts = naga::front::spv::Options::default();
    match naga::front::spv::parse_u8_slice(source, &opts) {
        Ok(_) => {}
        Err(e) => {
            panic!("{e:#?}");
        }
    }
}
