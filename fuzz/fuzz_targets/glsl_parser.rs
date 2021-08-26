#![no_main]
use libfuzzer_sys::fuzz_target;
use naga::front::glsl::{Options, Parser};

fuzz_target!(|data: (Options, String)| {
    let (options, source) = data;
    // Ensure the parser can handle potentially malformed strings without crashing.
    let mut parser = Parser::default();
    let _result = parser.parse(&options, &source);
});
