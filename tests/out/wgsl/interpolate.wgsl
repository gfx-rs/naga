struct FragmentInput {
    @builtin(position) @size(16) position: vec4<f32>,
    @location(0) @interpolate(flat) @size(4) _flat: u32,
    @location(1) @interpolate(linear) @size(4) _linear: f32,
    @location(2) @interpolate(linear, centroid) @size(8) linear_centroid: vec2<f32>,
    @location(3) @interpolate(linear, sample) @size(16) linear_sample: vec3<f32>,
    @location(4) @size(16) perspective: vec4<f32>,
    @location(5) @interpolate(perspective, centroid) @size(4) perspective_centroid: f32,
    @location(6) @interpolate(perspective, sample) @size(12) perspective_sample: f32,
}

@vertex 
fn vert_main() -> FragmentInput {
    var out: FragmentInput;

    out.position = vec4<f32>(2.0, 4.0, 5.0, 6.0);
    out._flat = 8u;
    out._linear = 27.0;
    out.linear_centroid = vec2<f32>(64.0, 125.0);
    out.linear_sample = vec3<f32>(216.0, 343.0, 512.0);
    out.perspective = vec4<f32>(729.0, 1000.0, 1331.0, 1728.0);
    out.perspective_centroid = 2197.0;
    out.perspective_sample = 2744.0;
    let _e30 = out;
    return _e30;
}

@fragment 
fn frag_main(val: FragmentInput) {
    return;
}
