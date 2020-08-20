fn test_function(test: f32) -> f32 {
    return test;
}

fn main_vert() -> void {
    var test: f32 = test_function(1.0);
}

entry_point vertex as "main" = main_vert;