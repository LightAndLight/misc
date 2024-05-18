#[macro_export]
macro_rules! internal_error {
    () => {
        |err| panic!("internal error: {}", err)
    };
}
