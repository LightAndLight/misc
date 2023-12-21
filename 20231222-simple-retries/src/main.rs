use std::time::Duration;

struct Error<E> {
    error: E,
    can_retry: bool,
}

/** Run `action`, rerunning if it fails with a [retryable](Error::can_retry) error (up to `max_retries` times).
*/
fn retry<E, A>(
    action: &dyn Fn() -> Result<A, Error<E>>,
    max_retries: u8,
    retry_delay: Duration,
) -> Result<A, Error<E>> {
    let mut result = action();

    for _ in 0..max_retries {
        match &result {
            Err(err) if err.can_retry => {
                std::thread::sleep(retry_delay);
                result = action();
            }
            _ => {
                break;
            }
        }
    }

    result
}

fn main() {
    println!("Hello, world!");
}
