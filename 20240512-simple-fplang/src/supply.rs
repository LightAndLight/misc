use std::fmt;

use crate::internal_error;

pub struct Supply {
    counter: usize,
    storage: String,
}

impl Supply {
    pub fn new() -> Self {
        Self {
            counter: 0,
            storage: String::new(),
        }
    }

    pub fn fresh(&mut self) -> &str {
        self.storage.clear();
        fmt::Write::write_fmt(&mut self.storage, format_args!("{}", self.counter))
            .unwrap_or_else(internal_error!());
        self.counter += 1;
        &self.storage
    }
}

impl Default for Supply {
    fn default() -> Self {
        Self::new()
    }
}
