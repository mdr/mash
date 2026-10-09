//! The boundary between the interpreter and its host (native OS, browser, or a test fixture).

use std::fmt::Debug;

/// Host services the interpreter needs. Implementations live outside `mash-core` (e.g. `mash-os` for
/// native builds), so that the core stays portable. Grows as builtins are ported.
pub trait Platform: Debug {
    /// The value of an environment variable, if set.
    fn env_var(&self, name: &str) -> Option<String>;

    /// The current working directory, if the host has one.
    fn current_dir(&self) -> Option<String>;
}
