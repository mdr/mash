//! Native-only Mash builtins (filesystem, processes, users, git, http) and the native
//! [`Platform`](mash_core::platform::Platform) implementation.

use mash_core::platform::Platform;

/// The [`Platform`] for a native process: real environment and working directory.
#[derive(Debug, Default)]
pub struct NativePlatform;

impl Platform for NativePlatform {
    fn env_var(&self, name: &str) -> Option<String> {
        std::env::var(name).ok()
    }

    fn current_dir(&self) -> Option<String> {
        std::env::current_dir()
            .ok()
            .map(|dir| dir.to_string_lossy().into_owned())
    }
}
