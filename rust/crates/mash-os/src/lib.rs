//! Native-only Mash builtins (filesystem, processes, users, git, http) and the native
//! [`Platform`](mash_core::platform::Platform) implementation.

use jiff::Timestamp;
use jiff::tz::TimeZone;
use mash_core::platform::{DirEntry, FileKind, Platform};

/// The [`Platform`] for a native process: real environment, working directory, clock and time zone.
#[derive(Debug, Default)]
pub struct NativePlatform;

impl Platform for NativePlatform {
    fn env_var(&self, name: &str) -> Option<String> {
        std::env::var(name).ok()
    }

    fn env_vars(&self) -> Vec<(String, String)> {
        std::env::vars_os()
            .map(|(k, v)| (k.to_string_lossy().into_owned(), v.to_string_lossy().into_owned()))
            .collect()
    }

    fn current_dir(&self) -> Option<String> {
        std::env::current_dir()
            .ok()
            .map(|dir| dir.to_string_lossy().into_owned())
    }

    fn home_dir(&self) -> Option<String> {
        std::env::home_dir().map(|dir| dir.to_string_lossy().into_owned())
    }

    fn time_zone(&self) -> TimeZone {
        TimeZone::system()
    }

    fn now(&self) -> Timestamp {
        Timestamp::now()
    }

    fn read_dir(&self, path: &str) -> std::io::Result<Vec<DirEntry>> {
        let path = if path.is_empty() { "." } else { path };
        let mut entries = vec![];
        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            // Follow symbolic links to decide what an entry is
            let kind = match std::fs::metadata(entry.path()) {
                Ok(m) if m.is_dir() => FileKind::Directory,
                Ok(m) if m.is_file() => FileKind::File,
                _ => FileKind::Other,
            };
            entries.push(DirEntry {
                name: entry.file_name().to_string_lossy().into_owned(),
                kind,
            });
        }
        Ok(entries)
    }
}
