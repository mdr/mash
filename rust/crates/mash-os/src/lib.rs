//! Native-only Mash builtins (filesystem, processes, users, git, http) and the native
//! [`Platform`](mash_core::platform::Platform) implementation.

mod fs;
mod net;
mod process;
mod system;

use std::io;

use jiff::Timestamp;
use jiff::tz::TimeZone;
use mash_core::platform::{
    DirEntry, DiskSpace, FileKind, GroupEntry, HttpRequest, HttpResponse, Metadata, Platform, ProcessInfo,
    ProcessOutcome, ProcessRequest, Signal, TerminalSize, UserEntry,
};

/// The [`Platform`] for a native process: real environment, working directory, file system, processes, network,
/// clock, time zone, users and groups, clipboard, disks and terminal.
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

    fn sleep(&self, duration: std::time::Duration) {
        std::thread::sleep(duration);
    }

    fn write_output(&self, text: &str) {
        use std::io::Write;
        let mut stdout = std::io::stdout().lock();
        let _ = stdout.write_all(text.as_bytes());
        let _ = stdout.flush();
    }

    fn random_u64(&self) -> u64 {
        use std::io::Read;
        let mut bytes = [0u8; 8];
        match std::fs::File::open("/dev/urandom").and_then(|mut f| f.read_exact(&mut bytes)) {
            Ok(()) => u64::from_ne_bytes(bytes),
            // Fall back on the standard library's randomly-keyed hasher
            Err(_) => {
                use std::hash::{BuildHasher, Hasher};
                let mut hasher = std::collections::hash_map::RandomState::new().build_hasher();
                hasher.write_u128(Timestamp::now().as_nanosecond() as u128);
                hasher.finish()
            }
        }
    }

    fn set_current_dir(&self, path: &str) -> io::Result<()> {
        std::env::set_current_dir(path)
    }

    fn metadata(&self, path: &str) -> io::Result<Metadata> {
        Ok(fs::metadata(&std::fs::metadata(path)?))
    }

    fn symlink_metadata(&self, path: &str) -> io::Result<Metadata> {
        Ok(fs::metadata(&std::fs::symlink_metadata(path)?))
    }

    fn read_link(&self, path: &str) -> io::Result<String> {
        Ok(fs::path_string(&std::fs::read_link(path)?))
    }

    fn create_symlink(&self, target: &str, path: &str) -> io::Result<()> {
        std::os::unix::fs::symlink(target, path)
    }

    fn canonicalize(&self, path: &str) -> io::Result<String> {
        Ok(fs::path_string(&std::fs::canonicalize(path)?))
    }

    fn read_file(&self, path: &str) -> io::Result<Vec<u8>> {
        std::fs::read(path)
    }

    fn write_file(&self, path: &str, contents: &[u8], append: bool) -> io::Result<()> {
        fs::write_file(path, contents, append)
    }

    fn create_dir(&self, path: &str, intermediates: bool) -> io::Result<()> {
        if intermediates {
            std::fs::create_dir_all(path)
        } else {
            std::fs::create_dir(path)
        }
    }

    fn remove(&self, path: &str) -> io::Result<()> {
        fs::remove(path)
    }

    fn copy_file(&self, from: &str, to: &str) -> io::Result<()> {
        std::fs::copy(from, to).map(drop)
    }

    fn rename(&self, from: &str, to: &str) -> io::Result<()> {
        std::fs::rename(from, to)
    }

    fn set_permissions(&self, path: &str, permissions: u32) -> io::Result<()> {
        fs::set_permissions(path, permissions)
    }

    fn create_temp_file(&self) -> io::Result<String> {
        fs::create_temp(false, || self.random_u64())
    }

    fn create_temp_dir(&self) -> io::Result<String> {
        fs::create_temp(true, || self.random_u64())
    }

    fn run_process(&self, request: &ProcessRequest<'_>) -> io::Result<ProcessOutcome> {
        process::run_process(request)
    }

    fn http_request(&self, request: &HttpRequest<'_>) -> io::Result<HttpResponse> {
        net::http_request(request)
    }

    fn lookup_host(&self, host: &str) -> io::Result<Vec<String>> {
        net::lookup_host(host)
    }

    fn open_with_default_application(&self, target: &str) -> io::Result<()> {
        net::open_with_default_application(target)
    }

    fn users(&self) -> io::Result<Vec<UserEntry>> {
        system::users()
    }

    fn groups(&self) -> io::Result<Vec<GroupEntry>> {
        system::groups()
    }

    fn current_user(&self) -> io::Result<String> {
        system::current_user()
    }

    fn processes(&self) -> io::Result<Vec<ProcessInfo>> {
        system::processes()
    }

    fn process(&self, pid: u32) -> io::Result<Option<ProcessInfo>> {
        system::process(pid)
    }

    fn kill(&self, pid: u32, signal: &Signal) -> io::Result<()> {
        system::kill(pid, signal)
    }

    fn clipboard(&self) -> io::Result<String> {
        system::clipboard()
    }

    fn set_clipboard(&self, text: &str) -> io::Result<()> {
        system::set_clipboard(text)
    }

    fn disk_space(&self) -> io::Result<Vec<DiskSpace>> {
        system::disk_space()
    }

    fn terminal_size(&self) -> Option<TerminalSize> {
        system::terminal_size()
    }
}
