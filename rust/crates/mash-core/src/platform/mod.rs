//! The boundary between the interpreter and its host (native OS, browser, or a test fixture).
//!
//! Paths are strings. Relative paths are relative to the platform's current directory; the empty path is not
//! valid (callers use `.`).

mod memory;
mod net;
mod system;
mod test;

use std::fmt::Debug;
use std::io;
use std::time::Duration;

use jiff::Timestamp;
use jiff::tz::TimeZone;

pub use memory::MemoryFiles;
pub use net::{HttpRequest, HttpResponse};
pub use system::{DiskSpace, FakeSystem, GroupEntry, ProcessInfo, SIGNAL_NAMES, Signal, TerminalSize, UserEntry};
pub use test::{FakeCommand, FakeOutcome, FakeProcess, HttpRecord, ProcessRecord, TestPlatform};

/// Host services the interpreter needs. Implementations live outside `mash-core` (e.g. `mash-os` for
/// native builds), so that the core stays portable. Grows as builtins are ported.
///
/// File system and process operations have default implementations that fail with
/// [`io::ErrorKind::Unsupported`], for hosts without them (e.g. a browser without a virtual file system).
pub trait Platform: Debug {
    /// The value of an environment variable, if set.
    fn env_var(&self, name: &str) -> Option<String>;

    /// All environment variables
    fn env_vars(&self) -> Vec<(String, String)>;

    /// The current working directory, if the host has one.
    fn current_dir(&self) -> Option<String>;

    /// The user's home directory (for `~`), if the host has one
    fn home_dir(&self) -> Option<String>;

    /// The local time zone
    fn time_zone(&self) -> TimeZone;

    /// The current time. Never call `Timestamp::now()` directly: tests use a fixed clock.
    fn now(&self) -> Timestamp;

    /// The entries of a directory (relative paths are relative to the current directory), not including `.`
    /// and `..`. Symbolic links are followed to decide whether an entry is a directory.
    fn read_dir(&self, path: &str) -> io::Result<Vec<DirEntry>>;

    /// Pause for `duration` (callers sleep in short slices, so that they can respond to interrupts)
    fn sleep(&self, duration: Duration);

    /// 64 random bits (for `random.*`). Deterministic in tests.
    fn random_u64(&self) -> u64;

    /// Write Mash's own output (`print`): standard output natively
    fn write_output(&self, text: &str);

    /// Whether the user has asked to interrupt evaluation, for hosts that can't set
    /// [`Ctx::interrupt_flag`](crate::eval::Ctx::interrupt_flag) directly (in the browser, Ctrl-C is seen by
    /// another thread). Asked every so many evaluation steps, and while sleeping; asking consumes the request.
    fn interrupt_requested(&self) -> bool {
        false
    }

    /// Whether the stack is too nearly full for another Mash call, for hosts whose stack is too small for
    /// [`DEFAULT_MAX_DEPTH`](crate::eval::DEFAULT_MAX_DEPTH) calls, and can't be made bigger (in the browser,
    /// overflowing it leaves the interpreter unusable). Asked on every call of a Mash function, so it must be
    /// cheap.
    fn stack_nearly_full(&self) -> bool {
        false
    }

    // ------------------------------------------------------------------------------------------ file system

    /// Change the current working directory
    fn set_current_dir(&self, _path: &str) -> io::Result<()> {
        Err(unsupported())
    }

    /// Information about a path, following symbolic links (`stat`)
    fn metadata(&self, _path: &str) -> io::Result<Metadata> {
        Err(unsupported())
    }

    /// Information about a path itself, not following a final symbolic link (`lstat`)
    fn symlink_metadata(&self, _path: &str) -> io::Result<Metadata> {
        Err(unsupported())
    }

    /// The target of a symbolic link, as stored in the link
    fn read_link(&self, _path: &str) -> io::Result<String> {
        Err(unsupported())
    }

    /// Create a symbolic link at `path` to `target` (stored as given)
    fn create_symlink(&self, _target: &str, _path: &str) -> io::Result<()> {
        Err(unsupported())
    }

    /// The absolute path with all symbolic links, `.` and `..` resolved (the path must exist)
    fn canonicalize(&self, _path: &str) -> io::Result<String> {
        Err(unsupported())
    }

    /// The contents of a file
    fn read_file(&self, _path: &str) -> io::Result<Vec<u8>> {
        Err(unsupported())
    }

    /// Write a file (creating it if necessary), replacing its contents or appending to them
    fn write_file(&self, _path: &str, _contents: &[u8], _append: bool) -> io::Result<()> {
        Err(unsupported())
    }

    /// Create a directory, and if `intermediates`, any missing parents (then it's not an error if the
    /// directory already exists)
    fn create_dir(&self, _path: &str, _intermediates: bool) -> io::Result<()> {
        Err(unsupported())
    }

    /// Delete a file or symbolic link, or a directory and everything in it (never following symbolic links)
    fn remove(&self, _path: &str) -> io::Result<()> {
        Err(unsupported())
    }

    /// Copy a file's contents (and permissions), replacing the destination if it exists
    fn copy_file(&self, _from: &str, _to: &str) -> io::Result<()> {
        Err(unsupported())
    }

    /// Rename (move) a file or directory, replacing a destination file. Fails with
    /// [`io::ErrorKind::CrossesDevices`] if the destination is on another file system.
    fn rename(&self, _from: &str, _to: &str) -> io::Result<()> {
        Err(unsupported())
    }

    /// Set the permission bits (as in [`Metadata::permissions`]) of a path, following symbolic links
    fn set_permissions(&self, _path: &str, _permissions: u32) -> io::Result<()> {
        Err(unsupported())
    }

    /// Create a new, empty file in the temporary directory, returning its absolute path
    fn create_temp_file(&self) -> io::Result<String> {
        Err(unsupported())
    }

    /// Create a new, empty directory in the temporary directory, returning its absolute path
    fn create_temp_dir(&self) -> io::Result<String> {
        Err(unsupported())
    }

    // ------------------------------------------------------------------------------------------ processes

    /// Run an external program to completion. Its standard error is always inherited. Fails if the program
    /// can't be started (e.g. it doesn't exist).
    fn run_process(&self, _request: &ProcessRequest<'_>) -> io::Result<ProcessOutcome> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "running external programs is not supported here",
        ))
    }

    // ------------------------------------------------------------------------------------------ network

    /// Make an HTTP request, following redirects. Any response is a success, whatever its status; errors
    /// (unknown host, can't connect, TLS failures, …) have messages fit to show the user.
    fn http_request(&self, _request: &HttpRequest<'_>) -> io::Result<HttpResponse> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "HTTP is not supported here"))
    }

    /// The IP addresses of a host name (DNS), as strings (e.g. `93.184.215.14`, `::1`)
    fn lookup_host(&self, _host: &str) -> io::Result<Vec<String>> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "DNS lookups are not supported here",
        ))
    }

    /// Open a URL or file with the default application (`open` on macOS, `xdg-open` elsewhere)
    fn open_with_default_application(&self, _target: &str) -> io::Result<()> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "opening with the default application is not supported here",
        ))
    }
    // ------------------------------------------------------------------------------------------ the system

    /// Every user account (the password database)
    fn users(&self) -> io::Result<Vec<UserEntry>> {
        Err(unsupported_system("users"))
    }

    /// Every group (the group database)
    fn groups(&self) -> io::Result<Vec<GroupEntry>> {
        Err(unsupported_system("groups"))
    }

    /// The name of the user running Mash
    fn current_user(&self) -> io::Result<String> {
        Err(unsupported_system("users"))
    }

    /// Every running process (that can be seen)
    fn processes(&self) -> io::Result<Vec<ProcessInfo>> {
        Err(unsupported_system("processes"))
    }

    /// One process, if it's running
    fn process(&self, pid: u32) -> io::Result<Option<ProcessInfo>> {
        Ok(self.processes()?.into_iter().find(|p| p.pid == pid))
    }

    /// Send a signal to a process
    fn kill(&self, _pid: u32, _signal: &Signal) -> io::Result<()> {
        Err(unsupported_system("processes"))
    }

    /// The text on the clipboard
    fn clipboard(&self) -> io::Result<String> {
        Err(unsupported_system("the clipboard"))
    }

    /// Put text on the clipboard
    fn set_clipboard(&self, _text: &str) -> io::Result<()> {
        Err(unsupported_system("the clipboard"))
    }

    /// Space on each mounted file system
    fn disk_space(&self) -> io::Result<Vec<DiskSpace>> {
        Err(unsupported_system("disk space"))
    }

    /// The size of the terminal Mash is running in, if there is one
    fn terminal_size(&self) -> Option<TerminalSize> {
        None
    }
}

fn unsupported_system(what: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::Unsupported,
        format!("access to {what} is not supported here"),
    )
}

fn unsupported() -> io::Error {
    io::Error::new(io::ErrorKind::Unsupported, "file system access is not supported here")
}

/// An entry in a directory listing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirEntry {
    pub name: String,
    pub kind: FileKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileKind {
    File,
    Directory,
    /// A symbolic link (only from [`Platform::symlink_metadata`]: directory listings follow links)
    Symlink,
    Other,
}

/// Information about a file system object (from `stat`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Metadata {
    pub kind: FileKind,
    /// In bytes
    pub size: u64,
    /// Permission bits: `0o400` owner read … `0o001` others execute
    pub permissions: u32,
    /// The owning user's name (or numeric id, if it has no name)
    pub owner: String,
    /// The owning group's name (or numeric id, if it has no name)
    pub group: String,
    pub modified: Timestamp,
    pub accessed: Timestamp,
}

impl Metadata {
    pub fn is_dir(&self) -> bool {
        self.kind == FileKind::Directory
    }

    pub fn is_file(&self) -> bool {
        self.kind == FileKind::File
    }
}

/// A request to run an external program.
#[derive(Debug, Clone)]
pub struct ProcessRequest<'a> {
    /// The program, then its arguments. The program is looked up on the `PATH` (of the host, not `env`)
    /// unless it contains a `/`.
    pub argv: &'a [String],
    /// The program's entire environment
    pub env: &'a [(String, String)],
    pub stdin: ProcessInput<'a>,
    pub stdout: ProcessOutput<'a>,
}

/// Where a child process's standard input comes from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessInput<'a> {
    /// Mash's own standard input (the terminal)
    Inherit,
    /// This text (then end of file)
    Text(&'a str),
    /// A file
    File(&'a str),
}

/// Where a child process's standard output goes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessOutput<'a> {
    /// Mash's own standard output (the terminal)
    Inherit,
    /// Captured into [`ProcessOutcome::stdout`]
    Capture,
    /// A file (created or truncated)
    File(&'a str),
}

/// How a process run went.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessOutcome {
    /// The exit code, or 128 + the signal number if the process was killed by a signal
    pub exit_status: i32,
    /// Captured standard output (lossily decoded UTF-8); empty unless captured
    pub stdout: String,
    pub started: Timestamp,
    pub finished: Timestamp,
    /// The process was killed by an interrupt (Ctrl-C): evaluation should stop
    pub interrupted: bool,
}

/// `path` made absolute (relative to `cwd`) and normalised (`.` and `..` resolved lexically)
pub fn resolve(cwd: &str, path: &str) -> String {
    let joined = if path.starts_with('/') {
        path.to_string()
    } else {
        format!("{cwd}/{path}")
    };
    let mut segments: Vec<&str> = vec![];
    for segment in joined.split('/') {
        match segment {
            "" | "." => {}
            ".." => {
                segments.pop();
            }
            s => segments.push(s),
        }
    }
    format!("/{}", segments.join("/"))
}
