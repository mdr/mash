//! The browser's [`Platform`]: an in-memory file system (lost when the page is closed), one user, the clock,
//! HTTP, the clipboard (write only) and opening URLs through the [`Bridge`]. Running programs, processes and
//! disk space aren't available.

use std::cell::{Cell, RefCell};
use std::io;
use std::rc::Rc;
use std::time::Duration;

use jiff::Timestamp;
use jiff::tz::TimeZone;
use mash_core::platform::{
    DirEntry, GroupEntry, HttpRequest, HttpResponse, MemoryFiles, Metadata, Platform, TerminalSize, UserEntry,
};

use crate::bridge::Bridge;

/// The one user
pub const USER: &str = "guest";
pub const HOME: &str = "/home/guest";
const UID: u32 = 1000;

/// Files the browser's file system starts with, in the home directory
const SAMPLE_FILES: &[(&str, &str)] = &[
    ("README.md", include_str!("../samples/README.md")),
    ("examples/fib.mash", include_str!("../samples/fib.mash")),
    ("examples/planets.json", include_str!("../samples/planets.json")),
    ("examples/sales.csv", include_str!("../samples/sales.csv")),
];

#[derive(Debug)]
pub struct WebPlatform {
    bridge: Rc<dyn Bridge>,
    files: MemoryFiles,
    cwd: RefCell<String>,
    time_zone: TimeZone,
    /// The terminal's size (rows, columns), kept up to date by the host
    terminal: Rc<Cell<(u16, u16)>>,
}

impl WebPlatform {
    /// A platform with the sample files in the home directory, which is the working directory
    pub fn new(bridge: Rc<dyn Bridge>, terminal: Rc<Cell<(u16, u16)>>) -> WebPlatform {
        // jiff bundles the time zone database on wasm
        let time_zone = bridge
            .time_zone()
            .and_then(|name| TimeZone::get(&name).ok())
            .unwrap_or(TimeZone::UTC);
        let files = MemoryFiles::of(&[&format!("{HOME}/"), "/tmp/"]).with_owner(USER);
        let platform = WebPlatform {
            bridge,
            files,
            cwd: RefCell::new(HOME.into()),
            time_zone,
            terminal,
        };
        let now = platform.now();
        for (path, contents) in SAMPLE_FILES {
            let path = format!("{HOME}/{path}");
            if let Some((dir, _)) = path.rsplit_once('/') {
                let _ = platform.files.create_dir("/", dir, true);
            }
            let _ = platform.files.write_file("/", &path, contents.as_bytes(), false, now);
        }
        platform
    }

    fn cwd(&self) -> String {
        self.cwd.borrow().clone()
    }
}

fn unsupported(what: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::Unsupported,
        format!("{what} is not possible in the browser"),
    )
}

impl Platform for WebPlatform {
    fn env_var(&self, name: &str) -> Option<String> {
        self.env_vars().into_iter().find(|(n, _)| n == name).map(|(_, v)| v)
    }

    fn env_vars(&self) -> Vec<(String, String)> {
        vec![
            ("HOME".into(), HOME.into()),
            ("TERM".into(), "xterm-256color".into()),
            ("USER".into(), USER.into()),
        ]
    }

    fn current_dir(&self) -> Option<String> {
        Some(self.cwd())
    }

    fn home_dir(&self) -> Option<String> {
        Some(HOME.into())
    }

    fn time_zone(&self) -> TimeZone {
        self.time_zone.clone()
    }

    fn now(&self) -> Timestamp {
        Timestamp::from_millisecond(self.bridge.now_millis() as i64).unwrap_or(Timestamp::UNIX_EPOCH)
    }

    fn read_dir(&self, path: &str) -> io::Result<Vec<DirEntry>> {
        self.files.read_dir(&self.cwd(), path)
    }

    fn sleep(&self, duration: Duration) {
        self.bridge.sleep(duration.as_secs_f64() * 1000.0);
    }

    fn random_u64(&self) -> u64 {
        u64::from(self.bridge.random_u32()) << 32 | u64::from(self.bridge.random_u32())
    }

    /// As a terminal in its normal mode does, newlines start a new line
    fn write_output(&self, text: &str) {
        self.bridge.write(&text.replace('\n', "\r\n"));
    }

    fn interrupt_requested(&self) -> bool {
        self.bridge.interrupt_requested()
    }

    // ------------------------------------------------------------------------------------------ file system

    fn set_current_dir(&self, path: &str) -> io::Result<()> {
        let real = self.files.canonicalize(&self.cwd(), path)?;
        if !self.files.metadata("/", &real, true)?.is_dir() {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!("Not a directory: {path}"),
            ));
        }
        *self.cwd.borrow_mut() = real;
        Ok(())
    }

    fn metadata(&self, path: &str) -> io::Result<Metadata> {
        self.files.metadata(&self.cwd(), path, true)
    }

    fn symlink_metadata(&self, path: &str) -> io::Result<Metadata> {
        self.files.metadata(&self.cwd(), path, false)
    }

    fn read_link(&self, path: &str) -> io::Result<String> {
        self.files.read_link(&self.cwd(), path)
    }

    fn canonicalize(&self, path: &str) -> io::Result<String> {
        self.files.canonicalize(&self.cwd(), path)
    }

    fn read_file(&self, path: &str) -> io::Result<Vec<u8>> {
        self.files.read_file(&self.cwd(), path)
    }

    fn write_file(&self, path: &str, contents: &[u8], append: bool) -> io::Result<()> {
        self.files.write_file(&self.cwd(), path, contents, append, self.now())
    }

    fn create_dir(&self, path: &str, intermediates: bool) -> io::Result<()> {
        self.files.create_dir(&self.cwd(), path, intermediates)
    }

    fn remove(&self, path: &str) -> io::Result<()> {
        self.files.remove(&self.cwd(), path)
    }

    fn copy_file(&self, from: &str, to: &str) -> io::Result<()> {
        self.files.copy_file(&self.cwd(), from, to, self.now())
    }

    fn rename(&self, from: &str, to: &str) -> io::Result<()> {
        self.files.rename(&self.cwd(), from, to)
    }

    fn set_permissions(&self, path: &str, permissions: u32) -> io::Result<()> {
        self.files.set_permissions(&self.cwd(), path, permissions)
    }

    fn create_temp_file(&self) -> io::Result<String> {
        self.files.create_temp(false)
    }

    fn create_temp_dir(&self) -> io::Result<String> {
        self.files.create_temp(true)
    }

    // ------------------------------------------------------------------------------------------ processes

    fn run_process(
        &self,
        _request: &mash_core::platform::ProcessRequest<'_>,
    ) -> io::Result<mash_core::platform::ProcessOutcome> {
        Err(unsupported("running programs"))
    }

    // ------------------------------------------------------------------------------------------ network

    fn http_request(&self, request: &HttpRequest<'_>) -> io::Result<HttpResponse> {
        self.bridge.http_request(request)
    }

    fn lookup_host(&self, _host: &str) -> io::Result<Vec<String>> {
        Err(unsupported("looking up host names"))
    }

    /// Web addresses open in a new tab
    fn open_with_default_application(&self, target: &str) -> io::Result<()> {
        if target.starts_with("http://") || target.starts_with("https://") {
            self.bridge.open_url(target)
        } else {
            Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "only web addresses (http and https) can be opened in the browser",
            ))
        }
    }

    // ------------------------------------------------------------------------------------------ the system

    fn users(&self) -> io::Result<Vec<UserEntry>> {
        Ok(vec![UserEntry {
            name: USER.into(),
            uid: UID,
            gid: UID,
            gecos: "Guest".into(),
            home: HOME.into(),
            shell: "/bin/mash".into(),
        }])
    }

    fn groups(&self) -> io::Result<Vec<GroupEntry>> {
        Ok(vec![GroupEntry {
            name: USER.into(),
            gid: UID,
            members: vec![],
        }])
    }

    fn current_user(&self) -> io::Result<String> {
        Ok(USER.into())
    }

    fn processes(&self) -> io::Result<Vec<mash_core::platform::ProcessInfo>> {
        Err(unsupported("listing processes"))
    }

    fn kill(&self, _pid: u32, _signal: &mash_core::platform::Signal) -> io::Result<()> {
        Err(unsupported("signalling processes"))
    }

    fn clipboard(&self) -> io::Result<String> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "reading the clipboard is not possible in the browser (paste with Ctrl-V or Cmd-V instead)",
        ))
    }

    fn set_clipboard(&self, text: &str) -> io::Result<()> {
        self.bridge.set_clipboard(text)
    }

    fn disk_space(&self) -> io::Result<Vec<mash_core::platform::DiskSpace>> {
        Err(unsupported("finding disk space"))
    }

    fn terminal_size(&self) -> Option<TerminalSize> {
        let (rows, columns) = self.terminal.get();
        Some(TerminalSize { rows, columns })
    }
}
