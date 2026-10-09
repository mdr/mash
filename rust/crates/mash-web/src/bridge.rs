//! What Mash needs from the JavaScript around it. In the browser this is an object in the Web Worker that runs
//! Mash (`www/worker.js`), which passes messages to the page; tests use [`FakeBridge`].

use std::cell::{Cell, RefCell};
use std::fmt::Debug;
use std::io;

use mash_core::platform::{HttpRequest, HttpResponse};

pub trait Bridge: Debug {
    /// Write to the terminal (text and ANSI escape sequences)
    fn write(&self, text: &str);

    /// The time, in milliseconds since the Unix epoch
    fn now_millis(&self) -> f64;

    /// The local time zone's IANA name (e.g. `Europe/London`), if known
    fn time_zone(&self) -> Option<String>;

    /// 32 random bits
    fn random_u32(&self) -> u32;

    /// Whether the user has pressed Ctrl-C while a command was running, since last asked
    fn interrupt_requested(&self) -> bool;

    /// Pause for `millis`, waking early if the user presses Ctrl-C
    fn sleep(&self, millis: f64);

    /// A command has started or finished running (Ctrl-C interrupts while one is running, and is a key
    /// otherwise)
    fn command_running(&self, running: bool);

    /// Keep a history entry (a JSON object, see `HistoryEntry::to_json`)
    fn save_history(&self, entry: &str);

    /// Put text on the clipboard
    fn set_clipboard(&self, text: &str) -> io::Result<()>;

    /// Open a URL in a new tab
    fn open_url(&self, url: &str) -> io::Result<()>;

    /// Make an HTTP request (subject to the browser's rules: CORS, forbidden headers)
    fn http_request(&self, request: &HttpRequest<'_>) -> io::Result<HttpResponse>;
}

/// A bridge for tests: records what Mash does, with a clock that moves only when slept.
#[derive(Debug, Default)]
pub struct FakeBridge {
    pub output: RefCell<String>,
    pub now: Cell<f64>,
    pub time_zone: Option<String>,
    /// Interrupt when next asked
    pub interrupt: Cell<bool>,
    /// `command_running` calls, in order
    pub running: RefCell<Vec<bool>>,
    pub history: RefCell<Vec<String>>,
    pub clipboard: RefCell<Option<String>>,
    pub opened: RefCell<Vec<String>>,
    pub requests: RefCell<Vec<String>>,
}

impl Bridge for FakeBridge {
    fn write(&self, text: &str) {
        self.output.borrow_mut().push_str(text);
    }

    fn now_millis(&self) -> f64 {
        self.now.get()
    }

    fn time_zone(&self) -> Option<String> {
        self.time_zone.clone()
    }

    fn random_u32(&self) -> u32 {
        4
    }

    fn interrupt_requested(&self) -> bool {
        self.interrupt.replace(false)
    }

    fn sleep(&self, millis: f64) {
        self.now.set(self.now.get() + millis);
    }

    fn command_running(&self, running: bool) {
        self.running.borrow_mut().push(running);
    }

    fn save_history(&self, entry: &str) {
        self.history.borrow_mut().push(entry.into());
    }

    fn set_clipboard(&self, text: &str) -> io::Result<()> {
        *self.clipboard.borrow_mut() = Some(text.into());
        Ok(())
    }

    fn open_url(&self, url: &str) -> io::Result<()> {
        self.opened.borrow_mut().push(url.into());
        Ok(())
    }

    fn http_request(&self, request: &HttpRequest<'_>) -> io::Result<HttpResponse> {
        self.requests
            .borrow_mut()
            .push(format!("{} {}", request.method, request.url));
        Ok(HttpResponse::new(200, "ok"))
    }
}
