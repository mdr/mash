//! An interactive Mash session in the browser: the REPL, with the [`WebPlatform`], drawing through a [`Bridge`]
//! to xterm.js. Independent of wasm-bindgen, so that it can be tested natively.

use std::cell::Cell;
use std::rc::Rc;

use mash_core::eval::Ctx;
use mash_repl::history::{HistoryEntry, HistoryStorage, new_session_id};
use mash_repl::host::Host;
use mash_repl::input::InputEvent;
use mash_repl::screen::Dimensions;
use mash_repl::{Repl, ReplStatus};

use crate::bridge::Bridge;
use crate::keys::translate_key;
use crate::platform::WebPlatform;

/// The REPL's [`Host`]: output goes to the bridge.
#[derive(Debug)]
pub struct WebHost {
    bridge: Rc<dyn Bridge>,
    /// Rows and columns
    size: Rc<Cell<(u16, u16)>>,
    buffer: String,
    /// A command is running: the terminal is in its normal mode, where a newline also returns the cursor to
    /// the start of the line
    running: bool,
}

impl Host for WebHost {
    fn size(&self) -> Dimensions {
        let (rows, columns) = self.size.get();
        Dimensions::new(rows.into(), columns.into())
    }

    fn write(&mut self, s: &str) {
        if self.running {
            self.buffer.push_str(&s.replace("\r\n", "\n").replace('\n', "\r\n"));
        } else {
            self.buffer.push_str(s);
        }
    }

    fn flush(&mut self) {
        if !self.buffer.is_empty() {
            self.bridge.write(&self.buffer);
            self.buffer.clear();
        }
    }

    fn begin_command(&mut self) {
        self.flush();
        self.running = true;
        // Clear any Ctrl-C pressed too late to interrupt the last command
        self.bridge.interrupt_requested();
        self.bridge.command_running(true);
    }

    fn end_command(&mut self) {
        self.flush();
        self.running = false;
        self.bridge.command_running(false);
    }
}

/// History kept by the page (in the browser's local storage): the entries it had when the shell started, and
/// new ones passed back through the bridge.
#[derive(Debug)]
struct WebHistoryStorage {
    entries: Vec<HistoryEntry>,
    bridge: Rc<dyn Bridge>,
}

impl HistoryStorage for WebHistoryStorage {
    fn load(&mut self) -> Vec<HistoryEntry> {
        self.entries.clone()
    }

    fn save(&mut self, entry: &HistoryEntry) {
        self.bridge.save_history(&entry.to_json().to_string());
    }
}

#[derive(Debug)]
pub struct Shell {
    repl: Repl,
    host: WebHost,
    exited: bool,
}

impl Shell {
    /// A session in a terminal of `rows` and `columns`, with the history entries the page kept (JSON objects;
    /// any that don't parse are skipped)
    pub fn new(bridge: Rc<dyn Bridge>, rows: u16, columns: u16, history: &[String]) -> Shell {
        let size = Rc::new(Cell::new((rows, columns)));
        let platform = Rc::new(WebPlatform::new(bridge.clone(), size.clone()));
        let session_id = new_session_id(platform.as_ref());
        let mut entries: Vec<HistoryEntry> = history
            .iter()
            .filter_map(|line| HistoryEntry::from_json(&serde_json::from_str(line).ok()?))
            .collect();
        entries.sort_by_key(|e| e.timestamp);
        let storage = WebHistoryStorage {
            entries,
            bridge: bridge.clone(),
        };
        let repl = Repl::new(Ctx::new(platform), Box::new(storage), session_id);
        let host = WebHost {
            bridge,
            size,
            buffer: String::new(),
            running: false,
        };
        Shell {
            repl,
            host,
            exited: false,
        }
    }

    /// Show the startup tip and the first prompt
    pub fn start(&mut self) {
        self.repl.start(&mut self.host);
        self.host.flush();
    }

    /// Handle a `keydown` event. Returns false once the user has left the shell (Ctrl-D).
    pub fn key(&mut self, key: &str, code: &str, ctrl: bool, alt: bool, shift: bool, meta: bool) -> bool {
        match translate_key(key, code, ctrl, alt, shift, meta) {
            Some(key) => self.event(InputEvent::Key(key)),
            None => !self.exited,
        }
    }

    /// Handle pasted (or composed) text
    pub fn paste(&mut self, text: &str) -> bool {
        self.event(InputEvent::Paste(text.into()))
    }

    /// The terminal has changed size
    pub fn resize(&mut self, rows: u16, columns: u16) -> bool {
        self.host.size.set((rows, columns));
        self.event(InputEvent::Resize(Dimensions::new(rows.into(), columns.into())))
    }

    fn event(&mut self, event: InputEvent) -> bool {
        if self.exited {
            return false;
        }
        if self.repl.handle_event(event, &mut self.host) == ReplStatus::Exit {
            self.exited = true;
        }
        self.host.flush();
        !self.exited
    }

    pub fn repl(&mut self) -> &mut Repl {
        &mut self.repl
    }
}
