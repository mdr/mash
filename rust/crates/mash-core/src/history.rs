//! The REPL's command history, as `os.history` and `jump` see it. The REPL owns the history; it gives the
//! interpreter a view of it with [`Ctx::set_command_history`](crate::eval::Ctx::set_command_history).

use std::cell::RefCell;
use std::fmt::Debug;

use jiff::Timestamp;

use crate::value::Value;

/// A command run at the REPL (Scala `HistoryEntry`).
#[derive(Debug, Clone)]
pub struct HistoryEntry {
    /// The same for every command in one REPL session
    pub session_id: String,
    pub command_number: u64,
    pub timestamp: Timestamp,
    pub command: String,
    /// Whether the command was run in mish mode
    pub mish: bool,
    /// The command's result, if it's known (commands from this session), else null
    pub result: Value,
    pub working_directory: String,
}

/// Access to the commands run so far.
pub trait CommandHistory: Debug {
    /// The entries, oldest first
    fn entries(&self) -> Vec<HistoryEntry>;
}

/// A history kept in memory, recorded by its owner (the REPL).
#[derive(Debug, Default)]
pub struct RecordedHistory {
    entries: RefCell<Vec<HistoryEntry>>,
}

impl RecordedHistory {
    /// A history starting with `entries` (oldest first)
    pub fn new(entries: Vec<HistoryEntry>) -> RecordedHistory {
        RecordedHistory {
            entries: RefCell::new(entries),
        }
    }

    /// Add the newest entry
    pub fn record(&self, entry: HistoryEntry) {
        self.entries.borrow_mut().push(entry);
    }
}

impl CommandHistory for RecordedHistory {
    fn entries(&self) -> Vec<HistoryEntry> {
        self.entries.borrow().clone()
    }
}
