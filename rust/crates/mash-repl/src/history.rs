//! Command history entries and where they are kept. The file-backed store (`~/.mash/history`, one JSON object
//! per line, compatible with Scala Mash) lives in `mash-cli`; tests use [`MemoryHistoryStorage`].

use jiff::Timestamp;

/// One command run at the REPL (Scala `HistoryEntry`, without the result).
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct HistoryEntry {
    /// A UUID, the same for every command in one REPL session
    pub session_id: String,
    pub command_number: u64,
    pub timestamp: Timestamp,
    pub command: String,
    /// Whether the command was run in mish mode
    pub mish: bool,
    pub working_directory: String,
}

pub trait HistoryStorage {
    /// Previously saved entries, oldest first
    fn load(&mut self) -> Vec<HistoryEntry>;

    /// Add an entry (persisting it, where that makes sense)
    fn save(&mut self, entry: &HistoryEntry);
}

/// History kept only in memory.
#[derive(Clone, Debug, Default)]
pub struct MemoryHistoryStorage {
    pub entries: Vec<HistoryEntry>,
}

impl HistoryStorage for MemoryHistoryStorage {
    fn load(&mut self) -> Vec<HistoryEntry> {
        self.entries.clone()
    }

    fn save(&mut self, entry: &HistoryEntry) {
        self.entries.push(entry.clone());
    }
}
