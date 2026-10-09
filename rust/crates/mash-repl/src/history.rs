//! Command history entries and where they are kept. Entries are stored as JSON objects in the same format as
//! Scala Mash ([`HistoryEntry::to_json`]). The file-backed store (`~/.mash/history`, one object per line) lives
//! in `mash-cli`, and the browser's in `mash-web`; tests use [`MemoryHistoryStorage`].

use std::fmt;
use std::ops::Range;

use jiff::Timestamp;
use mash_core::platform::Platform;
use mash_core::value::Value;
use serde_json::{Value as Json, json};

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

impl HistoryEntry {
    /// The entry as the interpreter sees it (for `os.history` and `jump`), with the command's result
    pub fn for_interpreter(&self, result: Value) -> mash_core::history::HistoryEntry {
        mash_core::history::HistoryEntry {
            session_id: self.session_id.clone(),
            command_number: self.command_number,
            timestamp: self.timestamp,
            command: self.command.clone(),
            mish: self.mish,
            result,
            working_directory: self.working_directory.clone(),
        }
    }

    /// The entry as stored, in Scala Mash's format
    pub fn to_json(&self) -> Json {
        json!({
            "sessionId": self.session_id,
            "commandNumber": self.command_number,
            "timestamp": self.timestamp.to_string(),
            "command": self.command,
            "mish": self.mish,
            "workingDirectory": self.working_directory,
        })
    }

    /// An entry from its stored form (see [`HistoryEntry::to_json`]), if it is one
    pub fn from_json(json: &Json) -> Option<HistoryEntry> {
        Some(HistoryEntry {
            session_id: json.get("sessionId")?.as_str()?.to_string(),
            command_number: json.get("commandNumber")?.as_u64()?,
            timestamp: json.get("timestamp")?.as_str()?.parse().ok()?,
            command: json.get("command")?.as_str()?.to_string(),
            mish: json.get("mish").and_then(Json::as_bool).unwrap_or(false),
            working_directory: json.get("workingDirectory")?.as_str()?.to_string(),
        })
    }
}

/// A new session id: a random (version 4) UUID
pub fn new_session_id(platform: &dyn Platform) -> String {
    let bits = (u128::from(platform.random_u64()) << 64 | u128::from(platform.random_u64()))
        & !(0xf000 << 64 | 0xc000 << 48)
        | (0x4000 << 64 | 0x8000 << 48);
    let hex = format!("{bits:032x}");
    format!(
        "{}-{}-{}-{}-{}",
        &hex[..8],
        &hex[8..12],
        &hex[12..16],
        &hex[16..20],
        &hex[20..]
    )
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

/// A history entry matching a search (Scala `History.Match`).
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct HistoryMatch {
    pub command: String,
    /// The part of the command that matches
    pub range: Range<usize>,
    pub timestamp: Timestamp,
    pub working_directory: String,
}

/// The REPL's command history (Scala `History` and `HistoryImpl`): previous commands, newest first, a
/// position for stepping through them with Up and Down, and searches.
pub struct History {
    storage: Box<dyn HistoryStorage>,
    session_id: String,
    /// Newest first
    entries: Vec<HistoryEntry>,
    /// Index into `entries` while stepping through history
    position: Option<usize>,
    /// The line being edited before stepping into history, restored on stepping back out
    in_progress: Option<String>,
    /// Whether the user has started editing the current line (as opposed to scrolling past it)
    committed: bool,
}

impl fmt::Debug for History {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("History")
            .field("entries", &self.entries.len())
            .field("position", &self.position)
            .finish_non_exhaustive()
    }
}

impl History {
    /// History backed by `storage`; new entries are recorded under `session_id`
    pub fn new(mut storage: Box<dyn HistoryStorage>, session_id: impl Into<String>) -> History {
        let mut entries = storage.load();
        entries.reverse();
        // Newest first; among equal timestamps, the later entry first
        entries.sort_by_key(|e| std::cmp::Reverse(e.timestamp));
        History {
            storage,
            session_id: session_id.into(),
            entries,
            position: None,
            in_progress: None,
            committed: true,
        }
    }

    /// The entries, newest first
    pub fn entries(&self) -> &[HistoryEntry] {
        &self.entries
    }

    /// Record a command that has just been run, returning its entry
    pub fn record(
        &mut self,
        command: &str,
        command_number: u64,
        working_directory: &str,
        mish: bool,
        timestamp: Timestamp,
    ) -> &HistoryEntry {
        let entry = HistoryEntry {
            session_id: self.session_id.clone(),
            command_number,
            timestamp,
            command: command.into(),
            mish,
            working_directory: working_directory.into(),
        };
        self.storage.save(&entry);
        let index = self.entries.partition_point(|e| e.timestamp > timestamp);
        self.entries.insert(index, entry);
        self.committed = true;
        &self.entries[index]
    }

    /// Stop stepping through history
    pub fn reset_position(&mut self) {
        self.position = None;
        self.in_progress = None;
    }

    /// Step to the next newer command (or back to the line in progress); None if not in history
    pub fn go_forwards(&mut self) -> Option<String> {
        let position = self.position?;
        self.committed = false;
        if position == 0 {
            self.position = None;
            self.in_progress.take()
        } else {
            self.position = Some(position - 1);
            Some(self.entries[position - 1].command.clone())
        }
    }

    /// Step to the next older command, remembering `in_progress` if just starting; None at the oldest
    pub fn go_backwards(&mut self, in_progress: &str) -> Option<String> {
        if self.position.is_none() {
            self.in_progress = Some(in_progress.into());
        }
        let next = self.position.map_or(0, |p| p + 1);
        if next < self.entries.len() {
            self.committed = false;
            self.position = Some(next);
            Some(self.entries[next].command.clone())
        } else {
            None
        }
    }

    /// Commit to editing the current line, as opposed to scrolling past it
    pub fn commit_to_entry(&mut self) {
        self.committed = true;
    }

    pub fn is_committed_to_entry(&self) -> bool {
        self.committed
    }

    /// Commands containing `search` (ignoring case), newest first, optionally only those run in `directory`
    pub fn matches<'a>(
        &'a self,
        search: &'a str,
        directory: Option<&'a str>,
    ) -> impl Iterator<Item = HistoryMatch> + 'a {
        self.entries
            .iter()
            .filter(move |e| directory.is_none_or(|d| e.working_directory == d))
            .filter_map(move |e| {
                find_ignoring_case(&e.command, search).map(|range| HistoryMatch {
                    command: e.command.clone(),
                    range,
                    timestamp: e.timestamp,
                    working_directory: e.working_directory.clone(),
                })
            })
    }

    /// The `index`th match (0 is the newest)
    pub fn find_match(&self, search: &str, index: usize, directory: Option<&str>) -> Option<HistoryMatch> {
        self.matches(search, directory).nth(index)
    }

    /// The oldest match, and its index
    pub fn find_last_match(&self, search: &str, directory: Option<&str>) -> Option<(HistoryMatch, usize)> {
        self.matches(search, directory).enumerate().last().map(|(i, m)| (m, i))
    }

    /// The last argument of the `index`th most recent command that has one (for Alt-.)
    pub fn historical_argument(&self, index: usize) -> Option<String> {
        self.entries.iter().filter_map(last_argument).nth(index)
    }
}

/// The last word-like token (identifier, keyword, literal or mish word) of a command
fn last_argument(entry: &HistoryEntry) -> Option<String> {
    use mash_syntax::SyntaxKind::*;
    mash_syntax::lexer::tokenise(&entry.command, entry.mish)
        .tokens
        .iter()
        .rfind(|t| t.kind.is_keyword() || matches!(t.kind, IDENTIFIER | NUMBER_LITERAL | STRING_LITERAL | MISH_WORD))
        .map(|t| entry.command[t.range].to_string())
}

/// The first occurrence of `needle` in `haystack`, comparing characters case-insensitively
fn find_ignoring_case(haystack: &str, needle: &str) -> Option<Range<usize>> {
    let same = |a: char, b: char| a == b || a.to_lowercase().eq(b.to_lowercase());
    let starts = haystack.char_indices().map(|(i, _)| i).chain([haystack.len()]);
    starts.into_iter().find_map(|start| {
        let mut rest = haystack[start..].char_indices();
        let mut end = start;
        for n in needle.chars() {
            let (i, h) = rest.next()?;
            if !same(h, n) {
                return None;
            }
            end = start + i + h.len_utf8();
        }
        Some(start..end)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn case_insensitive_search() {
        assert_eq!(find_ignoring_case("fooBAR", "bar"), Some(3..6));
        assert_eq!(find_ignoring_case("foo", ""), Some(0..0));
        assert_eq!(find_ignoring_case("foo", "x"), None);
        assert_eq!(find_ignoring_case("ÉCOLE", "école"), Some(0..6));
    }

    #[test]
    fn session_ids_are_version_4_uuids() {
        let id = new_session_id(&mash_core::platform::TestPlatform::default());
        assert_eq!(id.len(), 36);
        assert_eq!(&id[14..15], "4");
        assert!("89ab".contains(&id[19..20]), "{id}");
        assert_eq!(id.matches('-').count(), 4);
    }
}
