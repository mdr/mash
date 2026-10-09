//! Command history in `~/.mash/history`: one JSON object per line, in the same format as Scala Mash, so both
//! can share it.

use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::PathBuf;

use mash_repl::history::{HistoryEntry, HistoryStorage};

/// Only this much of the end of the file is read: older history is never needed in one session.
const TAIL_BYTES: u64 = 4 << 20;

#[derive(Debug)]
pub(crate) struct FileHistoryStorage {
    path: PathBuf,
}

impl FileHistoryStorage {
    pub(crate) fn new(path: PathBuf) -> Self {
        FileHistoryStorage { path }
    }

    /// The text of the last [`TAIL_BYTES`] of the file, starting at a line boundary
    fn read_tail(&self) -> std::io::Result<String> {
        let mut file = File::open(&self.path)?;
        let len = file.metadata()?.len();
        let start = len.saturating_sub(TAIL_BYTES);
        file.seek(SeekFrom::Start(start))?;
        let mut bytes = vec![];
        file.read_to_end(&mut bytes)?;
        let text = String::from_utf8_lossy(&bytes);
        Ok(match (start, text.find('\n')) {
            // Started mid-line: skip the partial line
            (1.., Some(newline)) => text[newline + 1..].to_string(),
            _ => text.into_owned(),
        })
    }

    fn append(&self, line: &str) -> std::io::Result<()> {
        if let Some(dir) = self.path.parent() {
            std::fs::create_dir_all(dir)?;
        }
        let mut file = OpenOptions::new().create(true).append(true).open(&self.path)?;
        // Other Mash sessions may be appending too
        file.lock()?;
        file.write_all(format!("{line}\n").as_bytes())
    }
}

impl HistoryStorage for FileHistoryStorage {
    fn load(&mut self) -> Vec<HistoryEntry> {
        let Ok(text) = self.read_tail() else {
            return vec![];
        };
        let mut entries: Vec<HistoryEntry> = text.lines().filter_map(parse_entry).collect();
        entries.sort_by_key(|e| e.timestamp);
        entries
    }

    fn save(&mut self, entry: &HistoryEntry) {
        // Losing a history entry isn't worth interrupting the user for
        let _ = self.append(&entry.to_json().to_string());
    }
}

fn parse_entry(line: &str) -> Option<HistoryEntry> {
    HistoryEntry::from_json(&serde_json::from_str(line).ok()?)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(n: u64, timestamp: &str, command: &str) -> HistoryEntry {
        HistoryEntry {
            session_id: "e5a6c37d-6bd9-489e-a1f9-4cc6b52a2644".into(),
            command_number: n,
            timestamp: timestamp.parse().unwrap(),
            command: command.into(),
            mish: false,
            working_directory: "/home/alice".into(),
        }
    }

    #[test]
    fn reads_scala_mash_history() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("history");
        std::fs::write(
            &path,
            concat!(
                r#"{"sessionId":"e5a6c37d-6bd9-489e-a1f9-4cc6b52a2644","commandNumber":5,"timestamp":"2026-10-09T11:39:04.431702Z","command":"git.branch","mish":false,"workingDirectory":"/Users/matt/repos/mdr/mash"}"#,
                "\n",
                "not json\n",
                r#"{"sessionId":"e5a6c37d-6bd9-489e-a1f9-4cc6b52a2644","commandNumber":4,"timestamp":"2026-10-09T11:39:04.419217Z","command":"user","mish":true,"workingDirectory":"/Users/matt/repos/mdr/mash"}"#,
                "\n",
            ),
        )
        .unwrap();
        let entries = FileHistoryStorage::new(path).load();
        // Oldest first; bad lines skipped
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].command, "user");
        assert!(entries[0].mish);
        assert_eq!(entries[1].command, "git.branch");
        assert_eq!(entries[1].command_number, 5);
    }

    #[test]
    fn saved_entries_round_trip_in_the_scala_format() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("mash").join("history");
        let mut storage = FileHistoryStorage::new(path.clone());
        storage.save(&entry(1, "2026-10-09T11:39:04.419217Z", "ls"));
        storage.save(&entry(2, "2026-10-09T11:40:00Z", "\"multi\nline\""));
        let text = std::fs::read_to_string(&path).unwrap();
        assert_eq!(
            text.lines().next().unwrap(),
            r#"{"sessionId":"e5a6c37d-6bd9-489e-a1f9-4cc6b52a2644","commandNumber":1,"timestamp":"2026-10-09T11:39:04.419217Z","command":"ls","mish":false,"workingDirectory":"/home/alice"}"#
        );
        let loaded = storage.load();
        assert_eq!(
            loaded,
            vec![
                entry(1, "2026-10-09T11:39:04.419217Z", "ls"),
                entry(2, "2026-10-09T11:40:00Z", "\"multi\nline\"")
            ]
        );
    }

    #[test]
    fn only_the_tail_of_a_large_file_is_read() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("history");
        let line = entry(1, "2026-10-09T11:39:04Z", &"x".repeat(1000))
            .to_json()
            .to_string();
        let count = (TAIL_BYTES as usize / line.len()) * 2;
        std::fs::write(&path, format!("{line}\n").repeat(count)).unwrap();
        let loaded = FileHistoryStorage::new(path).load();
        // Whole lines (with their newlines) that fit in the tail, less one partial line
        let expected = TAIL_BYTES as usize / (line.len() + 1);
        assert!(
            loaded.len() < count && loaded.len() >= expected - 1,
            "{} of {count}",
            loaded.len()
        );
    }
}
