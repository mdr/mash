//! The boundary between the interpreter and its host (native OS, browser, or a test fixture).

use std::cell::Cell;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::time::Duration;

use jiff::Timestamp;
use jiff::tz::TimeZone;

/// Host services the interpreter needs. Implementations live outside `mash-core` (e.g. `mash-os` for
/// native builds), so that the core stays portable. Grows as builtins are ported.
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
    fn read_dir(&self, path: &str) -> std::io::Result<Vec<DirEntry>>;

    /// Pause for `duration` (callers sleep in short slices, so that they can respond to interrupts)
    fn sleep(&self, duration: Duration);

    /// 64 random bits (for `random.*`). Deterministic in tests.
    fn random_u64(&self) -> u64;
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
    Other,
}

/// An in-memory file tree: absolute paths of files and directories (parents are implied).
#[derive(Debug, Clone, Default)]
pub struct MemoryFiles {
    entries: BTreeMap<String, FileKind>,
}

impl MemoryFiles {
    /// A file tree containing the given paths; those ending in `/` are directories, the rest files
    pub fn of(paths: &[&str]) -> MemoryFiles {
        let mut files = MemoryFiles::default();
        for path in paths {
            match path.strip_suffix('/') {
                Some(dir) => files.add(dir, FileKind::Directory),
                None => files.add(path, FileKind::File),
            }
        }
        files
    }

    fn add(&mut self, path: &str, kind: FileKind) {
        let mut ancestor = String::new();
        let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
        for (i, segment) in segments.iter().enumerate() {
            ancestor.push('/');
            ancestor.push_str(segment);
            let segment_kind = if i + 1 == segments.len() {
                kind
            } else {
                FileKind::Directory
            };
            self.entries.entry(ancestor.clone()).or_insert(segment_kind);
        }
    }

    pub fn read_dir(&self, cwd: &str, path: &str) -> std::io::Result<Vec<DirEntry>> {
        let absolute = resolve(cwd, path);
        let prefix = if absolute == "/" {
            "/".to_string()
        } else {
            format!("{absolute}/")
        };
        if absolute != "/" && self.entries.get(&absolute) != Some(&FileKind::Directory) {
            return Err(std::io::Error::new(std::io::ErrorKind::NotFound, absolute));
        }
        Ok(self
            .entries
            .iter()
            .filter_map(|(p, kind)| {
                let rest = p.strip_prefix(&prefix)?;
                (!rest.is_empty() && !rest.contains('/')).then(|| DirEntry {
                    name: rest.to_string(),
                    kind: *kind,
                })
            })
            .collect())
    }
}

/// `path` made absolute (relative to `cwd`) and normalised (`.` and `..` resolved)
fn resolve(cwd: &str, path: &str) -> String {
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

/// A deterministic platform for tests: fixed environment, home `/home/alice`, UTC, a fixed clock (which `sleep`
/// advances instead of sleeping), and an in-memory file tree (empty by default).
#[derive(Debug, Clone)]
pub struct TestPlatform {
    pub env: BTreeMap<String, String>,
    pub home: String,
    pub cwd: String,
    pub time_zone: TimeZone,
    pub files: MemoryFiles,
    pub now: Cell<Timestamp>,
    /// State of the deterministic random number generator
    pub random_state: Cell<u64>,
}

impl Default for TestPlatform {
    fn default() -> Self {
        TestPlatform {
            env: BTreeMap::from([("HOME".into(), "/home/alice".into()), ("USER".into(), "alice".into())]),
            home: "/home/alice".into(),
            cwd: "/home/alice".into(),
            time_zone: TimeZone::UTC,
            files: MemoryFiles::default(),
            now: Cell::new("2026-10-09T12:00:00Z".parse().expect("valid timestamp")),
            random_state: Cell::new(0x853c_49e6_748f_ea9b),
        }
    }
}

impl Platform for TestPlatform {
    fn env_var(&self, name: &str) -> Option<String> {
        self.env.get(name).cloned()
    }

    fn env_vars(&self) -> Vec<(String, String)> {
        self.env.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    }

    fn current_dir(&self) -> Option<String> {
        Some(self.cwd.clone())
    }

    fn home_dir(&self) -> Option<String> {
        Some(self.home.clone())
    }

    fn time_zone(&self) -> TimeZone {
        self.time_zone.clone()
    }

    fn now(&self) -> Timestamp {
        self.now.get()
    }

    fn sleep(&self, duration: Duration) {
        let later = self.now.get().checked_add(duration).unwrap_or(Timestamp::MAX);
        self.now.set(later);
    }

    fn read_dir(&self, path: &str) -> std::io::Result<Vec<DirEntry>> {
        self.files.read_dir(&self.cwd, path)
    }

    /// splitmix64
    fn random_u64(&self) -> u64 {
        let state = self.random_state.get().wrapping_add(0x9e37_79b9_7f4a_7c15);
        self.random_state.set(state);
        let mut z = state;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }
}
