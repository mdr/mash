//! The boundary between the interpreter and its host (native OS, browser, or a test fixture).

use std::collections::BTreeMap;
use std::fmt::Debug;

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
}

/// A deterministic platform for tests: fixed environment, home `/home/alice`, UTC, and a fixed clock.
#[derive(Debug, Clone)]
pub struct TestPlatform {
    pub env: BTreeMap<String, String>,
    pub home: String,
    pub cwd: String,
    pub time_zone: TimeZone,
    pub now: Timestamp,
}

impl Default for TestPlatform {
    fn default() -> Self {
        TestPlatform {
            env: BTreeMap::from([("HOME".into(), "/home/alice".into()), ("USER".into(), "alice".into())]),
            home: "/home/alice".into(),
            cwd: "/home/alice".into(),
            time_zone: TimeZone::UTC,
            now: "2026-10-09T12:00:00Z".parse().expect("valid timestamp"),
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
        self.now
    }
}
