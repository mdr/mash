//! What the [`Platform`](super::Platform) reports about the system: users and groups, processes, disk space and
//! the terminal; and [`FakeSystem`], the deterministic stand-in used by [`TestPlatform`](super::TestPlatform).

use std::cell::RefCell;
use std::io;

/// A user account (an entry in the password database).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserEntry {
    pub name: String,
    pub uid: u32,
    /// The user's primary group
    pub gid: u32,
    /// The GECOS field: the full name, then optionally comma-separated contact details
    pub gecos: String,
    pub home: String,
    pub shell: String,
}

/// A group (an entry in the group database).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GroupEntry {
    pub name: String,
    pub gid: u32,
    /// Users that have this group as a supplementary group (not those for whom it's the primary group)
    pub members: Vec<String>,
}

/// A running process.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessInfo {
    pub pid: u32,
    pub parent_pid: Option<u32>,
    pub name: String,
    /// The command line (the arguments joined with spaces), or the name if that's not available
    pub command: String,
    /// The owning user's name (or numeric id, if it has no name); None if it can't be found (e.g. another user's
    /// process, on macOS)
    pub owner: Option<String>,
    /// Resident set size, in bytes
    pub resident_size: u64,
    /// Virtual memory size, in bytes
    pub virtual_size: u64,
}

/// A signal to send to a process.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Signal {
    /// By name, without the `SIG` prefix (e.g. `TERM`): one of [`SIGNAL_NAMES`]
    Named(String),
    Number(i32),
}

/// The signals that can be sent by name: those POSIX defines, available on both Linux and macOS
pub const SIGNAL_NAMES: &[&str] = &[
    "ABRT", "ALRM", "BUS", "CHLD", "CONT", "FPE", "HUP", "ILL", "INT", "IO", "KILL", "PIPE", "PROF", "QUIT", "SEGV",
    "STOP", "SYS", "TERM", "TRAP", "TSTP", "TTIN", "TTOU", "URG", "USR1", "USR2", "VTALRM", "WINCH", "XCPU", "XFSZ",
];

/// Space on a mounted file system.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiskSpace {
    /// The device or name of the file system (e.g. `/dev/sda1`)
    pub filesystem: String,
    /// In bytes
    pub total: u64,
    pub used: u64,
    /// Available to unprivileged users
    pub available: u64,
    pub mount_point: String,
}

/// The size of the terminal, in character cells.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TerminalSize {
    pub rows: u16,
    pub columns: u16,
}

/// A fake system for tests: users `root`, `alice` (the current user) and `bob`; a few groups and processes;
/// a clipboard; two file systems; and an 80×24 terminal. Records signals sent.
#[derive(Debug, Clone)]
pub struct FakeSystem {
    pub users: Vec<UserEntry>,
    pub groups: Vec<GroupEntry>,
    pub current_user: String,
    /// Processes that are running; a `KILL`, `TERM`, `INT` or `HUP` signal (or 9, 15, 2 or 1) removes one
    pub processes: RefCell<Vec<ProcessInfo>>,
    /// Every signal sent, in order
    pub signals: RefCell<Vec<(u32, Signal)>>,
    pub clipboard: RefCell<String>,
    pub disks: Vec<DiskSpace>,
    pub terminal: Option<TerminalSize>,
}

fn user(name: &str, uid: u32, gecos: &str, home: &str, shell: &str) -> UserEntry {
    UserEntry {
        name: name.into(),
        uid,
        gid: uid,
        gecos: gecos.into(),
        home: home.into(),
        shell: shell.into(),
    }
}

fn group(name: &str, gid: u32, members: &[&str]) -> GroupEntry {
    GroupEntry {
        name: name.into(),
        gid,
        members: members.iter().map(|m| m.to_string()).collect(),
    }
}

fn process(pid: u32, parent_pid: Option<u32>, name: &str, command: &str, owner: &str) -> ProcessInfo {
    ProcessInfo {
        pid,
        parent_pid,
        name: name.into(),
        command: command.into(),
        owner: Some(owner.into()),
        resident_size: 1024 * u64::from(pid),
        virtual_size: 4096 * u64::from(pid),
    }
}

impl Default for FakeSystem {
    fn default() -> Self {
        FakeSystem {
            users: vec![
                user("root", 0, "System Administrator", "/root", "/bin/sh"),
                user("alice", 1000, "Alice Liddell,Room 1,,", "/home/alice", "/bin/bash"),
                user("bob", 1001, "", "/home/bob", "/bin/zsh"),
            ],
            groups: vec![
                group("root", 0, &[]),
                group("wheel", 10, &["alice"]),
                group("staff", 20, &["alice", "bob"]),
                group("alice", 1000, &[]),
                group("bob", 1001, &[]),
            ],
            current_user: "alice".into(),
            processes: RefCell::new(vec![
                process(1, None, "init", "/sbin/init", "root"),
                process(100, Some(1), "mash", "mash", "alice"),
                process(101, Some(100), "sleep", "sleep 100", "alice"),
                process(102, Some(100), "vim", "vim notes.txt", "alice"),
                process(200, Some(1), "sshd", "/usr/sbin/sshd -D", "root"),
            ]),
            signals: RefCell::new(vec![]),
            clipboard: RefCell::new(String::new()),
            disks: vec![
                DiskSpace {
                    filesystem: "/dev/sda1".into(),
                    total: 100 * 1024 * 1024 * 1024,
                    used: 40 * 1024 * 1024 * 1024,
                    available: 55 * 1024 * 1024 * 1024,
                    mount_point: "/".into(),
                },
                DiskSpace {
                    filesystem: "tmpfs".into(),
                    total: 1024 * 1024 * 1024,
                    used: 0,
                    available: 1024 * 1024 * 1024,
                    mount_point: "/tmp".into(),
                },
            ],
            terminal: Some(TerminalSize { rows: 24, columns: 80 }),
        }
    }
}

impl FakeSystem {
    pub(super) fn kill(&self, pid: u32, signal: &Signal) -> io::Result<()> {
        let mut processes = self.processes.borrow_mut();
        let index = processes
            .iter()
            .position(|p| p.pid == pid)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "No such process"))?;
        self.signals.borrow_mut().push((pid, signal.clone()));
        let terminates = match signal {
            Signal::Named(name) => ["KILL", "TERM", "INT", "HUP"].contains(&name.as_str()),
            Signal::Number(n) => [9, 15, 2, 1].contains(n),
        };
        if terminates {
            processes.remove(index);
        }
        Ok(())
    }
}
