//! Users and groups, processes, signals, the clipboard, opening things, disk space and the terminal, natively.

use std::collections::HashMap;
use std::ffi::OsStr;
use std::io::{self, IsTerminal, Read, Write};
use std::process::{Command, Stdio};
use std::str::FromStr;
use std::sync::Mutex;

use mash_core::platform::{DiskSpace, GroupEntry, ProcessInfo, Signal, TerminalSize, UserEntry};
use nix::sys::signal;
use nix::unistd::{Pid, User, getuid};
use sysinfo::{ProcessRefreshKind, ProcessesToUpdate, System, UpdateKind};
use uzers::os::unix::{GroupExt, UserExt};

fn lossy(s: &OsStr) -> String {
    s.to_string_lossy().into_owned()
}

// ------------------------------------------------------------------------------------------------ users

/// Serialises walks of the user and group databases (`getpwent`, `getgrent`), which keep global state
static DATABASE: Mutex<()> = Mutex::new(());

/// Every entry in the password database (including directory services, on macOS)
#[allow(unsafe_code)]
pub(crate) fn users() -> io::Result<Vec<UserEntry>> {
    let _guard = DATABASE.lock().unwrap_or_else(|e| e.into_inner());
    // SAFETY: `all_users` walks the database with `setpwent`/`getpwent`/`endpwent`, whose iteration state is
    // global. Nothing else in Mash walks it, and `DATABASE` serialises walks here, so no other iteration can
    // interleave with this one.
    let users: Vec<uzers::User> = unsafe { uzers::all_users() }.collect();
    Ok(users
        .iter()
        .map(|u| UserEntry {
            name: lossy(u.name()),
            uid: u.uid(),
            gid: u.primary_group_id(),
            gecos: lossy(u.gecos()),
            home: lossy(u.home_dir().as_os_str()),
            shell: lossy(u.shell().as_os_str()),
        })
        .collect())
}

/// Every entry in the group database
#[allow(unsafe_code)]
pub(crate) fn groups() -> io::Result<Vec<GroupEntry>> {
    let _guard = DATABASE.lock().unwrap_or_else(|e| e.into_inner());
    // SAFETY: as for `users`: `getgrent`'s iteration state is global, and `DATABASE` serialises walks.
    let groups: Vec<uzers::Group> = unsafe { uzers::all_groups() }.collect();
    Ok(groups
        .iter()
        .map(|g| GroupEntry {
            name: lossy(g.name()),
            gid: g.gid(),
            members: g.members().iter().map(|m| lossy(m)).collect(),
        })
        .collect())
}

/// The name of the user running Mash
pub(crate) fn current_user() -> io::Result<String> {
    let uid = getuid();
    match User::from_uid(uid)? {
        Some(user) => Ok(user.name),
        None => Ok(uid.to_string()),
    }
}

// ------------------------------------------------------------------------------------------------ processes

fn refresh(system: &mut System, which: ProcessesToUpdate<'_>) {
    let kind = ProcessRefreshKind::nothing()
        .without_tasks()
        .with_memory()
        .with_cmd(UpdateKind::Always)
        .with_user(UpdateKind::Always);
    system.refresh_processes_specifics(which, true, kind);
}

/// The names of users by uid (for process owners)
fn usernames() -> HashMap<u32, String> {
    users()
        .unwrap_or_default()
        .into_iter()
        .map(|u| (u.uid, u.name))
        .collect()
}

fn process_info(process: &sysinfo::Process, usernames: &HashMap<u32, String>) -> ProcessInfo {
    let name = lossy(process.name());
    let command = process.cmd().iter().map(|a| lossy(a)).collect::<Vec<_>>().join(" ");
    let owner = process
        .user_id()
        .map(|uid| usernames.get(uid).cloned().unwrap_or_else(|| uid.to_string()));
    ProcessInfo {
        pid: process.pid().as_u32(),
        parent_pid: process.parent().map(|p| p.as_u32()).filter(|p| *p != 0),
        command: if command.is_empty() { name.clone() } else { command },
        name,
        owner,
        resident_size: process.memory(),
        virtual_size: process.virtual_memory(),
    }
}

/// The running processes (not threads), by pid
pub(crate) fn processes() -> io::Result<Vec<ProcessInfo>> {
    let mut system = System::new();
    refresh(&mut system, ProcessesToUpdate::All);
    let usernames = usernames();
    let mut processes: Vec<ProcessInfo> = system
        .processes()
        .values()
        .filter(|p| p.thread_kind().is_none())
        .map(|p| process_info(p, &usernames))
        .collect();
    processes.sort_by_key(|p| p.pid);
    Ok(processes)
}

pub(crate) fn process(pid: u32) -> io::Result<Option<ProcessInfo>> {
    let mut system = System::new();
    let pid = sysinfo::Pid::from_u32(pid);
    refresh(&mut system, ProcessesToUpdate::Some(&[pid]));
    Ok(system.process(pid).map(|p| process_info(p, &usernames())))
}

pub(crate) fn kill(pid: u32, signal: &Signal) -> io::Result<()> {
    let invalid = || io::Error::new(io::ErrorKind::InvalidInput, "Invalid signal");
    let pid = i32::try_from(pid).map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "Invalid pid"))?;
    let signal = match signal {
        Signal::Named(name) => signal::Signal::from_str(&format!("SIG{name}")).map_err(|_| invalid())?,
        Signal::Number(n) => signal::Signal::try_from(*n).map_err(|_| invalid())?,
    };
    signal::kill(Pid::from_raw(pid), signal)?;
    Ok(())
}

// ------------------------------------------------------------------------------------------------ clipboard

/// Whether a program can be found on the `PATH`
fn on_path(program: &str) -> bool {
    std::env::var_os("PATH").is_some_and(|path| std::env::split_paths(&path).any(|dir| dir.join(program).is_file()))
}

/// The commands to copy to and paste from the clipboard: `pbcopy`/`pbpaste` on macOS; on Linux, `wl-copy`/
/// `wl-paste` under Wayland, else (with an X display) `xclip` or `xsel`
fn clipboard_commands() -> io::Result<(Vec<&'static str>, Vec<&'static str>)> {
    if cfg!(target_os = "macos") {
        return Ok((vec!["pbcopy"], vec!["pbpaste"]));
    }
    if std::env::var_os("WAYLAND_DISPLAY").is_some() && on_path("wl-copy") {
        return Ok((vec!["wl-copy"], vec!["wl-paste", "--no-newline"]));
    }
    if std::env::var_os("DISPLAY").is_none() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            "there is no display (neither WAYLAND_DISPLAY nor DISPLAY is set)",
        ));
    }
    if on_path("xclip") {
        return Ok((
            vec!["xclip", "-selection", "clipboard"],
            vec!["xclip", "-selection", "clipboard", "-out"],
        ));
    }
    if on_path("xsel") {
        return Ok((
            vec!["xsel", "--clipboard", "--input"],
            vec!["xsel", "--clipboard", "--output"],
        ));
    }
    Err(io::Error::new(
        io::ErrorKind::NotFound,
        "no clipboard program found (install wl-clipboard, xclip or xsel)",
    ))
}

fn command(argv: &[&str]) -> Command {
    let mut command = Command::new(argv[0]);
    command.args(&argv[1..]).stderr(Stdio::null());
    command
}

fn check_status(status: std::process::ExitStatus, program: &str) -> io::Result<()> {
    if status.success() {
        Ok(())
    } else {
        Err(io::Error::other(format!("{program} failed ({status})")))
    }
}

pub(crate) fn clipboard() -> io::Result<String> {
    let (_, paste) = clipboard_commands()?;
    let output = command(&paste).stdin(Stdio::null()).output()?;
    check_status(output.status, paste[0])?;
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

pub(crate) fn set_clipboard(text: &str) -> io::Result<()> {
    let (copy, _) = clipboard_commands()?;
    let mut child = command(&copy).stdin(Stdio::piped()).stdout(Stdio::null()).spawn()?;
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(text.as_bytes())?;
    }
    check_status(child.wait()?, copy[0])
}

// ------------------------------------------------------------------------------------------------ disk space

/// Space on each mounted file system, from `df -kP` (POSIX output format, sizes in KiB)
pub(crate) fn disk_space() -> io::Result<Vec<DiskSpace>> {
    let mut child = Command::new("df")
        .args(["-kP"])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()?;
    let mut output = String::new();
    if let Some(mut stdout) = child.stdout.take() {
        stdout.read_to_string(&mut output)?;
    }
    // df exits with an error if some file system couldn't be read, but still lists the others
    child.wait()?;
    Ok(output.lines().skip(1).filter_map(parse_df_line).collect())
}

/// A line of `df -kP` output: file system, total, used and available KiB, capacity, mount point. The file
/// system and mount point may contain spaces.
fn parse_df_line(line: &str) -> Option<DiskSpace> {
    let words: Vec<(usize, &str)> = line
        .split_whitespace()
        .map(|w| (w.as_ptr() as usize - line.as_ptr() as usize, w))
        .collect();
    let is_number = |w: &str| !w.is_empty() && w.bytes().all(|b| b.is_ascii_digit());
    let is_capacity = |w: &str| w.strip_suffix('%').is_some_and(|n| is_number(n) || n == "-");
    let i = (1..words.len().saturating_sub(4))
        .find(|&i| words[i..i + 3].iter().all(|(_, w)| is_number(w)) && is_capacity(words[i + 3].1))?;
    let mount_start = words.get(i + 4)?.0;
    let kib = |w: &str| w.parse::<u64>().ok().map(|n| n * 1024);
    Some(DiskSpace {
        filesystem: line[..words[i].0].trim_end().to_string(),
        total: kib(words[i].1)?,
        used: kib(words[i + 1].1)?,
        available: kib(words[i + 2].1)?,
        mount_point: line[mount_start..].trim_end().to_string(),
    })
}

// ------------------------------------------------------------------------------------------------ terminal

/// The size of the terminal, if Mash is running in one (one of its standard streams is a terminal)
pub(crate) fn terminal_size() -> Option<TerminalSize> {
    let terminal = io::stdout().is_terminal() || io::stdin().is_terminal() || io::stderr().is_terminal();
    if !terminal {
        return None;
    }
    let (columns, rows) = crossterm::terminal::size().ok()?;
    (rows > 0 && columns > 0).then_some(TerminalSize { rows, columns })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsing_df_output() {
        assert_eq!(
            parse_df_line("/dev/disk3s1s1   971350180  12276920 134264176     9%    /"),
            Some(DiskSpace {
                filesystem: "/dev/disk3s1s1".into(),
                total: 971350180 * 1024,
                used: 12276920 * 1024,
                available: 134264176 * 1024,
                mount_point: "/".into(),
            })
        );
        let spaced = parse_df_line("map auto_home         0         0         0   100%    /System/Volumes/Data/home")
            .expect("parses");
        assert_eq!(spaced.filesystem, "map auto_home");
        assert_eq!(spaced.mount_point, "/System/Volumes/Data/home");
        let mount_with_space = parse_df_line("/dev/sdb1 100 50 50 50% /media/My Disk").expect("parses");
        assert_eq!(mount_with_space.mount_point, "/media/My Disk");
        assert_eq!(parse_df_line("tmpfs 0 0 0 -% /run/x").map(|d| d.total), Some(0));
        assert_eq!(
            parse_df_line("Filesystem 1024-blocks Used Available Capacity Mounted on"),
            None
        );
        assert_eq!(parse_df_line(""), None);
    }
}
