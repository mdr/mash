//! File system operations for the native platform.

use std::fs;
use std::io;
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::path::Path;

use jiff::Timestamp;
use mash_core::platform::{FileKind, Metadata};
use nix::unistd::{Gid, Group, Uid, User};

pub(crate) fn path_string(path: &Path) -> String {
    path.to_string_lossy().into_owned()
}

fn timestamp(seconds: i64, nanoseconds: i64) -> Timestamp {
    Timestamp::new(seconds, nanoseconds as i32).unwrap_or(Timestamp::UNIX_EPOCH)
}

fn user_name(uid: u32) -> String {
    match User::from_uid(Uid::from_raw(uid)) {
        Ok(Some(user)) => user.name,
        _ => uid.to_string(),
    }
}

fn group_name(gid: u32) -> String {
    match Group::from_gid(Gid::from_raw(gid)) {
        Ok(Some(group)) => group.name,
        _ => gid.to_string(),
    }
}

pub(crate) fn metadata(m: &fs::Metadata) -> Metadata {
    let file_type = m.file_type();
    let kind = if file_type.is_symlink() {
        FileKind::Symlink
    } else if file_type.is_dir() {
        FileKind::Directory
    } else if file_type.is_file() {
        FileKind::File
    } else {
        FileKind::Other
    };
    Metadata {
        kind,
        size: m.len(),
        permissions: m.mode() & 0o7777,
        owner: user_name(m.uid()),
        group: group_name(m.gid()),
        modified: timestamp(m.mtime(), m.mtime_nsec()),
        accessed: timestamp(m.atime(), m.atime_nsec()),
    }
}

pub(crate) fn write_file(path: &str, contents: &[u8], append: bool) -> io::Result<()> {
    use std::io::Write;
    let mut file = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .append(append)
        .truncate(!append)
        .open(path)?;
    file.write_all(contents)
}

pub(crate) fn remove(path: &str) -> io::Result<()> {
    if fs::symlink_metadata(path)?.is_dir() {
        fs::remove_dir_all(path)
    } else {
        fs::remove_file(path)
    }
}

pub(crate) fn set_permissions(path: &str, permissions: u32) -> io::Result<()> {
    fs::set_permissions(path, fs::Permissions::from_mode(permissions))
}

/// Create a new file or directory with a unique name in the temporary directory
pub(crate) fn create_temp(directory: bool, random: impl Fn() -> u64) -> io::Result<String> {
    let dir = std::env::temp_dir();
    loop {
        let name = format!("mash-{:016x}{}", random(), if directory { "" } else { ".tmp" });
        let path = dir.join(name);
        let created = if directory {
            fs::create_dir(&path)
        } else {
            fs::OpenOptions::new()
                .write(true)
                .create_new(true)
                .open(&path)
                .map(drop)
        };
        match created {
            Ok(()) => return Ok(path_string(&path)),
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(e) => return Err(e),
        }
    }
}
