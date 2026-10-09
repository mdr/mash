//! An in-memory file system, for tests (and potentially a browser).

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::io::{self, ErrorKind};

use jiff::Timestamp;

use super::{DirEntry, FileKind, Metadata, resolve};

/// An in-memory file tree, keyed by absolute path; the root directory always exists. Interior mutability lets
/// a platform write through `&self`.
///
/// Files have permissions `rw-r--r--` and directories `rwxr-xr-x`, owner and group `alice`, and a fixed
/// modification time unless written.
#[derive(Debug, Clone, Default)]
pub struct MemoryFiles {
    nodes: RefCell<BTreeMap<String, Node>>,
}

#[derive(Debug, Clone)]
struct Node {
    kind: NodeKind,
    permissions: u32,
    modified: Timestamp,
}

#[derive(Debug, Clone)]
enum NodeKind {
    File(Vec<u8>),
    Directory,
    Symlink(String),
}

/// Links followed before giving up (as Linux's `ELOOP`)
const MAX_LINKS: usize = 40;

const FILE_PERMISSIONS: u32 = 0o644;
const DIRECTORY_PERMISSIONS: u32 = 0o755;
const OWNER: &str = "alice";

fn default_time() -> Timestamp {
    "2026-10-01T09:00:00Z".parse().expect("valid timestamp")
}

fn not_found(path: &str) -> io::Error {
    io::Error::new(ErrorKind::NotFound, format!("No such file or directory: {path}"))
}

fn error(kind: ErrorKind, message: &str, path: &str) -> io::Error {
    io::Error::new(kind, format!("{message}: {path}"))
}

fn parent_of(path: &str) -> Option<&str> {
    match path.rfind('/') {
        _ if path == "/" => None,
        Some(0) => Some("/"),
        Some(i) => Some(&path[..i]),
        None => None,
    }
}

fn child_path(dir: &str, name: &str) -> String {
    if dir == "/" {
        format!("/{name}")
    } else {
        format!("{dir}/{name}")
    }
}

impl MemoryFiles {
    /// A file tree containing the given paths; those ending in `/` are directories, the rest empty files
    pub fn of(paths: &[&str]) -> MemoryFiles {
        let files = MemoryFiles::default();
        for path in paths {
            match path.strip_suffix('/') {
                Some(dir) => files.add(dir, NodeKind::Directory),
                None => files.add(path, NodeKind::File(vec![])),
            }
        }
        files
    }

    /// Add a file with the given contents (and any missing parent directories)
    pub fn with_file(self, path: &str, contents: &str) -> MemoryFiles {
        self.add(path, NodeKind::File(contents.as_bytes().to_vec()));
        self
    }

    /// Add a symbolic link to `target` (and any missing parent directories)
    pub fn with_link(self, path: &str, target: &str) -> MemoryFiles {
        self.add(path, NodeKind::Symlink(target.to_string()));
        self
    }

    /// Set the permission bits of an existing path
    pub fn with_permissions(self, path: &str, permissions: u32) -> MemoryFiles {
        if let Some(node) = self.nodes.borrow_mut().get_mut(&resolve("/", path)) {
            node.permissions = permissions;
        }
        self
    }

    fn add(&self, path: &str, kind: NodeKind) {
        let path = resolve("/", path);
        let mut nodes = self.nodes.borrow_mut();
        let mut ancestor = parent_of(&path);
        while let Some(dir) = ancestor {
            if dir != "/" {
                nodes.entry(dir.to_string()).or_insert_with(|| Node::new(NodeKind::Directory));
            }
            ancestor = parent_of(dir);
        }
        if path != "/" {
            nodes.insert(path, Node::new(kind));
        }
    }

    /// The contents of a file (as text), if it exists (for tests)
    pub fn contents(&self, path: &str) -> Option<String> {
        match &self.nodes.borrow().get(&resolve("/", path))?.kind {
            NodeKind::File(bytes) => Some(String::from_utf8_lossy(bytes).into_owned()),
            _ => None,
        }
    }

    /// Whether anything (including a dangling link) exists at an absolute path (for tests)
    pub fn exists(&self, path: &str) -> bool {
        let path = resolve("/", path);
        path == "/" || self.nodes.borrow().contains_key(&path)
    }

    /// Every path in the tree, in order (for tests)
    pub fn paths(&self) -> Vec<String> {
        self.nodes.borrow().keys().cloned().collect()
    }

    /// The kind of the node at an absolute path with no links in it (the root is a directory)
    fn kind_at(&self, path: &str) -> Option<FileKind> {
        if path == "/" {
            return Some(FileKind::Directory);
        }
        self.nodes.borrow().get(path).map(|node| node.kind.file_kind())
    }

    /// `path` (relative to `cwd`) made absolute, with symbolic links resolved in every segment but the last,
    /// and in the last too if `follow`
    fn real_path(&self, cwd: &str, path: &str, follow: bool) -> io::Result<String> {
        let mut links = 0;
        let mut pending: Vec<String> = resolve(cwd, path)
            .split('/')
            .filter(|s| !s.is_empty())
            .rev()
            .map(str::to_string)
            .collect();
        let mut current = "/".to_string();
        while let Some(segment) = pending.pop() {
            match segment.as_str() {
                "." => continue,
                ".." => {
                    current = parent_of(&current).unwrap_or("/").to_string();
                    continue;
                }
                _ => {}
            }
            if self.kind_at(&current) != Some(FileKind::Directory) {
                return Err(not_found(path));
            }
            let next = child_path(&current, &segment);
            let is_last = pending.is_empty();
            let target = match self.nodes.borrow().get(&next).map(|n| &n.kind) {
                Some(NodeKind::Symlink(target)) if follow || !is_last => Some(target.clone()),
                _ => None,
            };
            match target {
                Some(target) => {
                    links += 1;
                    if links > MAX_LINKS {
                        return Err(error(ErrorKind::Other, "Too many levels of symbolic links", path));
                    }
                    if target.starts_with('/') {
                        current = "/".to_string();
                    }
                    pending.extend(target.split('/').filter(|s| !s.is_empty()).rev().map(str::to_string));
                }
                None => current = next,
            }
        }
        Ok(current)
    }

    /// The real path of something that exists
    fn existing(&self, cwd: &str, path: &str, follow: bool) -> io::Result<String> {
        let real = self.real_path(cwd, path, follow)?;
        if self.kind_at(&real).is_some() {
            Ok(real)
        } else {
            Err(not_found(path))
        }
    }

    pub fn read_dir(&self, cwd: &str, path: &str) -> io::Result<Vec<DirEntry>> {
        let dir = self.existing(cwd, path, true)?;
        if self.kind_at(&dir) != Some(FileKind::Directory) {
            return Err(error(ErrorKind::NotADirectory, "Not a directory", path));
        }
        let names: Vec<String> = {
            let prefix = child_path(&dir, "");
            let nodes = self.nodes.borrow();
            nodes
                .range(prefix.clone()..)
                .take_while(|(p, _)| p.starts_with(&prefix))
                .filter(|(p, _)| !p[prefix.len()..].contains('/'))
                .map(|(p, _)| p[prefix.len()..].to_string())
                .collect()
        };
        Ok(names
            .into_iter()
            .map(|name| {
                let kind = match self.metadata(&dir, &name, true) {
                    Ok(m) => m.kind,
                    Err(_) => FileKind::Other,
                };
                DirEntry { name, kind }
            })
            .collect())
    }

    pub fn metadata(&self, cwd: &str, path: &str, follow: bool) -> io::Result<Metadata> {
        let real = self.existing(cwd, path, follow)?;
        let nodes = self.nodes.borrow();
        let root = Node::new(NodeKind::Directory);
        let node = nodes.get(&real).unwrap_or(&root);
        Ok(Metadata {
            kind: node.kind.file_kind(),
            size: match &node.kind {
                NodeKind::File(bytes) => bytes.len() as u64,
                NodeKind::Directory => 4096,
                NodeKind::Symlink(target) => target.len() as u64,
            },
            permissions: node.permissions,
            owner: OWNER.into(),
            group: OWNER.into(),
            modified: node.modified,
            accessed: node.modified,
        })
    }

    pub fn read_link(&self, cwd: &str, path: &str) -> io::Result<String> {
        let real = self.existing(cwd, path, false)?;
        match &self.nodes.borrow().get(&real).map(|n| &n.kind) {
            Some(NodeKind::Symlink(target)) => Ok(target.clone()),
            _ => Err(error(ErrorKind::InvalidInput, "Not a symbolic link", path)),
        }
    }

    pub fn canonicalize(&self, cwd: &str, path: &str) -> io::Result<String> {
        self.existing(cwd, path, true)
    }

    pub fn read_file(&self, cwd: &str, path: &str) -> io::Result<Vec<u8>> {
        let real = self.existing(cwd, path, true)?;
        match &self.nodes.borrow().get(&real).map(|n| &n.kind) {
            Some(NodeKind::File(bytes)) => Ok(bytes.clone()),
            _ => Err(error(ErrorKind::IsADirectory, "Is a directory", path)),
        }
    }

    /// The real path a new entry would be created at: the parent must be an existing directory
    fn new_entry(&self, cwd: &str, path: &str) -> io::Result<String> {
        let real = self.real_path(cwd, path, true)?;
        let parent = parent_of(&real).ok_or_else(|| error(ErrorKind::AlreadyExists, "File exists", path))?;
        match self.kind_at(parent) {
            Some(FileKind::Directory) => Ok(real),
            Some(_) => Err(error(ErrorKind::NotADirectory, "Not a directory", path)),
            None => Err(not_found(path)),
        }
    }

    pub fn write_file(&self, cwd: &str, path: &str, contents: &[u8], append: bool, now: Timestamp) -> io::Result<()> {
        let real = self.new_entry(cwd, path)?;
        let mut nodes = self.nodes.borrow_mut();
        match nodes.get_mut(&real) {
            Some(Node {
                kind: NodeKind::File(bytes),
                modified,
                ..
            }) => {
                if !append {
                    bytes.clear();
                }
                bytes.extend_from_slice(contents);
                *modified = now;
            }
            Some(_) => return Err(error(ErrorKind::IsADirectory, "Is a directory", path)),
            None => {
                let mut node = Node::new(NodeKind::File(contents.to_vec()));
                node.modified = now;
                nodes.insert(real, node);
            }
        }
        Ok(())
    }

    pub fn create_dir(&self, cwd: &str, path: &str, intermediates: bool) -> io::Result<()> {
        if intermediates {
            let absolute = resolve(cwd, path);
            let mut so_far = String::new();
            for segment in absolute.split('/').filter(|s| !s.is_empty()) {
                so_far.push('/');
                so_far.push_str(segment);
                match self.metadata("/", &so_far, true) {
                    Ok(m) if m.is_dir() => {}
                    Ok(_) => return Err(error(ErrorKind::AlreadyExists, "File exists", path)),
                    Err(_) => self.create_dir("/", &so_far, false)?,
                }
            }
            return Ok(());
        }
        let real = self.new_entry(cwd, path)?;
        if self.kind_at(&real).is_some() {
            return Err(error(ErrorKind::AlreadyExists, "File exists", path));
        }
        self.nodes.borrow_mut().insert(real, Node::new(NodeKind::Directory));
        Ok(())
    }

    pub fn remove(&self, cwd: &str, path: &str) -> io::Result<()> {
        let real = self.existing(cwd, path, false)?;
        if real == "/" {
            return Err(error(ErrorKind::PermissionDenied, "Permission denied", path));
        }
        let prefix = child_path(&real, "");
        self.nodes
            .borrow_mut()
            .retain(|p, _| *p != real && !p.starts_with(&prefix));
        Ok(())
    }

    pub fn copy_file(&self, cwd: &str, from: &str, to: &str, now: Timestamp) -> io::Result<()> {
        let contents = self.read_file(cwd, from)?;
        let permissions = self.metadata(cwd, from, true)?.permissions;
        self.write_file(cwd, to, &contents, false, now)?;
        let real = self.real_path(cwd, to, true)?;
        if let Some(node) = self.nodes.borrow_mut().get_mut(&real) {
            node.permissions = permissions;
        }
        Ok(())
    }

    pub fn rename(&self, cwd: &str, from: &str, to: &str) -> io::Result<()> {
        let source = self.existing(cwd, from, false)?;
        let destination = self.new_entry(cwd, to)?;
        if source == destination {
            return Ok(());
        }
        if destination.starts_with(&child_path(&source, "")) {
            return Err(error(ErrorKind::InvalidInput, "Invalid argument", to));
        }
        match self.kind_at(&destination) {
            Some(FileKind::Directory) => {
                let empty = self.read_dir("/", &destination)?.is_empty();
                if self.kind_at(&source) != Some(FileKind::Directory) {
                    return Err(error(ErrorKind::IsADirectory, "Is a directory", to));
                }
                if !empty {
                    return Err(error(ErrorKind::DirectoryNotEmpty, "Directory not empty", to));
                }
            }
            Some(_) if self.kind_at(&source) == Some(FileKind::Directory) => {
                return Err(error(ErrorKind::NotADirectory, "Not a directory", to));
            }
            _ => {}
        }
        let mut nodes = self.nodes.borrow_mut();
        let prefix = child_path(&source, "");
        let moving: Vec<String> = nodes
            .keys()
            .filter(|p| **p == source || p.starts_with(&prefix))
            .cloned()
            .collect();
        nodes.remove(&destination);
        for old in moving {
            let node = nodes.remove(&old).expect("node being moved");
            let new = format!("{destination}{}", &old[source.len()..]);
            nodes.insert(new, node);
        }
        Ok(())
    }

    pub fn set_permissions(&self, cwd: &str, path: &str, permissions: u32) -> io::Result<()> {
        let real = self.existing(cwd, path, true)?;
        match self.nodes.borrow_mut().get_mut(&real) {
            Some(node) => {
                node.permissions = permissions & 0o7777;
                Ok(())
            }
            None => Err(error(ErrorKind::PermissionDenied, "Permission denied", path)),
        }
    }

    /// Create a new file or directory with a unique name under `/tmp`
    pub fn create_temp(&self, directory: bool) -> io::Result<String> {
        self.add("/tmp", NodeKind::Directory);
        let mut n = 1;
        loop {
            let path = format!("/tmp/mash{n}{}", if directory { "" } else { ".tmp" });
            if !self.exists(&path) {
                let kind = if directory {
                    NodeKind::Directory
                } else {
                    NodeKind::File(vec![])
                };
                self.add(&path, kind);
                return Ok(path);
            }
            n += 1;
        }
    }
}

impl Node {
    fn new(kind: NodeKind) -> Node {
        let permissions = match kind {
            NodeKind::Directory => DIRECTORY_PERMISSIONS,
            NodeKind::Symlink(_) => 0o777,
            NodeKind::File(_) => FILE_PERMISSIONS,
        };
        Node {
            kind,
            permissions,
            modified: default_time(),
        }
    }
}

impl NodeKind {
    fn file_kind(&self) -> FileKind {
        match self {
            NodeKind::File(_) => FileKind::File,
            NodeKind::Directory => FileKind::Directory,
            NodeKind::Symlink(_) => FileKind::Symlink,
        }
    }
}
