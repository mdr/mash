//! Completing paths in the file system (Scala `PathCompleter`, and `TildeExpander`'s expanding and
//! "retilding").

use mash_syntax::TextRange;
use mash_syntax::escapes::{escape_chars_full, unescape};

use super::{Completion, CompletionLocation, CompletionResult, CompletionType};
use crate::platform::{FileKind, Platform};

/// A path found for some search string. `pos` is where in `path` (in chars) the text searched for starts;
/// `prefix_length` is the length of the directory part.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathCompletion {
    pub path: String,
    pub kind: Option<CompletionType>,
    pub prefix_length: usize,
    pub pos: usize,
}

impl PathCompletion {
    fn sort_key(&self) -> &str {
        self.path.strip_suffix('/').unwrap_or(&self.path)
    }
}

/// `~` or `~/…` with the home directory expanded
fn expand_tilde(s: &str, home: &str) -> Option<String> {
    if s == "~" {
        Some(home.to_string())
    } else {
        s.strip_prefix("~/").map(|rest| format!("{home}/{rest}"))
    }
}

/// A path with a leading home directory replaced by `~`, and how many chars that removed
fn retilde(path: &str, home: &str) -> (String, usize) {
    if home.is_empty() {
        return (path.to_string(), 0);
    }
    match path.strip_prefix(home) {
        Some(rest) if rest.is_empty() || rest.starts_with('/') => (format!("~{rest}"), home.chars().count() - 1),
        _ => (path.to_string(), 0),
    }
}

/// Completions of `s` (the contents of a string literal at `region`) as a path
pub fn complete_paths(
    platform: &dyn Platform,
    s: &str,
    region: TextRange,
    directories_only: bool,
    substring: bool,
) -> Option<CompletionResult> {
    let home = platform.home_dir().unwrap_or_default();
    let expanded = expand_tilde(s, &home);
    let search = unescape(expanded.as_deref().unwrap_or(s));
    let completions = path_completions(platform, &search, directories_only, substring)
        .into_iter()
        .map(|c| {
            let (retilded, chars_lost) = if expanded.is_some() {
                retilde(&c.path, &home)
            } else {
                (c.path.clone(), 0)
            };
            let display_pos = c.pos.saturating_sub(chars_lost);
            let prefix_length = c.prefix_length.saturating_sub(chars_lost);
            let escaped = escape_chars_full(&retilded);
            let insert_pos = escaped
                .index_map
                .get(display_pos)
                .copied()
                .unwrap_or(escaped.escaped.chars().count());
            Completion {
                display: retilded,
                insert: Some(escaped.escaped),
                quoted: true,
                kind: c.kind,
                description: Some(c.path),
                location: CompletionLocation {
                    display_pos,
                    insert_pos,
                    display_prefix_length: prefix_length,
                    insert_prefix_length: prefix_length,
                },
            }
        })
        .collect();
    CompletionResult::of(completions, region)
}

/// Paths matching a search string: children of its directory part whose names start with (or, if
/// `substring`, contain) the rest, plus `.` and `..` where they match
pub fn path_completions(
    platform: &dyn Platform,
    search: &str,
    directories_only: bool,
    substring: bool,
) -> Vec<PathCompletion> {
    let (dir, fragment) = match search.rfind('/') {
        Some(i) if i == search.len() - 1 => (search.trim_end_matches('/'), ""),
        Some(i) => (&search[..i], &search[i + 1..]),
        None => ("", search),
    };
    // The directory as written, for building completions: "" (current directory), "/" or "a/b"
    let dir = if dir.is_empty() && search.starts_with('/') {
        "/"
    } else {
        dir
    };
    let join = |name: &str| match dir {
        "" => name.to_string(),
        "/" => format!("/{name}"),
        d => format!("{d}/{name}"),
    };
    let prefix_length = match dir {
        "" => 0,
        "/" => 1,
        d => d.chars().count() + 1,
    };
    let mut completions = vec![];
    for special in match fragment {
        "." => &[".", ".."][..],
        ".." => &[".."][..],
        _ => &[],
    } {
        completions.push(PathCompletion {
            path: format!("{}/", join(special)),
            kind: Some(CompletionType::Directory),
            prefix_length,
            pos: prefix_length,
        });
    }
    let ignore_dot_files = substring || !fragment.starts_with('.');
    let entries = platform
        .read_dir(if dir.is_empty() { "." } else { dir })
        .unwrap_or_default();
    for entry in entries {
        if ignore_dot_files && entry.name.starts_with('.') {
            continue;
        }
        if directories_only && entry.kind != FileKind::Directory {
            continue;
        }
        let found = if substring {
            entry.name.find(fragment)
        } else {
            entry.name.starts_with(fragment).then_some(0)
        };
        let Some(byte_pos) = found else { continue };
        let pos = entry.name[..byte_pos].chars().count();
        let suffix = if entry.kind == FileKind::Directory { "/" } else { "" };
        completions.push(PathCompletion {
            path: format!("{}{suffix}", join(&entry.name)),
            kind: match entry.kind {
                FileKind::Directory => Some(CompletionType::Directory),
                FileKind::File => Some(CompletionType::File),
                FileKind::Other | FileKind::Symlink => None,
            },
            prefix_length,
            pos: prefix_length + pos,
        });
    }
    completions.sort_by(|a, b| a.sort_key().cmp(b.sort_key()));
    completions
}
