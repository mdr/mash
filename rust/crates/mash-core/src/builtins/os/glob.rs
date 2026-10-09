//! Glob patterns: `glob "src/**/*.rs"` and `String.glob` (Scala `LinuxFileSystem.glob`, `GlobHelper`).
//!
//! Patterns are matched one path segment at a time: `*` matches any run of characters within a segment, `?`
//! one character, `[abc]`/`[a-z]`/`[!a]` a character class, `{a,b}` alternatives, and a segment `**` any
//! number of directories (including none). `\` escapes the next character. A name starting with `.` is only
//! matched by a segment pattern that also starts with `.` (so `*` and `**` skip dotfiles). Symbolic links to
//! directories are followed, but not round in circles.

use std::collections::HashSet;

use mash_macros::{builtin, builtin_method};

use crate::eval::{Ctx, R};
use crate::platform::FileKind;
use crate::value::{List, Str};

use super::files::{complete_files, path_summaries, summary};
use super::paths::{fs, join};

/// Does a segment contain glob syntax?
fn is_pattern(segment: &str) -> bool {
    segment.contains(['*', '?', '[', '{', '\\'])
}

/// The directory a glob search starts from: the leading segments without glob syntax ("" for the current
/// directory)
pub(crate) fn glob_start(pattern: &str) -> String {
    let absolute = pattern.starts_with('/');
    let literal: Vec<&str> = pattern
        .split('/')
        .filter(|s| !s.is_empty())
        .take_while(|s| !is_pattern(s))
        .collect();
    let joined = literal.join("/");
    if absolute { format!("/{joined}") } else { joined }
}

/// Expand `{a,b}` alternatives (which may nest)
fn expand_braces(pattern: &str) -> Vec<String> {
    let chars: Vec<char> = pattern.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            '\\' => i += 2,
            '{' => {
                // Find the matching close brace, splitting at top-level commas
                let mut depth = 0;
                let mut parts = vec![];
                let mut start = i + 1;
                let mut j = i;
                while j < chars.len() {
                    match chars[j] {
                        '\\' => j += 1,
                        '{' => depth += 1,
                        '}' => {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                        ',' if depth == 1 => {
                            parts.push(chars[start..j].iter().collect::<String>());
                            start = j + 1;
                        }
                        _ => {}
                    }
                    j += 1;
                }
                if j >= chars.len() {
                    return vec![pattern.to_string()];
                }
                parts.push(chars[start..j].iter().collect());
                let prefix: String = chars[..i].iter().collect();
                let suffix: String = chars[j + 1..].iter().collect();
                return parts
                    .iter()
                    .flat_map(|part| expand_braces(&format!("{prefix}{part}{suffix}")))
                    .collect();
            }
            _ => i += 1,
        }
    }
    vec![pattern.to_string()]
}

/// Does `name` match a segment pattern without braces?
fn matches_simple(pattern: &[char], name: &[char]) -> bool {
    match pattern.split_first() {
        None => name.is_empty(),
        Some(('*', rest)) => (0..=name.len()).any(|i| matches_simple(rest, &name[i..])),
        Some(('?', rest)) => !name.is_empty() && matches_simple(rest, &name[1..]),
        Some(('[', rest)) => {
            let Some((&c, name_rest)) = name.split_first() else {
                return false;
            };
            let (negated, class) = match rest.split_first() {
                Some(('!' | '^', class)) => (true, class),
                _ => (false, rest),
            };
            // The first character may be `]` itself
            let Some(close) = class.iter().skip(1).position(|&c| c == ']').map(|p| p + 1) else {
                // No closing bracket: a literal `[`
                return c == '[' && matches_simple(rest, name_rest);
            };
            let mut found = false;
            let mut k = 0;
            while k < close {
                if k + 2 < close && class[k + 1] == '-' {
                    found |= class[k] <= c && c <= class[k + 2];
                    k += 3;
                } else {
                    found |= class[k] == c;
                    k += 1;
                }
            }
            found != negated && matches_simple(&class[close + 1..], name_rest)
        }
        Some(('\\', rest)) => match rest.split_first() {
            Some((escaped, rest)) => name.first() == Some(escaped) && matches_simple(rest, &name[1..]),
            None => name == ['\\'],
        },
        Some((c, rest)) => name.first() == Some(c) && matches_simple(rest, &name[1..]),
    }
}

/// Does a name match a segment pattern? Dotfiles only match patterns starting with a dot.
pub(crate) fn matches_segment(pattern: &str, name: &str) -> bool {
    if name.starts_with('.') && !pattern.starts_with('.') {
        return false;
    }
    let name: Vec<char> = name.chars().collect();
    expand_braces(pattern)
        .iter()
        .any(|p| matches_simple(&p.chars().collect::<Vec<_>>(), &name))
}

struct Search<'a> {
    ctx: &'a mut Ctx,
    found: Vec<String>,
    seen: HashSet<String>,
    /// Real paths of the directories being searched by `**`, to avoid following links round in circles
    ancestors: HashSet<String>,
}

impl Search<'_> {
    fn found(&mut self, path: String) {
        if self.seen.insert(path.clone()) {
            self.found.push(path);
        }
    }

    fn entries(&self, dir: &str) -> Vec<crate::platform::DirEntry> {
        let mut entries = self.ctx.platform().read_dir(fs(dir)).unwrap_or_default();
        entries.sort_by(|a, b| a.name.cmp(&b.name));
        entries
    }

    fn walk(&mut self, dir: &str, segments: &[&str]) -> R<()> {
        self.ctx.check_interrupted()?;
        let Some((segment, rest)) = segments.split_first() else {
            self.found(dir.to_string());
            return Ok(());
        };
        if *segment == "**" {
            self.walk(dir, rest)?;
            let Ok(real) = self.ctx.platform().canonicalize(fs(dir)) else {
                return Ok(());
            };
            if !self.ancestors.insert(real.clone()) {
                return Ok(());
            }
            for entry in self.entries(dir) {
                if entry.kind == FileKind::Directory && !entry.name.starts_with('.') {
                    self.walk(&join(dir, &entry.name), segments)?;
                }
            }
            self.ancestors.remove(&real);
            return Ok(());
        }
        if !is_pattern(segment) {
            let path = join(dir, segment);
            let found = if rest.is_empty() {
                self.ctx.platform().symlink_metadata(fs(&path)).is_ok()
            } else {
                self.ctx.platform().metadata(fs(&path)).is_ok_and(|m| m.is_dir())
            };
            if found {
                self.walk(&path, rest)?;
            }
            return Ok(());
        }
        for entry in self.entries(dir) {
            if !matches_segment(segment, &entry.name) {
                continue;
            }
            let path = join(dir, &entry.name);
            if rest.is_empty() {
                self.found(path);
            } else if entry.kind == FileKind::Directory {
                self.walk(&path, rest)?;
            }
        }
        Ok(())
    }
}

/// The paths matching a glob pattern
pub(crate) fn glob_paths(ctx: &mut Ctx, pattern: &str) -> R<Vec<String>> {
    let start = glob_start(pattern);
    let rest: Vec<&str> = pattern
        .split('/')
        .filter(|s| !s.is_empty())
        .skip(super::paths::segments(&start).len())
        .collect();
    if rest.is_empty() {
        // No glob syntax: the path itself, if it exists
        let exists = ctx.platform().symlink_metadata(fs(&start)).is_ok();
        return Ok(if exists { vec![start] } else { vec![] });
    }
    let mut search = Search {
        ctx,
        found: vec![],
        seen: HashSet::new(),
        ancestors: HashSet::new(),
    };
    search.walk(&start, &rest)?;
    Ok(search.found)
}

fn glob_summaries(ctx: &mut Ctx, pattern: &str) -> R<List> {
    let mut summaries = vec![];
    for path in glob_paths(ctx, pattern)? {
        if let Ok(s) = summary(ctx, &path) {
            summaries.push(s);
        }
    }
    Ok(List::new(summaries))
}

/// Return files matching a glob pattern
///
/// `*` matches any characters within a path segment, `?` any one character, `[abc]` a character class,
/// `{a,b}` alternatives, and `**` any number of directories. Names starting with a dot are only matched
/// by patterns starting with a dot.
///
/// Examples:
/// <mash>
///   glob "*.txt"       # Text files in the current directory
///   glob "src/**/*.rs" # Rust files anywhere under src
/// </mash>
#[builtin(namespace = "os", returns = path_summaries(), completion = complete_files)]
pub(crate) fn glob(ctx: &mut Ctx, #[param("Pattern to match path names against")] pattern: Str) -> R<List> {
    glob_summaries(ctx, pattern.as_str())
}

/// Return paths matching a glob pattern
#[builtin_method(name = "glob", aliases = ["g"], returns = path_summaries())]
pub(crate) fn string_glob(ctx: &mut Ctx, target: Str) -> R<List> {
    glob_summaries(ctx, target.as_str())
}

#[cfg(test)]
mod tests {
    use super::*;

    // Port of GlobHelperTest
    #[test]
    fn finding_the_start_directory_for_a_glob() {
        assert_eq!(glob_start("/etc/*"), "/etc");
        assert_eq!(glob_start("/*"), "/");
        assert_eq!(glob_start("foo/bar/*.java"), "foo/bar");
        assert_eq!(glob_start("foo/*.java"), "foo");
        assert_eq!(glob_start("*.java"), "");
    }

    #[test]
    fn matching_segments() {
        assert!(matches_segment("*.txt", "a.txt"));
        assert!(!matches_segment("*.txt", "a.txt.bak"));
        assert!(!matches_segment("*", ".hidden"));
        assert!(matches_segment(".*", ".hidden"));
        assert!(matches_segment("a?c", "abc"));
        assert!(matches_segment("[ab]x", "bx"));
        assert!(!matches_segment("[!ab]x", "bx"));
        assert!(matches_segment("[a-c]x", "bx"));
        assert!(matches_segment("*.{rs,toml}", "Cargo.toml"));
        assert!(matches_segment("{a,b{c,d}}", "bd"));
        assert!(matches_segment("\\*", "*"));
        assert!(!matches_segment("\\*", "x"));
    }
}
