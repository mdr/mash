//! Paths as strings, with the syntax rules of Java's `Path` (which Scala Mash used): repeated and trailing
//! slashes are dropped, `.` and `..` are kept, and the empty path means the current directory.

use std::io;

use crate::eval::{MashError, R};
use crate::function::FromValue;
use crate::value::{Str, Value};

use super::{PATH_CLASS, PATH_SUMMARY_CLASS};

/// A path as Java would print it: repeated slashes collapsed, no trailing slash (except for the root)
pub(crate) fn normalise(path: &str) -> String {
    let absolute = path.starts_with('/');
    let joined = segments(path).join("/");
    if absolute { format!("/{joined}") } else { joined }
}

/// The names between slashes
pub(crate) fn segments(path: &str) -> Vec<&str> {
    path.split('/').filter(|s| !s.is_empty()).collect()
}

/// The last segment; None for the root (Java `getFileName`)
pub(crate) fn file_name(path: &str) -> Option<String> {
    match segments(path).last() {
        Some(name) => Some(name.to_string()),
        None if path.starts_with('/') => None,
        None => Some(String::new()),
    }
}

/// The path without its last segment; None if there isn't one (Java `getParent`)
pub(crate) fn parent(path: &str) -> Option<String> {
    let segments = segments(path);
    let absolute = path.starts_with('/');
    match segments.split_last() {
        Some((_, [])) if absolute => Some("/".into()),
        Some((_, [])) | None => None,
        Some((_, rest)) => Some(if absolute {
            format!("/{}", rest.join("/"))
        } else {
            rest.join("/")
        }),
    }
}

/// `child` relative to `base` (Java `resolve`): `child` itself if it's absolute or `base` is empty
pub(crate) fn join(base: &str, child: &str) -> String {
    if child.starts_with('/') || base.is_empty() {
        normalise(child)
    } else if child.is_empty() {
        normalise(base)
    } else {
        normalise(&format!("{base}/{child}"))
    }
}

/// A path to give the platform: the empty path is the current directory
pub(crate) fn fs(path: &str) -> &str {
    if path.is_empty() { "." } else { path }
}

/// A path-tagged string
pub(crate) fn path_value(path: impl Into<std::rc::Rc<str>>) -> Value {
    Value::Str(Str::tagged(path, &PATH_CLASS))
}

/// An I/O error as a Mash error, e.g. "Could not read 'foo': No such file or directory"
pub(crate) fn io_error(action: &str, path: &str, error: &io::Error) -> MashError {
    MashError::new(format!("Could not {action} '{path}': {}", describe(error)))
}

/// An I/O error's description, without Rust's " (os error N)" suffix
pub(crate) fn describe(error: &io::Error) -> String {
    let text = error.to_string();
    match text.find(" (os error ") {
        Some(i) => text[..i].to_string(),
        None => text,
    }
}

/// The path a value denotes: a string (with any tag), or a `PathSummary`'s `path` (Scala
/// `safeInterpretAsPath`)
pub(crate) fn path_of(value: &Value) -> Option<String> {
    match value {
        Value::Str(s) => Some(s.as_str().to_string()),
        Value::Object(o) if o.class().is_some_and(|c| c.is_builtin(&PATH_SUMMARY_CLASS)) => {
            o.get_str("path").and_then(|p| p.as_str().map(str::to_string))
        }
        _ => None,
    }
}

/// The path a method's target denotes
pub(crate) fn target_path(target: &Value) -> R<String> {
    path_of(target).ok_or_else(|| {
        MashError::new(format!(
            "Could not interpret a value of type {} as a path",
            target.type_name()
        ))
    })
}

/// A path argument: a string, or a `PathSummary` (Scala `validatePath`).
#[derive(Debug, Clone)]
pub struct PathArg(pub String);

impl FromValue for PathArg {
    fn from_value(value: &Value) -> Result<Self, String> {
        path_of(value)
            .map(PathArg)
            .ok_or_else(|| format!("Must be a path, but was a {}", value.type_name()))
    }
}

/// One or more paths: a path, or a list of them (Scala `validatePaths`).
#[derive(Debug, Clone)]
pub struct PathArgs(pub Vec<String>);

impl FromValue for PathArgs {
    fn from_value(value: &Value) -> Result<Self, String> {
        let invalid = || format!("Could not interpret value of type {} as a path.", value.type_name());
        match value {
            Value::List(xs) => xs
                .snapshot()
                .iter()
                .map(|x| path_of(x).ok_or_else(invalid))
                .collect::<Result<_, _>>()
                .map(PathArgs),
            _ => path_of(value).map(|p| PathArgs(vec![p])).ok_or_else(invalid),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn java_path_syntax() {
        assert_eq!(normalise("a//b/"), "a/b");
        assert_eq!(normalise("/"), "/");
        assert_eq!(file_name("a//b/").as_deref(), Some("b"));
        assert_eq!(file_name("/"), None);
        assert_eq!(file_name("").as_deref(), Some(""));
        assert_eq!(parent("a//b/").as_deref(), Some("a"));
        assert_eq!(parent("/a").as_deref(), Some("/"));
        assert_eq!(parent("a"), None);
        assert_eq!(parent("/"), None);
        assert_eq!(parent("a/./b/../c").as_deref(), Some("a/./b/.."));
        assert_eq!(join("", "foo"), "foo");
        assert_eq!(join("dir", "foo"), "dir/foo");
        assert_eq!(join("/", "foo"), "/foo");
        assert_eq!(join("dir", "/abs"), "/abs");
    }
}
