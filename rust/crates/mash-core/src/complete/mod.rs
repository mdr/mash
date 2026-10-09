//! Completion (tab completion in the REPL), tested without a terminal. Port of
//! `com.github.mdr.mash.completions`.

use crate::infer::Type;

/// How an argument of a builtin can be completed (Scala `CompletionSpec`).
#[derive(Debug, Clone)]
pub enum CompletionSpec {
    /// A directory path
    Directory,
    /// A file (or directory) path
    File,
    /// The name of a member of a value of this type (e.g. a field to sort by)
    Members(Type),
    /// One of these strings
    Items(Vec<String>),
}
