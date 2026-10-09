//! The command history: `os.history`, the `core.HistoryEntry` class, and `jump` (Scala `HistoryFunction`,
//! `HistoryEntryClass`, `JumpFunction`). The history comes from the REPL, through
//! [`Ctx::command_history`]; without a REPL it is empty.

use std::collections::BTreeMap;

use mash_macros::{builtin, builtin_method};

use crate::complete::CompletionSpec;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, Params};
use crate::history::HistoryEntry;
use crate::infer::{Type, TypedArgs};
use crate::value::{BuiltinClass, ClassRef, Field, List, Object, Value};

use super::super::constructor::FieldsConstructor;
use super::super::core::OBJECT_CLASS;
use super::dirs::enter_directory;
use super::files::{date_time, is_dir};
use super::paths::{file_name, path_value};

pub static HISTORY_ENTRY_CLASS: BuiltinClass = BuiltinClass {
    summary: "An entry in Mash command history",
    fields: &[
        Field {
            name: "session",
            summary: "ID of shell session",
            ty: || Type::STRING,
        },
        Field {
            name: "commandNumber",
            summary: "Number of the command within the session",
            ty: || Type::NUMBER,
        },
        Field {
            name: "timestamp",
            summary: "Time command was executed",
            ty: || Type::Instance(&crate::builtins::time::DATE_TIME_CLASS),
        },
        Field {
            name: "command",
            summary: "Command",
            ty: || Type::STRING,
        },
        Field {
            name: "mish",
            summary: "Whether the command was executed in mish mode",
            ty: || Type::BOOLEAN,
        },
        Field {
            name: "result",
            summary: "Result of the command (if available, else null)",
            ty: || Type::ANY,
        },
        Field {
            name: "workingDirectory",
            summary: "Directory where the command was executed",
            ty: || Type::PATH,
        },
    ],
    methods: &[&ReexecuteMethod],
    static_methods: &[&HISTORY_ENTRY_NEW],
    ..BuiltinClass::new("core", "HistoryEntry", Some(&OBJECT_CLASS))
};

static HISTORY_ENTRY_NEW: FieldsConstructor = FieldsConstructor::new(
    "core.HistoryEntry",
    &HISTORY_ENTRY_CLASS,
    "Construct a new HistoryEntry object",
);

/// The history entries, oldest first (none without a REPL)
fn entries(ctx: &Ctx) -> Vec<HistoryEntry> {
    ctx.command_history().map(|h| h.entries()).unwrap_or_default()
}

fn entry_object(entry: HistoryEntry) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(&HISTORY_ENTRY_CLASS)));
    object.set_str("session", Value::from(entry.session_id));
    object.set_str("commandNumber", Value::num(entry.command_number as f64));
    object.set_str("timestamp", date_time(entry.timestamp));
    object.set_str("command", Value::from(entry.command));
    object.set_str("mish", Value::Bool(entry.mish));
    object.set_str("result", entry.result);
    object.set_str("workingDirectory", path_value(entry.working_directory));
    Value::Object(object)
}

/// Return the command history as a sequence of History objects
#[builtin(namespace = "os", returns = Type::seq(Type::Instance(&HISTORY_ENTRY_CLASS)))]
pub(crate) fn history(ctx: &mut Ctx) -> R<List> {
    Ok(List::new(entries(ctx).into_iter().map(entry_object).collect()))
}

/// Re-execute the command ran in this history entry
#[builtin_method]
fn reexecute(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let Value::Object(entry) = target else {
        return Err(MashError::new("Invalid history entry"));
    };
    let command = match entry.get_str("command") {
        Some(Value::Str(s)) => s.as_str().to_string(),
        _ => return Err(MashError::new("Invalid history entry")),
    };
    let mish = entry.get_str("mish").is_some_and(|m| m.is_truthy());
    let settings = ctx.settings;
    ctx.settings.mish = mish;
    let result = ctx.eval_source("eval", &command);
    ctx.settings = settings;
    result
}

// ------------------------------------------------------------------------------------------------ jump

/// The directories commands were run in that still exist, with how many commands were run in each
fn scored_paths(ctx: &Ctx) -> Vec<(String, usize)> {
    let mut counts: BTreeMap<String, usize> = BTreeMap::new();
    for entry in entries(ctx) {
        *counts.entry(entry.working_directory).or_default() += 1;
    }
    counts.into_iter().filter(|(path, _)| is_dir(ctx, path)).collect()
}

#[derive(Clone, Copy)]
enum Matching {
    Exact,
    IgnoringCase,
    Subsequence,
}

/// Whether a path's last segment matches the query
fn matches(path: &str, query: &str, matching: Matching) -> bool {
    let Some(name) = file_name(path) else {
        return false;
    };
    match matching {
        Matching::Exact => name.contains(query),
        Matching::IgnoringCase => name.to_lowercase().contains(&query.to_lowercase()),
        Matching::Subsequence => {
            let mut name = name.to_lowercase().chars().collect::<Vec<_>>().into_iter();
            query.to_lowercase().chars().all(|q| name.any(|c| c == q))
        }
    }
}

/// The paths matching a query: the first of exact, case-insensitive and subsequence matching that matches any
fn matching_paths(scored: &[(String, usize)], query: &str) -> Vec<(String, usize)> {
    [Matching::Exact, Matching::IgnoringCase, Matching::Subsequence]
        .into_iter()
        .map(|matching| {
            scored
                .iter()
                .filter(|(path, _)| matches(path, query, matching))
                .cloned()
                .collect::<Vec<_>>()
        })
        .find(|paths| !paths.is_empty())
        .unwrap_or_default()
}

fn jump_candidates(ctx: &mut Ctx, query: &str) -> Vec<String> {
    matching_paths(&scored_paths(ctx), query)
        .into_iter()
        .map(|(path, _)| path)
        .collect()
}

fn complete_jump(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    match args.bind(params).param_at(params, arg_pos) {
        Some("query") => vec![CompletionSpec::Computed(jump_candidates)],
        _ => vec![],
    }
}

/// Change directory to a working directory from the history that matches the argument
///
/// The query is matched against the last segment of directories that commands have been run in: first as a
/// substring, then ignoring case, then as a subsequence of characters (ignoring case). The directory used most
/// often wins. A query that is itself a directory is used as it is.
///
/// Examples:
/// <mash>
///   jump "proj"  # e.g. cd to ~/projects
/// </mash>
#[builtin(namespace = "os", returns = Type::UNIT, completion = complete_jump)]
pub(crate) fn jump(ctx: &mut Ctx, args: &Args, #[param("Pattern to match a path")] query: String) -> R<()> {
    let path = if is_dir(ctx, &query) {
        query
    } else {
        matching_paths(&scored_paths(ctx), &query)
            .into_iter()
            .max_by_key(|(_, score)| *score)
            .map(|(path, _)| path)
            .ok_or_else(|| args.invalid("query", "No matching path"))?
    };
    if enter_directory(ctx, &path)? {
        Ok(())
    } else {
        Err(args.invalid("query", &format!("Could not change directory to '{path}'")))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matching() {
        assert!(matches("/home/alice/projects", "proj", Matching::Exact));
        assert!(!matches("/home/alice/Projects", "proj", Matching::Exact));
        assert!(matches("/home/alice/Projects", "proj", Matching::IgnoringCase));
        assert!(!matches("/home/alice/Projects", "pjs", Matching::IgnoringCase));
        assert!(matches("/home/alice/Projects", "pjs", Matching::Subsequence));
        assert!(!matches("/home/alice/Projects", "sjp", Matching::Subsequence));
        assert!(!matches("/", "", Matching::Exact));
    }

    #[test]
    fn matching_tiers() {
        let scored = vec![
            ("/a/Foo".to_string(), 1),
            ("/b/foo".to_string(), 2),
            ("/c/fxoxo".to_string(), 3),
        ];
        assert_eq!(matching_paths(&scored, "Foo"), vec![("/a/Foo".to_string(), 1)]);
        assert_eq!(matching_paths(&scored, "FOO").len(), 2);
        assert_eq!(matching_paths(&scored, "fxx"), vec![("/c/fxoxo".to_string(), 3)]);
        assert!(matching_paths(&scored, "zzz").is_empty());
    }
}
