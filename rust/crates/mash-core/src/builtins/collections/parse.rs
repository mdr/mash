//! `parse`: lines of text into objects, by a pattern with named holes (as Nushell's `parse`) or a regex with
//! named groups.

use std::rc::Rc;

use mash_macros::builtin;

use super::seq_like::{SequenceLike, sequence_like};
use super::sequences::split_into_lines;
use crate::builtins::core::{compile_regex, regex_runtime_error};
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, Params};
use crate::infer::{Inferencer, Type, TypedArgs};
use crate::value::{Object, Str, Value};

/// Turn lines of text into objects, by a pattern with a named hole for each field
///
/// Each line that matches the pattern becomes an object with a field for each {name} in the pattern; lines
/// that don't match are left out. The pattern matches the whole line: each field takes as little as it can,
/// and the last takes the rest of the line. {_} matches without making a field. A space matches any run of
/// whitespace, so column-aligned output parses; other text matches itself. The fields' values are strings.
///
/// With --regex, the pattern is a regular expression, and each match of it (anywhere in a line, and as many
/// as there are) becomes an object with a field for each named group, (?<name>…): null for a group that
/// took no part in the match.
///
/// Examples:
/// <mash>
///   !{git log --oneline} | parse '{hash} {subject}'
///   ['x=1', 'y=2'] | parse '{name}={value}'           # [{ name: x, value: 1 }, { name: y, value: 2 }]
///   'took 35ms, then 2s' | parse --regex '(?<n>\d+)(?<unit>ms|s)'
/// </mash>
#[builtin(namespace = "collections", infer = infer_parse)]
pub(crate) fn parse(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "Interpret pattern as a regular expression with named groups (default false)",
        short = 'r',
        boolean_flag
    )]
    regex: bool,
    #[param("Pattern each line must match, with a {name} for each field")] pattern: Str,
    #[param("Lines to parse: a sequence, or a string (treated as a sequence of lines)")] input: Value,
) -> R<Value> {
    let _ = input;
    let lines: Vec<String> = match sequence_like(ctx, args, "input")? {
        SequenceLike::List(items) => items.iter().map(|item| ctx.stringify(item)).collect::<R<_>>()?,
        SequenceLike::Str(s) => split_into_lines(s.as_str()),
        SequenceLike::Object(_) => {
            return Err(args.invalid("input", "Must be a List or String, but was an Object"));
        }
    };
    let source = if regex {
        pattern.as_str().to_string()
    } else {
        pattern_regex(pattern.as_str())?
    };
    let compiled = compile_regex(&source, false)?;
    let names: Vec<Rc<str>> = compiled.capture_names().flatten().map(Rc::from).collect();
    if regex && names.is_empty() {
        return Err(args.invalid(
            "pattern",
            "The regular expression has no named groups, (?<name>…), to make fields from",
        ));
    }
    let mut rows = vec![];
    for line in &lines {
        for captures in compiled.captures_iter(line) {
            let captures = captures.map_err(regex_runtime_error)?;
            let row = Object::new();
            for name in &names {
                let value = captures.name(name).map_or(Value::Null, |m| Value::str(m.as_str()));
                row.set_str(name, value);
            }
            rows.push(Value::Object(row));
        }
    }
    Ok(Value::list(rows))
}

/// The regex for a pattern: anchored to the whole line, each `{name}` a lazy named group, `{_}` a lazy
/// unnamed match, each run of whitespace `\s+`, and anything else literal
fn pattern_regex(pattern: &str) -> R<String> {
    let mut regex = String::from("^");
    let mut names: Vec<&str> = vec![];
    let mut rest = pattern;
    while let Some(c) = rest.chars().next() {
        if c.is_whitespace() {
            rest = rest.trim_start();
            regex.push_str(r"\s+");
        } else if let Some(name) = field(rest) {
            if name == "_" {
                regex.push_str("(?:.*?)");
            } else {
                if names.contains(&name) {
                    return Err(MashError::new(format!(
                        "Invalid pattern: the field '{name}' appears more than once"
                    )));
                }
                names.push(name);
                regex.push_str(&format!("(?<{name}>.*?)"));
            }
            rest = &rest[name.len() + 2..];
        } else {
            regex.push_str(&fancy_regex::escape(&c.to_string()));
            rest = &rest[c.len_utf8()..];
        }
    }
    regex.push('$');
    Ok(regex)
}

/// The name in a `{name}` at the start of `s`: letters, digits and underscores, not starting with a digit
fn field(s: &str) -> Option<&str> {
    let inner = s.strip_prefix('{')?;
    let end = inner.find('}')?;
    let name = &inner[..end];
    let valid = name.chars().next().is_some_and(|c| c.is_alphabetic() || c == '_')
        && name.chars().all(|c| c.is_alphanumeric() || c == '_');
    valid.then_some(name)
}

/// A list of objects with a string field for each name in the pattern, if it's known
fn infer_parse(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let regex = bound.value(params, "regex").is_some_and(|v| v.is_truthy());
    let names: Option<Vec<Rc<str>>> = match bound.value(params, "pattern") {
        Some(Value::Str(pattern)) => {
            let source = if regex {
                Some(pattern.as_str().to_string())
            } else {
                pattern_regex(pattern.as_str()).ok()
            };
            source
                .and_then(|source| compile_regex(&source, false).ok())
                .map(|compiled| compiled.capture_names().flatten().map(Rc::from).collect())
        }
        _ => None,
    };
    Some(Type::seq(match names {
        Some(names) => Type::object(names.into_iter().map(|name| (name, Type::STRING))),
        None => Type::OBJECT,
    }))
}
