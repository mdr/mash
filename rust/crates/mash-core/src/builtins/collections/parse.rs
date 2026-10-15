//! Text into values: `parse`, lines into objects by a pattern with named holes (as Nushell's `parse`) or a
//! regex with named groups; `parseTable`, column-aligned text (as command output) into objects; and `scan`,
//! every match of a regex (as Python's `re.findall`).

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
    let lines = input_lines(ctx, args)?;
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

/// Turn column-aligned text, as many commands print, into objects
///
/// The first line is a header, whose words say where the columns are; each line after it becomes an object
/// with a field for each column. Each word of a line goes to the column whose header it sits under (left- or
/// right-aligned), or, if it's in the gap between two headers, to the column on its left; everything from the
/// last header on is the last column. A value too long for its column, running under the next header, is
/// taken to push the rest of its line along. A header word with nothing under it in any line is part of the name on
/// its left, so names with spaces ("Mounted on") stay whole. Values are strings, with their inner spaces; a
/// missing one is null. Blank lines are skipped.
///
/// With --noHeaders, there is no header line: each line is split at whitespace into fields column1, column2, ….
///
/// Examples:
/// <mash>
///   !{df -h} | parseTable | map (_['Mounted on'])
///   !{docker ps} | parseTable | where (_.STATUS.startsWith 'Exited') | map (.NAMES)
///   !{netstat -rn} | skip 3 | parseTable
/// </mash>
#[builtin(namespace = "collections", name = "parseTable", returns = Type::seq(Type::OBJECT))]
pub(crate) fn parse_table(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "There is no header line: split lines at whitespace, into fields column1, column2, … (default false)",
        short = 'n',
        boolean_flag
    )]
    no_headers: bool,
    #[param("Lines to parse: a sequence, or a string (treated as a sequence of lines)")] input: Value,
) -> R<Value> {
    let _ = input;
    let lines: Vec<Vec<char>> = input_lines(ctx, args)?
        .into_iter()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.trim_end().chars().collect())
        .collect();
    let text = |line: &[char], (start, end): (usize, usize)| Value::str(&line[start..end].iter().collect::<String>());
    if no_headers {
        let rows = lines.iter().map(|line| {
            let row = Object::new();
            for (i, word) in words(line).into_iter().enumerate() {
                row.set_str(&format!("column{}", i + 1), text(line, word));
            }
            Value::Object(row)
        });
        return Ok(Value::list(rows.collect()));
    }
    let Some((header, rows)) = lines.split_first() else {
        return Ok(Value::list(vec![]));
    };
    let columns = words(header);
    let last_start = columns.last().map_or(0, |c| c.0);
    // The words of each row, by column
    let assigned: Vec<Vec<Vec<(usize, usize)>>> = rows
        .iter()
        .map(|row| {
            let mut cells = vec![vec![]; columns.len()];
            // A value too long for its column pushes the rest of the line right: words after it are placed
            // as if that much further left (but their text is taken from where they are)
            let mut shift = 0;
            for (start, end) in words(row) {
                let (placed_start, placed_end) = (start.saturating_sub(shift), end.saturating_sub(shift));
                let k = column_of(&columns, last_start, placed_start, placed_end);
                cells[k].push((start, end));
                if let Some(&(next_start, _)) = columns.get(k + 1)
                    && placed_end > next_start
                {
                    shift += placed_end - next_start + 1;
                }
            }
            cells
        })
        .collect();
    // Columns grouped by name: a header word with nothing under it joins the group on its left
    let mut groups: Vec<Vec<usize>> = vec![];
    for k in 0..columns.len() {
        match groups.last_mut() {
            Some(group) if assigned.iter().all(|cells| cells[k].is_empty()) => group.push(k),
            _ => groups.push(vec![k]),
        }
    }
    let names: Vec<String> = groups
        .iter()
        .map(|group| {
            header[columns[group[0]].0..columns[group[group.len() - 1]].1]
                .iter()
                .collect()
        })
        .collect();
    let objects = rows.iter().zip(&assigned).map(|(row, cells)| {
        let object = Object::new();
        for (name, group) in names.iter().zip(&groups) {
            let spans = group.iter().flat_map(|&k| cells[k].iter());
            let start = spans.clone().map(|s| s.0).min();
            let end = spans.map(|s| s.1).max();
            let value = match (start, end) {
                (Some(start), Some(end)) => text(row, (start, end)),
                _ => Value::Null,
            };
            object.set_str(name, value);
        }
        Value::Object(object)
    });
    Ok(Value::list(objects.collect()))
}

/// The lines of the input: a list's items as strings, or a string's lines
fn input_lines(ctx: &mut Ctx, args: &Args) -> R<Vec<String>> {
    match sequence_like(ctx, args, "input")? {
        SequenceLike::List(items) => items.iter().map(|item| ctx.stringify(item)).collect(),
        SequenceLike::Str(s) => Ok(split_into_lines(s.as_str())),
        SequenceLike::Object(_) => Err(args.invalid("input", "Must be a List or String, but was an Object")),
    }
}

/// The start and end (in characters) of each run of non-whitespace in a line
fn words(line: &[char]) -> Vec<(usize, usize)> {
    let mut words = vec![];
    let mut start = None;
    for (i, c) in line.iter().enumerate() {
        match (c.is_whitespace(), start) {
            (false, None) => start = Some(i),
            (true, Some(s)) => {
                words.push((s, i));
                start = None;
            }
            _ => {}
        }
    }
    if let Some(s) = start {
        words.push((s, line.len()));
    }
    words
}

/// The column a word from `start` to `end` belongs to: the last if it's at or after the last header; else the
/// one whose header it overlaps most; else (in a gap) the one on its left
fn column_of(columns: &[(usize, usize)], last_start: usize, start: usize, end: usize) -> usize {
    if start >= last_start {
        return columns.len() - 1;
    }
    let overlap = |&(cs, ce): &(usize, usize)| end.min(ce).saturating_sub(start.max(cs));
    let (best, most) = columns
        .iter()
        .map(overlap)
        .enumerate()
        .max_by_key(|&(i, o)| (o, std::cmp::Reverse(i)))
        .unwrap_or((0, 0));
    if most > 0 {
        return best;
    }
    columns.iter().rposition(|&(_, ce)| ce <= start).unwrap_or(0)
}

/// Find every match of a regular expression in some text
///
/// What each match gives depends on the groups in the regex, as with Python's re.findall: with no groups, the
/// text matched; with one group, that group's value; with several, a list of their values (null for a group
/// that took no part). Named groups count as groups; to make objects with a field for each, use
/// parse --regex. For a case-insensitive match, start the regex with (?i).
///
/// A list is scanned item by item, and a process result by its output.
///
/// Examples:
/// <mash>
///   'disk 81%, swap 12%'.scan '\d+'              # ['81', '12']
///   'id=1 id=22'.scan 'id=(\d+)'                 # ['1', '22']
///   'a=1 b=2'.scan '(\w)=(\d)'                   # [['a', '1'], ['b', '2']]
///   !{curl -s https://example.com} | scan 'https?://[^"]+'
/// </mash>
#[builtin(namespace = "collections", infer = infer_scan)]
pub(crate) fn scan(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Regular expression to find")] regex: Str,
    #[param("Text to search: a string, or a sequence of them")] input: Value,
) -> R<Value> {
    let _ = input;
    let texts: Vec<String> = match sequence_like(ctx, args, "input")? {
        SequenceLike::List(items) => items.iter().map(|item| ctx.stringify(item)).collect::<R<_>>()?,
        SequenceLike::Str(s) => vec![s.as_str().to_string()],
        SequenceLike::Object(_) => {
            return Err(args.invalid("input", "Must be a List or String, but was an Object"));
        }
    };
    let compiled = compile_regex(regex.as_str(), false)?;
    let groups = compiled.captures_len() - 1;
    let text_of = |m: Option<fancy_regex::Match<'_>>| m.map_or(Value::Null, |m| Value::str(m.as_str()));
    let mut found = vec![];
    for text in &texts {
        for captures in compiled.captures_iter(text) {
            let captures = captures.map_err(regex_runtime_error)?;
            found.push(match groups {
                0 => text_of(captures.get(0)),
                1 => text_of(captures.get(1)),
                _ => Value::list((1..=groups).map(|i| text_of(captures.get(i))).collect()),
            });
        }
    }
    Ok(Value::list(found))
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

/// A list of strings, or of lists of strings if the regex (if it's known) has more than one group
fn infer_scan(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let groups = match bound.value(params, "regex") {
        Some(Value::Str(regex)) => compile_regex(regex.as_str(), false).ok().map(|r| r.captures_len() - 1),
        _ => None,
    };
    Some(Type::seq(match groups {
        Some(groups) if groups > 1 => Type::seq(Type::STRING),
        Some(_) => Type::STRING,
        None => Type::ANY,
    }))
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
