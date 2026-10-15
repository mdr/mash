//! Methods of strings (Scala `StringClass` and `stringClass/*`), and `core.split`.
//!
//! Lengths and indexes count chars (Unicode scalar values). Case mapping is locale-independent.

use mash_macros::{builtin, builtin_method};

use super::regex::compile;
use super::{CHARACTER_CLASS, REGEX_CLASS};
use crate::builtins::collections::{self, GrepOptions, run_grep, split_into_lines};
use crate::builtins::os::PATH_CLASS;
use crate::eval::{Ctx, MashError, R};
use crate::function::BuiltinMethod;
use crate::value::{ClassRef, List, Number, Str, Value};

/// Methods of `core.String`, in the order of Scala's `StringClass`: the collection functions it methodises,
/// then its own methods
pub(crate) const STRING_METHODS: &[&dyn BuiltinMethod] = &[
    &collections::ALL_METHOD,
    &collections::ALL_BUT_LAST_METHOD,
    &collections::ANY_METHOD,
    &collections::CHUNKED_METHOD,
    &collections::COUNT_BY_METHOD,
    &collections::COUNTS_METHOD,
    &collections::COUNT_MATCHES_METHOD,
    &collections::EACH_METHOD,
    &collections::FLAT_MAP_METHOD,
    &collections::FIND_METHOD,
    &collections::FIRST_METHOD,
    &collections::GROUP_BY_METHOD,
    &collections::IS_EMPTY_METHOD,
    &collections::JOIN_METHOD,
    &collections::LAST_METHOD,
    &collections::LENGTH_METHOD,
    &collections::MAP_METHOD,
    &collections::MAX_BY_METHOD,
    &collections::MAX_METHOD,
    &collections::MIN_BY_METHOD,
    &collections::MIN_METHOD,
    &collections::NON_EMPTY_METHOD,
    &collections::PARTITION_METHOD,
    &collections::PARSE_METHOD,
    &collections::PARSE_TABLE_METHOD,
    &collections::REDUCE_METHOD,
    &collections::REVERSE_METHOD,
    &collections::SCAN_METHOD,
    &collections::SKIP_METHOD,
    &collections::SKIP_UNTIL_METHOD,
    &collections::SKIP_WHILE_METHOD,
    &collections::SLIDING_METHOD,
    &collections::SORT_BY_METHOD,
    &collections::SORT_METHOD,
    &collections::SUM_BY_METHOD,
    &collections::SUM_METHOD,
    &collections::TAKE_WHILE_METHOD,
    &collections::UNIQUE_METHOD,
    &collections::UNIQUE_BY_METHOD,
    &collections::WHERE_METHOD,
    &collections::WHERE_NOT_METHOD,
    &ContainsMethod,
    &DeleteAfterMethod,
    &DeleteUpToMethod,
    &EndsWithMethod,
    &crate::builtins::os::StringGlobMethod,
    &GrepMethod,
    &IndexOfMethod,
    &LinesMethod,
    &MatchesMethod,
    &RegexMethod,
    &ReplaceMethod,
    &StartsWithMethod,
    &SplitMethod,
    &TagMethod,
    &ToCharacterMethod,
    &crate::builtins::time::ToDateMethod,
    &crate::builtins::time::ToDateTimeMethod,
    &ToListMethod,
    &ToLowerMethod,
    &ToNumberMethod,
    &ToPathMethod,
    &ToUpperMethod,
    &TrimMethod,
    &UntaggedMethod,
];

/// Test whether this string contains the given substring
#[builtin_method(returns = crate::infer::Type::BOOLEAN)]
fn contains(ctx: &mut Ctx, target: Str, #[param("Substring to match")] substring: Value) -> R<bool> {
    let substring = ctx.stringify(&substring)?;
    Ok(target.as_str().contains(&substring))
}

/// Delete characters after the first instance of a given substring
///
/// If no match is found, the string is returned unchanged.
///
/// Examples:
/// <mash>
///   '{ "value": 10 } # comment'.deleteAfter '}'     # '{ "value": 10 }'
///   '10 # comment'.deleteAfter --andIncluding ' #'  # '10'
/// </mash>
#[builtin_method(infer = super::infer_rules::same_string)]
fn delete_after(
    target: Str,
    #[param("String to search for")] substring: Str,
    #[param("If true, remove the substring as well (default false)", short = 'i', boolean_flag)] and_including: bool,
) -> R<Str> {
    let s = target.as_str();
    Ok(match s.find(substring.as_str()) {
        Some(i) => target.with_text(&s[..i + if and_including { 0 } else { substring.as_str().len() }]),
        None => target,
    })
}

/// Delete characters up to the first instance of a given substring
///
/// If no match is found, the string is returned unchanged.
///
/// Examples:
/// <mash>
///   'result: { "value": 10 }'.deleteUpTo '{'        # '{ "value": 10 }'
///   'value: 10'.deleteUpTo --andIncluding 'value: ' # '10'
/// </mash>
#[builtin_method(infer = super::infer_rules::same_string)]
fn delete_up_to(
    target: Str,
    #[param("String to search for")] substring: Str,
    #[param("If true, remove the substring as well (default false)", short = 'i', boolean_flag)] and_including: bool,
) -> R<Str> {
    let s = target.as_str();
    Ok(match s.find(substring.as_str()) {
        Some(i) => target.with_text(&s[i + if and_including { substring.as_str().len() } else { 0 }..]),
        None => target,
    })
}

/// Check if this string ends with another
#[builtin_method(returns = crate::infer::Type::BOOLEAN)]
fn ends_with(target: Str, #[param("Suffix to test")] suffix: Str) -> R<bool> {
    Ok(target.as_str().ends_with(suffix.as_str()))
}

/// Check if this string starts with another
#[builtin_method(returns = crate::infer::Type::BOOLEAN)]
fn starts_with(target: Str, #[param("Prefix to test")] prefix: Str) -> R<bool> {
    Ok(target.as_str().starts_with(prefix.as_str()))
}

/// Find all the elements in the lines of this String which match the given query
#[builtin_method(infer = super::infer_rules::string_grep)]
fn grep(
    ctx: &mut Ctx,
    target: Str,
    #[param(
        "Return the first match, if there is one, else null (default false)",
        short = 'f',
        boolean_flag
    )]
    first: bool,
    #[param("Perform a case-insensitive match (default false)", short = 'i', boolean_flag)] ignore_case: bool,
    #[param(
        "Find all items that don't match the given query (default false)",
        short = 'n',
        boolean_flag
    )]
    negate: bool,
    #[param(
        "Interpret query as a regular expression; otherwise, interpret query as the literal string (default false)",
        short = 'r',
        boolean_flag
    )]
    regex: bool,
    #[param("Query to find in the given input")] query: Value,
) -> R<Value> {
    let query = ctx.stringify(&query)?;
    let lines = split_into_lines(target.as_str())
        .into_iter()
        .map(|l| Value::Str(target.with_text(l)))
        .collect();
    let options = GrepOptions {
        ignore_case,
        regex,
        negate,
        first,
        ignore_fields: true,
    };
    run_grep(ctx, lines, &query, &options)
}

/// Return the index of the first occurrence of the given substring, if any, else null
///
/// Examples:
/// <mash>
///   'abcde'.indexOf 'cd' # 2
///   'abcde'.indexOf 'x'  # null
/// </mash>
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn index_of(target: Str, #[param("Substring to find")] substring: Str) -> R<Value> {
    let s = target.as_str();
    Ok(match s.find(substring.as_str()) {
        Some(byte_index) => Value::num(s[..byte_index].chars().count()),
        None => Value::Null,
    })
}

/// Split this string into a sequence of lines
#[builtin_method(infer = super::infer_rules::string_split)]
fn lines(target: Str) -> R<Vec<Value>> {
    Ok(split_into_lines(target.as_str())
        .into_iter()
        .map(|l| Value::Str(target.with_text(l)))
        .collect())
}

/// Test whether this string contains a match within it to a given regular expression
#[builtin_method(returns = crate::infer::Type::BOOLEAN)]
fn matches(
    ctx: &mut Ctx,
    target: Str,
    #[param("Regular expression pattern")] pattern: Value,
    #[param("Perform a case-insensitive match", short = 'i', boolean_flag)] ignore_case: bool,
) -> R<bool> {
    let pattern = ctx.stringify(&pattern)?;
    let regex = compile(&pattern, ignore_case)?;
    regex.is_match(target.as_str()).map_err(super::regex::runtime_error)
}

/// This string as a regular expression
#[builtin_method(aliases = ["r"], returns = crate::infer::Type::tagged_string(&super::REGEX_CLASS))]
fn regex(target: Str) -> R<Str> {
    Ok(target.with_tag(Some(&REGEX_CLASS)))
}

/// Replace occurrences of a string with another
///
/// With `--regex`, the target is a regular expression, and the replacement can refer to its groups as `$1` or
/// `${1}` (or `${name}` for named groups); `$$` is a literal `$`.
#[builtin_method(infer = super::infer_rules::same_string)]
fn replace(
    target: Str,
    #[param("String to replace", name = "target")] target_string: Str,
    #[param("Replacement string")] replacement: Str,
    #[param(
        "Interpret target as a regular expression (default false)",
        short = 'r',
        boolean_flag
    )]
    regex: bool,
) -> R<Str> {
    let s = target.as_str();
    let result = if regex {
        let pattern = compile(target_string.as_str(), false)?;
        replace_all(&pattern, s, replacement.as_str())?
    } else {
        s.replace(target_string.as_str(), replacement.as_str())
    };
    Ok(target.with_text(result))
}

/// Check that a replacement refers only to groups the regex has: an unknown one would be replaced by nothing,
/// quietly losing text (`$1x` is the group named `1x`, not group 1 then `x`)
fn check_group_references(regex: &fancy_regex::Regex, replacement: &str) -> R<()> {
    let is_name_char = |c: char| c.is_ascii_alphanumeric() || c == '_';
    let mut rest = replacement;
    while let Some(dollar) = rest.find('$') {
        rest = &rest[dollar + 1..];
        let name = if let Some(braced) = rest.strip_prefix('{') {
            match braced.find('}') {
                Some(end) => {
                    rest = &braced[end + 1..];
                    &braced[..end]
                }
                // As the regex crate, an unclosed brace is literal text
                None => continue,
            }
        } else if let Some(after) = rest.strip_prefix('$') {
            // `$$`: a literal `$`
            rest = after;
            continue;
        } else {
            let end = rest.find(|c: char| !is_name_char(c)).unwrap_or(rest.len());
            let name = &rest[..end];
            rest = &rest[end..];
            name
        };
        if name.is_empty() {
            continue;
        }
        let known = match name.parse::<usize>() {
            Ok(index) => index < regex.captures_len(),
            Err(_) => regex.capture_names().flatten().any(|n| n == name),
        };
        if !known {
            let hint = if name.starts_with(|c: char| c.is_ascii_digit()) {
                " (to follow a group with text, use ${1}x)"
            } else {
                ""
            };
            return Err(MashError::new(format!(
                "No group '{name}' in the regular expression{hint}"
            )));
        }
    }
    Ok(())
}

fn replace_all(regex: &fancy_regex::Regex, s: &str, replacement: &str) -> R<String> {
    check_group_references(regex, replacement)?;
    let mut out = String::new();
    let mut last = 0;
    for captures in regex.captures_iter(s) {
        let captures = captures.map_err(super::regex::runtime_error)?;
        let whole = captures.get(0).expect("group 0 is the whole match");
        out.push_str(&s[last..whole.start()]);
        captures.expand(replacement, &mut out);
        last = whole.end();
    }
    out.push_str(&s[last..]);
    Ok(out)
}

/// Split this string into a sequence of substrings using a separator
///
/// Without a separator, the string is split into words: runs of whitespace separate them, and whitespace at
/// the start or end is ignored, so there are no empty words. With a separator, every piece is kept, even an
/// empty one (two separators together, or one at the start or end).
///
/// Examples:
/// <mash>
///   ' foo bar  baz '.split          # ['foo', 'bar', 'baz']
///   '   '.split                     # []
///   'a:b:'.split ':'                # ['a', 'b', '']
///   'a1b22c'.split --regex '[0-9]+' # ['a', 'b', 'c']
/// </mash>
#[builtin_method(infer = super::infer_rules::string_split)]
fn split_(
    target: Str,
    #[param(
        "Interpret separator as a regular expression; otherwise, interpret separator as the literal string (default false)",
        short = 'r',
        boolean_flag
    )]
    regex: bool,
    #[param(
        "Separator to split string on; if not provided, the string is split into words, separated by whitespace",
        optional
    )]
    separator: Option<Value>,
) -> R<Vec<Value>> {
    split_string(&target, regex, separator.as_ref())
}

/// Split this string into a sequence of substrings using a separator
///
/// Without a separator, the string is split into words: runs of whitespace separate them, and whitespace at
/// the start or end is ignored. With a separator, every piece is kept, even an empty one.
#[builtin(namespace = "core", infer = super::infer_rules::split)]
pub(crate) fn split(
    #[param(
        "Interpret separator as a regular expression; otherwise, interpret separator as the literal string (default false)",
        short = 'r',
        boolean_flag
    )]
    regex: bool,
    #[param(
        "Separator to split string on; if not provided, the string is split into words, separated by whitespace",
        optional
    )]
    separator: Option<Value>,
    #[param("String to split")] string: Str,
) -> R<Vec<Value>> {
    split_string(&string, regex, separator.as_ref())
}

/// Split as Scala's `SplitMethod.split` does with Java's `split(…, -1)`: empty pieces are kept, including a
/// trailing one. Edge cases (an empty separator, a match at the start) follow Rust's `split`, as agreed.
/// Without a separator, the words: no empty pieces at the ends (as Python's `split()`, Nushell's `split words`
/// and awk; Scala kept them).
fn split_string(target: &Str, regex: bool, separator: Option<&Value>) -> R<Vec<Value>> {
    let s = target.as_str();
    let pieces: Vec<String> = match separator {
        None => s.split_whitespace().map(str::to_string).collect(),
        Some(Value::Str(sep)) if regex => regex_split(sep.as_str(), s)?,
        Some(Value::Str(sep)) => s.split(sep.as_str()).map(str::to_string).collect(),
        Some(other) => {
            return Err(MashError::argument(
                format!(
                    "Invalid argument 'separator'. Invalid separator of type {}",
                    other.type_name()
                ),
                None,
            ));
        }
    };
    Ok(pieces.into_iter().map(|p| Value::Str(target.with_text(p))).collect())
}

fn regex_split(pattern: &str, s: &str) -> R<Vec<String>> {
    compile(pattern, false)?
        .split(s)
        .map(|piece| piece.map(str::to_string).map_err(super::regex::runtime_error))
        .collect()
}

/// This string's tagged type, if any
#[builtin_method(returns = crate::infer::Type::CLASS)]
fn tag(target: Str) -> R<Value> {
    Ok(target.tag().map_or(Value::Null, |t| Value::Class(ClassRef::Builtin(t))))
}

/// This string without any tag class
#[builtin_method(returns = crate::infer::Type::STRING)]
fn untagged(target: Str) -> R<Str> {
    Ok(target.with_tag(None))
}

/// Tag this String, which must be a a single character, as a Character
#[builtin_method(aliases = ["c"], returns = crate::infer::Type::CHARACTER)]
fn to_character(target: Str) -> R<Str> {
    if target.len() == 1 {
        Ok(target.with_tag(Some(&CHARACTER_CLASS)))
    } else {
        Err(MashError::new("Must be a single character"))
    }
}

/// A List of this String's characters
///
/// Returns a list of Character-tagged Strings.
///
/// Example:
/// <mash>
///   'string'.characters  # ['s', 't', 'r', 'i', 'n', 'g']
/// </mash>
#[builtin_method(aliases = ["characters"], returns = crate::infer::Type::seq(crate::infer::Type::CHARACTER))]
fn to_list(target: Str) -> R<List> {
    Ok(target
        .as_str()
        .chars()
        .map(|c| Value::Str(Str::tagged(c.to_string(), &CHARACTER_CLASS)))
        .collect())
}

/// Convert string to lowercase
#[builtin_method(infer = super::infer_rules::same_string)]
fn to_lower(target: Str) -> R<Str> {
    Ok(target.with_text(target.as_str().to_lowercase()))
}

/// Convert string to uppercase
#[builtin_method(infer = super::infer_rules::same_string)]
fn to_upper(target: Str) -> R<Str> {
    Ok(target.with_text(target.as_str().to_uppercase()))
}

/// Parse this string as a number
///
/// Surrounding whitespace is ignored; `Infinity`, `-Infinity` and `NaN` are accepted.
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn to_number(target: Str) -> R<Number> {
    Number::parse(target.as_str())
        .ok_or_else(|| MashError::new(format!("Could not parse '{}' as a number", target.as_str())))
}

/// Tag this string as a path
#[builtin_method(returns = crate::infer::Type::PATH)]
fn to_path(target: Str) -> R<Str> {
    Ok(target.with_tag(Some(&PATH_CLASS)))
}

/// Strip initial and trailing whitespace
#[builtin_method(infer = super::infer_rules::same_string)]
fn trim(target: Str) -> R<Str> {
    Ok(target.with_text(target.as_str().trim()))
}
