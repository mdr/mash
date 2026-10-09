//! Completion (tab completion in the REPL), tested without a terminal. Port of
//! `com.github.mdr.mash.completions`.
//!
//! Positions and ranges are byte offsets into the text being completed.

mod assist;
mod find;
mod flags;
mod members;
mod paths;
mod strings;

use mash_syntax::lexer::Token;
use mash_syntax::{SyntaxKind, TextRange, TextSize};

use crate::ast::{Program, SourceUnit};
use crate::compile::Settings;
use crate::eval::Ctx;
use crate::infer::{self, Inferred, Type};

pub use assist::{Assistable, assistance, type_of_nearest_function};
pub use find::{InvocationInfo, contiguous_region, find_invocation_with_flag_arg, find_invocation_with_literal_arg};
pub use members::{MemberInfo, members_of_type, members_of_value};
pub use paths::{PathCompletion, complete_paths, path_completions};

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
    /// One of the strings this function computes (from the system, say), given the text being completed
    Computed(fn(&mut Ctx, &str) -> Vec<String>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionType {
    Directory,
    File,
    Flag,
    Field,
    Method,
    Binding,
    Function,
    Class,
}

/// Where, within a completion's text, the text being completed starts (in chars): e.g. completing `123` with
/// `foo123bar` gives position 3. The prefix is a leading part (such as a directory) common to all completions.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CompletionLocation {
    pub display_pos: usize,
    pub insert_pos: usize,
    pub display_prefix_length: usize,
    pub insert_prefix_length: usize,
}

/// One completion option.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Completion {
    /// Shown in the list of options
    pub display: String,
    /// Inserted if chosen (if different from `display`); includes escapes, but not quotes
    pub insert: Option<String>,
    /// Should be quoted when inserted
    pub quoted: bool,
    pub kind: Option<CompletionType>,
    pub description: Option<String>,
    pub location: CompletionLocation,
}

impl Completion {
    pub fn new(display: impl Into<String>) -> Completion {
        Completion {
            display: display.into(),
            insert: None,
            quoted: false,
            kind: None,
            description: None,
            location: CompletionLocation::default(),
        }
    }

    pub fn insert_text(&self) -> &str {
        self.insert.as_deref().unwrap_or(&self.display)
    }

    /// The text to put in place of the region being completed
    pub fn replacement(&self) -> String {
        if self.quoted {
            format!("\"{}\"", self.insert_text())
        } else {
            self.insert_text().to_string()
        }
    }
}

/// Completion options, and the region of the text they replace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionResult {
    pub completions: Vec<Completion>,
    pub replace: TextRange,
}

impl CompletionResult {
    /// None if there are no completions
    pub fn of(completions: Vec<Completion>, replace: TextRange) -> Option<CompletionResult> {
        (!completions.is_empty()).then_some(CompletionResult { completions, replace })
    }

    /// Combine two results: if both replace the same region, all their (distinct) completions; otherwise the
    /// first
    pub fn merge(first: Option<CompletionResult>, second: Option<CompletionResult>) -> Option<CompletionResult> {
        match (first, second) {
            (Some(mut a), Some(b)) if a.replace == b.replace => {
                for c in b.completions {
                    if !a.completions.contains(&c) {
                        a.completions.push(c);
                    }
                }
                Some(a)
            }
            (Some(a), _) => Some(a),
            (None, b) => b,
        }
    }

    fn sorted(mut self) -> CompletionResult {
        self.completions.sort_by(|a, b| a.display.cmp(&b.display));
        self
    }

    /// The text all the completions share (around the position being completed), to insert straight away
    pub fn common_insert_text(&self) -> String {
        let fragments: Vec<Fragment> = self.completions.iter().map(Fragment::of).collect();
        let common = fragments.into_iter().reduce(|a, b| a.common(&b)).unwrap_or_default();
        let text = format!(
            "{}{}{}",
            common.prefix,
            common.before_reversed.chars().rev().collect::<String>(),
            common.after
        );
        if self.completions.iter().all(|c| c.quoted) {
            format!("\"{text}\"")
        } else {
            text
        }
    }
}

/// Scala `CompletionFragment`: a completion's insert text split at the completion position
#[derive(Debug, Default)]
struct Fragment {
    prefix: String,
    before_reversed: String,
    after: String,
}

impl Fragment {
    fn of(completion: &Completion) -> Fragment {
        let chars: Vec<char> = completion.insert_text().chars().collect();
        let prefix_length = completion.location.insert_prefix_length.min(chars.len());
        let pos = completion.location.insert_pos.clamp(prefix_length, chars.len());
        Fragment {
            prefix: chars[..prefix_length].iter().collect(),
            before_reversed: chars[prefix_length..pos].iter().rev().collect(),
            after: chars[pos..].iter().collect(),
        }
    }

    fn common(&self, other: &Fragment) -> Fragment {
        Fragment {
            prefix: if self.prefix == other.prefix {
                self.prefix.clone()
            } else {
                String::new()
            },
            before_reversed: common_prefix(&self.before_reversed, &other.before_reversed),
            after: common_prefix(&self.after, &other.after),
        }
    }
}

fn common_prefix(a: &str, b: &str) -> String {
    a.chars()
        .zip(b.chars())
        .take_while(|(x, y)| x == y)
        .map(|(x, _)| x)
        .collect()
}

/// The text compiled forgivingly with types inferred: what the completers work from (Scala
/// `CompletionParser.parse`).
pub(crate) struct Analysis {
    pub(crate) program: Program,
    pub(crate) inferred: Inferred,
}

pub(crate) fn analyse(ctx: &mut Ctx, text: &str, mish: bool) -> Analysis {
    let settings = Settings {
        bare_words: false,
        mish,
    };
    let compiled = ctx.compile_forgiving_with(SourceUnit::new("<completion>", text), settings);
    let inferred = infer::infer(ctx, &compiled.program.body);
    Analysis {
        program: compiled.program,
        inferred,
    }
}

pub(crate) fn tokenise(text: &str, mish: bool) -> Vec<Token> {
    mash_syntax::lexer::tokenise(text, mish).tokens
}

fn contains(range: TextRange, pos: TextSize) -> bool {
    range.start() <= pos && pos < range.end()
}

/// Completions at byte offset `pos` of `text` (Scala `Completer.complete`), using the interpreter's globals.
pub fn complete(ctx: &mut Ctx, text: &str, pos: usize, mish: bool) -> Option<CompletionResult> {
    let pos = TextSize::from(pos as u32);
    let tokens = tokenise(text, mish);
    let completer = Completer { text, mish };
    let result = match nearby_token(&tokens, pos) {
        Some(token) => completer.complete_token(ctx, token, pos),
        None => completer.complete_misc(ctx, TextRange::empty(pos), pos),
    };
    result.map(CompletionResult::sorted)
}

fn is_primary(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    matches!(
        kind,
        STRING_LITERAL | IDENTIFIER | LONG_FLAG | MINUS | DOT | DOT_NULL_SAFE
    )
}

/// The token to start completing from: the one just before the cursor or the one under it, preferring
/// identifiers, strings, flags and dots
fn nearby_token(tokens: &[Token], pos: TextSize) -> Option<Token> {
    let before = tokens
        .iter()
        .find(|t| t.range.end() == pos && !t.range.is_empty())
        .copied();
    let on = tokens.iter().find(|t| contains(t.range, pos)).copied();
    match (before, on) {
        (Some(before), Some(on)) => Some(if is_primary(on.kind) || !is_primary(before.kind) {
            on
        } else {
            before
        }),
        (before, on) => on.or(before),
    }
}

pub(crate) struct Completer<'a> {
    pub(crate) text: &'a str,
    pub(crate) mish: bool,
}

impl Completer<'_> {
    fn complete_token(&self, ctx: &mut Ctx, token: Token, pos: TextSize) -> Option<CompletionResult> {
        use SyntaxKind::*;
        match token.kind {
            STRING_LITERAL | SHORT_FLAG => self.complete_as_string(ctx, token.range).result,
            LONG_FLAG => {
                let flags = flags::complete_long_flag(ctx, self, token);
                CompletionResult::merge(flags, self.complete_as_string(ctx, token.range).result)
            }
            MINUS => {
                let as_string = self.complete_as_string(ctx, token.range).result;
                CompletionResult::merge(as_string, flags::complete_all_flags(ctx, self, token))
            }
            IDENTIFIER => self.complete_identifier(ctx, token),
            DOT | DOT_NULL_SAFE => self.complete_dot(ctx, token),
            _ => self.complete_misc(ctx, token.range, pos),
        }
    }

    fn complete_identifier(&self, ctx: &mut Ctx, identifier: Token) -> Option<CompletionResult> {
        let strings::StringCompletion {
            path_completion,
            result: as_string,
        } = self.complete_as_string(ctx, identifier.range);
        let members = members::complete_identifier(ctx, self, identifier);
        let bindings = if members.is_member_expr {
            None
        } else {
            members::complete_bindings(ctx, self.text, self.mish, identifier, true)
        };
        let string_or_bindings = if path_completion {
            CompletionResult::merge(as_string, bindings)
        } else {
            as_string.or(bindings)
        };
        if members.prioritise {
            members.result.or(string_or_bindings)
        } else {
            string_or_bindings.or(members.result)
        }
    }

    fn complete_dot(&self, ctx: &mut Ctx, dot: Token) -> Option<CompletionResult> {
        let tokens = tokenise(self.text, self.mish);
        let index = tokens.iter().position(|t| t.range == dot.range)?;
        let previous = index.checked_sub(1).map(|i| tokens[i]);
        let previous_significant = tokens[..index].iter().rev().find(|t| !t.kind.is_trivia());
        let start = usize::from(dot.range.start());
        let char_before = self.text[..start].chars().next_back();
        let is_member_dot = char_before.is_some_and(|c| !c.is_whitespace() && c != '.');
        let after_string_or_identifier = previous.is_some_and(|t| {
            matches!(
                t.kind,
                SyntaxKind::STRING_LITERAL | SyntaxKind::STRING_END | SyntaxKind::IDENTIFIER
            )
        });
        let after_pipe = previous_significant.is_some_and(|t| t.kind == SyntaxKind::PIPE);
        if is_member_dot && !after_string_or_identifier || after_pipe {
            members::complete_after_dot(ctx, self, dot).or_else(|| self.complete_as_string(ctx, dot.range).result)
        } else {
            self.complete_as_string(ctx, dot.range)
                .result
                .or_else(|| members::complete_after_dot(ctx, self, dot))
        }
    }

    fn complete_misc(&self, ctx: &mut Ctx, region: TextRange, pos: TextSize) -> Option<CompletionResult> {
        let as_string = self.complete_as_string(ctx, region);
        let as_string = if as_string.result.is_some() {
            as_string
        } else {
            self.complete_as_string(ctx, TextRange::empty(pos))
        };
        let bindings = members::complete_bindings_at(ctx, self.text, self.mish, pos);
        if as_string.path_completion {
            CompletionResult::merge(as_string.result, bindings)
        } else {
            as_string.result.or(bindings)
        }
    }
}

/// The text with a range replaced
pub(crate) fn replace(text: &str, range: TextRange, replacement: &str) -> String {
    format!(
        "{}{replacement}{}",
        &text[..usize::from(range.start())],
        &text[usize::from(range.end())..]
    )
}

pub(crate) fn at(pos: usize, len: usize) -> TextRange {
    TextRange::at(TextSize::from(pos as u32), TextSize::from(len as u32))
}
