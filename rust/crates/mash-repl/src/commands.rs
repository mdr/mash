//! Running commands typed at the REPL (Scala `CommandRunner`, `ErrorPrinter`, `DebugCommandRunner`): mish
//! prefixes and suffixes, debug commands, compiling and evaluating, printing the result or the error.

use std::rc::Rc;

use mash_core::ast::SourceUnit;
use mash_core::compile::Settings;
use mash_core::eval::{Ctx, Display, MashError, R, StackFrame};
use mash_core::value::Value;
use mash_syntax::{Mode, TextRange};

use crate::browser::model::DisplayModel;
use crate::draw::styled_to_ansi;
use crate::host::Host;
use crate::render::highlight::{Highlight, highlight};
use crate::render::retilde;
use crate::screen::{Line, Style, StyledString, default_colours as colours};
use crate::view::{ViewConfig, Viewed, view_or_browse, view_value};

/// A line starting with `!` (but not `!{…}` or `!!{…}`) is mish: the length of the prefix up to and including
/// the `!`, and the rest (Scala `MishCommand`)
pub fn mish_command(text: &str) -> Option<(usize, &str)> {
    let bang = text.len() - text.trim_start().len();
    let rest = text[bang..].strip_prefix('!')?;
    if rest.starts_with('{') || rest.starts_with("!{") || rest.contains(['\n', '\r', '\u{85}', '\u{2028}', '\u{2029}'])
    {
        return None;
    }
    Some((bang + 1, rest))
}

/// A line ending with `!` is mish: the line without it (Scala `SuffixMishCommand`)
pub fn suffix_mish_command(text: &str) -> Option<&str> {
    text.strip_suffix('!')
}

/// A debug command (`:tokens …`, `:type …`, …): the keyword and the rest
fn debug_command(text: &str) -> Option<(&str, &str)> {
    let rest = text.trim_start().strip_prefix(':')?;
    let end = rest.find(|c: char| !c.is_alphabetic()).unwrap_or(rest.len());
    Some((&rest[..end], &rest[end..]))
}

/// What happened when a command was run.
#[derive(Debug)]
pub(crate) enum Outcome {
    /// The command was `!` alone: toggle mish mode
    ToggleMish,
    /// The command ran; its result, unless it failed, and what to show in the object browser if the result
    /// is to be browsed rather than printed
    Ran(Option<Value>, Option<DisplayModel>),
}

/// Runs commands, writing results and errors to the host.
pub(crate) struct CommandRunner<'a> {
    pub(crate) ctx: &'a mut Ctx,
    pub(crate) host: &'a mut dyn Host,
    pub(crate) bare_words: bool,
    pub(crate) view_config: ViewConfig,
    /// Print errors (inlining with Alt-E doesn't)
    pub(crate) print_errors: bool,
}

impl CommandRunner<'_> {
    /// Run a line typed at the prompt, printing its result
    pub(crate) fn run(&mut self, command: &str, unit_name: &str, mish: bool) -> Outcome {
        if let Some((keyword, args)) = debug_command(command) {
            self.run_debug_command(keyword, args);
            return Outcome::Ran(None, None);
        }
        let (code, mish) = if let Some((_, rest)) = mish_command(command) {
            if rest.is_empty() {
                return Outcome::ToggleMish;
            }
            (rest, true)
        } else if let Some(code) = suffix_mish_command(command) {
            (code, true)
        } else {
            (command, mish)
        };
        let result = self.run_unit(unit_name, code, mish);
        let browse = result.as_ref().and_then(|value| self.view(value));
        Outcome::Ran(result, browse)
    }

    /// Compile and evaluate some code, printing any error (if `print_errors`); its value, unless it failed
    pub(crate) fn run_unit(&mut self, unit_name: &str, code: &str, mish: bool) -> Option<Value> {
        let unit = SourceUnit::new(unit_name, code);
        self.ctx.settings = Settings {
            bare_words: self.bare_words,
            mish,
        };
        let compiled = match self.ctx.compile(unit.clone()) {
            Ok(compiled) => compiled,
            Err(errors) => {
                if self.print_errors
                    && let Some(error) = errors.first()
                {
                    let frame = StackFrame {
                        location: Some(mash_core::eval::Location {
                            unit: unit.clone(),
                            range: error.range,
                        }),
                        function: None,
                    };
                    let lines = error_lines("Syntax error", &error.message, &unit, &[frame], self.home());
                    self.write_lines(&lines);
                }
                return None;
            }
        };
        self.ctx.set_display(Some(self.display()));
        match self.ctx.eval_program(&compiled.program) {
            Ok(value) => Some(value),
            Err(error) if error.is_interrupted() => {
                let mut line = Style::fg(colours::YELLOW).with_bold().apply("Interrupted:");
                line.push_str(" command cancelled by user", Style::fg(colours::YELLOW));
                self.write_lines(&[line]);
                None
            }
            Err(error) => {
                if self.print_errors {
                    self.print_error(&error, &unit);
                }
                None
            }
        }
    }

    fn print_error(&mut self, error: &MashError, unit: &Rc<SourceUnit>) {
        let lines = error_lines("Error", &error.message, unit, &error.stack, self.home());
        self.write_lines(&lines);
    }

    /// Print the result, or return the model to browse it with
    fn view(&mut self, value: &Value) -> Option<DisplayModel> {
        let size = self.host.size();
        match view_or_browse(self.ctx, value, size, &self.view_config) {
            Ok(Viewed::Print(lines)) => self.write_lines(&lines),
            Ok(Viewed::Browse(model)) => return Some(model),
            Err(error) if error.is_interrupted() => {}
            Err(error) => {
                let unit = SourceUnit::new("<view>", "");
                self.print_error(&error, &unit);
            }
        }
        None
    }

    /// How `watch` draws results: as a command's result is printed (without the object browser), or as an
    /// error's first line, each line cut to the terminal's width
    fn display(&self) -> Display {
        let size = self.host.size();
        let config = ViewConfig {
            browse_large_output: false,
            ..self.view_config
        };
        let home = self.home();
        Display {
            render: Rc::new(move |ctx: &mut Ctx, result: &R<Value>| {
                let lines = match result {
                    Ok(value) => view_value(ctx, value, size, &config)?,
                    Err(error) => {
                        let unit = SourceUnit::new("<watch>", "");
                        error_lines("Error", &error.message, &unit, &[], home.clone())
                    }
                };
                Ok(lines
                    .into_iter()
                    .map(|line| styled_to_ansi(&Line::new(line).truncate(size.columns).string))
                    .collect())
            }),
            rows: size.rows,
        }
    }

    fn home(&self) -> Option<String> {
        self.ctx.platform().home_dir()
    }

    pub(crate) fn write_lines(&mut self, lines: &[StyledString]) {
        for line in lines {
            self.host.write(&styled_to_ansi(line));
            self.host.write("\r\n");
        }
        self.host.flush();
    }

    /// Scala `DebugCommandRunner`: `:tokens`, `:p`/`:pretty` (syntax tree), `:e`/`:expression` (the compiled
    /// expression), `:t`/`:type` (its inferred type)
    fn run_debug_command(&mut self, keyword: &str, code: &str) {
        let plain = |s: &str| s.lines().map(StyledString::plain).collect::<Vec<_>>();
        let lines = match keyword {
            "tokens" => mash_syntax::lexer::tokenise(code, false)
                .tokens
                .iter()
                .map(|t| StyledString::plain(&format!("{:?} {:?}", t.kind, &code[byte_range(t.range)])))
                .collect(),
            "p" | "pretty" => plain(&mash_syntax::parse(code, Mode::Mash).debug_tree()),
            "e" | "expression" | "t" | "type" => {
                let unit = SourceUnit::new("<debug>", code);
                let settings = Settings {
                    bare_words: self.bare_words,
                    mish: false,
                };
                let compiled = self.ctx.compile_forgiving_with(unit, settings);
                if keyword.starts_with('e') {
                    plain(&mash_core::ast::pretty(&compiled.program.body))
                } else {
                    let inferred = mash_core::infer::infer(self.ctx, &compiled.program.body);
                    let ty = inferred.types.get(&compiled.program.body.id);
                    plain(&format!("{ty:?}"))
                }
            }
            _ => plain(&format!("Unknown debug command '{keyword}'")),
        };
        self.write_lines(&lines);
    }
}

fn byte_range(range: TextRange) -> std::ops::Range<usize> {
    usize::from(range.start())..usize::from(range.end())
}

/// An error and the source lines it happened in, with each location underlined (Scala `ErrorPrinter`):
///
/// ```text
/// Error: Cannot find member 'foo' in value of type Number
/// command-2:1:f: def f x = x.foo
///                          ^----
/// 1.f
///   ^
/// ```
///
/// Locations in `unit` (the command just typed) have no prefix; others are prefixed with the unit name, line
/// number and function.
pub(crate) fn error_lines(
    kind: &str,
    message: &str,
    unit: &Rc<SourceUnit>,
    stack: &[StackFrame],
    home: Option<String>,
) -> Vec<StyledString> {
    let strong = Style::fg(colours::RED).with_bold();
    let mut header = strong.apply(&format!("{kind}:"));
    header.push_str(&format!(" {message}"), Style::DEFAULT);
    let mut lines = vec![header];
    if stack.is_empty() {
        lines.extend(unit.text.lines().map(|l| Style::fg(colours::RED).apply(l)));
    }
    for frame in stack {
        let Some(location) = &frame.location else {
            let function = frame.function.as_deref().unwrap_or("anonymous");
            let mut line = strong.apply(&format!("builtin:{function}: "));
            line.push_str("no associated source", Style::DEFAULT);
            lines.push(line);
            lines.push(StyledString::new());
            continue;
        };
        let text = &location.unit.text;
        let start = usize::from(location.range.start());
        let end = usize::from(location.range.end());
        let line_ranges = crate::line_buffer::line_ranges(text);
        let line_of = |offset: usize| line_ranges.iter().position(|r| offset <= r.end).unwrap_or(0);
        let immediate = Rc::ptr_eq(&location.unit, unit);
        let prefix = if immediate {
            String::new()
        } else {
            let name = match &home {
                Some(home) => retilde(&location.unit.name, home),
                None => location.unit.name.clone(),
            };
            let function = frame.function.as_deref().map(|f| format!(":{f}")).unwrap_or_default();
            format!("{name}:{}{function}: ", line_of(start) + 1)
        };
        let first = line_of(start);
        let last = line_of(if end > start { end - 1 } else { start });
        for (row, range) in line_ranges.iter().cloned().enumerate().take(last + 1).skip(first) {
            let source = &text[range.clone()];
            let actual_prefix = if row == first {
                prefix.clone()
            } else {
                " ".repeat(prefix.chars().count())
            };
            let mut line = strong.apply(&actual_prefix);
            line.append(&highlight(None, source, Highlight::default()));
            lines.push(line);
            let mut underline = " ".repeat(prefix.chars().count());
            for (offset, c) in source.char_indices().map(|(i, c)| (range.start + i, c)) {
                let width = unicode_width::UnicodeWidthChar::width(c).unwrap_or(0).max(1);
                let (first, rest) = if offset == start {
                    ('^', '-')
                } else if (start..end).contains(&offset) {
                    ('-', '-')
                } else {
                    (' ', ' ')
                };
                underline.push(first);
                underline.extend(std::iter::repeat_n(rest, width - 1));
            }
            if start == range.end {
                underline.push('^');
            }
            lines.push(strong.apply(underline.trim_end()));
        }
    }
    lines
}
