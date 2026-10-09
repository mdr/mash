//! The prompt and the line buffer (Scala `LineBufferRenderer`): highlighted, wrapped to the terminal width,
//! with continuation lines indented by dots under the prompt, the selection in inverse video, and a status
//! indicator (green if the line is valid, red if not) at the end of the last line.

use std::ops::Range;

use mash_core::ast::SourceUnit;
use mash_core::compile::Settings;
use mash_core::eval::Ctx;
use mash_syntax::Mode;

use super::LinesAndCursor;
use super::highlight::{Highlight, highlight, restyle};
use crate::commands::{mish_command, suffix_mish_command};
use crate::line_buffer::LineBuffer;
use crate::screen::{Line, Point, Style, StyledChar, StyledString, default_colours as colours};

/// The prompt: `[3] ~/src $ `, with `!` instead of `$` in mish mode
pub fn prompt(ctx: &Ctx, command_number: usize, mish: bool) -> StyledString {
    let mut s = Style::fg(colours::YELLOW).apply(&format!("[{command_number}] "));
    let cwd = ctx.platform().current_dir().unwrap_or_default();
    let cwd = match ctx.platform().home_dir() {
        Some(home) => super::retilde(&cwd, &home),
        None => cwd,
    };
    s.push_str(&cwd, Style::fg(colours::CYAN));
    let prompt_char = if mish { "!" } else { "$" };
    s.push_str(&format!(" {prompt_char} "), Style::fg(colours::GREEN).with_bold());
    s
}

/// How to render a line buffer.
#[derive(Clone, Debug)]
pub struct BufferOptions {
    pub columns: usize,
    pub prompt: StyledString,
    pub mish: bool,
    pub bare_words: bool,
    /// Underlined (the match of a history search)
    pub match_range: Option<Range<usize>>,
    /// The line has been accepted: no status indicator
    pub completed: bool,
}

/// Render a line buffer after a prompt, wrapped to `options.columns`.
pub fn render_line_buffer(ctx: &mut Ctx, line_buffer: &LineBuffer, options: &BufferOptions) -> LinesAndCursor {
    let columns = options.columns.max(1);
    let text = line_buffer.text();
    let highlight_options = Highlight {
        bare_words: options.bare_words,
        mish: options.mish,
        cursor: Some(line_buffer.cursor()),
    };
    let mut styled = highlight(Some(&mut *ctx), text, highlight_options);
    if let Some(range) = &options.match_range {
        restyle(&mut styled, text, range.clone(), Style::with_underline);
    }
    let selection = line_buffer.selection_or_cursor();
    restyle(&mut styled, text, selection.clone(), |_| Style::DEFAULT.with_inverse());

    // Character index of each byte offset that starts a character (and the end)
    let char_index = |offset: usize| text[..offset].chars().count();
    let dots = options.prompt.width().saturating_sub(1);
    let continuation = if options.prompt.is_empty() {
        StyledString::new()
    } else {
        StyledString::plain(&format!("{} ", ".".repeat(dots)))
    };

    let line_ranges = line_buffer.line_ranges();
    let cursor_row = line_buffer.cursor_row();
    let mut lines = vec![];
    let mut cursor = None;
    for (row, range) in line_ranges.iter().enumerate() {
        let mut line = if row == 0 {
            options.prompt.clone()
        } else {
            continuation.clone()
        };
        let prefix_len = line.len();
        let (start, end) = (char_index(range.start), char_index(range.end));
        line.chars.extend_from_slice(&styled.chars[start..end]);
        let (groups, positions) = wrap(&line, columns);
        if row == cursor_row {
            let index = prefix_len + text[range.start..line_buffer.cursor()].chars().count();
            let Point { row, column } = positions[index];
            cursor = Some(Point::new(lines.len() + row, column));
        }
        let selection_wraps = row + 1 < line_ranges.len() && selection.contains(&range.end);
        let count = groups.len();
        for (i, mut group) in groups.into_iter().enumerate() {
            let last = i + 1 == count;
            if last && selection_wraps {
                pad(&mut group, columns, Style::DEFAULT.with_inverse());
            }
            lines.push(Line {
                string: group,
                ends_in_newline: last,
            });
        }
    }

    if !options.completed
        && let Some(last) = lines.last_mut()
    {
        let style = if has_error(ctx, text, options) {
            Style::fg(colours::RED)
        } else {
            Style::fg(colours::GREEN)
        };
        pad(&mut last.string, columns, Style::DEFAULT);
        if let Some(c) = last.string.chars.last_mut() {
            *c = StyledChar::new('◉', style);
        }
    }
    LinesAndCursor { lines, cursor }
}

fn pad(s: &mut StyledString, columns: usize, style: Style) {
    while s.width() < columns {
        s.push(StyledChar::new(' ', style));
    }
}

/// Split a line into rows of at most `columns` columns, as a terminal wraps it (a wide character that doesn't
/// fit goes to the next row), and give the position of each character and of the end. If the last row is
/// full, the end is at the start of an extra, empty row.
pub(crate) fn wrap(s: &StyledString, columns: usize) -> (Vec<StyledString>, Vec<Point>) {
    let mut groups = vec![StyledString::new()];
    let mut positions = Vec::with_capacity(s.len() + 1);
    let (mut row, mut column) = (0, 0);
    for &c in &s.chars {
        let width = c.width();
        if column + width > columns && column > 0 {
            groups.push(StyledString::new());
            row += 1;
            column = 0;
        }
        positions.push(Point::new(row, column));
        groups[row].push(c);
        column += width;
    }
    if column >= columns {
        groups.push(StyledString::new());
        row += 1;
        column = 0;
    }
    positions.push(Point::new(row, column));
    (groups, positions)
}

/// Whether the line doesn't parse, or (without bare words) refers to unbound identifiers
fn has_error(ctx: &mut Ctx, text: &str, options: &BufferOptions) -> bool {
    let (code, mish) = if let Some(code) = suffix_mish_command(text) {
        (code, true)
    } else if let Some((_, code)) = mish_command(text) {
        (code, true)
    } else {
        (text, options.mish)
    };
    if !mash_syntax::parse(code, if mish { Mode::Mish } else { Mode::Mash }).ok() {
        return true;
    }
    if options.bare_words {
        return false;
    }
    let settings = Settings { bare_words: true, mish };
    !ctx.compile_forgiving_with(SourceUnit::new("<check>", code), settings)
        .bare_words
        .is_empty()
}
