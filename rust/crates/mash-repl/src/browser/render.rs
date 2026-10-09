//! Drawing the object browser (Scala `view.render.browser`): a status line at the top with the value's type
//! and path (or the expression being typed), the view itself, and a status line at the bottom with key hints
//! (or the search). It's drawn on the terminal's alternate screen.

use mash_core::eval::{Ctx, primary_class};
use mash_core::platform::FileKind;
use mash_core::value::Value;

use super::search::SearchState;
use super::{
    BrowserStack, BrowserState, HelpState, SelectionInfo, SingleObjectState, TextLinesState, TreeState, TwoDTableState,
    ValueState,
};
use crate::actions::describe_key;
use crate::input::{Key, KeyPress};
use crate::render::assistance::render_assistance;
use crate::render::buffer::{BufferOptions, render_line_buffer};
use crate::render::completions::render_completions;
use crate::render::highlight::{Highlight, highlight};
use crate::render::{LinesAndCursor, ellipsisise_styled};
use crate::screen::{Dimensions, Line, Screen, Style, StyledChar, StyledString, default_colours as colours};
use crate::view::Decorations;
use crate::view::object::render_object_table;
use crate::view::table::render_table;
use crate::view::tree::render_tree;

/// A key and what it does, shown in a status line (Scala `KeyHint`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct KeyHint {
    pub key: KeyPress,
    pub description: &'static str,
}

const fn hint(c: char, description: &'static str) -> KeyHint {
    KeyHint {
        key: KeyPress {
            key: Key::Char(c),
            ctrl: false,
            alt: false,
            shift: false,
        },
        description,
    }
}

const fn ctrl_hint(c: char, description: &'static str) -> KeyHint {
    KeyHint {
        key: KeyPress {
            key: Key::Char(c),
            ctrl: true,
            alt: false,
            shift: false,
        },
        description,
    }
}

/// The browser's key hints
pub mod hints {
    use super::{KeyHint, ctrl_hint, hint};
    use crate::input::{Key, KeyPress};

    pub const EXIT: KeyHint = hint('q', "exit");
    pub const FOCUS: KeyHint = hint('f', "focus");
    pub const BACK: KeyHint = hint('b', "back");
    pub const INSERT: KeyHint = hint('i', "insert");
    pub const INSERT_WHOLE: KeyHint = hint('I', "insert whole");
    pub const MARK: KeyHint = hint('m', "mark");
    pub const ROW: KeyHint = hint('R', "row");
    pub const TREE: KeyHint = hint('t', "tree");
    pub const TABLE: KeyHint = hint('t', "table");
    pub const HIDE_COLUMN: KeyHint = hint('h', "hide col");
    pub const SEARCH: KeyHint = hint('/', "find");
    pub const EXPRESSION: KeyHint = hint('e', "expr");
    pub const DIR: KeyHint = hint('d', "dir");
    pub const READ: KeyHint = hint('r', "read");
    pub const OPEN: KeyHint = hint('o', "open");
    pub const COPY: KeyHint = hint('c', "copy");
    pub const NEXT_PARENT: KeyHint = hint('N', "next prnt");
    pub const PREVIOUS_PARENT: KeyHint = hint('P', "prev prnt");
    pub const NEXT_HIT: KeyHint = ctrl_hint('n', "next");
    pub const PREVIOUS_HIT: KeyHint = ctrl_hint('p', "prev");
    pub const CASE_SENSITIVE: KeyHint = ctrl_hint('t', "case sensitive");
    pub const CASE_INSENSITIVE: KeyHint = ctrl_hint('t', "ignore case");
    pub const DONE: KeyHint = KeyHint {
        key: KeyPress {
            key: Key::Enter,
            ctrl: false,
            alt: false,
            shift: false,
        },
        description: "done",
    };
    pub const QUIT: KeyHint = ctrl_hint('g', "quit");
}

/// Hints such as `q exit, b back`, each key in inverse cyan
pub fn render_key_hints(hints: &[KeyHint]) -> StyledString {
    let key_style = Style::fg(colours::CYAN).with_inverse();
    let rendered = hints.iter().map(|hint| {
        let mut s = key_style.apply(&describe_key(hint.key));
        s.push_str(&format!(" {}", hint.description), Style::DEFAULT);
        s
    });
    StyledString::join(rendered, &StyledString::plain(", "))
}

/// What the browser needs to know to draw itself.
#[derive(Clone, Copy, Debug)]
pub struct BrowserRenderOptions {
    pub size: Dimensions,
    /// Unbound identifiers are strings (for highlighting the path and expression)
    pub bare_words: bool,
}

/// The browser's screen, showing its current state
pub fn render_browser(stack: &BrowserStack, ctx: &mut Ctx, options: BrowserRenderOptions) -> Screen {
    let state = stack.current();
    let size = options.size;
    let upper = upper_status_lines(state, ctx, options);
    let mut other = match state {
        BrowserState::TwoDTable(s) => two_d_table_lines(s, ctx, size),
        BrowserState::SingleObject(s) => single_object_lines(s, size),
        BrowserState::Tree(s) => tree_lines(s, size),
        BrowserState::TextLines(s) => text_lines(s, size),
        BrowserState::Help(s) => help_lines(s, size),
        BrowserState::Value(s) => value_lines(s, size),
    };
    // The upper status lines take the place of the first lines of the view, except the first
    let mut lines = upper.lines;
    other.drain(..lines.len().saturating_sub(1).min(other.len()));
    lines.extend(other);
    let title = format!("mash {}", ctx.platform().current_dir().unwrap_or_default());
    Screen {
        lines,
        cursor: upper.cursor,
        title,
        alternate: true,
    }
    .truncate(size)
}

/// The value's type and path, or the expression being typed (with any completions and assistance)
fn upper_status_lines(state: &BrowserState, ctx: &mut Ctx, options: BrowserRenderOptions) -> LinesAndCursor {
    let prefix = Style::fg(colours::ORANGE).apply(&format!("{}: ", type_name(&state.raw_value())));
    match state.expression() {
        Some(expression) => {
            let buffer_options = BufferOptions {
                columns: options.size.columns,
                prompt: prefix,
                mish: false,
                bare_words: options.bare_words,
                match_range: None,
                completed: false,
            };
            let rendered = render_line_buffer(ctx, &expression.line_buffer, &buffer_options);
            let assistance = match &expression.assistance {
                Some(assistance) => render_assistance(&assistance.0, options.size),
                None => vec![],
            };
            let mut lines = rendered.lines;
            if lines.is_empty() {
                lines.push(Line::new(StyledString::new()));
            }
            if let Some(completion) = &expression.completion {
                let used = lines.len() + assistance.len();
                let space = Dimensions::new(options.size.rows.saturating_sub(used), options.size.columns);
                lines.extend(render_completions(completion, space));
            }
            lines.extend(assistance);
            LinesAndCursor {
                lines,
                cursor: rendered.cursor,
            }
        }
        None => {
            let path = state.path().replace('\n', " ");
            let highlight_options = Highlight {
                bare_words: options.bare_words,
                mish: false,
                cursor: None,
            };
            let line = prefix + &highlight(Some(ctx), &path, highlight_options);
            LinesAndCursor {
                lines: vec![Line::new(line)],
                cursor: None,
            }
        }
    }
}

/// The name of a value's class, or of its tag (Scala `AbstractBrowserRenderer.typeName`)
fn type_name(value: &Value) -> String {
    match value.tag() {
        Some(tag) => tag.name.to_string(),
        None => primary_class(value).name(),
    }
}

/// `current/total`, padded to the width of `total`, in inverse video
fn render_count(current: usize, total: usize) -> StyledString {
    let width = total.to_string().len();
    Style::DEFAULT
        .with_inverse()
        .apply(&format!("{current:>width$}/{total}"))
}

/// `(hints)` after a count
fn counted_hints(count: StyledString, hints: &[KeyHint]) -> Line {
    let mut line = count;
    line.push_str(" (", Style::DEFAULT);
    line.append(&render_key_hints(hints));
    line.push_str(")", Style::DEFAULT);
    Line::new(line)
}

fn expression_status_line() -> Line {
    Line::new(render_key_hints(&[hints::QUIT, hints::DONE]))
}

/// The search's hit count, query and hints (Scala `StatusLineRenderers.renderIncrementalSearchStatusLine`)
fn search_status_line(current_row: usize, search: &SearchState) -> Line {
    let rows = search.rows();
    // As in Scala Mash, 0 if the current row has no hit
    let current_hit = rows.iter().position(|&r| r == current_row).map_or(0, |i| i + 1);
    let case_hint = if search.ignore_case {
        hints::CASE_SENSITIVE
    } else {
        hints::CASE_INSENSITIVE
    };
    let mut line = Style::DEFAULT
        .with_inverse()
        .apply(&format!("{current_hit}/{}", rows.len()));
    line.push_str(&format!(" Find: {} (", search.query), Style::DEFAULT);
    line.append(&render_key_hints(&[
        hints::NEXT_HIT,
        hints::PREVIOUS_HIT,
        hints::DONE,
        case_hint,
    ]));
    line.push_str(")", Style::DEFAULT);
    Line::new(line)
}

fn plain_lines(lines: Vec<StyledString>) -> Vec<Line> {
    lines.into_iter().map(Line::new).collect()
}

fn two_d_table_lines(state: &TwoDTableState, ctx: &Ctx, size: Dimensions) -> Vec<Line> {
    let editing = state.expression.is_some();
    let decorations = Decorations {
        marked: Some(&state.marked),
        current_row: (!editing).then_some(state.current_row),
        current_column: state.current_column.filter(|_| !editing),
        search: state.search.as_ref(),
    };
    let window = TwoDTableState::window_size(size.rows);
    let mut lines = plain_lines(render_table(&state.model, size, &decorations, state.first_row, window));
    lines.push(match (&state.search, editing) {
        (Some(search), _) => search_status_line(state.current_row, search),
        (None, true) => expression_status_line(),
        (None, false) => {
            use hints::*;
            let selected = selected_file_kind(ctx, state.selection_info());
            let mut hints = vec![
                EXIT,
                MARK,
                FOCUS,
                BACK,
                INSERT,
                INSERT_WHOLE,
                TREE,
                SEARCH,
                EXPRESSION,
                OPEN,
                COPY,
            ];
            if selected == Some(FileKind::Directory) {
                hints.push(DIR);
            }
            if selected == Some(FileKind::File) {
                hints.push(READ);
            }
            hints.extend([NEXT_PARENT, PREVIOUS_PARENT]);
            if state.current_column.is_some() {
                hints.extend([ROW, HIDE_COLUMN]);
            }
            counted_hints(
                render_count(state.current_row + 1, state.model.number_of_rows()),
                &hints,
            )
        }
    });
    lines
}

/// Whether the selection is a file or a directory: a path string, or a `PathSummary` (Scala
/// `ObjectBrowserActionHandler.getSelectedPath`)
pub(crate) fn selected_path(ctx: &Ctx, selection: Option<SelectionInfo>) -> Option<String> {
    let selection = selection?;
    let path = match &selection.value {
        Value::Str(s) => s.as_str().to_string(),
        Value::Object(o)
            if o.class()
                .is_some_and(|c| c.is_builtin(&mash_core::builtins::os::PATH_SUMMARY_CLASS)) =>
        {
            o.get_str("path")?.as_str()?.to_string()
        }
        _ => return None,
    };
    if path.starts_with('/') {
        Some(path)
    } else {
        let cwd = ctx.platform().current_dir()?;
        Some(format!("{}/{path}", cwd.trim_end_matches('/')))
    }
}

/// The kind of file the selection names, if it names one that exists
pub(crate) fn selected_file_kind(ctx: &Ctx, selection: Option<SelectionInfo>) -> Option<FileKind> {
    let path = selected_path(ctx, selection)?;
    ctx.platform().metadata(&path).ok().map(|m| m.kind)
}

fn single_object_lines(state: &SingleObjectState, size: Dimensions) -> Vec<Line> {
    let editing = state.expression.is_some();
    let decorations = Decorations {
        marked: Some(&state.marked),
        current_row: (!editing).then_some(state.current_row),
        current_column: None,
        search: state.search.as_ref(),
    };
    let window = state.window_size(size.rows);
    let mut lines = plain_lines(render_object_table(&state.model, &decorations, state.first_row, window));
    lines.push(match (&state.search, editing) {
        (Some(search), _) => search_status_line(state.current_row, search),
        (None, true) => expression_status_line(),
        (None, false) => {
            use hints::*;
            Line::new(render_key_hints(&[
                EXIT,
                MARK,
                FOCUS,
                BACK,
                INSERT,
                INSERT_WHOLE,
                TREE,
                SEARCH,
                EXPRESSION,
                OPEN,
                COPY,
                READ,
                NEXT_PARENT,
                PREVIOUS_PARENT,
            ]))
        }
    });
    lines
}

fn tree_lines(state: &TreeState, size: Dimensions) -> Vec<Line> {
    let window = TreeState::window_size(size.rows);
    let tree = render_tree(&state.model.root, Some(&state.selection), size);
    let mut lines: Vec<Line> = tree
        .into_iter()
        .skip(state.first_row)
        .take(window)
        .map(Line::new)
        .collect();
    lines.push(if state.expression.is_some() {
        expression_status_line()
    } else {
        use hints::*;
        Line::new(render_key_hints(&[
            EXIT,
            BACK,
            FOCUS,
            INSERT,
            INSERT_WHOLE,
            TABLE,
            OPEN,
            COPY,
            READ,
            NEXT_PARENT,
            PREVIOUS_PARENT,
        ]))
    });
    lines
}

fn text_lines(state: &TextLinesState, size: Dimensions) -> Vec<Line> {
    let window = TextLinesState::window_size(size.rows);
    let mut lines: Vec<Line> = state
        .model
        .lines
        .iter()
        .enumerate()
        .skip(state.first_row)
        .take(window)
        .map(|(index, text)| {
            let selected = index == state.selected_row && state.expression.is_none();
            let text = if selected && text.is_empty() { " " } else { text };
            let style = if selected {
                Style::DEFAULT.with_inverse()
            } else {
                Style::DEFAULT
            };
            Line::new(style.apply(text))
        })
        .collect();
    lines.push(if state.expression.is_some() {
        expression_status_line()
    } else {
        use hints::*;
        counted_hints(
            render_count(state.selected_row + 1, state.model.lines.len()),
            &[EXIT, BACK, INSERT_WHOLE, NEXT_PARENT, PREVIOUS_PARENT],
        )
    });
    lines
}

fn help_lines(state: &HelpState, size: Dimensions) -> Vec<Line> {
    let inner_width = size.columns.saturating_sub(2);
    let current_link = state.current_link();
    let with_border = |line: &StyledString| {
        let padded = line.pad_to(inner_width, StyledChar::new(' ', Style::DEFAULT));
        let inner = ellipsisise_styled(&padded, inner_width);
        Line::new(StyledString::plain("│") + &inner + &StyledString::plain("│"))
    };
    let horizontal = "─".repeat(size.columns.saturating_sub(2));
    let mut lines = vec![Line::new(StyledString::plain(&format!("┌{horizontal}┐")))];
    let window = HelpState::window_size(size.rows);
    for (index, line) in state.model.lines.iter().enumerate().skip(state.first_row).take(window) {
        let line = if index == state.current_row && state.expression.is_none() {
            match current_link {
                Some(link) => line.invert(link.start..link.start + link.len),
                None if line.is_empty() => Style::DEFAULT.with_inverse().apply(" "),
                None => line.invert(0..1),
            }
        } else {
            line.clone()
        };
        lines.push(with_border(&line));
    }
    lines.push(Line::new(StyledString::plain(&format!("└{horizontal}┘"))));
    lines.push(if state.expression.is_some() {
        expression_status_line()
    } else {
        use hints::*;
        let mut line = StyledString::plain("(");
        line.append(&render_key_hints(&[EXIT, BACK, INSERT_WHOLE]));
        line.push_str(")", Style::DEFAULT);
        Line::new(line)
    });
    lines
}

fn value_lines(state: &ValueState, size: Dimensions) -> Vec<Line> {
    let columns = size.columns.max(1);
    let mut lines = vec![];
    for text in state.model.rendered.split('\n') {
        let text = text.strip_suffix('\r').unwrap_or(text);
        // Each line in pieces as wide as the terminal (so, as in Scala Mash, an empty line shows nothing)
        let chars: Vec<char> = text.chars().collect();
        let pieces: Vec<&[char]> = chars.chunks(columns).collect();
        let count = pieces.len();
        for (i, piece) in pieces.into_iter().enumerate() {
            lines.push(Line {
                string: StyledString::plain(&piece.iter().collect::<String>()),
                ends_in_newline: i + 1 == count,
            });
        }
    }
    lines.truncate(size.rows.saturating_sub(2));
    lines.push(if state.expression.is_some() {
        expression_status_line()
    } else {
        use hints::*;
        Line::new(render_key_hints(&[
            EXIT,
            BACK,
            INSERT,
            READ,
            NEXT_PARENT,
            PREVIOUS_PARENT,
        ]))
    });
    lines
}
