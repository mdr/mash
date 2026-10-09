//! The incremental history search line, the current hit and key hints (Scala
//! `IncrementalHistorySearchRenderer` and `KeyHint`).

use mash_core::eval::Ctx;
use mash_core::value::{DateTime, Str, Value};

use super::{LinesAndCursor, retilde};
use crate::actions::describe_key;
use crate::input::{Key, KeyPress};
use crate::screen::{Line, Point, Style, StyledString, default_colours as colours};
use crate::state::{HistorySearchState, HitStatus};

/// A key and what it does, shown as a hint (`⌃g quit`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct KeyHint {
    pub key: KeyPress,
    pub description: &'static str,
}

impl KeyHint {
    pub const fn new(key: KeyPress, description: &'static str) -> KeyHint {
        KeyHint { key, description }
    }
}

const fn key(key: Key, ctrl: bool, alt: bool) -> KeyPress {
    KeyPress {
        key,
        ctrl,
        alt,
        shift: false,
    }
}

pub const NEXT_HISTORY_HIT: KeyHint = KeyHint::new(key(Key::Up, false, false), "next");
pub const DONE_SEARCH: KeyHint = KeyHint::new(key(Key::Enter, false, false), "done");
pub const QUIT: KeyHint = KeyHint::new(key(Key::Char('g'), true, false), "quit");
pub const CHANGE_DIRECTORY: KeyHint = KeyHint::new(key(Key::Char('d'), true, false), "cd");
pub const THIS_DIR_ONLY: KeyHint = KeyHint::new(key(Key::Char('d'), false, true), "this dir only");
pub const ALL_DIRS: KeyHint = KeyHint::new(key(Key::Char('d'), false, true), "all dirs");

/// Hints separated by commas, each key in inverse cyan
pub fn render_key_hints(hints: &[KeyHint]) -> StyledString {
    let mut s = StyledString::new();
    for (i, hint) in hints.iter().enumerate() {
        if i > 0 {
            s.push_str(", ", Style::DEFAULT);
        }
        s.push_str(&describe_key(hint.key), Style::fg(colours::CYAN).with_inverse());
        s.push_str(&format!(" {}", hint.description), Style::DEFAULT);
    }
    s
}

/// The search line (with the cursor at its end), the hit if any, and key hints. `ctx` supplies the clock and
/// home directory.
pub fn render_history_search(ctx: &mut Ctx, search: &HistorySearchState) -> LinesAndCursor {
    let mut search_line = StyledString::plain("Incremental history search: ");
    search_line.push_str(&search.search, Style::fg(colours::CYAN));
    let cursor = Point::new(0, search_line.width());
    let mut lines = vec![Line::new(search_line)];
    if let HitStatus::Hit(hit) = &search.hit {
        let time = fuzzy_time(ctx, hit.timestamp);
        let directory = match ctx.platform().home_dir() {
            Some(home) => retilde(&hit.working_directory, &home),
            None => hit.working_directory.clone(),
        };
        let text = format!("Hit {}: {time} in {directory}", hit.result_index + 1);
        lines.push(Line::new(Style::fg(colours::FOREGROUND).apply(&text)));
    }
    let dirs = if search.current_dir_only {
        ALL_DIRS
    } else {
        THIS_DIR_ONLY
    };
    let hints = [NEXT_HISTORY_HIT, DONE_SEARCH, QUIT, CHANGE_DIRECTORY, dirs];
    lines.push(Line::new(render_key_hints(&hints)));
    LinesAndCursor {
        lines,
        cursor: Some(cursor),
    }
}

/// "3 hours ago", as `DateTime.fuzzy` puts it
fn fuzzy_time(ctx: &mut Ctx, timestamp: jiff::Timestamp) -> String {
    let fuzzy = Value::Str(Str::new("fuzzy"));
    match ctx.call_values(&fuzzy, vec![Value::DateTime(DateTime(timestamp))]) {
        Ok(Value::Str(s)) => s.as_str().to_string(),
        _ => timestamp.to_string(),
    }
}
