//! Action handlers that work on the state alone, without the interpreter (Scala
//! `IncrementalHistorySearchActionHandler` and `InsertLastArgHandler`).

use crate::actions::Action;
use crate::history::{History, HistoryMatch};
use crate::line_buffer::LineBuffer;
use crate::state::{HistorySearchState, Hit, HitStatus, InsertLastArgState, ReplState};

/// What became of an action during a history search.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SearchOutcome {
    Consumed,
    /// The search has ended; the action should be handled normally
    NotConsumed,
    /// Change to this directory (the one the hit was run in)
    ChangeDirectory(String),
}

/// Start an incremental history search (Ctrl-R), keeping the current line to restore if it's abandoned
pub fn begin_search(state: &mut ReplState, history: &mut History) {
    history.reset_position();
    state.assistance = None;
    state.history_search = Some(HistorySearchState::new(state.line_buffer.text()));
}

/// Start a search for the text of the current line (Up on a non-empty line)
pub fn begin_search_from_line(state: &mut ReplState, history: &mut History, cwd: Option<&str>) {
    begin_search(state, history);
    let text = state.line_buffer.text().to_string();
    fresh_search(state, history, cwd, text);
}

/// Handle an action while searching. `cwd` is the current directory (for restricting the search to it).
pub fn handle_search_action(
    action: &Action,
    state: &mut ReplState,
    history: &mut History,
    cwd: Option<&str>,
) -> SearchOutcome {
    let Some(search) = state.history_search.clone() else {
        return SearchOutcome::NotConsumed;
    };
    match action {
        Action::SelfInsert(s) => fresh_search(state, history, cwd, format!("{}{s}", search.search)),
        Action::BackwardDeleteChar => {
            let mut chars = search.search.chars();
            match chars.next_back() {
                None => state.history_search = None,
                Some(_) if chars.as_str().is_empty() => {
                    state.line_buffer = LineBuffer::default();
                    state.history_search = Some(HistorySearchState {
                        search: String::new(),
                        hit: HitStatus::BeforeFirstHit,
                        ..search
                    });
                }
                Some(_) => fresh_search(state, history, cwd, chars.as_str().to_string()),
            }
        }
        Action::IncrementalHistorySearch | Action::Up => {
            let index = match &search.hit {
                HitStatus::Hit(hit) => hit.result_index + 1,
                _ => 0,
            };
            hit_at(state, history, cwd, index);
        }
        Action::Down => {
            let index = match &search.hit {
                HitStatus::Hit(hit) => hit.result_index.saturating_sub(1),
                _ => 0,
            };
            hit_at(state, history, cwd, index);
        }
        Action::Enter => state.history_search = None,
        Action::Quit => {
            state.line_buffer = LineBuffer::new(search.original_line);
            state.history_search = None;
        }
        Action::ChangeDirectory => {
            if let HitStatus::Hit(hit) = search.hit {
                return SearchOutcome::ChangeDirectory(hit.working_directory);
            }
        }
        Action::ToggleCurrentDirOnly => {
            state.history_search = Some(HistorySearchState {
                current_dir_only: !search.current_dir_only,
                ..search.clone()
            });
            fresh_search(state, history, cwd, search.search);
        }
        Action::FirstHit => hit_at(state, history, cwd, 0),
        Action::LastHit => {
            if search.hit != HitStatus::NoHits
                && let Some((found, index)) = history.find_last_match(&search.search, directory(&search, cwd))
            {
                match_found(state, search.search.clone(), index, found);
            }
        }
        _ => {
            state.history_search = None;
            return SearchOutcome::NotConsumed;
        }
    }
    SearchOutcome::Consumed
}

fn directory<'a>(search: &HistorySearchState, cwd: Option<&'a str>) -> Option<&'a str> {
    if search.current_dir_only { cwd } else { None }
}

fn fresh_search(state: &mut ReplState, history: &History, cwd: Option<&str>, search_string: String) {
    let Some(search) = state.history_search.clone() else {
        return;
    };
    match history.find_match(&search_string, 0, directory(&search, cwd)) {
        Some(found) => match_found(state, search_string, 0, found),
        None => {
            state.line_buffer = LineBuffer::default();
            state.history_search = Some(HistorySearchState {
                search: search_string,
                hit: HitStatus::NoHits,
                ..search
            });
        }
    }
}

fn hit_at(state: &mut ReplState, history: &History, cwd: Option<&str>, index: usize) {
    let Some(search) = state.history_search.clone() else {
        return;
    };
    if search.hit != HitStatus::NoHits
        && let Some(found) = history.find_match(&search.search, index, directory(&search, cwd))
    {
        match_found(state, search.search, index, found);
    }
}

fn match_found(state: &mut ReplState, search_string: String, index: usize, found: HistoryMatch) {
    let Some(search) = state.history_search.clone() else {
        return;
    };
    state.line_buffer = LineBuffer::new(found.command);
    state.history_search = Some(HistorySearchState {
        search: search_string,
        hit: HitStatus::Hit(Hit {
            result_index: index,
            match_range: found.range,
            timestamp: found.timestamp,
            working_directory: found.working_directory,
        }),
        ..search
    });
}

/// Insert the last argument of a previous command at the cursor (Alt-.); repeated, replace it with the one
/// from the command before. `argument(n)` is the `n`th most recent argument.
pub fn insert_last_arg(state: &mut ReplState, argument: impl Fn(usize) -> Option<String>) {
    let (index, old_range) = match &state.insert_last_arg {
        Some(previous) => (previous.arg_index + 1, previous.range.clone()),
        None => {
            let cursor = state.line_buffer.cursor();
            (0, cursor..cursor)
        }
    };
    if let Some(arg) = argument(index) {
        let text = state.line_buffer.text();
        let new_text = format!("{}{arg}{}", &text[..old_range.start], &text[old_range.end..]);
        let range = old_range.start..old_range.start + arg.len();
        state.line_buffer = LineBuffer::with_cursor(new_text, range.end);
        state.insert_last_arg = Some(InsertLastArgState {
            arg_index: index,
            range,
        });
    }
}
