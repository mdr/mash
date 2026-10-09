//! The editor's state (Scala `ReplState` and the states of its modes): plain data, updated by the action
//! handlers in [`crate::repl`] and drawn by [`crate::render`].

use std::ops::Range;
use std::rc::Rc;

use jiff::Timestamp;
use mash_core::complete::{Assistable, Completion};

use crate::line_buffer::LineBuffer;

/// Everything about the REPL that changes as the user types.
#[derive(Clone, PartialEq, Debug, Default)]
pub struct ReplState {
    pub line_buffer: LineBuffer,
    /// Text cut or copied, for pasting (Ctrl-Y)
    pub copied: Option<String>,
    pub undo_redo: UndoRedoState,
    /// Selections before each expansion (Alt-Up), most recent first, for unexpanding (Alt-Down)
    pub old_selections: Vec<Range<usize>>,
    /// The number of the next command (shown in the prompt; its result is saved as `r<n>`)
    pub command_number: usize,
    /// Mish (shell syntax) by default
    pub mish: bool,
    pub history_search: Option<HistorySearchState>,
    pub completion: Option<CompletionState>,
    /// The function whose calling syntax is shown, if invocation assistance is on
    pub assistance: Option<AssistanceState>,
    pub insert_last_arg: Option<InsertLastArgState>,
}

impl ReplState {
    /// A fresh state, keeping the command number and mish mode
    pub fn reset(&self) -> ReplState {
        ReplState {
            command_number: self.command_number,
            mish: self.mish,
            ..ReplState::default()
        }
    }

    pub fn update_line_buffer(&mut self, f: impl FnOnce(&LineBuffer) -> LineBuffer) {
        self.line_buffer = f(&self.line_buffer);
    }

    /// Select `range`, remembering the current selection for unexpanding
    pub fn push_selection(&mut self, range: Range<usize>) {
        self.old_selections.insert(0, self.line_buffer.selection_or_cursor());
        self.line_buffer = self.line_buffer.select(range);
    }

    /// Go back to the selection before the last expansion, if any
    pub fn pop_selection(&mut self) {
        if !self.old_selections.is_empty() {
            let range = self.old_selections.remove(0);
            self.line_buffer = self.line_buffer.select(range);
        }
    }
}

/// Undo and redo stacks of line buffers (Scala `UndoRedoState`).
#[derive(Clone, PartialEq, Debug, Default)]
pub struct UndoRedoState {
    /// Most recent last
    previous: Vec<LineBuffer>,
    undone: Vec<LineBuffer>,
}

impl UndoRedoState {
    /// Record the buffer as it was before an edit (clearing the redo stack)
    pub fn push(&mut self, line_buffer: LineBuffer) {
        self.previous.push(line_buffer);
        self.undone.clear();
    }

    /// The buffer to go back to, given the current one
    pub fn undo(&mut self, current: &LineBuffer) -> Option<LineBuffer> {
        let previous = self.previous.pop()?;
        self.undone.push(current.clone());
        Some(previous)
    }

    pub fn redo(&mut self, current: &LineBuffer) -> Option<LineBuffer> {
        let next = self.undone.pop()?;
        self.previous.push(current.clone());
        Some(next)
    }
}

/// Incremental history search (Ctrl-R) in progress (Scala `IncrementalHistorySearchState`).
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct HistorySearchState {
    pub search: String,
    /// The text before the search began, restored if it's abandoned
    pub original_line: String,
    pub hit: HitStatus,
    /// Only search commands run in the current directory
    pub current_dir_only: bool,
}

impl HistorySearchState {
    pub fn new(original_line: impl Into<String>) -> HistorySearchState {
        HistorySearchState {
            search: String::new(),
            original_line: original_line.into(),
            hit: HitStatus::BeforeFirstHit,
            current_dir_only: false,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum HitStatus {
    BeforeFirstHit,
    Hit(Hit),
    NoHits,
}

impl HitStatus {
    /// The part of the line buffer that matches the search
    pub fn match_range(&self) -> Option<Range<usize>> {
        match self {
            HitStatus::Hit(hit) => Some(hit.match_range.clone()),
            _ => None,
        }
    }
}

/// A history entry matching the search.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Hit {
    /// Index into the matches, most recent first
    pub result_index: usize,
    /// The part of the command that matches
    pub match_range: Range<usize>,
    pub timestamp: Timestamp,
    pub working_directory: String,
}

/// Completion in progress (Scala `CompletionState`).
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CompletionState {
    /// The user has asked for completions once and is still typing: the options are filtered as they type
    Incremental(IncrementalCompletionState),
    /// The user is choosing among the options with Tab and the arrow keys
    Browse(BrowseCompletionState),
}

impl CompletionState {
    pub fn completions(&self) -> &[Completion] {
        match self {
            CompletionState::Incremental(s) => &s.completions,
            CompletionState::Browse(s) => &s.completions,
        }
    }

    /// The part of the line the completions replace
    pub fn replace(&self) -> Range<usize> {
        match self {
            CompletionState::Incremental(s) => s.replace.clone(),
            CompletionState::Browse(s) => s.replace.clone(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IncrementalCompletionState {
    pub completions: Vec<Completion>,
    pub replace: Range<usize>,
    /// Nothing typed since completing (so another Tab starts browsing)
    pub immediately_after_completion: bool,
    /// The state before the last character was typed, restored by Backspace
    pub memento: Option<Box<(LineBuffer, IncrementalCompletionState)>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BrowseCompletionState {
    pub completions: Vec<Completion>,
    pub replace: Range<usize>,
    pub active: usize,
}

/// The function whose calling syntax is being shown.
#[derive(Clone, Debug)]
pub struct AssistanceState(pub Assistable);

impl PartialEq for AssistanceState {
    fn eq(&self, other: &Self) -> bool {
        use Assistable::*;
        match (&self.0, &other.0) {
            (Function(a), Function(b)) => a.full_name() == b.full_name(),
            (Method(a), Method(b)) => std::ptr::addr_eq(*a, *b),
            (UserFunction(a), UserFunction(b)) | (UserMethod(a), UserMethod(b)) => Rc::ptr_eq(a, b),
            (Constructor(a), Constructor(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

/// The argument most recently inserted by Alt-. (Scala `InsertLastArgState`).
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct InsertLastArgState {
    /// Which argument from history (0 is the most recent)
    pub arg_index: usize,
    /// Where it was inserted
    pub range: Range<usize>,
}
