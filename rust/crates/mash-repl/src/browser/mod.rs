//! The object browser (Scala `repl.browser`): a full-screen, interactive view of a command's result, drawn
//! on the terminal's alternate screen.
//!
//! It opens when a result is too large for the screen (if `config.view.browseLargeOutput` is set), for
//! `view.browse x`, and with Ctrl-V on the last result. Each value is shown according to its
//! [`model::DisplayModel`]: a 2D table, a table of an object's fields, a tree, text lines, help, or a plain
//! value. The user can move around it, focus on (dive into) the selected part, go back, mark rows, search
//! (`/`), type an expression to evaluate against the value (`e`), and insert the expression for the
//! selection into the command line (`i`).
//!
//! - [`BrowserStack`]: the browser's states, one per value visited; [`BrowserState`] and its variants: how
//!   each kind of view is being looked at
//! - [`actions`]: the browser's actions and key maps; `handler`: how the REPL handles them; `expression`:
//!   the expression line; [`search`]: incremental search; [`model`]: what each value is shown as
//! - [`render`]: drawing the browser as a [`crate::screen::Screen`]

pub mod actions;
mod expression;
mod handler;
pub mod model;
pub mod render;
pub mod search;
mod states;

use std::ops::Range;

use mash_core::builtins::meta::value_to_expression;
use mash_core::eval::Ctx;
use mash_core::value::Value;
use mash_syntax::expression_combiner::combine_safely;
use mash_syntax::lexer::is_legal_identifier;

pub use states::{HelpState, SingleObjectState, TextLinesState, TreeState, TwoDTableState, ValueState};

use crate::line_buffer::LineBuffer;
use crate::state::{AssistanceState, CompletionState};
use model::DisplayModel;

/// The browser's states, one for each value visited; the current one is the last (Scala
/// `ObjectBrowserStateStack`).
#[derive(Clone, PartialEq, Debug)]
pub struct BrowserStack {
    states: Vec<BrowserState>,
}

impl BrowserStack {
    pub fn new(state: BrowserState) -> BrowserStack {
        BrowserStack { states: vec![state] }
    }

    pub fn current(&self) -> &BrowserState {
        self.states.last().expect("a browser stack is never empty")
    }

    pub fn current_mut(&mut self) -> &mut BrowserState {
        self.states.last_mut().expect("a browser stack is never empty")
    }

    /// The state before the current one
    pub fn parent(&self) -> Option<&BrowserState> {
        self.states.iter().rev().nth(1)
    }

    pub fn replace(&mut self, state: BrowserState) {
        *self.current_mut() = state;
    }

    pub fn push(&mut self, state: BrowserState) {
        self.states.push(state);
    }

    /// Go back to the previous state; false if this is the first
    pub fn pop(&mut self) -> bool {
        if self.states.len() > 1 {
            self.states.pop();
            true
        } else {
            false
        }
    }

    pub fn depth(&self) -> usize {
        self.states.len()
    }
}

/// How the user is looking at a value in the browser (Scala `BrowserState`). Each variant holds the value's
/// display model, an expression for the value (its path, e.g. `r0[3].name`), where the cursor is, and the
/// expression being typed, if any.
#[derive(Clone, PartialEq, Debug)]
pub enum BrowserState {
    TwoDTable(TwoDTableState),
    SingleObject(SingleObjectState),
    Tree(TreeState),
    TextLines(TextLinesState),
    Help(HelpState),
    Value(ValueState),
}

/// The part of the value that's selected, and an expression for it.
#[derive(Clone, PartialEq, Debug)]
pub struct SelectionInfo {
    pub path: String,
    pub value: Value,
}

impl BrowserState {
    /// A state looking at a display model of the value at `path`
    pub fn from_model(model: DisplayModel, path: impl Into<String>) -> BrowserState {
        let path = path.into();
        match model {
            DisplayModel::TwoDTable(model) => BrowserState::TwoDTable(TwoDTableState::new(model, path)),
            DisplayModel::SingleObject(model) => BrowserState::SingleObject(SingleObjectState::new(model, path)),
            DisplayModel::Tree(model) => BrowserState::Tree(TreeState::new(model, path)),
            DisplayModel::TextLines(model) => BrowserState::TextLines(TextLinesState::new(model, path)),
            DisplayModel::Help(model) => BrowserState::Help(HelpState::new(model, path)),
            DisplayModel::Value(model) => BrowserState::Value(ValueState::new(model, path)),
        }
    }

    /// An expression for the value
    pub fn path(&self) -> &str {
        match self {
            BrowserState::TwoDTable(s) => &s.path,
            BrowserState::SingleObject(s) => &s.path,
            BrowserState::Tree(s) => &s.path,
            BrowserState::TextLines(s) => &s.path,
            BrowserState::Help(s) => &s.path,
            BrowserState::Value(s) => &s.path,
        }
    }

    pub fn set_path(&mut self, path: String) {
        match self {
            BrowserState::TwoDTable(s) => s.path = path,
            BrowserState::SingleObject(s) => s.path = path,
            BrowserState::Tree(s) => s.path = path,
            BrowserState::TextLines(s) => s.path = path,
            BrowserState::Help(s) => s.path = path,
            BrowserState::Value(s) => s.path = path,
        }
    }

    /// The value being browsed
    pub fn raw_value(&self) -> Value {
        match self {
            BrowserState::TwoDTable(s) => s.model.raw_value.clone(),
            BrowserState::SingleObject(s) => s.model.raw_value.clone(),
            BrowserState::Tree(s) => s.model.raw_value.clone(),
            BrowserState::TextLines(s) => s.model.raw_value.clone(),
            BrowserState::Help(s) => s.model.raw_value.clone(),
            BrowserState::Value(s) => s.model.raw_value.clone(),
        }
    }

    pub fn expression(&self) -> Option<&ExpressionState> {
        match self {
            BrowserState::TwoDTable(s) => s.expression.as_ref(),
            BrowserState::SingleObject(s) => s.expression.as_ref(),
            BrowserState::Tree(s) => s.expression.as_ref(),
            BrowserState::TextLines(s) => s.expression.as_ref(),
            BrowserState::Help(s) => s.expression.as_ref(),
            BrowserState::Value(s) => s.expression.as_ref(),
        }
    }

    pub fn expression_mut(&mut self) -> &mut Option<ExpressionState> {
        match self {
            BrowserState::TwoDTable(s) => &mut s.expression,
            BrowserState::SingleObject(s) => &mut s.expression,
            BrowserState::Tree(s) => &mut s.expression,
            BrowserState::TextLines(s) => &mut s.expression,
            BrowserState::Help(s) => &mut s.expression,
            BrowserState::Value(s) => &mut s.expression,
        }
    }

    /// Start typing an expression, beginning with the path
    pub fn begin_expression(&mut self) {
        let line_buffer = LineBuffer::new(self.path().to_string());
        *self.expression_mut() = Some(ExpressionState::new(line_buffer));
    }

    /// The incremental search in progress, if any (only in tables)
    pub fn search(&self) -> Option<&search::SearchState> {
        match self {
            BrowserState::TwoDTable(s) => s.search.as_ref(),
            BrowserState::SingleObject(s) => s.search.as_ref(),
            _ => None,
        }
    }

    /// The selected part of the value and an expression for it (None if nothing can be selected)
    pub fn selection_info(&self, ctx: &Ctx) -> Option<SelectionInfo> {
        match self {
            BrowserState::TwoDTable(s) => s.selection_info(),
            BrowserState::SingleObject(s) => s.selection_info(ctx),
            BrowserState::Tree(s) => s.selection_info(),
            BrowserState::TextLines(s) => s.selection_info(),
            BrowserState::Help(s) => s.selection_info(),
            BrowserState::Value(_) => None,
        }
    }
}

/// The expression line: an expression being typed, to evaluate in place of the value being browsed (Scala
/// `ExpressionState`). If it starts with the value's path, the rest is evaluated against the value itself.
#[derive(Clone, PartialEq, Debug)]
pub struct ExpressionState {
    pub line_buffer: LineBuffer,
    /// Text copied, for pasting
    pub copied: Option<String>,
    /// Selections before each expansion, most recent first
    pub old_selections: Vec<Range<usize>>,
    pub completion: Option<CompletionState>,
    pub assistance: Option<AssistanceState>,
}

impl ExpressionState {
    pub fn new(line_buffer: LineBuffer) -> ExpressionState {
        ExpressionState {
            line_buffer,
            copied: None,
            old_selections: vec![],
            completion: None,
            assistance: None,
        }
    }
}

/// `path` followed by a lookup of `property` (Scala `BrowserState.safeProperty`): `.name` for an identifier
/// (unless the member is a method that can be called with no arguments, as `.name` would call it),
/// `['name']` for other strings, and `[expression]` otherwise
pub(crate) fn safe_property(path: &str, property: &Value, allows_nullary: bool) -> String {
    match property {
        Value::Str(s) if is_legal_identifier(s.as_str()) && !allows_nullary => {
            combine_safely(path, &format!(".{}", s.as_str()))
        }
        Value::Str(s) => combine_safely(path, &format!("['{}']", s.as_str())),
        _ => match value_to_expression(property) {
            Some(expression) => combine_safely(path, &format!("[{expression}]")),
            None => path.to_string(),
        },
    }
}

/// Whether a value is a function (or method) that can be called with no arguments
pub(crate) fn allows_nullary(ctx: &Ctx, value: &Value) -> bool {
    matches!(value, Value::Function(_) | Value::Builtin(_) | Value::BoundMethod(_))
        && ctx.params_of(value).is_some_and(|p| p.allows_nullary())
}

/// The first row to show so that `current` is visible in a window of `window` rows that started at `first`
/// (Scala `adjustWindowToFit`)
pub(crate) fn fit_window(current: usize, first: usize, window: usize) -> usize {
    let (current, mut first, window) = (current as isize, first as isize, window as isize);
    let below = current - (first + window - 1);
    if below >= 0 {
        first += below;
    }
    let above = first - current;
    if above >= 0 {
        first -= above;
    }
    first.max(0) as usize
}
