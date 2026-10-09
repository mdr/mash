//! The REPL itself (Scala `Repl` and its action handlers), driven by events from the host.
//!
//! The host owns the input loop:
//!
//! ```text
//! let mut repl = Repl::new(ctx, storage, session_id);
//! repl.start(&mut host);                       // startup tip, first prompt
//! loop {
//!     let event = /* read a key press, paste or resize */;
//!     if repl.handle_event(event, &mut host) == ReplStatus::Exit {
//!         break;
//!     }
//! }
//! ```
//!
//! Each event is mapped to an [`Action`] by the key map of the current mode (normal editing, incremental
//! completion, browsing completions, history search) and handled; then the screen is redrawn. Accepting a
//! line runs it as a command between [`Host::begin_command`] and [`Host::end_command`]: its result (or
//! error) is written to the host, `it` and `r0`, `r1`, … are set, and the command is recorded in the history.
//! An interrupted command (the host sets [`Ctx::interrupt_flag`] on Ctrl-C) prints a message and returns to
//! the prompt.

use std::ops::Range;

use mash_core::builtins::meta::value_to_expression;
use mash_core::complete::{Completion, complete};
use mash_core::eval::Ctx;
use mash_core::value::{List, Str, Value, gc};
use mash_syntax::SyntaxKind;

use crate::actions::{Action, KeyMap};
use crate::commands::{CommandRunner, Outcome, mish_command};
use crate::config::ReplConfig;
use crate::draw::{ScreenDrawer, accept_screen, escapes};
use crate::editor;
use crate::handlers::{self, SearchOutcome};
use crate::history::{History, HistoryStorage};
use crate::host::Host;
use crate::input::InputEvent;
use crate::line_buffer::LineBuffer;
use crate::render::completions::number_of_columns;
use crate::render::{RenderOptions, render_repl};
use crate::screen::{Dimensions, Screen};
use crate::state::{
    AssistanceState, BrowseCompletionState, CompletionState, IncrementalCompletionState, ReplState, UndoRedoState,
};
use crate::view::ViewConfig;

/// The variable holding the last result
pub const IT: &str = "it";
/// The list of all results, and the prefix of the variables holding each (`r0`, `r1`, …)
pub const RESULTS: &str = "r";

/// Whether the REPL carries on after an event.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ReplStatus {
    Continue,
    /// The user asked to leave (Ctrl-D on an empty line)
    Exit,
}

/// An interactive Mash session: the interpreter, the editor's state and the history.
#[derive(Debug)]
pub struct Repl {
    ctx: Ctx,
    state: ReplState,
    history: History,
    /// What's on the terminal, for drawing only what changes
    previous_screen: Option<Screen>,
    swapped_out_screen: Option<Screen>,
}

impl Repl {
    /// A REPL running code with `ctx`, keeping history in `storage` under `session_id`
    pub fn new(ctx: Ctx, storage: Box<dyn HistoryStorage>, session_id: impl Into<String>) -> Repl {
        Repl {
            ctx,
            state: ReplState::default(),
            history: History::new(storage, session_id),
            previous_screen: None,
            swapped_out_screen: None,
        }
    }

    pub fn ctx(&mut self) -> &mut Ctx {
        &mut self.ctx
    }

    pub fn state(&self) -> &ReplState {
        &self.state
    }

    pub fn history(&self) -> &History {
        &self.history
    }

    /// The settings from the `config` global
    pub fn config(&self) -> ReplConfig {
        ReplConfig::from_globals(self.ctx.globals())
    }

    /// Show a startup tip (unless turned off) and the first prompt
    pub fn start(&mut self, host: &mut dyn Host) {
        if self.config().show_startup_tips {
            let index = self.ctx.platform().random_u64() as usize;
            let lines: Vec<_> = crate::tips::render_tip(index, host.size())
                .into_iter()
                .map(|l| l.string)
                .collect();
            let mut runner = self.runner(host, true);
            runner.write_lines(&lines);
        }
        self.draw(host);
    }

    /// Handle an input event and redraw. On [`ReplStatus::Exit`], the screen has been tidied up and the cursor
    /// left on a fresh line.
    pub fn handle_event(&mut self, event: InputEvent, host: &mut dyn Host) -> ReplStatus {
        let status = match event {
            InputEvent::Key(key) => {
                let action = self.key_map().action(key, self.is_line_empty());
                self.handle_action(action, host)
            }
            InputEvent::Paste(text) => {
                // Pasted text is inserted as is: it ends completion or search, and is never a key binding
                self.state.completion = None;
                self.state.history_search = None;
                self.text_change(|repl| repl.state.update_line_buffer(|b| b.insert_at_cursor(&text)));
                self.history.commit_to_entry();
                self.update_assistance();
                ReplStatus::Continue
            }
            InputEvent::Resize(_) => ReplStatus::Continue,
        };
        self.draw(host);
        if status == ReplStatus::Exit {
            host.write("\r\n");
            host.flush();
        }
        status
    }

    /// The key map for the current mode
    pub fn key_map(&self) -> KeyMap {
        match (&self.state.completion, &self.state.history_search) {
            (Some(CompletionState::Browse(_)), _) => KeyMap::BrowseCompletions,
            (Some(CompletionState::Incremental(_)), _) => KeyMap::Normal,
            (None, Some(_)) => KeyMap::IncrementalHistorySearch,
            (None, None) => KeyMap::Normal,
        }
    }

    fn is_line_empty(&self) -> bool {
        self.state.completion.is_none() && self.state.history_search.is_none() && self.state.line_buffer.is_empty()
    }

    /// Handle an action (without redrawing, unless the action itself draws, as accepting a line does)
    pub fn handle_action(&mut self, action: Action, host: &mut dyn Host) -> ReplStatus {
        let status = match self.state.completion.clone() {
            Some(CompletionState::Incremental(completion)) => {
                self.handle_incremental_completion(action, completion, host)
            }
            Some(CompletionState::Browse(completion)) => self.handle_browse_completion(action, completion, host),
            None if self.state.history_search.is_some() => self.handle_history_search(action, host),
            None => self.handle_normal(action, host),
        };
        self.update_assistance();
        status
    }

    /// Draw the REPL (the prompt, line and anything below it)
    pub fn draw(&mut self, host: &mut dyn Host) {
        self.draw_with(host, false);
    }

    fn draw_with(&mut self, host: &mut dyn Host, completed: bool) {
        let size = host.size();
        let screen = self.render_with(size, completed);
        let draw = ScreenDrawer::new(size.columns).draw(
            &screen,
            self.previous_screen.as_ref(),
            self.swapped_out_screen.as_ref(),
        );
        self.previous_screen = Some(screen);
        self.swapped_out_screen = draw.swapped_out;
        host.write(&draw.output);
        host.flush();
    }

    /// The screen for the current state
    pub fn render(&mut self, size: Dimensions) -> Screen {
        self.render_with(size, false)
    }

    fn render_with(&mut self, size: Dimensions, completed: bool) -> Screen {
        let options = RenderOptions {
            size,
            bare_words: self.config().bare_words,
            completed,
        };
        render_repl(&self.state, &mut self.ctx, options)
    }

    /// Run a script (such as an init file), printing any error; its value, unless it failed (Scala
    /// `ScriptExecutor`)
    pub fn run_script(&mut self, name: &str, text: &str, host: &mut dyn Host) -> Option<Value> {
        host.begin_command();
        let result = self.runner(host, true).run_unit(name, text, false);
        host.end_command();
        result
    }

    fn runner<'a>(&'a mut self, host: &'a mut dyn Host, print_errors: bool) -> CommandRunner<'a> {
        let config = self.config();
        CommandRunner {
            ctx: &mut self.ctx,
            host,
            bare_words: config.bare_words,
            view_config: ViewConfig {
                fuzzy_time: config.fuzzy_time,
            },
            print_errors,
        }
    }

    fn cwd(&self) -> Option<String> {
        self.ctx.platform().current_dir()
    }

    // ------------------------------------------------------------------------------------------- normal mode

    fn handle_normal(&mut self, action: Action, host: &mut dyn Host) -> ReplStatus {
        use Action::*;
        let mut status = ReplStatus::Continue;
        match &action {
            Enter => self.handle_enter(host),
            BackwardKillWord => self.text_change(|repl| {
                if let Some(text) = repl.state.line_buffer.selected_text() {
                    repl.state.copied = Some(text.to_string());
                }
                repl.state.update_line_buffer(LineBuffer::delete_backward_word);
            }),
            Complete => self.text_change(Repl::handle_complete),
            RedrawScreen => {
                host.write(escapes::MOVE_CURSOR_TO_TOP_LEFT);
                host.write(escapes::CLEAR_SCREEN);
                self.previous_screen = None;
            }
            EndOfFile => {
                self.state = self.state.reset();
                status = ReplStatus::Exit;
            }
            Up => self.handle_up(),
            Down => self.handle_down(),
            UpExtendingSelection => self.text_change(|repl| repl.state.update_line_buffer(|b| b.cursor_up(true))),
            DownExtendingSelection => self.text_change(|repl| repl.state.update_line_buffer(|b| b.cursor_down(true))),
            AssistInvocation => {
                if self.state.assistance.is_some() {
                    self.state.assistance = None;
                } else {
                    self.state.assistance = self.assistance();
                }
            }
            InsertLastArg => self.text_change(|repl| {
                let history = &repl.history;
                handlers::insert_last_arg(&mut repl.state, |i| history.historical_argument(i));
            }),
            ToggleQuote => self.text_change(|repl| {
                let mish = repl.state.mish;
                repl.state.update_line_buffer(|b| editor::toggle_quotes(b, mish));
            }),
            ToggleMish => self.text_change(|repl| {
                repl.state.update_line_buffer(|b| {
                    if b.text().starts_with('!') {
                        b.replace(0..1, "")
                    } else {
                        b.insert("!", 0)
                    }
                })
            }),
            IncrementalHistorySearch => {
                self.text_change(|repl| handlers::begin_search(&mut repl.state, &mut repl.history))
            }
            // The object browser arrives in a later milestone
            BrowseLastResult => {}
            Inline => self.text_change(|repl| repl.handle_inline(host)),
            ExpandSelection => {
                if let Some(range) = editor::expand_selection(&self.state.line_buffer, self.state.mish) {
                    self.state.push_selection(range);
                }
            }
            UnexpandSelection => self.state.pop_selection(),
            Copy => {
                if let Some(text) = self.state.line_buffer.selected_text() {
                    self.state.copied = Some(text.to_string());
                }
            }
            Paste => self.text_change(|repl| {
                if let Some(copied) = repl.state.copied.clone() {
                    repl.state.update_line_buffer(|b| b.insert_at_cursor(&copied));
                }
            }),
            Undo => {
                if let Some(previous) = self.state.undo_redo.undo(&self.state.line_buffer) {
                    self.state.line_buffer = previous;
                    self.state.old_selections.clear();
                    self.history.reset_position();
                }
            }
            Redo => {
                if let Some(next) = self.state.undo_redo.redo(&self.state.line_buffer) {
                    self.state.line_buffer = next;
                    self.state.old_selections.clear();
                    self.history.reset_position();
                }
            }
            Quit => self.handle_quit(host),
            action => {
                if let Some(edited) = line_buffer_edit(action, &self.state.line_buffer) {
                    self.text_change(|repl| repl.state.line_buffer = edited);
                }
            }
        }
        if !matches!(action, Down | Up | RedrawScreen) {
            self.history.commit_to_entry();
        }
        if !matches!(action, InsertLastArg | RedrawScreen) {
            self.state.insert_last_arg = None;
        }
        status
    }

    /// Run `f`, then if it changed the text, record the old buffer for undo and stop stepping through history
    fn text_change(&mut self, f: impl FnOnce(&mut Repl)) {
        let old = self.state.line_buffer.clone();
        f(self);
        if old.text() != self.state.line_buffer.text() {
            self.history.reset_position();
            self.state.undo_redo.push(old.without_selection());
        }
        if old != self.state.line_buffer {
            self.state.old_selections.clear();
        }
    }

    fn handle_up(&mut self) {
        let buffer = &self.state.line_buffer;
        let committed = self.history.is_committed_to_entry();
        if buffer.is_multiline() && committed {
            self.text_change(|repl| repl.state.update_line_buffer(|b| b.cursor_up(false)));
        } else if !buffer.is_multiline() && !buffer.text().trim().is_empty() && committed {
            let cwd = self.cwd();
            self.text_change(|repl| {
                handlers::begin_search_from_line(&mut repl.state, &mut repl.history, cwd.as_deref())
            });
        } else {
            let text = buffer.text().to_string();
            match self.history.go_backwards(&text) {
                Some(command) => self.state.line_buffer = LineBuffer::new(command),
                None => self.state.update_line_buffer(|b| b.cursor_up(false)),
            }
        }
    }

    fn handle_down(&mut self) {
        if self.state.line_buffer.is_multiline() && self.history.is_committed_to_entry() {
            self.text_change(|repl| repl.state.update_line_buffer(|b| b.cursor_down(false)));
        } else {
            match self.history.go_forwards() {
                Some(command) => self.state.line_buffer = LineBuffer::new(command),
                None => self.state.update_line_buffer(|b| b.cursor_down(false)),
            }
        }
    }

    fn handle_enter(&mut self, host: &mut dyn Host) {
        self.history.reset_position();
        if self.can_accept_line() {
            self.accept_line(host);
        } else {
            self.text_change(|repl| repl.state.update_line_buffer(|b| b.insert_at_cursor("\n")));
        }
    }

    /// Whether Enter runs the line: its braces balance, and the cursor is at the end (or it's one line)
    fn can_accept_line(&self) -> bool {
        use SyntaxKind::*;
        let buffer = &self.state.line_buffer;
        let tokens = mash_syntax::lexer::tokenise(buffer.text(), self.state.mish).tokens;
        let opening = tokens
            .iter()
            .filter(|t| {
                matches!(
                    t.kind,
                    LBRACE
                        | MISH_INTERPOLATION_START
                        | MISH_INTERPOLATION_START_NO_CAPTURE
                        | STRING_INTERPOLATION_START_COMPLEX
                )
            })
            .count();
        let closing = tokens.iter().filter(|t| t.kind == RBRACE).count();
        (buffer.cursor_at_end() || !buffer.is_multiline()) && opening == closing
    }

    fn accept_line(&mut self, host: &mut dyn Host) {
        self.finish_with_line(host);
        let command = self.state.line_buffer.text().to_string();
        self.clear_line();
        if !command.trim().is_empty() {
            self.run_command(&command, host);
        }
    }

    fn clear_line(&mut self) {
        self.state.line_buffer = LineBuffer::default();
        self.state.undo_redo = UndoRedoState::default();
        self.state.old_selections.clear();
    }

    /// Draw the line as finished (no completions, assistance or status indicator) and move past it
    fn finish_with_line(&mut self, host: &mut dyn Host) {
        self.state.completion = None;
        self.state.assistance = None;
        self.state.update_line_buffer(|b| b.move_to_end_of_buffer(false));
        self.draw_with(host, true);
        if let Some(previous) = self.previous_screen.take() {
            host.write(&accept_screen(&previous));
            host.flush();
        }
    }

    fn handle_quit(&mut self, host: &mut dyn Host) {
        if self.state.assistance.is_some() {
            self.state.assistance = None;
        } else {
            self.state.line_buffer = LineBuffer::default();
            self.history.reset_position();
            self.finish_with_line(host);
            self.clear_line();
        }
    }

    fn run_command(&mut self, command: &str, host: &mut dyn Host) {
        let number = self.state.command_number;
        let mish = self.state.mish;
        host.begin_command();
        let outcome = self.runner(host, true).run(command, &format!("command-{number}"), mish);
        host.end_command();
        match outcome {
            Outcome::ToggleMish => self.state.mish = !mish,
            Outcome::Ran(result) => {
                let cwd = self.cwd().unwrap_or_default();
                let now = self.ctx.platform().now();
                self.history.record(command, number as u64, &cwd, mish, now);
                self.state.command_number += 1;
                if let Some(result) = result {
                    self.save_result(number, result);
                }
            }
        }
        gc::collect();
    }

    /// Set `it`, `r<number>` and `r[number]`
    fn save_result(&mut self, number: usize, result: Value) {
        let globals = self.ctx.globals();
        globals.set_str(IT, result.clone());
        globals.set_str(&format!("{RESULTS}{number}"), result.clone());
        let mut results = match globals.get_str(RESULTS) {
            Some(Value::List(list)) => list.snapshot(),
            _ => vec![],
        };
        if results.len() <= number {
            results.resize(number + 1, Value::Null);
        }
        results[number] = result;
        globals.set_str(RESULTS, Value::List(List::new(results)));
    }

    /// Alt-E: evaluate the line (or selection) and put the result, as an expression, in its place
    fn handle_inline(&mut self, host: &mut dyn Host) {
        let buffer = self.state.line_buffer.clone();
        let code = buffer.selected_text().unwrap_or(buffer.text()).to_string();
        let mish = self.state.mish;
        host.begin_command();
        let result = self.runner(host, false).run_unit("command-inline", &code, mish);
        host.end_command();
        if let Some(expression) = result.as_ref().and_then(value_to_expression) {
            self.state.line_buffer = match buffer.selection() {
                Some(range) => buffer.replace(range, &expression),
                None => LineBuffer::new(expression),
            };
        }
    }

    // ------------------------------------------------------------------------------------------- assistance

    /// The function to show assistance for at the cursor
    fn assistance(&mut self) -> Option<AssistanceState> {
        let buffer = &self.state.line_buffer;
        let (text, pos, mish) = (buffer.text().to_string(), buffer.cursor(), self.state.mish);
        let assistable = match mish_command(&text) {
            Some((prefix, code)) => {
                let code = code.to_string();
                mash_core::complete::assistance(&mut self.ctx, &code, pos.checked_sub(prefix)?, true)
            }
            None => mash_core::complete::assistance(&mut self.ctx, &text, pos, mish),
        };
        assistable.map(AssistanceState)
    }

    /// Keep assistance up to date as the line changes (Scala `InvocationAssistanceUpdater`): show the function
    /// now at the cursor, or keep the last one unless the line has been cleared
    fn update_assistance(&mut self) {
        if self.state.assistance.is_some() {
            let blank = self.state.line_buffer.text().trim().is_empty();
            self.state.assistance = self
                .assistance()
                .or_else(|| if blank { None } else { self.state.assistance.take() });
        }
    }

    // ------------------------------------------------------------------------------------------- completion

    /// Completions at the cursor of `buffer`, and the byte range they replace
    fn complete(&mut self, buffer: &LineBuffer, mish: bool) -> Option<(Vec<Completion>, Range<usize>)> {
        let text = buffer.text();
        let pos = buffer.cursor();
        let (shift, result) = match mish_command(text) {
            Some((prefix, code)) => (prefix, complete(&mut self.ctx, code, pos.checked_sub(prefix)?, true)?),
            None => (0, complete(&mut self.ctx, text, pos, mish)?),
        };
        let range = usize::from(result.replace.start()) + shift..usize::from(result.replace.end()) + shift;
        Some((result.completions, range))
    }

    fn handle_complete(&mut self) {
        let buffer = self.state.line_buffer.clone();
        let Some((completions, range)) = self.complete(&buffer, self.state.mish) else {
            return;
        };
        self.history.reset_position();
        if let [completion] = completions.as_slice() {
            let replacement = completion.replacement();
            let cursor = range.start + replacement.len();
            let text = buffer.replace(range, &replacement).text().to_string();
            self.state.line_buffer = LineBuffer::with_cursor(text, cursor);
        } else {
            let result = mash_core::complete::CompletionResult {
                completions,
                replace: mash_syntax::TextRange::new((range.start as u32).into(), (range.end as u32).into()),
            };
            let common = result.common_insert_text();
            let all_quoted = result.completions.iter().all(|c| c.quoted);
            let new_range = range.start..range.start + common.len();
            let text = buffer.replace(range, &common).text().to_string();
            let cursor = new_range.end - usize::from(all_quoted);
            self.state.line_buffer = LineBuffer::with_cursor(text, cursor);
            self.state.assistance = None;
            self.state.completion = Some(CompletionState::Incremental(IncrementalCompletionState {
                completions: result.completions,
                replace: new_range,
                immediately_after_completion: true,
                memento: None,
            }));
        }
    }

    fn handle_incremental_completion(
        &mut self,
        action: Action,
        completion: IncrementalCompletionState,
        host: &mut dyn Host,
    ) -> ReplStatus {
        match action {
            Action::SelfInsert(s) => {
                let buffer = self.state.line_buffer.clone();
                let memento = (
                    buffer.clone(),
                    IncrementalCompletionState {
                        immediately_after_completion: false,
                        ..completion.clone()
                    },
                );
                let new_buffer = buffer.insert_at_cursor(&s);
                let mut new_state = None;
                if let Some((completions, range)) = self.complete(&new_buffer, self.state.mish)
                    && range.start == completion.replace.start
                {
                    let replaced = &new_buffer.text()[range.clone()];
                    let exact_match = matches!(completions.as_slice(), [c] if c.replacement() == replaced);
                    if !exact_match {
                        new_state = Some(CompletionState::Incremental(IncrementalCompletionState {
                            completions,
                            replace: range,
                            immediately_after_completion: false,
                            memento: Some(Box::new(memento)),
                        }));
                    }
                }
                self.state.line_buffer = new_buffer;
                self.state.completion = new_state;
                ReplStatus::Continue
            }
            Action::BackwardDeleteChar if completion.memento.is_some() => {
                if let Some(memento) = completion.memento {
                    let (buffer, previous) = *memento;
                    self.state.line_buffer = buffer;
                    self.state.completion = Some(CompletionState::Incremental(previous));
                }
                ReplStatus::Continue
            }
            Action::Complete if completion.immediately_after_completion => {
                self.browse_completions(completion.completions, completion.replace, 0);
                ReplStatus::Continue
            }
            Action::Quit => {
                self.state.completion = None;
                ReplStatus::Continue
            }
            action => {
                self.state.completion = None;
                self.handle_normal(action, host)
            }
        }
    }

    /// Browse completions, with option `active` in place of the region being completed
    fn browse_completions(&mut self, completions: Vec<Completion>, replace: Range<usize>, active: usize) {
        let replacement = completions[active].replacement();
        let buffer = self.state.line_buffer.replace(replace.clone(), &replacement);
        let new_range = replace.start..replace.start + replacement.len();
        self.state.line_buffer = LineBuffer::with_cursor(buffer.text(), new_range.end);
        self.state.completion = Some(CompletionState::Browse(BrowseCompletionState {
            completions,
            replace: new_range,
            active,
        }));
    }

    fn handle_browse_completion(
        &mut self,
        action: Action,
        completion: BrowseCompletionState,
        host: &mut dyn Host,
    ) -> ReplStatus {
        let total = completion.completions.len();
        let columns = number_of_columns(&completion.completions, host.size());
        let grid = Grid {
            total,
            columns,
            pos: completion.active,
        };
        let target = match action {
            Action::NextCompletion => grid.next(),
            Action::PreviousCompletion => grid.previous(),
            Action::NavigateRight => grid.right(),
            Action::NavigateLeft => grid.left(),
            Action::NavigateDown => grid.down(),
            Action::NavigateUp => grid.up(),
            Action::AcceptCompletion | Action::Quit => {
                self.state.completion = None;
                return ReplStatus::Continue;
            }
            action => {
                self.state.completion = None;
                return self.handle_normal(action, host);
            }
        };
        self.browse_completions(completion.completions, completion.replace, target);
        ReplStatus::Continue
    }

    // ------------------------------------------------------------------------------------------- history search

    fn handle_history_search(&mut self, action: Action, host: &mut dyn Host) -> ReplStatus {
        let cwd = self.cwd();
        match handlers::handle_search_action(&action, &mut self.state, &mut self.history, cwd.as_deref()) {
            SearchOutcome::Consumed => ReplStatus::Continue,
            SearchOutcome::NotConsumed => self.handle_normal(action, host),
            SearchOutcome::ChangeDirectory(directory) => {
                // Errors (e.g. the directory has gone) are ignored, as in Scala Mash
                if let Some(cd) = self.ctx.globals().get_str("cd") {
                    let _ = self.ctx.call_values(&cd, vec![Value::Str(Str::new(directory))]);
                }
                ReplStatus::Continue
            }
        }
    }
}

/// Moving around a grid of `total` items, `columns` wide, whose last row may be short, wrapping at the edges
/// (Scala `RaggedGridNavigator`)
#[derive(Clone, Copy, Debug)]
struct Grid {
    total: usize,
    columns: usize,
    pos: usize,
}

impl Grid {
    fn row(&self) -> usize {
        self.pos / self.columns
    }

    fn row_length(&self) -> usize {
        (self.total - self.row() * self.columns).min(self.columns)
    }

    fn next(&self) -> usize {
        (self.pos + 1) % self.total
    }

    fn previous(&self) -> usize {
        (self.pos + self.total - 1) % self.total
    }

    fn right(&self) -> usize {
        self.row() * self.columns + (self.pos % self.columns + 1) % self.row_length()
    }

    fn left(&self) -> usize {
        let length = self.row_length();
        self.row() * self.columns + (self.pos % self.columns + length - 1) % length
    }

    fn up(&self) -> usize {
        let (total, columns) = (self.total as isize, self.columns as isize);
        let mut next = self.pos as isize - columns;
        if next < 0 {
            next += total - total % columns + columns;
        }
        if next >= total {
            next -= columns;
        }
        next as usize
    }

    fn down(&self) -> usize {
        let next = self.pos + self.columns;
        if next < self.total {
            next
        } else {
            let full = self.total - self.total % self.columns;
            if full == 0 { 0 } else { next % full % self.columns }
        }
    }
}

/// The line buffer after a simple editing action (Scala `LineBufferActionHandler`)
fn line_buffer_edit(action: &Action, b: &LineBuffer) -> Option<LineBuffer> {
    use Action::*;
    Some(match action {
        SelfInsert(s) => b.insert_at_cursor(s),
        BeginningOfLine => b.move_to_start(false),
        EndOfLine => b.move_to_end(false),
        BeginningOfLineExtendingSelection => b.move_to_start(true),
        EndOfLineExtendingSelection => b.move_to_end(true),
        ForwardChar => b.cursor_right(false),
        BackwardChar => b.cursor_left(false),
        ForwardCharExtendingSelection => b.cursor_right(true),
        BackwardCharExtendingSelection => b.cursor_left(true),
        ForwardWord => b.forward_word(false),
        BackwardWord => b.backward_word(false),
        ForwardWordExtendingSelection => b.forward_word(true),
        BackwardWordExtendingSelection => b.backward_word(true),
        DeleteChar => b.delete(),
        BackwardDeleteChar => b.backspace(),
        KillLine => b.delete_to_end_of_line(),
        BackwardKillLine => b.delete_to_start_of_line(),
        KillWord => b.delete_forward_word(),
        BeginningOfBuffer => b.move_to_start_of_buffer(false),
        EndOfBuffer => b.move_to_end_of_buffer(false),
        Newline => b.insert_at_cursor("\n"),
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::Grid;

    #[test]
    fn ragged_grid_navigation() {
        // 0 1 2 3
        // 4 5 6 7
        // 8 9
        let grid = |pos| Grid {
            total: 10,
            columns: 4,
            pos,
        };
        assert_eq!(grid(6).down(), 2);
        assert_eq!(grid(5).down(), 9);
        assert_eq!(grid(9).down(), 1);
        assert_eq!(grid(2).up(), 6);
        assert_eq!(grid(1).up(), 9);
        assert_eq!(grid(9).right(), 8);
        assert_eq!(grid(8).left(), 9);
        assert_eq!(grid(9).next(), 0);
        assert_eq!(grid(0).previous(), 9);
    }
}
