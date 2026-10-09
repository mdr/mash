//! How the REPL handles the object browser's actions (Scala `ObjectBrowserActionHandler` and the handlers for
//! each kind of view).

use mash_core::platform::FileKind;
use mash_core::value::Value;
use mash_syntax::escapes::escape_chars;
use mash_syntax::expression_combiner::combine_safely;
use mash_syntax::lexer::is_legal_identifier;
use mash_syntax::lookup_decomposer::{
    decompose_lookup_with_integer_index, decompose_lookup_with_string_index, decompose_member,
};

use super::actions::BrowserAction;
use super::model::{self, DisplayModel};
use super::render::{selected_file_kind, selected_path};
use super::{BrowserStack, BrowserState, SingleObjectState, TextLinesState, TreeState, TwoDTableState};
use crate::actions::Action;
use crate::draw::escapes;
use crate::host::Host;
use crate::repl::{RESULTS, Repl};
use crate::screen::Screen;
use crate::view::{ViewConfig, object, table};

impl Repl {
    /// Open the browser on `value`, whose expression is `path`
    pub(crate) fn browse(&mut self, value: &Value, path: String, host: &dyn Host) {
        if let Some(state) = self.new_browser_state(value, path, false, host) {
            self.state.object_browser = Some(BrowserStack::new(state));
        }
    }

    /// Open the browser on a display model of the value at `path`
    pub(crate) fn browse_model(&mut self, model: DisplayModel, path: String) {
        self.state.object_browser = Some(BrowserStack::new(BrowserState::from_model(model, path)));
    }

    /// The selected part of the value being browsed, and an expression for it
    pub fn browser_selection(&self) -> Option<super::SelectionInfo> {
        self.state.object_browser.as_ref()?.current().selection_info(&self.ctx)
    }

    /// Ctrl-V: browse the last result (Scala `handleBrowseLastResult`)
    pub(crate) fn browse_last_result(&mut self, host: &dyn Host) {
        if let Some(number) = self.state.command_number.checked_sub(1) {
            let path = format!("{RESULTS}{number}");
            if let Some(value) = self.ctx.globals().get_str(&path) {
                self.browse(&value, path, host);
            }
        }
    }

    fn view_config(&self) -> ViewConfig {
        let config = self.config();
        ViewConfig {
            fuzzy_time: config.fuzzy_time,
            browse_large_output: config.browse_large_output,
        }
    }

    /// A state showing `value` as usual, or as a tree if `tree` is set (and it's a list or object). None if it
    /// can't be shown (its display failed).
    fn new_browser_state(&mut self, value: &Value, path: String, tree: bool, host: &dyn Host) -> Option<BrowserState> {
        let config = self.view_config();
        if tree && let Ok(Some(model)) = model::tree_model(&mut self.ctx, value, &config) {
            return Some(BrowserState::Tree(TreeState::new(model, path)));
        }
        let model = model::display_model(&mut self.ctx, value, host.size(), &config).ok()?;
        Some(BrowserState::from_model(model, path))
    }

    fn stack(&mut self) -> &mut BrowserStack {
        self.state.object_browser.as_mut().expect("the browser is open")
    }

    fn current(&self) -> &BrowserState {
        self.state
            .object_browser
            .as_ref()
            .expect("the browser is open")
            .current()
    }

    /// Show `value` (at `path`) after the current state, to come back to with Back
    pub(crate) fn focus(&mut self, value: &Value, path: String, host: &dyn Host) {
        if let Some(state) = self.new_browser_state(value, path, false, host) {
            self.stack().push(state);
        }
    }

    /// Handle an action while the browser is open
    pub(crate) fn handle_browser_action(&mut self, action: Action, host: &mut dyn Host) {
        match action {
            Action::RedrawScreen => self.redraw_browser(host),
            Action::Browser(BrowserAction::Rerender) => self.rerender_browser(host),
            action if self.current().expression().is_some() => self.handle_expression_action(action, host),
            action if self.current().search().is_some() => self.handle_search_action(action, host),
            Action::Browser(action) if !self.handle_common_action(action, host) => {
                self.handle_view_action(action, host);
            }
            _ => {}
        }
    }

    /// Clear the terminal and draw the browser afresh
    fn redraw_browser(&mut self, host: &mut dyn Host) {
        host.write(escapes::MOVE_CURSOR_TO_TOP_LEFT);
        host.write(escapes::CLEAR_SCREEN);
        // Still on the alternate screen, but with nothing drawn on it
        self.previous_screen = Some(Screen {
            alternate: true,
            ..Screen::default()
        });
    }

    /// Rebuild tables for the terminal's size, and redraw
    fn rerender_browser(&mut self, host: &mut dyn Host) {
        let size = host.size();
        let config = self.view_config();
        match self.stack().current().clone() {
            BrowserState::TwoDTable(mut state) => {
                if let Ok(model) = table::create_model(
                    &mut self.ctx,
                    &state.model.raw_value,
                    size,
                    &config,
                    true,
                    &state.hidden_columns,
                ) {
                    state.model = model;
                    self.stack().replace(BrowserState::TwoDTable(state));
                }
            }
            BrowserState::SingleObject(mut state) => {
                let Value::Object(o) = &state.model.raw_value else {
                    return;
                };
                if let Ok(model) = object::create_model(&mut self.ctx, o, size, &config, true) {
                    state.model = model;
                    self.stack().replace(BrowserState::SingleObject(state));
                }
            }
            _ => {}
        }
        self.redraw_browser(host);
    }

    /// Actions that work the same in every view; false if `action` isn't one of them
    fn handle_common_action(&mut self, action: BrowserAction, host: &mut dyn Host) -> bool {
        use BrowserAction::*;
        match action {
            Exit => self.state.object_browser = None,
            NextParentItem => self.select_parent_item(1, host),
            PreviousParentItem => self.select_parent_item(-1, host),
            Open => self.run_on_selection(" | open", host),
            Copy => self.run_on_selection(" | clipboard", host),
            Back => {
                if !self.stack().pop() {
                    self.state.object_browser = None;
                }
            }
            Focus => {
                if let Some(selection) = self.current().selection_info(&self.ctx) {
                    self.focus(&selection.value, selection.path, host);
                }
            }
            FocusDirectory => self.browse_file(FileKind::Directory, "children", host),
            ReadFile => self.browse_file(FileKind::File, "readLines", host),
            InsertItem => {
                if let Some(selection) = self.current().selection_info(&self.ctx) {
                    self.insert(&selection.path);
                }
            }
            InsertWholeItem => {
                let path = self.current().path().to_string();
                self.insert(&path);
            }
            BeginExpression => self.stack().current_mut().begin_expression(),
            View1D => self.view_1d(host),
            View2D => self.view_2d(host),
            _ => return false,
        }
        true
    }

    /// Actions particular to each kind of view
    fn handle_view_action(&mut self, action: BrowserAction, host: &mut dyn Host) {
        use BrowserAction::*;
        let rows = host.size().rows;
        if action == ViewAsTree {
            return self.view_as_tree(host);
        }
        if action == HideColumn {
            return self.hide_column(host);
        }
        match self.stack().current_mut() {
            BrowserState::TwoDTable(state) => match action {
                NextColumn => state.next_column(),
                PreviousColumn => state.previous_column(),
                UnfocusColumn => state.unfocus_column(),
                FirstColumn => state.first_column(),
                LastColumn => state.last_column(),
                NextItem => state.next_item(rows),
                PreviousItem => state.previous_item(rows),
                NextPage => state.next_page(rows),
                PreviousPage => state.previous_page(rows),
                FirstItem => state.first_item(rows),
                LastItem => state.last_item(rows),
                ToggleMarked => state.toggle_mark(),
                BeginSearch => state.begin_search(),
                _ => {}
            },
            BrowserState::SingleObject(state) => match action {
                NextItem => state.next_item(rows),
                PreviousItem => state.previous_item(rows),
                NextPage => state.next_page(rows),
                PreviousPage => state.previous_page(rows),
                FirstItem => state.first_item(rows),
                LastItem => state.last_item(rows),
                ToggleMarked => state.toggle_mark(),
                BeginSearch => state.begin_search(),
                _ => {}
            },
            BrowserState::Tree(state) => match action {
                NextColumn => state.right(),
                PreviousColumn => state.left(),
                NextItem => state.next_item(rows),
                PreviousItem => state.previous_item(rows),
                _ => {}
            },
            BrowserState::TextLines(state) => match action {
                NextItem => state.next_item(rows),
                PreviousItem => state.previous_item(rows),
                NextPage => state.next_page(rows),
                PreviousPage => state.previous_page(rows),
                FirstItem => state.first_item(rows),
                LastItem => state.last_item(rows),
                _ => {}
            },
            BrowserState::Help(state) => match action {
                NextItem => state.next_item(rows),
                PreviousItem => state.previous_item(rows),
                NextPage => state.next_page(rows),
                PreviousPage => state.previous_page(rows),
                FirstItem => state.first_item(rows),
                LastItem => state.last_item(rows),
                _ => {}
            },
            BrowserState::Value(_) => {}
        }
    }

    /// Typing in an incremental search (Scala `handleIncrementalSearchAction`)
    fn handle_search_action(&mut self, action: Action, host: &mut dyn Host) {
        use BrowserAction::*;
        let rows = host.size().rows;
        let Some(query) = self.current().search().map(|s| s.query.clone()) else {
            return;
        };
        match (self.stack().current_mut(), action) {
            (BrowserState::TwoDTable(state), action) => match action {
                Action::SelfInsert(s) => state.set_search(&format!("{query}{s}"), rows),
                Action::Browser(ToggleCase) => state.toggle_case(rows),
                Action::Browser(Unsearch) if !query.is_empty() => state.set_search(&without_last_char(&query), rows),
                Action::Browser(NextHit) => state.next_hit(rows),
                Action::Browser(PreviousHit) => state.previous_hit(rows),
                Action::Browser(ExitSearch) => state.stop_searching(),
                _ => {}
            },
            (BrowserState::SingleObject(state), action) => match action {
                Action::SelfInsert(s) => state.set_search(&format!("{query}{s}"), rows),
                Action::Browser(ToggleCase) => state.toggle_case(rows),
                Action::Browser(Unsearch) if !query.is_empty() => state.set_search(&without_last_char(&query), rows),
                Action::Browser(NextHit) => state.next_hit(rows),
                Action::Browser(PreviousHit) => state.previous_hit(rows),
                Action::Browser(ExitSearch) => state.stop_searching(),
                _ => {}
            },
            _ => {}
        }
    }

    /// Paste while browsing: into the expression line or search, if either is active
    pub(crate) fn browser_paste(&mut self, text: &str, host: &mut dyn Host) {
        if self.current().expression().is_some() || self.current().search().is_some() {
            self.handle_browser_action(Action::SelfInsert(text.to_string()), host);
        }
    }

    /// Insert `expression` into the command line, and leave the browser
    fn insert(&mut self, expression: &str) {
        self.text_change(|repl| repl.state.update_line_buffer(|b| b.insert_at_cursor(expression)));
        self.state.object_browser = None;
    }

    /// Run a command on the selection (e.g. ` | open`), ignoring its result
    fn run_on_selection(&mut self, suffix: &str, host: &mut dyn Host) {
        if let Some(selection) = self.current().selection_info(&self.ctx) {
            let command = combine_safely(&selection.path, suffix);
            self.run_in_browser(&command, None, host);
        }
    }

    /// Browse the children of the selected directory, or the lines of the selected file
    fn browse_file(&mut self, kind: FileKind, method: &str, host: &mut dyn Host) {
        let selection = self.current().selection_info(&self.ctx);
        if selected_file_kind(&self.ctx, selection.clone()) == Some(kind)
            && let Some(path) = selected_path(&self.ctx, selection)
        {
            let expression = format!("\"{}\".{method}", escape_chars(&path));
            self.accept_replacement_expression(&expression, host);
        }
    }

    /// Evaluate `expression` and browse its result (Scala `acceptReplacementExpression`)
    pub(crate) fn accept_replacement_expression(&mut self, expression: &str, host: &mut dyn Host) {
        if let Some(result) = self.run_in_browser(expression, None, host) {
            self.focus(&result, expression.to_string(), host);
        }
    }

    /// Evaluate code with `it` set to `it` (if given), without printing errors, and without changing the
    /// global variables (Scala uses a copy of them)
    pub(crate) fn run_in_browser(&mut self, code: &str, it: Option<Value>, host: &mut dyn Host) -> Option<Value> {
        let globals = self.ctx.globals().clone();
        let saved = globals.entries();
        if let Some(it) = it {
            globals.set_str(crate::repl::IT, it);
        }
        host.begin_command();
        let result = self.runner(host, false).run_unit("browser-expression", code, false);
        host.end_command();
        for key in globals.keys() {
            if !saved.iter().any(|(k, _)| *k == key) {
                globals.remove(&key);
            }
        }
        for (key, value) in saved {
            globals.set(key, value);
        }
        result
    }

    /// Show the value as a tree, or a tree as usual (Scala `viewAsTree`)
    fn view_as_tree(&mut self, host: &mut dyn Host) {
        let current = self.current().clone();
        let (value, path) = (current.raw_value(), current.path().to_string());
        let tree = !matches!(current, BrowserState::Tree(_));
        if !tree || matches!(value, Value::List(_) | Value::Object(_)) {
            let state = self.new_browser_state(&value, path, tree, host);
            if let Some(state) = state.filter(|s| !tree || matches!(s, BrowserState::Tree(_))) {
                self.stack().replace(state);
            }
        }
    }

    /// View an object as a table of fields, or a list as lines
    fn view_1d(&mut self, host: &mut dyn Host) {
        let (value, path) = (self.current().raw_value(), self.current().path().to_string());
        let config = self.view_config();
        let state = match &value {
            Value::Object(o) if !o.is_empty() => object::create_model(&mut self.ctx, o, host.size(), &config, true)
                .ok()
                .map(|model| BrowserState::SingleObject(SingleObjectState::new(model, path))),
            Value::List(xs) => model::text_lines_model(&mut self.ctx, xs)
                .ok()
                .map(|model| BrowserState::TextLines(TextLinesState::new(model, path))),
            _ => None,
        };
        if let Some(state) = state {
            self.stack().replace(state);
        }
    }

    /// View a list of (or object of) objects or lists as a 2D table
    fn view_2d(&mut self, host: &mut dyn Host) {
        let (value, path) = (self.current().raw_value(), self.current().path().to_string());
        let rows_or_lists = |values: Vec<Value>| values.iter().all(|v| matches!(v, Value::Object(_) | Value::List(_)));
        let suitable = match &value {
            Value::Object(o) => rows_or_lists(o.values()),
            Value::List(xs) => rows_or_lists(xs.snapshot()),
            _ => false,
        };
        if suitable {
            let config = self.view_config();
            if let Ok(model) = table::create_model(&mut self.ctx, &value, host.size(), &config, true, &[]) {
                self.stack()
                    .replace(BrowserState::TwoDTable(TwoDTableState::new(model, path)));
            }
        }
    }

    /// Hide the selected column of a 2D table
    fn hide_column(&mut self, host: &mut dyn Host) {
        let BrowserState::TwoDTable(mut state) = self.current().clone() else {
            return;
        };
        let Some(column) = state.current_column.filter(|&c| c > 0) else {
            return;
        };
        let Some(id) = state.model.columns[column].id else {
            return;
        };
        state.hidden_columns.push(id);
        let config = self.view_config();
        let Ok(model) = table::create_model(
            &mut self.ctx,
            &state.model.raw_value,
            host.size(),
            &config,
            true,
            &state.hidden_columns,
        ) else {
            return;
        };
        if column >= model.number_of_columns() {
            state.current_column = Some(column - 1);
        }
        state.model = model;
        self.stack().replace(BrowserState::TwoDTable(state));
    }

    /// Show the next (or previous) item of the list, or field of the object, that the current value came
    /// from, in its place (Scala `selectParentItem`)
    fn select_parent_item(&mut self, delta: isize, host: &mut dyn Host) {
        let Some(parent) = self.state.object_browser.as_ref().and_then(|s| s.parent()) else {
            return;
        };
        let parent = parent.raw_value();
        let path = self.current().path().to_string();
        let sibling =
            parent_item_by_index(&parent, &path, delta).or_else(|| parent_item_by_name(&parent, &path, delta));
        if let Some((item, path)) = sibling
            && let Some(state) = self.new_browser_state(&item, path, false, host)
        {
            self.stack().replace(state);
        }
    }
}

fn without_last_char(s: &str) -> String {
    let mut s = s.to_string();
    s.pop();
    s
}

/// `xs[i]` → the item `delta` along in the parent list, and its path
fn parent_item_by_index(parent: &Value, path: &str, delta: isize) -> Option<(Value, String)> {
    let lookup = decompose_lookup_with_integer_index(path)?;
    let Value::List(xs) = parent else {
        return None;
    };
    let items = xs.snapshot();
    let len = items.len() as i64;
    if len == 0 {
        return None;
    }
    let index = (lookup.index + delta as i64).rem_euclid(len) as usize;
    let item = items.get(index)?.clone();
    Some((item, combine_safely(&lookup.target, &format!("[{index}]"))))
}

/// `obj.name` or `obj['name']` → the field `delta` along in the parent object, and its path
fn parent_item_by_name(parent: &Value, path: &str, delta: isize) -> Option<(Value, String)> {
    let lookup = decompose_lookup_with_string_index(path).or_else(|| decompose_member(path))?;
    let Value::Object(o) = parent else {
        return None;
    };
    let fields = o.keys();
    let i = fields.iter().position(|f| f.as_str() == Some(lookup.index.as_str()))?;
    let index = (i as isize + delta).rem_euclid(fields.len() as isize) as usize;
    let field = fields[index].as_str()?.to_string();
    let item = o.get_str(&field)?;
    let suffix = if is_legal_identifier(&field) {
        format!(".{field}")
    } else {
        format!("['{}']", escape_chars(&field))
    };
    Some((item, combine_safely(&lookup.target, &suffix)))
}
