//! The browser's expression line (Scala `ExpressionActionHandler`): line editing with completion and
//! invocation assistance, and evaluating the expression on Enter.
//!
//! An expression that starts with the value's path is a follow-on: the rest is evaluated with `it` bound to
//! the value (so `r0 | reverse` reverses the value being browsed, without re-evaluating `r0`). Anything
//! else replaces it. Either way, the result is browsed next (Back returns).

use mash_syntax::expression_combiner::combine_safely;

use super::ExpressionState;
use crate::actions::Action;
use crate::editor;
use crate::host::Host;
use crate::line_buffer::LineBuffer;
use crate::repl::{IT, Repl, browse_completions, browse_target, line_buffer_edit};
use crate::state::CompletionState;

impl Repl {
    fn expression(&mut self) -> &mut ExpressionState {
        self.state
            .object_browser
            .as_mut()
            .and_then(|stack| stack.current_mut().expression_mut().as_mut())
            .expect("an expression is being typed")
    }

    /// Handle an action while typing an expression
    pub(crate) fn handle_expression_action(&mut self, action: Action, host: &mut dyn Host) {
        match self.expression().completion.clone() {
            Some(CompletionState::Incremental(completion)) => match action {
                Action::SelfInsert(s) => {
                    let buffer = self.expression().line_buffer.clone();
                    let (buffer, completion) = self.incremental_completion_insert(&buffer, &completion, &s, false);
                    let expression = self.expression();
                    expression.line_buffer = buffer;
                    expression.completion = completion;
                }
                Action::BackwardDeleteChar if completion.memento.is_some() => {
                    if let Some(memento) = completion.memento {
                        let (buffer, previous) = *memento;
                        let expression = self.expression();
                        expression.line_buffer = buffer;
                        expression.completion = Some(CompletionState::Incremental(previous));
                    }
                }
                Action::Complete if completion.immediately_after_completion => {
                    let expression = self.expression();
                    let (buffer, completion) =
                        browse_completions(&expression.line_buffer, completion.completions, completion.replace, 0);
                    expression.line_buffer = buffer;
                    expression.completion = Some(completion);
                }
                action => {
                    self.expression().completion = None;
                    self.handle_normal_expression_action(action, host);
                }
            },
            Some(CompletionState::Browse(completion)) => match browse_target(&action, &completion, host.size()) {
                Some(target) => {
                    let expression = self.expression();
                    let (buffer, completion) = browse_completions(
                        &expression.line_buffer,
                        completion.completions,
                        completion.replace,
                        target,
                    );
                    expression.line_buffer = buffer;
                    expression.completion = Some(completion);
                }
                None if action == Action::AcceptCompletion => self.expression().completion = None,
                None => {
                    self.expression().completion = None;
                    self.handle_normal_expression_action(action, host);
                }
            },
            None => self.handle_normal_expression_action(action, host),
        }
    }

    fn handle_normal_expression_action(&mut self, action: Action, host: &mut dyn Host) {
        use Action::*;
        match action {
            Enter => return self.accept_expression(host),
            Quit => {
                *self.stack_current_expression() = None;
                return;
            }
            BackwardKillWord => {
                let expression = self.expression();
                if let Some(text) = expression.line_buffer.selected_text() {
                    expression.copied = Some(text.to_string());
                }
                expression.line_buffer = expression.line_buffer.delete_backward_word();
            }
            Complete => {
                let buffer = self.expression().line_buffer.clone();
                if let Some((buffer, completion)) = self.start_completion(&buffer, false) {
                    let expression = self.expression();
                    expression.line_buffer = buffer;
                    expression.completion = completion;
                }
            }
            ToggleQuote => {
                let expression = self.expression();
                expression.line_buffer = editor::toggle_quotes(&expression.line_buffer, false);
            }
            ExpandSelection => {
                let expression = self.expression();
                if let Some(range) = editor::expand_selection(&expression.line_buffer, false) {
                    expression
                        .old_selections
                        .insert(0, expression.line_buffer.selection_or_cursor());
                    expression.line_buffer = expression.line_buffer.select(range);
                }
            }
            UnexpandSelection => {
                let expression = self.expression();
                if !expression.old_selections.is_empty() {
                    let range = expression.old_selections.remove(0);
                    expression.line_buffer = expression.line_buffer.select(range);
                }
            }
            AssistInvocation => {
                if self.expression().assistance.is_some() {
                    self.expression().assistance = None;
                } else {
                    let buffer = self.expression().line_buffer.clone();
                    self.expression().assistance = self.assistance_for(&buffer, false);
                }
                return;
            }
            Copy => {
                let expression = self.expression();
                if let Some(text) = expression.line_buffer.selected_text() {
                    expression.copied = Some(text.to_string());
                }
            }
            Paste => {
                let expression = self.expression();
                if let Some(copied) = expression.copied.clone() {
                    expression.line_buffer = expression.line_buffer.insert_at_cursor(&copied);
                }
            }
            Inline => self.inline_expression(host),
            action => {
                let expression = self.expression();
                if let Some(edited) = line_buffer_edit(&action, &expression.line_buffer) {
                    if edited != expression.line_buffer {
                        expression.old_selections.clear();
                    }
                    expression.line_buffer = edited;
                }
            }
        }
        self.update_expression_assistance();
    }

    fn stack_current_expression(&mut self) -> &mut Option<ExpressionState> {
        self.state
            .object_browser
            .as_mut()
            .expect("the browser is open")
            .current_mut()
            .expression_mut()
    }

    /// Keep invocation assistance up to date as the expression changes
    fn update_expression_assistance(&mut self) {
        if self.expression().assistance.is_some() {
            let buffer = self.expression().line_buffer.clone();
            let blank = buffer.text().trim().is_empty();
            let new = self.assistance_for(&buffer, false);
            let expression = self.expression();
            expression.assistance = new.or_else(|| if blank { None } else { expression.assistance.take() });
        }
    }

    /// Alt-E: evaluate the expression (or selection) and put the result, as an expression, in its place
    fn inline_expression(&mut self, host: &mut dyn Host) {
        let buffer = self.expression().line_buffer.clone();
        let code = buffer.selected_text().unwrap_or(buffer.text()).to_string();
        let result = self.run_in_browser(&code, None, host);
        if let Some(expression) = result.as_ref().and_then(mash_core::builtins::meta::value_to_expression) {
            self.expression().line_buffer = match buffer.selection() {
                Some(range) => buffer.replace(range, &expression),
                None => LineBuffer::new(expression),
            };
        }
    }

    /// Enter: stop typing, evaluate the expression, and browse its result (Scala `acceptExpression`)
    fn accept_expression(&mut self, host: &mut dyn Host) {
        let text = self.expression().line_buffer.text().to_string();
        *self.stack_current_expression() = None;
        let stack = self.state.object_browser.as_ref().expect("the browser is open");
        let (path, value) = (stack.current().path().to_string(), stack.current().raw_value());
        match text.strip_prefix(&path) {
            Some(follow_on) => {
                let new_path = combine_safely(&path, follow_on);
                let code = combine_safely(IT, follow_on);
                if let Some(result) = self.run_in_browser(&code, Some(value), host) {
                    self.focus(&result, new_path, host);
                }
            }
            None => self.accept_replacement_expression(&text, host),
        }
    }
}
