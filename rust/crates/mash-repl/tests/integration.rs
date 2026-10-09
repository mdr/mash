//! Ports of `integration/MiscIntegrationTest.scala`, `IncrementalCompletionIntegrationTest.scala` and
//! `ReplRendererIntegrationTest.scala` (minus the object browser cases), plus new end-to-end tests of
//! running commands, key events and drawing.

mod common;

use common::{InMemoryTerminal, TestRepl, screen_text};
use mash_core::eval::with_big_stack;
use mash_repl::ReplStatus;
use mash_repl::actions::Action;
use mash_repl::input::{InputEvent, Key, KeyPress};
use mash_repl::screen::{Dimensions, Point};
use mash_repl::state::CompletionState;

fn run(test: fn()) {
    with_big_stack(test);
}

// ------------------------------------------------------------------------------------------- MiscIntegrationTest

#[test]
fn repl_works() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("1").enter();
        assert_eq!(t.last_value_string(), "1");
    });
}

#[test]
fn single_tab_completes_a_unique_completion() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("whereNo").complete();
        assert_eq!(t.text(), "whereNot");
        t.assert_buffer("whereNot▶");
    });
}

#[test]
fn two_tabs_browse_completions() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("where").complete().complete();
        assert!(matches!(t.repl.state().completion, Some(CompletionState::Browse(_))));
    });
}

#[test]
fn completion_after_a_hyphen() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("ls -42 # foo").left(8);
        t.assert_buffer("ls -▶42 # foo");
        t.complete();
        t.assert_buffer("ls -▶42 # foo");
    });
}

#[test]
fn history_forwards_past_the_current_line() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("1").enter();
        t.input("2").enter();
        assert_eq!(t.text(), "");
        assert_eq!(t.up().text(), "2");
        assert_eq!(t.down().text(), "");
        assert_eq!(t.down().text(), "");
        assert_eq!(t.up().text(), "2");
        assert_eq!(t.up().text(), "1");
    });
}

#[test]
fn history_resets_if_an_old_line_is_edited() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("command1").enter();
        t.input("command2").enter();
        t.input("command3").enter();
        t.up().up();
        assert_eq!(t.text(), "command2");
        assert_eq!(t.down().text(), "command3");
        assert_eq!(t.up().backspace().text(), "command");
        t.down();
        assert_eq!(t.text(), "command");
    });
}

#[test]
fn up_moves_the_cursor_in_a_multiline_editor() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("{").enter().input("}");
        t.up().up();
        assert_eq!(t.text(), "{\n}");
    });
}

#[test]
fn down_moves_the_cursor_in_a_multiline_editor() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("{").enter().input("}").enter();
        t.input("command2").enter();
        t.up().up().act(Action::EndOfLine);
        assert_eq!(t.text(), "{\n}");
        t.down();
        assert_eq!(t.text(), "{\n}");
    });
}

#[test]
fn history_navigates_past_a_multiline_entry() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("command1").enter();
        t.input("{").enter().input("}").enter();
        t.input("command3").enter();
        t.up().up();
        assert_eq!(t.text(), "{\n}");
        t.up();
        assert_eq!(t.text(), "command1");
        t.down();
        assert_eq!(t.text(), "{\n}");
        t.down();
        assert_eq!(t.text(), "command3");
    });
}

#[test]
fn toggling_quotes() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("foo");
        assert_eq!(t.act(Action::ToggleQuote).text(), "\"foo\"");
        assert_eq!(t.act(Action::ToggleQuote).text(), "foo");
    });
}

#[test]
fn delete_at_the_first_character() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("123").left(3).act(Action::DeleteChar);
        t.assert_buffer("▶23");
    });
}

#[test]
fn bare_words_setting() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("config.language.bareWords = true").enter();
        t.input("foo").enter();
        assert_eq!(t.last_value_string(), "foo");

        t.input("config.language.bareWords = false").enter();
        t.input("foo").enter();
        // `foo` was an error, so `it` is still the result of the assignment
        assert_eq!(t.last_value_string(), "false");
    });
}

#[test]
fn type_inference_loop_bug() {
    run(|| {
        TestRepl::new().input("a => a").enter().complete();
    });
}

#[test]
fn local_variables_do_not_collide_with_globals() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("a = 0").enter();
        t.input("def setA n = { a = n }").enter();
        t.input("setA 42").enter();
        t.input("a").enter();
        assert_eq!(t.last_value_string(), "0");
    });
}

#[test]
fn completing_dotfiles() {
    run(|| {
        let mut t = TestRepl::with_files(&["/.dotfiles/.bashrc"], "/");
        t.input(r#""/.dotfiles/"."#).complete();
        assert_eq!(t.text(), r#""/.dotfiles/.""#);
    });
}

#[test]
fn multiline_editing() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("{").enter().input("  42").enter().input("}").enter();
        assert_eq!(t.last_value_string(), "42");
    });
}

#[test]
fn completion_through_previously_defined_nullary_functions() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("foo = { bar: => { baz: 100 } }").enter();
        t.input("foo.bar.ba").complete();
        assert_eq!(t.text(), "foo.bar.baz");
    });
}

#[test]
fn incremental_history_search_ignores_case() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("FOO = 100").enter();
        t.input("foobar = 42").enter();
        t.act(Action::IncrementalHistorySearch).input("FOO");
        assert_eq!(t.text(), "foobar = 42");
        t.act(Action::IncrementalHistorySearch);
        assert_eq!(t.text(), "FOO = 100");
    });
}

#[test]
fn incremental_history_search_from_a_partial_line() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("FOO = 100").enter();
        t.input("foobar = 42").enter();
        t.input("FOO").up();
        assert_eq!(t.text(), "foobar = 42");
        t.up();
        assert_eq!(t.text(), "FOO = 100");
    });
}

#[test]
fn inserting_the_last_argument() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("sort 'arg1'").enter();
        t.input("sort 'arg2'").enter();
        t.input("sort ").act(Action::InsertLastArg);
        assert_eq!(t.text(), "sort 'arg2'");
        t.act(Action::InsertLastArg);
        assert_eq!(t.text(), "sort 'arg1'");
        t.act(Action::InsertLastArg);
        assert_eq!(t.text(), "sort 'arg1'");
    });
}

#[test]
fn invocation_assistance() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("reverse").act(Action::AssistInvocation);
        let assistance = t.repl.state().assistance.as_ref().expect("no assistance");
        assert!(matches!(&assistance.0, mash_core::complete::Assistable::Function(f) if f.name() == "reverse"));
        t.act(Action::AssistInvocation);
        assert!(t.repl.state().assistance.is_none());
    });
}

#[test]
fn inlining_an_expression() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("1 + 2 * 3").act(Action::Inline);
        assert_eq!(t.text(), "7");
    });
}

#[test]
fn expanding_and_unexpanding_the_selection() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("1 + 2 * 3").left(2);
        t.assert_buffer("1 + 2 *▶ 3");
        t.act(Action::ExpandSelection);
        t.assert_buffer("1 + ▷2 * 3▶");
        t.act(Action::ExpandSelection);
        t.assert_buffer("▷1 + 2 * 3▶");
        t.act(Action::UnexpandSelection);
        t.assert_buffer("1 + ▷2 * 3▶");
        t.act(Action::UnexpandSelection);
        t.assert_buffer("1 + 2 *▶ 3");
        t.act(Action::UnexpandSelection);
        t.assert_buffer("1 + 2 *▶ 3");
    });
}

#[test]
fn cutting_the_selection_and_pasting() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("1 + 2 * 3")
            .left(2)
            .act(Action::ExpandSelection)
            .act(Action::BackwardKillWord)
            .act(Action::Paste)
            .input(" + ")
            .act(Action::Paste);
        t.assert_buffer("1 + 2 * 3 + 2 * 3▶");
    });
}

#[test]
fn copy_and_paste() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("1 + 1")
            .left_extending(4)
            .act(Action::Copy)
            .act(Action::EndOfLine)
            .act(Action::Paste);
        t.assert_buffer("1 + 1 + 1▶");
    });
}

#[test]
fn abandoning_a_command() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("{").enter().input("  def foo = 42").act(Action::Quit);
        t.assert_buffer("▶");
        assert_eq!(t.repl.state().command_number, 0);
    });
}

#[test]
fn inserting_a_newline() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("def foo =").act(Action::Newline);
        t.assert_buffer("def foo =\n▶");
    });
}

#[test]
fn undo_and_redo() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("a").input("b").input("c").act(Action::BackwardKillWord);
        t.assert_buffer("▶");
        for expected in ["abc▶", "ab▶", "a▶", "▶", "▶"] {
            t.act(Action::Undo);
            t.assert_buffer(expected);
        }
        for expected in ["a▶", "ab▶", "abc▶", "▶", "▶"] {
            t.act(Action::Redo);
            t.assert_buffer(expected);
        }
    });
}

// ------------------------------------------------------------------------------------------- incremental completion

fn displays(t: &TestRepl) -> Vec<String> {
    t.incremental_completion()
        .completions
        .iter()
        .map(|c| c.display.clone())
        .collect()
}

#[test]
fn incremental_completion_stays_incremental_while_typing() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("whe");
        assert_eq!(t.complete().text(), "where");
        assert_eq!(t.incremental_completion().replace, 0.."where".len());
        let completions = displays(&t);
        assert!(completions.contains(&"where".into()) && completions.contains(&"whereNot".into()));
        t.input("N");
        t.incremental_completion();
    });
}

#[test]
fn incremental_completion_ends_on_an_exact_match() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("where").complete().input("Not");
        assert_eq!(t.repl.state().completion, None);
    });
}

#[test]
fn incremental_completion_ends_without_matches() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("where").complete().input("a");
        assert_eq!(t.repl.state().completion, None);
    });
}

#[test]
fn incremental_completion_backspace_removes_added_characters() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("where").complete().input("N").backspace();
        assert_eq!(t.text(), "where");
        let completions = displays(&t);
        assert!(completions.contains(&"where".into()) && completions.contains(&"whereNot".into()));
    });
}

#[test]
fn incremental_completion_allows_further_tab_completions() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("where").complete().input("N").complete();
        assert_eq!(t.text(), "whereNot");
        assert_eq!(t.repl.state().completion, None);
    });
}

#[test]
fn incremental_completion_ends_on_backspacing_past_the_first_completion() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("where").complete().input("N").backspace().backspace();
        assert_eq!(t.repl.state().completion, None);
    });
}

#[test]
fn incremental_completion_of_a_common_prefix_with_escapes() {
    run(|| {
        let mut t = TestRepl::with_files(&["/foo$bar", "/foo$baz"], "/");
        t.input("foo").complete();
        t.assert_buffer(r#""foo`$ba"◀"#);
        assert_eq!(displays(&t), ["foo$bar", "foo$baz"]);
    });
}

#[test]
fn incremental_completion_of_a_common_fragment() {
    run(|| {
        let mut t = TestRepl::with_files(&["/---foobar---", "/--goobab--"], "/");
        t.input("ob").complete();
        t.assert_buffer(r#""ooba"◀"#);
        assert_eq!(displays(&t), ["---foobar---", "--goobab--"]);
        let fragment = mash_repl::render::completions::CommonFragment::of(&t.incremental_completion().completions);
        assert_eq!(fragment.text(), "ooba");
    });
}

#[test]
fn incremental_completion_of_a_common_fragment_with_escapes() {
    run(|| {
        let mut t = TestRepl::with_files(&["/---foob$ar---", "/--goob$ab--"], "/");
        t.input("ob").complete();
        t.assert_buffer(r#""oob`$a"◀"#);
        assert_eq!(displays(&t), ["---foob$ar---", "--goob$ab--"]);
        let fragment = mash_repl::render::completions::CommonFragment::of(&t.incremental_completion().completions);
        assert_eq!(fragment.text(), "oob$a");
    });
}

#[test]
fn incremental_completion_after_completing_a_path() {
    run(|| {
        let mut t = TestRepl::with_files(&["/etc/foo.conf", "/etc/bar.conf"], "/");
        t.input("/etc/").complete();
        t.assert_buffer(r#""/etc/▶""#);
    });
}

#[test]
fn incremental_completion_of_substrings_with_path_prefixes() {
    run(|| {
        let mut t = TestRepl::with_files(&["/etc/foobar", "/etc/gooban"], "/");
        t.input("/etc/ob").complete();
        t.assert_buffer(r#""/etc/ooba▶""#);
        let fragment = mash_repl::render::completions::CommonFragment::of(&t.incremental_completion().completions);
        assert_eq!(fragment.prefix, "/etc/");
        assert_eq!(fragment.text(), "ooba");
        assert_eq!(displays(&t), ["/etc/foobar", "/etc/gooban"]);
    });
}

// ------------------------------------------------------------------------------------------- ReplRendererIntegrationTest

#[test]
fn rendering_the_initial_repl() {
    run(|| {
        let mut t = TestRepl::with_size(Dimensions::new(1000, 10));
        let screen = t.render();
        assert_eq!(screen_text(&screen), "[0] ~ $  ◉");
        assert_eq!(screen.cursor, Some(Point::new(0, 8)));
        assert_eq!(screen.title, "~");
    });
}

#[test]
fn rendering_wraps_in_a_narrow_terminal() {
    run(|| {
        let mut t = TestRepl::with_size(Dimensions::new(1000, 3));
        assert_eq!(t.screen_text(), "[0]⤦\n ~ ⤦\n$ ◉");
        assert_eq!(t.render().cursor, Some(Point::new(2, 2)));
    });
}

#[test]
fn rendering_a_line_as_wide_as_the_terminal() {
    run(|| {
        let mut t = TestRepl::with_size(Dimensions::new(1000, 10));
        t.input("89");
        assert_eq!(t.screen_text(), "[0] ~ $ 89⤦\n         ◉");
        assert_eq!(t.render().cursor, Some(Point::new(1, 0)));
    });
}

#[test]
fn rendering_multiple_lines_indented_with_dots() {
    run(|| {
        let mut t = TestRepl::with_size(Dimensions::new(1000, 15));
        t.input("{").enter().input("  42").enter().input("}");
        assert_eq!(t.screen_text(), "[0] ~ $ {\n.......   42\n....... }     ◉");
    });
}

// ------------------------------------------------------------------------------------------- new

#[test]
fn results_are_printed_and_saved() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("40 + 2").enter();
        t.input("[1, 2]").enter();
        assert!(
            strip_escapes(&t.host.output).contains("42\r\n"),
            "output: {:?}",
            t.host.output
        );
        assert_eq!(t.host.commands_run, 2);
        t.input("r0 + r[1].length").enter();
        assert_eq!(t.last_value_string(), "44");
        assert_eq!(t.repl.state().command_number, 3);
        assert_eq!(t.repl.history().entries().len(), 3);
    });
}

#[test]
fn errors_are_printed_with_the_location() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("def f x = x.foo").enter();
        t.host.output.clear();
        t.input("f 1").enter();
        let plain = strip_escapes(&t.host.output);
        assert!(plain.contains("Error: Cannot find member 'foo'"), "output: {plain:?}");
        assert!(
            plain.contains("command-0:1:f: def f x = x.foo\r\n"),
            "output: {plain:?}"
        );
        assert!(plain.contains("f 1\r\n^--\r\n"), "output: {plain:?}");
    });
}

#[test]
fn syntax_errors_are_printed() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("(1 + 2").enter().input(")").enter();
        // Parentheses don't continue lines; the unbalanced line is run, and fails
        t.host.output.clear();
        t.input("1 +").enter();
        let plain = strip_escapes(&t.host.output);
        assert!(plain.contains("Syntax error:"), "output: {plain:?}");
    });
}

#[test]
fn interrupted_commands_return_to_the_prompt() {
    run(|| {
        let mut t = TestRepl::new();
        t.repl.ctx().step_budget = Some(100);
        t.input("(1.to 100000) | map (x => x + 1) | sum").enter();
        let plain = strip_escapes(&t.host.output);
        assert!(
            plain.contains("Interrupted: command cancelled by user"),
            "output: {plain:?}"
        );
        assert_eq!(t.repl.state().command_number, 1);
    });
}

#[test]
fn bang_toggles_mish_mode() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("!").enter();
        assert!(t.repl.state().mish);
        assert!(t.screen_text().starts_with("[0] ~ ! "));
        assert_eq!(t.repl.state().command_number, 0);
        t.input("!").enter();
        assert!(!t.repl.state().mish);
    });
}

#[test]
fn toggle_mish_adds_and_removes_the_bang() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("ls").act(Action::ToggleMish);
        assert_eq!(t.text(), "!ls");
        t.act(Action::ToggleMish);
        assert_eq!(t.text(), "ls");
    });
}

#[test]
fn key_events_paste_and_exit() {
    run(|| {
        let mut t = TestRepl::new();
        let key = |c| InputEvent::Key(KeyPress::plain(Key::Char(c)));
        assert_eq!(t.repl.handle_event(key('4'), &mut t.host), ReplStatus::Continue);
        t.repl.handle_event(InputEvent::Paste("2\n+ 1".into()), &mut t.host);
        assert_eq!(t.text(), "42\n+ 1");
        t.repl.handle_event(InputEvent::Key(KeyPress::ctrl('a')), &mut t.host);
        t.repl.handle_event(InputEvent::Key(KeyPress::ctrl('k')), &mut t.host);
        assert_eq!(t.text(), "42\n");
        t.repl.handle_event(InputEvent::Key(KeyPress::ctrl('u')), &mut t.host);
        t.repl
            .handle_event(InputEvent::Key(KeyPress::plain(Key::Backspace)), &mut t.host);
        t.repl.handle_event(InputEvent::Key(KeyPress::ctrl('u')), &mut t.host);
        assert_eq!(t.text(), "");
        let status = t.repl.handle_event(InputEvent::Key(KeyPress::ctrl('d')), &mut t.host);
        assert_eq!(status, ReplStatus::Exit);
        assert!(t.host.output.ends_with("\r\n"));
    });
}

#[test]
fn drawing_reproduces_the_rendered_screen() {
    run(|| {
        let size = Dimensions::new(50, 12);
        let mut t = TestRepl::with_size(size);
        let mut terminal = InMemoryTerminal::new(size);
        let mut check = |t: &mut TestRepl| {
            // Compare the text of each line (an erased line stays on the terminal, empty)
            let screen = terminal.send(&std::mem::take(&mut t.host.output));
            let expected = t.render();
            let lines = |s: &mash_repl::screen::Screen| {
                let mut lines: Vec<String> = s.lines.iter().map(|l| l.string.to_string()).collect();
                while lines.last().is_some_and(String::is_empty) {
                    lines.pop();
                }
                lines
            };
            assert_eq!(lines(&screen), lines(&expected));
            assert_eq!(screen.cursor, expected.cursor);
        };
        t.repl.draw(&mut t.host);
        check(&mut t);
        for c in "reverse 日本語 [1, 2]".chars() {
            t.repl
                .handle_event(InputEvent::Key(KeyPress::plain(Key::Char(c))), &mut t.host);
            check(&mut t);
        }
        for _ in 0..6 {
            t.repl
                .handle_event(InputEvent::Key(KeyPress::plain(Key::Backspace)), &mut t.host);
            check(&mut t);
        }
        let events = [
            KeyPress::ctrl(' '),
            KeyPress::ctrl('u'),
            KeyPress::plain(Key::Char('w')),
            KeyPress::plain(Key::Char('h')),
            KeyPress::plain(Key::Tab),
            KeyPress::plain(Key::Tab),
            KeyPress::plain(Key::Right),
            KeyPress::plain(Key::Enter),
            KeyPress::alt(Key::Enter),
            KeyPress::plain(Key::Char('1')),
            KeyPress::ctrl('r'),
            KeyPress::plain(Key::Char('x')),
            KeyPress::ctrl('g'),
        ];
        for key in events {
            t.repl.handle_event(InputEvent::Key(key), &mut t.host);
            check(&mut t);
        }
    });
}

#[test]
fn startup_tip_unless_turned_off() {
    run(|| {
        let mut t = TestRepl::new();
        t.repl.start(&mut t.host);
        assert!(strip_escapes(&t.host.output).contains("┌─ Tip "));
        let mut t = TestRepl::new();
        t.input("config.cli.showStartupTips = false").enter();
        t.host.output.clear();
        t.repl.start(&mut t.host);
        assert!(!strip_escapes(&t.host.output).contains("Tip"));
    });
}

/// `s` without escape sequences
fn strip_escapes(s: &str) -> String {
    let mut out = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            match chars.next() {
                Some('[') => {
                    for c in chars.by_ref() {
                        if c.is_ascii_alphabetic() {
                            break;
                        }
                    }
                }
                Some(']') => {
                    for c in chars.by_ref() {
                        if c == '\x07' {
                            break;
                        }
                    }
                }
                _ => {}
            }
        } else {
            out.push(c);
        }
    }
    out
}
