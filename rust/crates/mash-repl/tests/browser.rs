//! The object browser: ports of `repl/browser/TwoDTableBrowserStateTest.scala`,
//! `integration/BrowserIntegrationTest.scala` and the object-browser cases of `MiscIntegrationTest` and
//! `ReplRendererIntegrationTest`, plus new tests of opening the browser, navigation, search, marking, the
//! expression line, inserting into the command line, and rendering. Expected screens are from Scala Mash's
//! browser renderers except where a comment marks a deliberate difference.

mod common;

use common::{InMemoryTerminal, TestRepl};
use mash_core::eval::{Ctx, with_big_stack};
use mash_core::platform::TestPlatform;
use mash_repl::actions::Action;
use mash_repl::browser::actions::BrowserAction::*;
use mash_repl::browser::model::{DisplayModel, display_model};
use mash_repl::browser::{BrowserState, TwoDTableState};
use mash_repl::input::{InputEvent, Key, KeyPress};
use mash_repl::line_buffer::LineBuffer;
use mash_repl::screen::{Dimensions, Style, default_colours as colours};
use mash_repl::view::ViewConfig;

fn run(test: fn()) {
    with_big_stack(test);
}

/// A REPL showing `view.browse (expr)` on a terminal of the given size
fn browse(expr: &str, rows: usize, columns: usize) -> TestRepl {
    let mut t = TestRepl::with_size(Dimensions::new(rows, columns));
    t.input(&format!("view.browse ({expr})")).enter();
    t
}

fn browse_80x24(expr: &str) -> TestRepl {
    browse(expr, 24, 80)
}

fn assert_screen(t: &mut TestRepl, expected: &str) {
    assert_eq!(t.screen_text(), expected.trim_start_matches('\n'));
}

fn selection(t: &TestRepl) -> Option<String> {
    t.repl.browser_selection().map(|s| s.path)
}

// ------------------------------------------------------------------------------------ TwoDTableBrowserStateTest

fn two_d_state(expr: &str) -> TwoDTableState {
    let mut ctx = Ctx::new(std::rc::Rc::new(TestPlatform::default()));
    let value = ctx.run("test", expr).unwrap();
    match display_model(&mut ctx, &value, Dimensions::new(24, 80), &ViewConfig::default()).unwrap() {
        DisplayModel::TwoDTable(model) => TwoDTableState::new(model, "result".into()),
        other => panic!("not a 2D table: {other:?}"),
    }
}

#[test]
fn moving_to_the_next_column() {
    run(|| {
        let mut state = two_d_state("[{ a: 1, b: 2 }, { a: 3, b: 4 }]");
        assert_eq!(state.current_column, None);
        for expected in [0, 1, 2, 0] {
            state.next_column();
            assert_eq!(state.current_column, Some(expected));
        }
    });
}

#[test]
fn moving_to_the_previous_column() {
    run(|| {
        let mut state = two_d_state("[{ a: 1, b: 2 }, { a: 3, b: 4 }]");
        for expected in [2, 1, 0, 2] {
            state.previous_column();
            assert_eq!(state.current_column, Some(expected));
        }
    });
}

// ------------------------------------------------------------------------------------ BrowserIntegrationTest

#[test]
fn two_d_table_browser_allows_column_selection() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("view.browse [{ a: 1, b: 2 }, { a: 3, b: 4 }]").enter();
        assert_eq!(t.two_d().current_row, 0);
        assert_eq!(t.two_d().current_column, None);
        assert_eq!(t.browse_act(NextColumn).two_d().current_column, Some(0));
        assert_eq!(t.browse_act(UnfocusColumn).two_d().current_column, None);
        assert_eq!(t.browse_act(PreviousColumn).two_d().current_column, Some(2));
    });
}

fn pairs(rows: &[(&str, &str)]) -> Vec<(String, String)> {
    rows.iter().map(|(a, b)| (a.to_string(), b.to_string())).collect()
}

#[test]
fn moving_backwards_and_forwards_through_the_parent_items() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("view.browse [{ a: 1, b: 2 }, { a: 3, b: 4 }, { a: 5, b: 6 }]")
            .enter();
        t.two_d();
        t.browse_act(Focus);
        assert_eq!(t.single_object_rows(), pairs(&[("a", "1"), ("b", "2")]));
        assert_eq!(t.browser().path(), "r0[0]");

        t.browse_act(NextParentItem);
        assert_eq!(t.single_object_rows(), pairs(&[("a", "3"), ("b", "4")]));
        assert_eq!(t.browser().path(), "r0[1]");
        for expected in ["r0[2]", "r0[0]"] {
            assert_eq!(t.browse_act(NextParentItem).browser().path(), expected);
        }
        for expected in ["r0[2]", "r0[1]"] {
            assert_eq!(t.browse_act(PreviousParentItem).browser().path(), expected);
        }
        t.browse_act(Back);
        assert_eq!(t.two_d().path, "r0");
    });
}

#[test]
fn moving_backwards_and_forwards_through_the_parent_fields() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("view.browse { first: { a: 1, b: 2 }, 'second-item': { a: 3, b: 4 } }")
            .enter();
        t.two_d();
        t.browse_act(Focus);
        assert_eq!(t.single_object_rows(), pairs(&[("a", "1"), ("b", "2")]));
        assert_eq!(t.browser().path(), "r0.first");

        t.browse_act(NextParentItem);
        assert_eq!(t.single_object_rows(), pairs(&[("a", "3"), ("b", "4")]));
        assert_eq!(t.browser().path(), "r0['second-item']");

        t.browse_act(PreviousParentItem);
        assert_eq!(t.browser().path(), "r0.first");
    });
}

fn rows(rows: &[&[&str]]) -> Vec<Vec<String>> {
    rows.iter().map(|r| r.iter().map(|c| c.to_string()).collect()).collect()
}

#[test]
fn navigating_to_a_follow_on_expression() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("view.browse [{ a: 1, b: 2 }, { a: 3, b: 4 }]").enter();
        t.browse_act(BeginExpression).input(" | reverse").enter();
        assert_eq!(t.two_d().path, "r0 | reverse");
        assert_eq!(t.two_d_rows(), rows(&[&["0", "3", "4"], &["1", "1", "2"]]));

        t.browse_act(Back);
        assert_eq!(t.two_d().path, "r0");
        assert_eq!(t.two_d_rows(), rows(&[&["0", "1", "2"], &["1", "3", "4"]]));
    });
}

#[test]
fn navigating_to_a_replacement_expression() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("view.browse [{ a: 1, b: 2 }]").enter();
        t.browse_act(BeginExpression)
            .act(Action::BackwardKillLine)
            .input("[{ a: 3, b: 4 }]")
            .enter();
        assert_eq!(t.two_d().path, "[{ a: 3, b: 4 }]");
        assert_eq!(t.two_d_rows(), rows(&[&["0", "3", "4"]]));
    });
}

fn expression_buffer(t: &TestRepl) -> LineBuffer {
    t.browser()
        .expression()
        .expect("not typing an expression")
        .line_buffer
        .clone()
}

#[test]
fn expression_browser_supports_copy_and_paste() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("view.browse [{ a: 1, b: 2 }]").enter();
        t.browse_act(BeginExpression)
            .act(Action::BackwardKillLine)
            .input("1 + 1")
            .left_extending(4)
            .act(Action::Copy)
            .act(Action::EndOfLine)
            .act(Action::Paste);
        assert_eq!(expression_buffer(&t), LineBuffer::parse_marked("1 + 1 + 1▶"));
    });
}

// ----------------------------------------------------------------------- MiscIntegrationTest, ReplRendererIntegrationTest

#[test]
fn invocation_assistance_works_in_the_expression_browser() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("view.browse [{ foo: 42 }]").enter();
        t.two_d();
        t.browse_act(BeginExpression)
            .input(" | reverse")
            .act(Action::AssistInvocation);
        let assistance = |t: &TestRepl| t.browser().expression().unwrap().assistance.clone();
        match assistance(&t).map(|a| a.0) {
            Some(mash_core::complete::Assistable::Function(f)) => assert_eq!(f.name(), "reverse"),
            other => panic!("expected assistance for reverse, got {other:?}"),
        }
        t.act(Action::AssistInvocation);
        assert_eq!(assistance(&t), None);
    });
}

#[test]
fn empty_lines_are_rendered_in_the_browser_expression_editor() {
    run(|| {
        let mut t = TestRepl::with_size(Dimensions::new(24, 15));
        t.input("view.browse [{ a: 1, b: 2 }, { a: 3, b: 4 }]").enter();
        t.two_d();
        t.browse_act(BeginExpression)
            .act(Action::Newline)
            .act(Action::Newline)
            .act(Action::Newline)
            .input("XXX");
        assert_screen(
            &mut t,
            "
List: r0
.....\x20
.....\x20
..... XXX     ◉
║ │0│1│2║
║ │1│3│4║
╚═╧═╧═╧═╝
⌃g quit, ↩ done",
        );
    });
}

// ------------------------------------------------------------------------------------------- opening the browser

#[test]
fn output_too_large_for_the_screen_opens_the_browser() {
    run(|| {
        let mut t = TestRepl::with_size(Dimensions::new(10, 80));
        // Fits: 5 rows plus 4 lines of borders and headers, and the prompt
        t.input("1.to 5 | map (n => { n })").enter();
        assert!(t.repl.state().object_browser.is_none());
        t.input("1.to 6 | map (n => { n })").enter();
        assert_eq!(t.two_d().path, "r1");
        assert_eq!(t.two_d().model.number_of_rows(), 6);
        // `it` is the result
        assert_eq!(
            t.last_value_string(),
            "[{ n: 1 }, { n: 2 }, { n: 3 }, { n: 4 }, { n: 5 }, { n: 6 }]"
        );
    });
}

#[test]
fn large_lists_objects_and_help_open_the_browser() {
    run(|| {
        let mut t = TestRepl::with_size(Dimensions::new(5, 80));
        for (command, kind) in [
            ("(1.to 5) | map (.toString)", "TextLines"),
            ("{ a: 1, b: 2, c: 3 }", "SingleObject"),
            ("maths.Stats", "Help"),
            ("view.tree { a: [1, 2, 3, 4] }", "Tree"),
        ] {
            t.input(command).enter();
            let state = format!("{:?}", t.browser());
            assert!(state.starts_with(kind), "{command}: {state}");
            t.browse_act(Exit);
        }
        // Small enough to print
        t.input("{ a: 1, b: 2 }").enter();
        assert!(t.repl.state().object_browser.is_none());
    });
}

#[test]
fn large_output_is_printed_if_browsing_it_is_turned_off() {
    run(|| {
        let mut t = TestRepl::with_size(Dimensions::new(5, 80));
        t.input("config.view.browseLargeOutput = false").enter();
        t.input("1.to 10 | map (n => { n })").enter();
        assert!(t.repl.state().object_browser.is_none());
        t.input("config.view.browseLargeOutput = true").enter();
        t.input("view.print (1.to 10 | map (n => { n }))").enter();
        assert!(t.repl.state().object_browser.is_none());
        // `it` is the data, not the view
        let items: Vec<String> = (1..=10).map(|n| format!("{{ n: {n} }}")).collect();
        assert_eq!(t.last_value_string(), format!("[{}]", items.join(", ")));
    });
}

#[test]
fn view_browse_always_browses() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("view.browse 42").enter();
        assert!(matches!(t.browser(), BrowserState::Value(_)));
        t.browse_act(Exit);
        t.input("view.browse (view.tree [1, [2]])").enter();
        assert!(matches!(t.browser(), BrowserState::Tree(_)));
        assert_eq!(t.last_value_string(), "[1, [2]]");
    });
}

#[test]
fn ctrl_v_browses_the_last_result() {
    run(|| {
        let mut t = TestRepl::new();
        t.act(Action::BrowseLastResult);
        assert!(t.repl.state().object_browser.is_none());
        t.input("[1, 2]").enter();
        t.input("{ a: 1 }").enter();
        let key = t.repl.key_map().action(KeyPress::ctrl('v'), true);
        t.act(key);
        assert_eq!(t.single_object().path, "r1");
        assert_eq!(t.single_object_rows(), pairs(&[("a", "1")]));
    });
}

#[test]
fn back_from_the_first_value_leaves_the_browser() {
    run(|| {
        let mut t = browse_80x24("[1, 2]");
        t.browse_act(Back);
        assert!(t.repl.state().object_browser.is_none());
    });
}

// ------------------------------------------------------------------------------------------- keys

fn press(t: &mut TestRepl, key: KeyPress) {
    t.repl.handle_event(InputEvent::Key(key), &mut t.host);
}

fn char_key(c: char) -> KeyPress {
    KeyPress::plain(Key::Char(c))
}

#[test]
fn keys_drive_the_browser() {
    run(|| {
        let mut t = browse_80x24("[{ a: 1, b: 2 }, { a: 3, b: 4 }, { a: 5, b: 6 }]");
        press(&mut t, KeyPress::plain(Key::Down));
        press(&mut t, KeyPress::ctrl('n'));
        assert_eq!(t.two_d().current_row, 2);
        press(&mut t, char_key('g'));
        assert_eq!(t.two_d().current_row, 0);
        press(&mut t, char_key('G'));
        assert_eq!(t.two_d().current_row, 2);
        press(&mut t, KeyPress::plain(Key::Right));
        press(&mut t, KeyPress::ctrl('e'));
        assert_eq!(t.two_d().current_column, Some(2));
        press(&mut t, KeyPress::ctrl('a'));
        assert_eq!(t.two_d().current_column, Some(0));
        press(&mut t, char_key('R'));
        assert_eq!(t.two_d().current_column, None);
        press(&mut t, char_key('m'));
        assert!(t.two_d().marked.contains(&2));
        // Unbound keys do nothing
        press(&mut t, char_key('z'));
        press(&mut t, char_key('q'));
        assert!(t.repl.state().object_browser.is_none());
    });
}

#[test]
fn key_maps_follow_the_browser_mode() {
    use mash_repl::actions::KeyMap;
    run(|| {
        let mut t = browse_80x24("[{ a: 1 }]");
        assert_eq!(t.repl.key_map(), KeyMap::ObjectBrowser);
        t.browse_act(BeginSearch);
        assert_eq!(t.repl.key_map(), KeyMap::ObjectBrowserSearch);
        assert_eq!(
            t.repl.key_map().action(char_key('q'), false),
            Action::SelfInsert("q".into())
        );
        t.browse_act(ExitSearch).browse_act(BeginExpression);
        assert_eq!(t.repl.key_map(), KeyMap::ObjectBrowserExpression);
        // Line editing, but no history
        assert_eq!(
            t.repl.key_map().action(KeyPress::ctrl('a'), false),
            Action::BeginningOfLine
        );
        assert_eq!(t.repl.key_map().action(KeyPress::plain(Key::Up), false), Action::Noop);
        assert_eq!(t.repl.key_map().action(KeyPress::ctrl('d'), true), Action::DeleteChar);
    });
}

// ------------------------------------------------------------------------------------------- navigation

#[test]
fn rows_wrap_round_and_pages_scroll() {
    run(|| {
        let mut t = browse("1.to 30 | map (n => { n })", 12, 80);
        // 6 rows fit
        t.browse_act(PreviousItem);
        assert_eq!((t.two_d().current_row, t.two_d().first_row), (29, 24));
        t.browse_act(NextItem);
        assert_eq!((t.two_d().current_row, t.two_d().first_row), (0, 0));
        t.browse_act(NextPage);
        assert_eq!((t.two_d().current_row, t.two_d().first_row), (5, 0));
        t.browse_act(NextPage);
        assert_eq!((t.two_d().current_row, t.two_d().first_row), (10, 5));
        t.browse_act(PreviousPage);
        assert_eq!((t.two_d().current_row, t.two_d().first_row), (3, 3));
        t.browse_act(LastItem);
        assert_eq!((t.two_d().current_row, t.two_d().first_row), (29, 24));
        t.browse_act(FirstItem);
        assert_eq!((t.two_d().current_row, t.two_d().first_row), (0, 0));
    });
}

#[test]
fn focusing_on_a_cell() {
    run(|| {
        let mut t = browse_80x24("[{ a: { b: 1 }, c: 2 }]");
        t.browse_act(NextColumn).browse_act(NextColumn);
        assert_eq!(selection(&t).as_deref(), Some("r0[0].a"));
        t.browse_act(Focus);
        assert_eq!(t.single_object().path, "r0[0].a");
        assert_eq!(t.single_object_rows(), pairs(&[("b", "1")]));
        t.browse_act(Back).browse_act(PreviousColumn);
        // The row label: the index itself
        assert_eq!(selection(&t).as_deref(), Some("r0 | _ => 0"));
        t.browse_act(UnfocusColumn);
        assert_eq!(selection(&t).as_deref(), Some("r0[0]"));
    });
}

#[test]
fn fields_are_selected_by_name() {
    run(|| {
        let mut t = browse_80x24("{ f: (x => x), 'two words': 2, n: 3 }");
        assert_eq!(selection(&t).as_deref(), Some("r0.f"));
        t.browse_act(NextItem);
        assert_eq!(selection(&t).as_deref(), Some("r0['two words']"));
        t.browse_act(NextItem);
        assert_eq!(selection(&t).as_deref(), Some("r0.n"));
    });
}

#[test]
fn numeric_keys_are_looked_up() {
    run(|| {
        // Scala Mash gives `r02` (it leaves out the brackets)
        let mut t = browse_80x24("{ (1): { a: 2 }, (2): { a: 3 } }");
        t.browse_act(NextItem);
        assert_eq!(selection(&t).as_deref(), Some("r0[2]"));
        t.browse_act(NextColumn).browse_act(NextColumn);
        assert_eq!(selection(&t).as_deref(), Some("r0[2].a"));
    });
}

#[test]
fn marking_rows_selects_them_all() {
    run(|| {
        let mut t = browse_80x24("[{ a: 1 }, { a: 2 }, { a: 3 }]");
        t.browse_act(ToggleMarked)
            .browse_act(NextItem)
            .browse_act(NextItem)
            .browse_act(ToggleMarked);
        assert_eq!(selection(&t).as_deref(), Some("r0 | [_[0], _[2]]"));
        t.browse_act(Focus);
        assert_eq!(t.two_d_rows(), rows(&[&["0", "1"], &["1", "3"]]));
        t.browse_act(Back).browse_act(ToggleMarked);
        // One marked row: that row, even with a column selected
        t.browse_act(NextColumn);
        assert_eq!(selection(&t).as_deref(), Some("r0[0]"));
    });
}

#[test]
fn marking_fields_selects_them_all() {
    run(|| {
        let mut t = browse_80x24("{ a: 1, b: 2, c: 3 }");
        t.browse_act(ToggleMarked).browse_act(LastItem).browse_act(ToggleMarked);
        assert_eq!(selection(&t).as_deref(), Some("r0 | select --a --c"));
        t.browse_act(Focus);
        assert_eq!(t.single_object_rows(), pairs(&[("a", "1"), ("c", "3")]));
        assert_eq!(t.last_value_string(), "{ a: 1, b: 2, c: 3 }");
    });
}

#[test]
fn objects_of_objects_are_tables_by_field() {
    run(|| {
        let mut t = browse_80x24("{ x: { a: 1 }, y: { a: 2 }, z: { a: 3 } }");
        t.browse_act(ToggleMarked).browse_act(NextItem).browse_act(ToggleMarked);
        assert_eq!(selection(&t).as_deref(), Some("r0 | select --x --y"));
    });
}

#[test]
fn hiding_columns() {
    run(|| {
        let mut t = browse_80x24("[{ a: 1, b: 2, c: 3 }]");
        t.browse_act(HideColumn);
        assert_eq!(t.two_d().model.column_names(), ["#", "a", "b", "c"]);
        t.browse_act(LastColumn).browse_act(HideColumn);
        assert_eq!(t.two_d().model.column_names(), ["#", "a", "b"]);
        assert_eq!(t.two_d().current_column, Some(2));
        t.browse_act(PreviousColumn).browse_act(HideColumn);
        assert_eq!(t.two_d().model.column_names(), ["#", "b"]);
        assert_eq!(t.two_d().current_column, Some(1));
        // The row label column can't be hidden
        t.browse_act(FirstColumn).browse_act(HideColumn);
        assert_eq!(t.two_d().model.column_names(), ["#", "b"]);
    });
}

#[test]
fn the_tree_view() {
    run(|| {
        let mut t = browse_80x24("[{ a: 1, b: { c: [1, 2], d: {} } }, [3, []]]");
        t.browse_act(ViewAsTree);
        let tree = |t: &TestRepl| match t.browser() {
            BrowserState::Tree(state) => state.clone(),
            other => panic!("not a tree: {other:?}"),
        };
        assert_eq!(selection(&t).as_deref(), Some("r0[0]"));
        t.browse_act(NextColumn).browse_act(NextColumn);
        assert_eq!(selection(&t).as_deref(), Some("r0[0].a"));
        t.browse_act(NextItem);
        assert_eq!(selection(&t).as_deref(), Some("r0[0].b"));
        // Down from a label moves to the next part at the same depth: the items of a list are at the depth of
        // their field's label
        t.browse_act(NextColumn).browse_act(NextColumn).browse_act(NextItem);
        assert_eq!(selection(&t).as_deref(), Some("r0[0].b.c[0]"));
        t.browse_act(NextItem).browse_act(NextItem);
        assert_eq!(selection(&t).as_deref(), Some("r0[0].b.d"));
        t.browse_act(PreviousColumn)
            .browse_act(PreviousColumn)
            .browse_act(PreviousColumn);
        assert_eq!(selection(&t).as_deref(), Some("r0[0].b"));
        t.browse_act(Focus);
        assert_eq!(t.single_object().path, "r0[0].b");
        t.browse_act(Back);
        assert!(!tree(&t).selection.0.is_empty());
        // `t` again: back to the usual view (lines, as the items aren't all objects or all lists)
        t.browse_act(ViewAsTree);
        assert!(matches!(t.browser(), BrowserState::TextLines(_)));
        // Empty containers can't be shown as trees
        let mut t = browse_80x24("[]");
        t.browse_act(ViewAsTree);
        assert!(matches!(t.browser(), BrowserState::TextLines(_)));
    });
}

#[test]
fn views_in_one_and_two_dimensions() {
    run(|| {
        let mut t = browse_80x24("[{ a: 1 }, { a: 2 }]");
        t.browse_act(View1D);
        assert!(matches!(t.browser(), BrowserState::TextLines(_)));
        t.browse_act(View2D);
        assert_eq!(t.two_d_rows(), rows(&[&["0", "1"], &["1", "2"]]));
        let mut t = browse_80x24("{ x: { a: 1 }, y: { a: 2 } }");
        t.browse_act(View1D);
        assert_eq!(t.single_object_rows(), pairs(&[("x", "{ a: 1 }"), ("y", "{ a: 2 }")]));
    });
}

#[test]
fn text_lines() {
    run(|| {
        let mut t = browse("['a', 'b\\tc', 'd']", 24, 80);
        t.browse_act(NextItem);
        assert_eq!(selection(&t).as_deref(), Some("r0[1]"));
        t.browse_act(Focus);
        assert!(matches!(t.browser(), BrowserState::Value(_)));
        assert_eq!(t.browser().path(), "r0[1]");
        // Nothing to select in a value
        assert_eq!(selection(&t), None);
    });
}

#[test]
fn help_links_go_to_other_help() {
    run(|| {
        let mut t = browse_80x24("maths.Stats");
        // On the parent class's line
        for _ in 0..4 {
            t.browse_act(NextItem);
        }
        assert_eq!(selection(&t).as_deref(), Some("core.Object"));
        for _ in 0..4 {
            t.browse_act(NextItem);
        }
        assert_eq!(selection(&t).as_deref(), Some("r0.helpForField \"max\""));
        t.browse_act(Focus);
        assert_eq!(t.browser().path(), "r0.helpForField \"max\"");
        let BrowserState::Help(help) = t.browser() else {
            panic!("not help");
        };
        assert_eq!(help.model.lines[0].to_string(), "FIELD");
        // From the field to its type and class
        t.browse_act(LastItem);
        while selection(&t).is_none() {
            t.browse_act(PreviousItem);
        }
        assert_eq!(selection(&t).as_deref(), Some("core.Number"));
        t.browse_act(PreviousItem);
        while selection(&t).is_none() {
            t.browse_act(PreviousItem);
        }
        assert_eq!(selection(&t).as_deref(), Some("maths.Stats"));
    });
}

#[test]
fn help_static_method_links() {
    run(|| {
        let mut t = browse_80x24("view.View");
        t.browse_act(LastItem);
        while !selection(&t).is_some_and(|s| s.contains("new")) {
            t.browse_act(PreviousItem);
        }
        assert_eq!(selection(&t).as_deref(), Some("r0.new"));
        t.browse_act(Focus);
        assert!(matches!(t.browser(), BrowserState::Help(_)));
    });
}

// ------------------------------------------------------------------------------------------- search

#[test]
fn incremental_search() {
    run(|| {
        let mut t = browse_80x24("[{ a: 'xa', b: 2 }, { a: 'y', b: 'A' }, { a: 1, b: 'z' }, { a: 'a', b: 0 }]");
        t.browse_act(NextColumn).browse_act(NextItem);
        t.browse_act(BeginSearch);
        // Searching selects whole rows
        assert_eq!(t.two_d().current_column, None);
        t.input("a");
        assert_eq!(t.two_d().search.as_ref().unwrap().rows(), [0, 1, 3]);
        assert_eq!(t.two_d().current_row, 1);
        t.browse_act(NextHit);
        assert_eq!(t.two_d().current_row, 3);
        t.browse_act(NextHit);
        assert_eq!(t.two_d().current_row, 0);
        t.browse_act(PreviousHit);
        assert_eq!(t.two_d().current_row, 3);
        t.browse_act(ToggleCase);
        assert_eq!(t.two_d().search.as_ref().unwrap().rows(), [0, 3]);
        t.input("x");
        assert_eq!(t.two_d().search.as_ref().unwrap().rows(), [] as [usize; 0]);
        t.browse_act(Unsearch);
        assert_eq!(t.two_d().search.as_ref().unwrap().query, "a");
        // Not a valid regex: literal text
        t.input("(");
        assert_eq!(t.two_d().search.as_ref().unwrap().rows(), [] as [usize; 0]);
        t.browse_act(Unsearch).browse_act(ExitSearch);
        assert!(t.two_d().search.is_none());
        assert_eq!(t.two_d().current_row, 3);
    });
}

#[test]
fn searching_fields() {
    run(|| {
        let mut t = browse_80x24("{ apple: 1, banana: 'pear', cherry: 3 }");
        t.browse_act(BeginSearch).input("e");
        assert_eq!(t.single_object().search.as_ref().unwrap().rows(), [0, 1, 2]);
        t.input("a");
        assert_eq!(t.single_object().current_row, 1);
        t.repl.handle_event(InputEvent::Paste("r".into()), &mut t.host);
        assert_eq!(t.single_object().search.as_ref().unwrap().query, "ear");
    });
}

// ------------------------------------------------------------------------------------------- expressions

#[test]
fn expressions_are_evaluated_without_changing_globals() {
    run(|| {
        let mut t = browse_80x24("[1, 2, 3]");
        t.browse_act(BeginExpression).input(" | map (x => x * 10)").enter();
        assert_eq!(t.browser().path(), "r0 | map (x => x * 10)");
        assert_eq!(mash_core::eval::render(&t.browser().raw_value()), "[10, 20, 30]");
        t.browse_act(BeginExpression)
            .act(Action::BackwardKillLine)
            .input("x = 42; x")
            .enter();
        assert_eq!(t.browser().path(), "x = 42; x");
        t.browse_act(Exit);
        t.input("x").enter();
        assert_eq!(t.last_value_string(), "[1, 2, 3]");
        // A failing expression leaves the browser as it was
        let mut t = browse_80x24("[1, 2, 3]");
        t.browse_act(BeginExpression).input(".nope").enter();
        assert_eq!(t.browser().path(), "r0");
        assert!(t.browser().expression().is_none());
        // Ctrl-G abandons the expression
        t.browse_act(BeginExpression).act(Action::Quit);
        assert!(t.browser().expression().is_none());
    });
}

#[test]
fn completion_in_the_expression_line() {
    run(|| {
        let mut t = browse_80x24("[1, 2, 3]");
        t.browse_act(BeginExpression).input(" | whereNo").act(Action::Complete);
        assert_eq!(expression_buffer(&t), LineBuffer::parse_marked("r0 | whereNot▶"));
        t.act(Action::BackwardKillWord).input("whe").act(Action::Complete);
        assert!(matches!(
            t.browser().expression().unwrap().completion,
            Some(mash_repl::state::CompletionState::Incremental(_))
        ));
        t.act(Action::Complete);
        assert_eq!(t.repl.key_map(), mash_repl::actions::KeyMap::BrowseCompletions);
        t.act(Action::NextCompletion).act(Action::AcceptCompletion);
        assert!(t.browser().expression().unwrap().completion.is_none());
        assert!(expression_buffer(&t).text().starts_with("r0 | where"));
    });
}

// ------------------------------------------------------------------------------------------- inserting

#[test]
fn inserting_the_selection_into_the_command_line() {
    run(|| {
        let mut t = TestRepl::new();
        t.input("[{ a: 1 }, { a: 2 }]").enter();
        t.input("foo ");
        t.act(Action::BrowseLastResult);
        t.browse_act(NextItem)
            .browse_act(NextColumn)
            .browse_act(NextColumn)
            .browse_act(InsertItem);
        assert!(t.repl.state().object_browser.is_none());
        t.assert_buffer("foo r0[1].a▶");
        t.act(Action::BrowseLastResult).browse_act(InsertWholeItem);
        t.assert_buffer("foo r0[1].ar0▶");
        t.act(Action::Undo);
        t.assert_buffer("foo r0[1].a▶");
    });
}

// ------------------------------------------------------------------------------------------- files

#[test]
fn browsing_directories_and_files() {
    run(|| {
        let mut t = TestRepl::with_files(&["/dir/a.txt", "/dir/sub/", "/dir/sub/b"], "/dir");
        t.input("view.browse (ls)").enter();
        assert_eq!(selection(&t).as_deref(), Some("r0[0]"));
        let status = t.screen_text().lines().last().unwrap().to_string();
        assert!(status.contains("r read") && !status.contains("d dir"), "{status}");
        t.browse_act(ReadFile);
        assert_eq!(t.browser().path(), "\"/dir/a.txt\".readLines");
        t.browse_act(Back).browse_act(NextItem).browse_act(FocusDirectory);
        assert_eq!(t.browser().path(), "\"/dir/sub\".children");
        assert_eq!(t.two_d().model.number_of_rows(), 1);
        // Reading a directory does nothing
        t.browse_act(Back).browse_act(ReadFile);
        assert_eq!(t.browser().path(), "r0");
    });
}

// ------------------------------------------------------------------------------------------- rendering

#[test]
fn rendering_a_2d_table() {
    run(|| {
        let mut t = browse_80x24("[{a: 1, b: 2}, {a: 3, b: 4}]");
        t.browse_act(NextItem).browse_act(NextColumn).browse_act(ToggleMarked);
        assert_screen(
            &mut t,
            "
List: r0
╔═╤═╤═╤═╗
║ │#│a│b║
╟─┼─┼─┼─╢
║ │0│1│2║
║◈│1│3│4║
╚═╧═╧═╧═╝
2/2 (q exit, m mark, f focus, b back, i insert, I insert whole, t tree, / find,…",
        );
        let screen = t.render();
        assert!(screen.alternate);
        let row = &screen.lines[5].string.chars;
        // The selected cell is highlighted: the row label, in yellow
        assert_eq!(row[3].style, Style::fg(colours::YELLOW).with_inverse());
        assert_eq!(row[5].style, Style::DEFAULT);
        assert_eq!(row[1].style, Style::DEFAULT);
        let status = &screen.lines[7].string.chars;
        assert!(status[0].style.inverse);
        assert_eq!(status[5].style, Style::fg(colours::CYAN).with_inverse());
    });
}

#[test]
fn rendering_a_scrolled_table() {
    run(|| {
        let mut t = browse("1.to 30 | map (n => {n, sq: n * n})", 10, 40);
        t.browse_act(NextPage).browse_act(NextPage);
        assert_screen(
            &mut t,
            "
List: r0
╔═╤══╤══╤═══╗
║ │# │n │sq ║
╟─┼──┼↑─┼───╢
║ │3 │4 │16 ║
║ │4 │5 │25 ║
║ │5 │6 │36 ║
║ │6 │7 │49 ║
╚═╧══╧↓═╧═══╝
 7/30 (q exit, m mark, f focus, b back,…",
        );
    });
}

#[test]
fn rendering_a_single_object_table() {
    run(|| {
        let mut t = browse("maths.stats [1, 2, 3]", 8, 80);
        t.browse_act(NextItem).browse_act(NextItem);
        assert_screen(
            &mut t,
            "
Stats: r0
╔═════════════════════╗
║        Stats        ║
╟─┬────────↓────────┬─╢
║ │max              │3║
║ │mean             │2║
╚═╧════════↓════════╧═╝
q exit, m mark, f focus, b back, i insert, I insert whole, t tree, / find, e ex…",
        );
    });
}

#[test]
fn rendering_a_tree() {
    run(|| {
        let mut t = browse("[{a: 1, b: {c: [1, 2], d: {}}}, [3, []]]", 12, 40);
        t.browse_act(ViewAsTree)
            .browse_act(NextColumn)
            .browse_act(NextColumn)
            .browse_act(NextItem);
        assert_screen(
            &mut t,
            "
List: r0
┌──┬─ a: 1
│  └─ b:
│     ├─ c:
│     │  ├─ 1
│     │  └─ 2
│     └─ d: {}
└──┬─ 3
   └─ []
q exit, b back, f focus, i insert, I in…",
        );
        let screen = t.render();
        let line = screen.lines[2].string.to_string();
        let start = line.chars().position(|c| c == 'b').unwrap();
        assert!(screen.lines[2].string.chars[start].style.inverse);
    });
}

#[test]
fn rendering_help() {
    run(|| {
        let mut t = browse("maths.Stats", 14, 60);
        for _ in 0..8 {
            t.browse_act(NextItem);
        }
        assert_screen(
            &mut t,
            "
Class: r0
┌──────────────────────────────────────────────────────────┐
│CLASS                                                     │
│    maths.Stats - Statistics for a data set               │
│                                                          │
│PARENT                                                    │
│    core.Object                                           │
│                                                          │
│FIELDS                                                    │
│    min - Smallest value in data set                      │
│    max - Largest value in data set                       │
│    mean - Average value in the data set                  │
└──────────────────────────────────────────────────────────┘
(q exit, b back, I insert whole)",
        );
        // The link is highlighted
        let screen = t.render();
        let line = &screen.lines[10].string.chars;
        assert!(line[5].style.inverse && line[7].style.inverse && !line[8].style.inverse);
    });
}

#[test]
fn rendering_a_search() {
    run(|| {
        let mut t = browse("[{a: 'xa', b: 2}, {a: 'y', b: 'A'}, {a: 1, b: 'z'}]", 10, 60);
        t.browse_act(BeginSearch).input("a");
        assert_screen(
            &mut t,
            "
List: r0
╔═╤═╤══╤═╗
║ │#│a │b║
╟─┼─┼──┼─╢
║ │0│xa│2║
║ │1│y │A║
║ │2│1 │z║
╚═╧═╧══╧═╝
1/2 Find: a (⌃n next, ⌃p prev, ↩ done, ⌃t case sensitive)",
        );
        let screen = t.render();
        let hit = screen.lines[4].string.chars[6];
        assert_eq!(
            (hit.c, hit.style),
            ('a', Style::fg(colours::CYAN).with_bold().with_inverse())
        );
    });
}

#[test]
fn rendering_text_lines_and_values() {
    run(|| {
        let mut t = browse("['a', '', 'c', 'd', 'e']", 6, 50);
        t.browse_act(NextItem);
        assert_screen(
            &mut t,
            "
List: r0
a
\x20
c
d
2/5 (q exit, b back, I insert whole, N next prnt,…",
        );
        let mut t = browse("\"abcdefghijklmno`n`nxyz\"", 6, 10);
        // As in Scala Mash, an empty line shows nothing
        assert_screen(
            &mut t,
            "
Path: r0
abcdefghij⤦
klmno
xyz
q exit, b…",
        );
    });
}

// ------------------------------------------------------------------------------------------- drawing

#[test]
fn the_browser_is_drawn_on_the_alternate_screen() {
    run(|| {
        let size = Dimensions::new(12, 40);
        let mut t = TestRepl::with_size(size);
        let mut terminal = InMemoryTerminal::new(size);
        // The normal screen is drawn below the commands run so far; the alternate screen fills the terminal
        let mut check = |t: &mut TestRepl| {
            let screen = terminal.send(&std::mem::take(&mut t.host.output));
            let expected = t.render();
            let lines = |s: &mash_repl::screen::Screen| {
                let mut lines: Vec<String> = s.lines.iter().map(|l| l.string.to_string()).collect();
                while lines.last().is_some_and(String::is_empty) {
                    lines.pop();
                }
                lines
            };
            let (actual, expected_lines) = (lines(&screen), lines(&expected));
            let offset = actual.len() - expected_lines.len();
            assert!(!expected.alternate || offset == 0);
            assert_eq!(actual[offset..], expected_lines);
            assert_eq!(screen.alternate, expected.alternate);
            let shift = |p: mash_repl::screen::Point| mash_repl::screen::Point::new(p.row + offset, p.column);
            assert_eq!(screen.cursor, expected.cursor.map(shift));
        };
        t.repl.draw(&mut t.host);
        check(&mut t);
        for c in "1.to 20 | map (n => { n })".chars() {
            press(&mut t, char_key(c));
        }
        check(&mut t);
        press(&mut t, KeyPress::plain(Key::Enter));
        check(&mut t);
        assert!(t.render().alternate);
        for key in [
            KeyPress::plain(Key::Down),
            char_key(' '),
            char_key('m'),
            char_key('e'),
            char_key('x'),
            KeyPress::ctrl('g'),
            char_key('/'),
            char_key('1'),
            KeyPress::plain(Key::Enter),
            KeyPress::ctrl('l'),
            char_key('f'),
            char_key('b'),
        ] {
            press(&mut t, key);
            check(&mut t);
        }
        t.repl
            .handle_event(InputEvent::Resize(Dimensions::new(12, 30)), &mut t.host);
        check(&mut t);
        press(&mut t, char_key('q'));
        // Back on the normal screen, at a fresh prompt
        check(&mut t);
        assert!(!t.render().alternate);
        assert!(t.screen_text().starts_with("[1] ~ $"));
    });
}

#[test]
fn rerendering_after_a_resize_refits_the_table() {
    run(|| {
        let mut t = browse(
            "[{ a: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' }]",
            24,
            80,
        );
        t.host.size = Dimensions::new(24, 20);
        t.repl
            .handle_event(InputEvent::Resize(Dimensions::new(24, 20)), &mut t.host);
        assert_eq!(t.two_d().model.column_widths(), vec![1, 14]);
    });
}
