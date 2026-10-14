//! The interpreter sees the REPL's history: `os.history` (with this session's results) and `jump`.

mod common;

use std::rc::Rc;

use common::{FakeHost, LARGE, TestRepl};
use mash_core::eval::{Ctx, render, with_big_stack};
use mash_core::history::HistoryEntry as InterpreterEntry;
use mash_core::platform::TestPlatform;
use mash_repl::Repl;
use mash_repl::history::{HistoryEntry, HistoryStorage, MemoryHistoryStorage};

#[test]
fn history_lists_commands_with_their_results() {
    with_big_stack(|| {
        let mut t = TestRepl::new();
        t.input("40 + 2").enter();
        t.input("[1, 2].length").enter();
        // A command is recorded once it has run
        t.input("history.command").enter();
        assert_eq!(render(&t.last_value()), "[40 + 2, \"[1, 2].length\"]");
        t.input("history.result").enter();
        let results = t.last_value();
        assert_eq!(render(&results), "[42, 2, [40 + 2, \"[1, 2].length\"]]");
        t.input("history.first.session").enter();
        assert_eq!(t.last_value_string(), "test-session");
        t.input("history.first.reexecute").enter();
        assert_eq!(t.last_value_string(), "42");
    });
}

#[test]
fn jump_uses_directories_from_the_history() {
    with_big_stack(|| {
        let mut t = TestRepl::with_files(&["/home/alice/projects/mash/", "/tmp/"], "/home/alice");
        t.input(r#"cd "projects/mash""#).enter();
        t.input("1").enter();
        t.input(r#"cd "/tmp""#).enter();
        t.input(r#"jump "mash""#).enter();
        t.input("pwd").enter();
        assert_eq!(t.last_value_string(), "/home/alice/projects/mash");
    });
}

#[test]
fn saved_history_is_visible_without_results() {
    let mut storage = MemoryHistoryStorage::default();
    storage.save(&HistoryEntry {
        session_id: "old".into(),
        command_number: 0,
        timestamp: "2026-01-01T00:00:00Z".parse().expect("timestamp"),
        command: "1 + 1".into(),
        mish: false,
        working_directory: "/home/alice".into(),
    });
    let entry: InterpreterEntry = storage.entries[0].for_interpreter(mash_core::value::Value::Null);
    assert_eq!(entry.command, "1 + 1");
    with_big_stack(move || {
        let platform = Rc::new(TestPlatform::default());
        let repl = Repl::new(Ctx::new(platform.clone()), Box::new(storage), "new");
        let mut t = TestRepl {
            repl,
            host: FakeHost::new(LARGE),
            platform,
        };
        t.input("history | first | .session").enter();
        assert_eq!(t.last_value_string(), "old");
        t.input("history.first.result").enter();
        assert_eq!(t.last_value_string(), "null");
    });
}
