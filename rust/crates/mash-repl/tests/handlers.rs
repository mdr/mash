//! Ports of `repl/handler/InsertLastArgHandlerTest.scala`, `IncrementalHistorySearchActionHandlerTest.scala`
//! and `repl/history/HistoryTest.scala`.

use jiff::{SignedDuration, Timestamp};
use mash_repl::actions::Action;
use mash_repl::handlers::{SearchOutcome, begin_search, begin_search_from_line, handle_search_action, insert_last_arg};
use mash_repl::history::{History, MemoryHistoryStorage};
use mash_repl::line_buffer::LineBuffer;
use mash_repl::state::{HistorySearchState, Hit, HitStatus, InsertLastArgState, ReplState};

const WORKING_DIRECTORY: &str = "/home/alice";

fn time(i: usize) -> Timestamp {
    "2026-01-01T00:00:00Z".parse::<Timestamp>().unwrap() + SignedDuration::from_secs(i as i64)
}

/// History with these commands, run in this order (each a second after the last)
fn history_with_paths(entries: &[(&str, &str)]) -> History {
    let mut history = History::new(Box::new(MemoryHistoryStorage::default()), "session");
    for (i, (command, path)) in entries.iter().enumerate() {
        history.record(command, i as u64, path, false, time(i));
    }
    history
}

fn history(commands: &[&str]) -> History {
    let entries: Vec<_> = commands.iter().map(|c| (*c, WORKING_DIRECTORY)).collect();
    history_with_paths(&entries)
}

fn state(s: &str) -> ReplState {
    ReplState {
        line_buffer: LineBuffer::parse_marked(s),
        ..ReplState::default()
    }
}

// ------------------------------------------------------------------------------------------- insert last arg

#[test]
fn insert_last_arg_cycles_through_arguments() {
    let args = |i: usize| ["1", "22", "3"].get(i).map(|s| s.to_string());
    let with_arg = |s: &str, arg_index: usize, range| ReplState {
        insert_last_arg: Some(InsertLastArgState { arg_index, range }),
        ..state(s)
    };
    let mut s = state("foo ▶");
    insert_last_arg(&mut s, args);
    assert_eq!(s, with_arg("foo 1▶", 0, 4..5));
    insert_last_arg(&mut s, args);
    assert_eq!(s, with_arg("foo 22▶", 1, 4..6));
    insert_last_arg(&mut s, args);
    assert_eq!(s, with_arg("foo 3▶", 2, 4..5));
    insert_last_arg(&mut s, args);
    assert_eq!(s, with_arg("foo 3▶", 2, 4..5));
}

// ------------------------------------------------------------------------------------------- history search

fn searching(s: &str, search: &str, original: &str, hit: HitStatus, current_dir_only: bool) -> ReplState {
    ReplState {
        history_search: Some(HistorySearchState {
            search: search.into(),
            original_line: original.into(),
            hit,
            current_dir_only,
        }),
        ..state(s)
    }
}

fn before_first_hit(s: &str, original: &str) -> ReplState {
    searching(s, "", original, HitStatus::BeforeFirstHit, false)
}

/// A hit on the `index`th match, which is the `entry`th command recorded
fn hit(s: &str, search: &str, index: usize, range: std::ops::Range<usize>, entry: usize) -> ReplState {
    hit_in(s, search, index, range, entry, WORKING_DIRECTORY, false)
}

fn hit_in(
    s: &str,
    search: &str,
    index: usize,
    range: std::ops::Range<usize>,
    entry: usize,
    directory: &str,
    current_dir_only: bool,
) -> ReplState {
    let hit = HitStatus::Hit(Hit {
        result_index: index,
        match_range: range,
        timestamp: time(entry),
        working_directory: directory.into(),
    });
    searching(s, search, "existing", hit, current_dir_only)
}

struct Search {
    history: History,
    state: ReplState,
    cwd: &'static str,
}

impl Search {
    fn new(history: History) -> Search {
        let mut search = Search {
            history,
            state: state("existing▶"),
            cwd: WORKING_DIRECTORY,
        };
        begin_search(&mut search.state, &mut search.history);
        assert_eq!(search.state, before_first_hit("existing▶", "existing"));
        search
    }

    #[track_caller]
    fn act(&mut self, action: Action, consumed: bool) -> &ReplState {
        let outcome = handle_search_action(&action, &mut self.state, &mut self.history, Some(self.cwd));
        let expected = if consumed {
            SearchOutcome::Consumed
        } else {
            SearchOutcome::NotConsumed
        };
        assert_eq!(outcome, expected);
        &self.state
    }

    fn insert(&mut self, s: &str) -> &ReplState {
        self.act(Action::SelfInsert(s.into()), true)
    }
}

#[test]
fn cycles_through_matches_most_recent_first() {
    let mut search = Search::new(history(&["foo", "bar", "baz"]));
    assert_eq!(*search.insert("b"), hit("baz▶", "b", 0, 0..1, 2));
    assert_eq!(
        *search.act(Action::IncrementalHistorySearch, true),
        hit("bar▶", "b", 1, 0..1, 1)
    );
    assert_eq!(
        *search.act(Action::IncrementalHistorySearch, true),
        hit("bar▶", "b", 1, 0..1, 1)
    );
}

#[test]
fn characters_can_be_deleted_from_the_search() {
    let mut search = Search::new(history(&["foo", "bar", "baz"]));
    assert_eq!(*search.insert("a"), hit("baz▶", "a", 0, 1..2, 2));
    assert_eq!(*search.insert("r"), hit("bar▶", "ar", 0, 1..3, 1));
    assert_eq!(
        *search.act(Action::BackwardDeleteChar, true),
        hit("baz▶", "a", 0, 1..2, 2)
    );
    assert_eq!(
        *search.act(Action::BackwardDeleteChar, true),
        before_first_hit("▶", "existing")
    );
    assert_eq!(*search.act(Action::BackwardDeleteChar, true), state("▶"));
}

#[test]
fn accepting_a_result_ends_the_search() {
    let mut search = Search::new(history(&["foo", "bar", "baz"]));
    assert_eq!(*search.insert("f"), hit("foo▶", "f", 0, 0..1, 0));
    assert_eq!(*search.act(Action::Enter, true), state("foo▶"));
}

#[test]
fn another_action_implicitly_accepts_the_result() {
    let mut search = Search::new(history(&["foo", "bar", "baz"]));
    assert_eq!(*search.insert("f"), hit("foo▶", "f", 0, 0..1, 0));
    assert_eq!(*search.act(Action::BeginningOfLine, false), state("foo▶"));
}

#[test]
fn searches_ignore_case() {
    let mut search = Search::new(history(&["foo", "FOO"]));
    assert_eq!(*search.insert("f"), hit("FOO▶", "f", 0, 0..1, 1));
    assert_eq!(
        *search.act(Action::IncrementalHistorySearch, true),
        hit("foo▶", "f", 1, 0..1, 0)
    );
}

#[test]
fn search_continues_without_hits() {
    let mut search = Search::new(history(&["apple"]));
    assert_eq!(*search.insert("a"), hit("apple▶", "a", 0, 0..1, 0));
    assert_eq!(
        *search.insert("a"),
        searching("▶", "aa", "existing", HitStatus::NoHits, false)
    );
}

#[test]
fn search_can_start_from_the_line() {
    let mut history = history(&["apple"]);
    let mut s = state("app▶");
    begin_search_from_line(&mut s, &mut history, Some(WORKING_DIRECTORY));
    let mut expected = hit("apple▶", "app", 0, 0..3, 0);
    expected.history_search.as_mut().unwrap().original_line = "app".into();
    assert_eq!(s, expected);
}

#[test]
fn jump_to_first_or_last_hit() {
    let mut search = Search::new(history(&["foo", "bar", "baz"]));
    assert_eq!(*search.act(Action::LastHit, true), hit("foo▶", "", 2, 0..0, 0));
    assert_eq!(*search.act(Action::FirstHit, true), hit("baz▶", "", 0, 0..0, 2));
}

#[test]
fn restrict_to_current_directory() {
    let history = history_with_paths(&[("apple", "/appleDir"), ("ball", "/ballDir")]);
    let mut search = Search::new(history);
    search.cwd = "/appleDir";
    assert_eq!(
        *search.act(Action::ToggleCurrentDirOnly, true),
        hit_in("apple▶", "", 0, 0..0, 0, "/appleDir", true)
    );
    assert_eq!(
        *search.act(Action::ToggleCurrentDirOnly, true),
        hit_in("ball▶", "", 0, 0..0, 1, "/ballDir", false)
    );
}

#[test]
fn change_directory_to_the_hit() {
    let mut search = Search::new(history_with_paths(&[("apple", "/appleDir")]));
    search.insert("a");
    let outcome = handle_search_action(&Action::ChangeDirectory, &mut search.state, &mut search.history, None);
    assert_eq!(outcome, SearchOutcome::ChangeDirectory("/appleDir".into()));
}

// ------------------------------------------------------------------------------------------- history

#[test]
fn history_stops_at_the_first_entry() {
    let mut history = history(&["command1", "command2"]);
    assert_eq!(history.go_backwards("inprogress").as_deref(), Some("command2"));
    assert_eq!(history.go_backwards("command2").as_deref(), Some("command1"));
    assert_eq!(history.go_backwards("command1"), None);
}

#[test]
fn history_remembers_the_command_in_progress() {
    let mut history = history(&["command1"]);
    assert_eq!(history.go_backwards("inprogress").as_deref(), Some("command1"));
    assert_eq!(history.go_forwards().as_deref(), Some("inprogress"));
}

#[test]
fn history_is_loaded_from_storage_and_saved_to_it() {
    let mut storage = MemoryHistoryStorage::default();
    {
        let mut history = History::new(Box::new(storage.clone()), "s1");
        history.record("one", 0, "/", false, time(0));
        storage.entries.extend(history.entries().iter().cloned());
    }
    let mut history = History::new(Box::new(storage), "s2");
    assert_eq!(history.go_backwards("").as_deref(), Some("one"));
    assert_eq!(history.historical_argument(0).as_deref(), Some("one"));
}
