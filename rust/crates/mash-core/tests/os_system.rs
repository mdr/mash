//! Users and groups, processes and signals, the clipboard, `open`, disk space, the terminal, `os.history` and
//! `jump`, against `TestPlatform`'s fake system (see `FakeSystem`) and a recorded history. Scala Mash on macOS
//! has no users or groups, and can't run most of these without a terminal; results follow its code, except
//! where a comment marks a deliberate difference.

use std::rc::Rc;

use jiff::Timestamp;
use mash_core::ast::SourceUnit;
use mash_core::complete::complete;
use mash_core::eval::{Ctx, render, with_big_stack};
use mash_core::history::{HistoryEntry, RecordedHistory};
use mash_core::infer;
use mash_core::platform::{MemoryFiles, Signal, TestPlatform};
use mash_core::value::Value;

fn platform() -> TestPlatform {
    TestPlatform {
        files: MemoryFiles::of(&[
            "/home/alice/projects/mash/",
            "/home/alice/Projects2/",
            "/home/alice/photos/",
            "/tmp/",
        ])
        .with_file("/home/alice/a.txt", "hi\n"),
        ..TestPlatform::default()
    }
}

fn entry(number: u64, command: &str, working_directory: &str) -> HistoryEntry {
    HistoryEntry {
        session_id: "s1".into(),
        command_number: number,
        timestamp: Timestamp::from_second(1_700_000_000 + number as i64).expect("valid"),
        command: command.into(),
        mish: false,
        result: Value::Null,
        working_directory: working_directory.into(),
    }
}

/// A history: three commands in `~/projects/mash`, two in `~/projects`, one in `~/photos`, one in a directory
/// that's gone, one mish command in `~/Projects2`
fn recorded_history() -> Rc<RecordedHistory> {
    let history = RecordedHistory::new(vec![
        entry(0, "1 + 2", "/home/alice/projects/mash"),
        entry(1, "ls", "/home/alice/projects/mash"),
        entry(2, "pwd", "/home/alice/projects/mash"),
        entry(3, "now", "/home/alice/photos"),
        entry(4, "ls", "/home/alice/projects"),
        entry(5, "ls", "/home/alice/projects"),
        entry(6, "42", "/home/alice/deleted"),
    ]);
    history.record(HistoryEntry {
        mish: true,
        result: Value::num(7.0),
        ..entry(7, "echo hi", "/home/alice/Projects2")
    });
    Rc::new(history)
}

fn ctx(platform: &Rc<TestPlatform>) -> Ctx {
    let mut ctx = Ctx::new(platform.clone());
    ctx.set_command_history(recorded_history());
    ctx
}

/// Run code, returning the rendered result (or the error) and the platform afterwards
fn run_with(platform: TestPlatform, code: &str) -> (String, Rc<TestPlatform>) {
    let platform = Rc::new(platform);
    let mut ctx = ctx(&platform);
    let result = match ctx.run("test", code) {
        Ok(value) => render(&value),
        Err(e) => format!("error: {e}"),
    };
    (result, platform)
}

/// Check each `(code, expected rendering)` pair, in a fresh interpreter each
fn check(cases: &'static [(&'static str, &'static str)]) {
    with_big_stack(move || {
        let failures: Vec<String> = cases
            .iter()
            .filter_map(|(code, expected)| {
                let actual = run_with(platform(), code).0;
                (actual != *expected).then(|| format!("{code:?}: got {actual:?}, expected {expected:?}"))
            })
            .collect();
        assert!(
            failures.is_empty(),
            "{} failing:\n  {}",
            failures.len(),
            failures.join("\n  ")
        );
    })
}

#[test]
fn users() {
    check(&[
        (
            "user",
            "{ name: alice, uid: 1000, primaryGroup: alice, home: /home/alice, shell: /bin/bash }",
        ),
        ("user.getClass", "os.UserSummary"),
        ("user.name.getClass", "core.String"),
        ("user.name.tag", "os.Username"),
        ("user.uid.tag", "os.Uid"),
        ("user.home.tag", "os.Path"),
        ("user.fullName", "Alice Liddell"),
        ("users.name", "[root, alice, bob]"),
        ("users.uid", "[0, 1000, 1001]"),
        // An empty GECOS field gives an empty full name
        (r#"(users | where (.name == "bob")).first.fullName"#, ""),
        ("user.groups", "[alice, wheel, staff]"),
        ("users.groups", "[[root], [alice, wheel, staff], [bob, staff]]"),
        // Username methods: info, the fields and methods of the user
        ("user.name.info.home", "/home/alice"),
        ("user.name.uid", "1000"),
        ("user.name.name", "alice"),
        ("user.name.primaryGroup", "alice"),
        ("user.name.shell", "/bin/bash"),
        ("user.name.fullName", "Alice Liddell"),
        ("user.name.groups", "[alice, wheel, staff]"),
        // Uid methods, likewise
        ("user.uid.info.name", "alice"),
        ("user.uid.name", "alice"),
        ("user.uid.home", "/home/alice"),
        ("user.uid.fullName", "Alice Liddell"),
        ("user.uid.groups", "[alice, wheel, staff]"),
        ("(users.first.name).home", "/root"),
        // Paths' owners are usernames
        (r#""a.txt".owner.fullName"#, "Alice Liddell"),
        (r#""a.txt".info.owner.uid"#, "1000"),
        (r#""a.txt".group.gid"#, "1000"),
    ]);
}

#[test]
fn groups() {
    check(&[
        ("groups | first", "{ name: root, gid: 0, users: [root] }"),
        ("groups.name", "[root, wheel, staff, alice, bob]"),
        ("groups.gid", "[0, 10, 20, 1000, 1001]"),
        ("groups.users", "[[root], [alice], [alice, bob], [alice], [bob]]"),
        ("(groups | first).getClass", "os.GroupInfo"),
        ("groups.first.name.tag", "os.Group"),
        ("groups.first.gid.tag", "os.Gid"),
        // Group methods
        ("user.primaryGroup.gid", "1000"),
        ("user.primaryGroup.users", "[alice]"),
        ("user.groups.gid", "[1000, 10, 20]"),
        ("user.groups.users", "[[alice], [alice], [alice, bob]]"),
        // Gid methods
        ("user.primaryGroup.gid.name", "alice"),
        ("user.primaryGroup.gid.name.tag", "os.Group"),
        ("(groups | where (.name == 'staff')).first.gid.users", "[alice, bob]"),
    ]);
}

#[test]
fn processes() {
    check(&[
        (
            "ps | first",
            "{ pid: 1, parentPid: null, name: init, command: /sbin/init, owner: root, residentSize: 1024, virtualSize: \
             4096 }",
        ),
        ("ps.pid", "[1, 100, 101, 102, 200]"),
        ("processes.name", "[init, mash, sleep, vim, sshd]"),
        ("os.processes | length", "5"),
        ("(ps | first).getClass", "os.Process"),
        ("ps.first.pid.tag", "os.Pid"),
        ("ps.first.owner.tag", "os.Username"),
        ("ps.first.residentSize.tag", "core.Bytes"),
        (r#"(ps | where (.name == "vim")).first.parent.name"#, "mash"),
        ("ps.first.parent", "null"),
        (r#"(ps | where (.name == "mash")).first.children.name"#, "[sleep, vim]"),
        ("ps.first.owner.fullName", "System Administrator"),
        // Pid methods: info, the methods and fields of the process
        ("ps.last.pid.info.name", "sshd"),
        ("ps.last.pid.name", "sshd"),
        ("ps.last.pid.command", "/usr/sbin/sshd -D"),
        ("ps.last.pid.parent.name", "init"),
        ("ps.first.pid.children.name", "[mash, sshd]"),
        ("ps.last.pid.parentPid.name", "init"),
        ("ps.last.pid.owner", "root"),
        ("ps.last.pid.residentSize", "204800"),
    ]);
}

/// Code, its result, the processes left, and the signals sent
type KillCase<'a> = (&'a str, &'a str, &'a [u32], &'a [(u32, Signal)]);

#[test]
fn kill() {
    with_big_stack(|| {
        let cases: &[KillCase] = &[
            (
                "kill 101",
                "",
                &[1, 100, 102, 200],
                &[(101, Signal::Named("TERM".into()))],
            ),
            (
                r#"kill --signal="HUP" 101 102"#,
                "",
                &[1, 100, 200],
                &[(101, Signal::Named("HUP".into())), (102, Signal::Named("HUP".into()))],
            ),
            (
                "kill --signal=9 101",
                "",
                &[1, 100, 102, 200],
                &[(101, Signal::Number(9))],
            ),
            // A signal that doesn't end the process
            (
                r#"kill --signal="STOP" 101"#,
                "",
                &[1, 100, 101, 102, 200],
                &[(101, Signal::Named("STOP".into()))],
            ),
            // Process objects, and (unlike Scala, where the documented example fails) lists of them
            (
                r#"ps | where (.owner == "root") | kill"#,
                "",
                &[100, 101, 102],
                &[(1, Signal::Named("TERM".into())), (200, Signal::Named("TERM".into()))],
            ),
            (
                "kill ps.last",
                "",
                &[1, 100, 101, 102],
                &[(200, Signal::Named("TERM".into()))],
            ),
            // Process.kill and Pid.kill take the signal as an ordinary argument
            (
                r#"ps.last.kill "INT""#,
                "",
                &[1, 100, 101, 102],
                &[(200, Signal::Named("INT".into()))],
            ),
            (
                "ps.last.pid.kill",
                "",
                &[1, 100, 101, 102],
                &[(200, Signal::Named("TERM".into()))],
            ),
            (
                "ps.last.pid.kill 1",
                "",
                &[1, 100, 101, 102],
                &[(200, Signal::Number(1))],
            ),
            // Errors: Scala's "Unable to kill process with pid 999, error code -1" now gives the reason
            (
                "kill 999",
                "error: Unable to kill process with pid 999: No such process",
                &[1, 100, 101, 102, 200],
                &[],
            ),
            (
                r#"kill --signal="FOO" 101"#,
                "error: Invalid argument 'signal'. Invalid signal: FOO",
                &[1, 100, 101, 102, 200],
                &[],
            ),
            (
                r#"kill "abc""#,
                "error: Invalid process ID: abc",
                &[1, 100, 101, 102, 200],
                &[],
            ),
            // Not called without arguments
            ("kill", "os.kill", &[1, 100, 101, 102, 200], &[]),
        ];
        for (code, expected, remaining, signals) in cases {
            let (result, after) = run_with(platform(), code);
            assert_eq!(&result, expected, "{code}");
            let pids: Vec<u32> = after.system.processes.borrow().iter().map(|p| p.pid).collect();
            assert_eq!(&pids, remaining, "{code}");
            assert_eq!(&*after.system.signals.borrow(), signals, "{code}");
        }
    });
}

#[test]
fn signal_class() {
    check(&[("os.Signal", "os.Signal"), ("os.Signal.getClass", "core.Class")]);
}

#[test]
fn clipboard_open_disk_space_and_terminal() {
    check(&[
        ("clipboard", ""),
        (r#"clipboard "hello"; clipboard"#, "hello"),
        (r#"clipboard "hello""#, ""),
        ("clipboard [1, 2, 3]; clipboard", "1\n2\n3"),
        ("clipboard 42; clipboard.getClass", "core.String"),
        ("diskSpace.mountPoint", "[/, /tmp]"),
        ("df.filesystem", "[/dev/sda1, tmpfs]"),
        (
            "df | first",
            "{ filesystem: /dev/sda1, total: 107374182400, used: 42949672960, available: 59055800320, mountPoint: / }",
        ),
        ("df.first.getClass", "os.DiskSpaceEntry"),
        ("df.first.total.tag", "core.Bytes"),
        ("df.first.mountPoint.tag", "os.Path"),
        ("terminal", "{ rows: 24, columns: 80 }"),
        ("terminal.columns", "80"),
        ("terminal.getClass", "os.Terminal"),
        ("os.Terminal.new 1 2", "{ rows: 1, columns: 2 }"),
    ]);
    with_big_stack(|| {
        // Strings have `Path`'s methods, so `open` calls `Path.open`; so do lists of paths (vectorised) and
        // `PathSummary` objects (from `ls`)
        let (result, after) = run_with(
            platform(),
            r#"open "http://example.com" ["a.txt"]; ls.path.open; "x".open; open ls.first"#,
        );
        assert_eq!(result, "");
        assert_eq!(
            *after.opened.borrow(),
            [
                "http://example.com",
                "a.txt",
                "Projects2",
                "a.txt",
                "photos",
                "projects",
                "x",
                "Projects2"
            ]
        );
        let no_terminal = TestPlatform {
            system: mash_core::platform::FakeSystem {
                terminal: None,
                ..Default::default()
            },
            ..platform()
        };
        assert_eq!(run_with(no_terminal, "terminal").0, "error: Not running in a terminal");
    });
}

#[test]
fn history() {
    check(&[
        ("history.command", "[1 + 2, ls, pwd, now, ls, ls, 42, echo hi]"),
        ("os.history.commandNumber", "[0, 1, 2, 3, 4, 5, 6, 7]"),
        (
            "history.last",
            "{ session: s1, commandNumber: 7, timestamp: 2023-11-14T22:13:27Z, command: echo hi, mish: true, result: \
             7, workingDirectory: /home/alice/Projects2 }",
        ),
        ("history.first.getClass", "core.HistoryEntry"),
        ("history.first.workingDirectory.tag", "os.Path"),
        ("history.first.reexecute", "3"),
        ("history.first.timestamp.getClass", "time.DateTime"),
    ]);
    // Without a REPL, there's no history
    with_big_stack(|| {
        let mut ctx = Ctx::new(Rc::new(platform()));
        assert_eq!(render(&ctx.run("test", "history").expect("runs")), "[]");
    });
}

#[test]
fn jump() {
    check(&[
        // The most used directory whose name contains the query
        (r#"jump "proj"; pwd"#, "/home/alice/projects"),
        (r#"jump "mash"; pwd"#, "/home/alice/projects/mash"),
        (r#"jump "s"; pwd"#, "/home/alice/projects/mash"),
        (r#"jump "Proj"; pwd"#, "/home/alice/Projects2"),
        // Then ignoring case
        (r#"jump "PROJECTS"; pwd"#, "/home/alice/projects"),
        (r#"jump "PROJECTS2"; pwd"#, "/home/alice/Projects2"),
        // Then as a subsequence
        (r#"jump "pht"; pwd"#, "/home/alice/photos"),
        // A directory is used as it is
        (r#"jump "/tmp"; pwd"#, "/tmp"),
        (r#"jump "/tmp"; oldDirs"#, "[/home/alice]"),
        // Directories that no longer exist are ignored
        (r#"jump "deleted""#, "error: Invalid argument 'query'. No matching path"),
        (r#"jump "zzz""#, "error: Invalid argument 'query'. No matching path"),
    ]);
}

/// Completions (their insert text) of `input`, where ▶ marks the cursor
fn completions(input: &str) -> Vec<String> {
    let pos = input.find('▶').expect("cursor marker");
    let text = input.replacen('▶', "", 1);
    let platform = Rc::new(platform());
    let mut ctx = ctx(&platform);
    complete(&mut ctx, &text, pos, false)
        .map(|r| r.completions.iter().map(|c| c.insert_text().to_string()).collect())
        .unwrap_or_default()
}

#[test]
fn completion() {
    with_big_stack(|| {
        assert_eq!(completions(r#"kill --signal=H▶"#), ["HUP"]);
        assert_eq!(completions(r#"kill --signal="US▶"#), ["USR1", "USR2"]);
        assert_eq!(completions(r#"ps.first.kill "KI▶"#), ["KILL"]);
        // Usernames, groups and pids are known values
        assert_eq!(completions(r#"user.name == "b▶"#), ["bob"]);
        assert_eq!(completions(r#"user.primaryGroup == "w▶"#), ["wheel"]);
        assert_eq!(completions(r#"ps.first.pid == "10▶"#), ["100", "101", "102"]);
        // jump completes directories from the history
        assert_eq!(completions(r#"jump "proj▶"#), ["/home/alice/projects"]);
        assert_eq!(completions(r#"jump "Proj▶"#), ["/home/alice/Projects2"]);
        assert_eq!(completions(r#"jump "mas▶"#), ["/home/alice/projects/mash"]);
        // Methods of tag classes
        assert!(completions("user.name.fullNa▶").contains(&"fullName".to_string()));
        assert!(completions("ps.first.pid.chil▶").contains(&"children".to_string()));
    });
}

const INFERENCE: &[(&str, &str)] = &[
    ("user", "os.UserSummary"),
    ("users", "[os.UserSummary]"),
    ("groups", "[os.GroupInfo]"),
    ("user.name", "core.String tagged os.Username"),
    ("user.uid", "core.Number tagged os.Uid"),
    ("user.fullName", "core.String"),
    ("user.groups", "[core.String tagged os.Group]"),
    ("user.name.info", "os.UserSummary"),
    ("user.name.home", "core.String tagged os.Path"),
    ("user.name.groups", "[core.String tagged os.Group]"),
    ("user.uid.fullName", "core.String"),
    ("user.primaryGroup.users", "[core.String tagged os.Username]"),
    // Scala infers [core.Number tagged os.Username] here
    ("user.primaryGroup.gid.name", "core.String tagged os.Group"),
    ("user.primaryGroup.gid.users", "[core.String tagged os.Username]"),
    ("groups.first.users", "[core.String tagged os.Username]"),
    ("ps", "[os.Process]"),
    ("ps.first.pid", "core.Number tagged os.Pid"),
    ("ps.first.parent", "os.Process"),
    ("ps.first.children", "[os.Process]"),
    ("ps.first.kill", "core.Unit"),
    ("ps.first.residentSize", "core.Number tagged core.Bytes"),
    ("ps.first.pid.info", "os.Process"),
    ("ps.first.pid.parent", "os.Process"),
    ("ps.first.pid.owner", "core.String tagged os.Username"),
    ("kill 1", "core.Unit"),
    ("clipboard", "core.String"),
    ("open 'x'", "core.Unit"),
    ("'x'.toPath.open", "core.Unit"),
    ("diskSpace", "[os.DiskSpaceEntry]"),
    ("df.first.available", "core.Number tagged core.Bytes"),
    ("terminal", "os.Terminal"),
    ("terminal.rows", "core.Number"),
    ("history", "[core.HistoryEntry]"),
    ("history.first.timestamp", "time.DateTime"),
    ("jump 'x'", "core.Unit"),
    ("ls.first.owner.fullName", "core.String"),
];

#[test]
fn inference() {
    let failures = with_big_stack(|| {
        let mut failures = vec![];
        for (input, expected) in INFERENCE {
            let mut ctx = Ctx::new(Rc::new(platform()));
            let compiled = ctx.compile_forgiving(SourceUnit::new("test", *input));
            let inferred = infer::infer(&ctx, &compiled.program.body);
            let actual = match inferred.types.get(&compiled.program.body.id) {
                Some(ty) => format!("{ty:?}"),
                None => "<no type>".into(),
            };
            if actual != *expected {
                failures.push(format!("{input:?}: got {actual}, expected {expected}"));
            }
        }
        failures
    });
    assert!(
        failures.is_empty(),
        "{} failing:\n  {}",
        failures.len(),
        failures.join("\n  ")
    );
}

/// Without users or processes (e.g. in the browser), the builtins fail cleanly
#[test]
fn unsupported() {
    #[derive(Debug)]
    struct Bare(TestPlatform);
    impl mash_core::platform::Platform for Bare {
        fn env_var(&self, name: &str) -> Option<String> {
            self.0.env_var(name)
        }
        fn env_vars(&self) -> Vec<(String, String)> {
            self.0.env_vars()
        }
        fn current_dir(&self) -> Option<String> {
            self.0.current_dir()
        }
        fn home_dir(&self) -> Option<String> {
            self.0.home_dir()
        }
        fn time_zone(&self) -> jiff::tz::TimeZone {
            self.0.time_zone()
        }
        fn now(&self) -> Timestamp {
            self.0.now()
        }
        fn read_dir(&self, path: &str) -> std::io::Result<Vec<mash_core::platform::DirEntry>> {
            self.0.read_dir(path)
        }
        fn sleep(&self, duration: std::time::Duration) {
            self.0.sleep(duration)
        }
        fn random_u64(&self) -> u64 {
            self.0.random_u64()
        }
        fn write_output(&self, text: &str) {
            self.0.write_output(text)
        }
    }
    with_big_stack(|| {
        let mut ctx = Ctx::new(Rc::new(Bare(platform())));
        let mut run = |code: &str| match ctx.run("test", code) {
            Ok(value) => render(&value),
            Err(e) => format!("error: {e}"),
        };
        assert_eq!(
            run("user"),
            "error: Could not find the current user: access to users is not supported here"
        );
        assert_eq!(
            run("ps"),
            "error: Could not list processes: access to processes is not supported here"
        );
        assert_eq!(
            run("clipboard"),
            "error: Could not use the clipboard: access to the clipboard is not supported here"
        );
        assert_eq!(
            run("df"),
            "error: Could not find disk space: access to disk space is not supported here"
        );
        assert_eq!(
            run("open 'x'"),
            "error: Could not open 'x': opening with the default application is not supported here"
        );
        assert_eq!(run("terminal"), "error: Not running in a terminal");
        assert_eq!(run("history"), "[]");
    });
}
