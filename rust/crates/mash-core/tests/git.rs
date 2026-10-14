//! The `git` namespace against a fake `git` command (canned output captured from git 2.54 for a scratch
//! repository): parsing results into objects, the commands run for changes, errors, completion and type
//! inference. (`mash-os/tests/git.rs` runs the real git.) Expected values were checked against Scala Mash for
//! the same repository; comments mark deliberate differences.

use std::rc::Rc;

use mash_core::ast::SourceUnit;
use mash_core::complete;
use mash_core::eval::{Ctx, RunError, render, with_big_stack};
use mash_core::infer;
use mash_core::platform::{FakeOutcome, FakeProcess, MemoryFiles, TestPlatform};

const H1: &str = "24de7ab09e55b75957048e988b4411467cfa5278";
const H2: &str = "3b434f696c848140d2137675bf2dc7b174f0e8ff";

/// Ahead of origin/master by one commit, with a staged new file, a modified file, a deleted file and two
/// untracked files
const STATUS: &str = "# branch.oid 3b434f696c848140d2137675bf2dc7b174f0e8ff\0# branch.head master\0\
    # branch.upstream origin/master\0# branch.ab +1 -0\0\
    1 .D N... 100644 100644 000000 abaddc0b9edd523c69166a2c9f3a9e31a4c873e3 abaddc0b9edd523c69166a2c9f3a9e31a4c873e3 del.txt\0\
    1 .M N... 100644 100644 100644 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 keep.txt\0\
    1 A. N... 000000 100644 100644 0000000000000000000000000000000000000000 19d9cc8584ac2c7dcf57d2680375e80f099dc481 new.txt\0\
    ? sub/y.txt\0? u.txt\0";

const COMMIT1: &str = "24de7ab09e55b75957048e988b4411467cfa5278\0\x001767323045\0Alice Example\0alice@example.com\0\
    Bob Committer\0bob@example.com\0First commit\0First commit\n\nWith a body\nover two lines\n\0";

const COMMIT2: &str = "3b434f696c848140d2137675bf2dc7b174f0e8ff\x0024de7ab09e55b75957048e988b4411467cfa5278\0\
    1767323045\0Alice Example\0alice@example.com\0Bob Committer\0bob@example.com\0Second commit\0Second commit\n\0";

const BRANCHES: &str = "refs/heads/feature\x003b434f696c848140d2137675bf2dc7b174f0e8ff\0\0\n\
    refs/heads/master\x003b434f696c848140d2137675bf2dc7b174f0e8ff\0refs/remotes/origin/master\0ahead 1\n";

const REMOTE_BRANCHES: &str = "refs/remotes/origin/HEAD\x0024de7ab09e55b75957048e988b4411467cfa5278\0\0\n\
    refs/remotes/origin/master\x0024de7ab09e55b75957048e988b4411467cfa5278\0\0\n";

/// v1 is lightweight, v2 annotated
const TAGS: &str = "refs/tags/v1\x0024de7ab09e55b75957048e988b4411467cfa5278\0\n\
    refs/tags/v2\x001ec17d84cb2015a34f7cd3ff425ed33011adc206\x003b434f696c848140d2137675bf2dc7b174f0e8ff\n";

const REFLOG: &str = "3b434f696c848140d2137675bf2dc7b174f0e8ff\0commit: Second commit\0\
    24de7ab09e55b75957048e988b4411467cfa5278\0clone: from /tmp/origin\0";
const STASH_LIST: &str = "stash@{0}\0On master: try c\x001792000000\0stash@{1}\0WIP on master: 3b434f6 Second\x00\
    1791990000\0";

/// What the fake git prints for read-only queries; any other command succeeds silently
fn fake_git(p: &FakeProcess<'_>) -> FakeOutcome {
    let args: Vec<&str> = p.args.iter().map(String::as_str).collect();
    let last = args.iter().rev().find(|a| **a != "--").copied().unwrap_or_default();
    match args.first().copied().unwrap_or_default() {
        "status" => FakeOutcome::output(STATUS),
        "log" if args.contains(&"-1") => match last {
            H1 => FakeOutcome::output(COMMIT1),
            _ => FakeOutcome::output(COMMIT2),
        },
        "log" if last == H1 => FakeOutcome::output(COMMIT1),
        "log" => FakeOutcome::output(format!("{COMMIT1}{COMMIT2}")),
        "for-each-ref" => FakeOutcome::output(match last {
            "refs/heads" => BRANCHES,
            "refs/remotes" => REMOTE_BRANCHES,
            "refs/tags" => TAGS,
            _ => "",
        }),
        // At the top of the working tree
        "rev-parse" if args.contains(&"--show-prefix") => FakeOutcome::output("\n"),
        "rev-parse" => {
            let name = last.trim_end_matches("^{commit}");
            match name {
                "HEAD" | "master" | "feature" | "v2" | H2 => FakeOutcome::output(format!("{H2}\n")),
                "origin/master" | "v1" | H1 => FakeOutcome::output(format!("{H1}\n")),
                _ => FakeOutcome::status(1),
            }
        }
        "symbolic-ref" => FakeOutcome::output("refs/heads/master\n"),
        "merge-base" => FakeOutcome::status(if args[2] == H1 || args[2] == args[3] { 0 } else { 1 }),
        "reflog" => FakeOutcome::output(REFLOG),
        "stash" if args.get(1) == Some(&"list") => FakeOutcome::output(STASH_LIST),
        "diff-tree" => FakeOutcome::output(if args.contains(&"--root") {
            "A\0a.txt\0A\0del.txt\0A\0keep.txt\0"
        } else {
            "M\0a.txt\0"
        }),
        "remote" => FakeOutcome::output("origin\n"),
        "config" if args.contains(&"--get-regexp") => FakeOutcome::output(
            "remote.origin.url\n/tmp/origin\0remote.origin.fetch\n+refs/heads/*:refs/remotes/origin/*\0",
        ),
        "push" => FakeOutcome::output("To /tmp/origin\n*\trefs/heads/feature:refs/heads/feature\t[new branch]\nDone\n"),
        _ => FakeOutcome::default(),
    }
}

/// A repository in the home directory (the working directory), with the fake git
fn platform() -> TestPlatform {
    TestPlatform {
        files: MemoryFiles::of(&["/home/alice/.git/", "/home/alice/notes.txt", "/elsewhere/"]),
        ..TestPlatform::default()
    }
    .with_command("git", fake_git)
}

/// Run code, returning the rendered result (or the error) and the platform afterwards
fn run(code: &str) -> (String, Rc<TestPlatform>) {
    let platform = Rc::new(platform());
    let mut ctx = Ctx::new(platform.clone());
    let result = match ctx.run("test", code) {
        Ok(value) => render(&value),
        Err(RunError::Eval(e)) => format!("error: {}", e.message),
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
                let actual = run(code).0;
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
fn status() {
    check(&[
        (
            "git.status",
            // (Scala lists untracked files in hash-set order; they're sorted here)
            "{ branch: master, upstreamBranch: origin/master, aheadCount: 1, behindCount: 0, added: [new.txt], \
             changed: [], missing: [del.txt], modified: [keep.txt], removed: [], untracked: [sub/y.txt, u.txt], \
             conflicting: [] }",
        ),
        ("git.status.getClass", "git.Status"),
        ("git.status.isClean", "false"),
        (
            "git.Status.new",
            "error: Cannot find member 'new' in value of type Class",
        ),
        ("git.status.branch.isA git.branch.LocalBranchName", "true"),
        ("git.status.added.first.isA os.Path", "true"),
        ("git.status.upstreamBranch.isA git.branch.RemoteBranchName", "true"),
        ("git.status.branch.commit", H2),
        ("git.status.branch.upstreamBranch", "origin/master"),
        (
            "git.status.upstreamBranch.info",
            "{ remote: origin, name: master, commit: 24de7ab09e55b75957048e988b4411467cfa5278 }",
        ),
        ("git.status.upstreamBranch.commit", H1),
        ("git.status.upstreamBranch.log | length", "1"),
        ("git.isRepo '.'", "true"),
        ("git.isRepo '/elsewhere'", "false"),
    ]);
}

#[test]
fn log_and_commits() {
    check(&[
        ("git.log | length", "2"),
        (
            "git.log.last",
            "{ hash: 3b434f696c848140d2137675bf2dc7b174f0e8ff, commitTime: 2026-01-02T03:04:05+00:00, author: { author: \
             Alice Example, email: alice@example.com }, committer: { author: Bob Committer, email: bob@example.com }, \
             summary: Second commit, message: Second commit\n, parents: [24de7ab09e55b75957048e988b4411467cfa5278] }",
        ),
        ("git.log.first.message", "First commit\n\nWith a body\nover two lines\n"),
        ("git.log.first.summary", "First commit"),
        ("git.log.first.author.toString", "Alice Example <alice@example.com>"),
        ("git.log.first.parent", "null"),
        ("git.log.last.parent", H1),
        ("git.log.last.hash.parent.isA git.CommitHash", "true"),
        ("git.log.last.hash.summary", "Second commit"),
        ("git.log.last.hash.author.email", "alice@example.com"),
        ("git.log.last.hash.info.summary", "Second commit"),
        (
            "git.log.last.diff",
            "[{ type: Modify, oldPath: a.txt, newPath: a.txt }]",
        ),
        // Scala fails on a root commit (it has no parent to compare with): here, everything was added
        ("git.log.first.diff | (.type)", "[Add, Add, Add]"),
        ("git.log.first.diff.first.oldPath", "null"),
        ("git.log.last.hash.diff | length", "1"),
        ("git.log.first.isAncestorOf 'master'", "true"),
        ("git.log.last.isMergedInto 'origin/master'", "false"),
        ("git.log 'v1' | length", "1"),
        ("git.log (git.branch.get 'master') | length", "2"),
        (
            "git.log 'nope'",
            "error: Invalid argument 'commit'. Must be the name of a commit",
        ),
        (
            "git.log 42",
            "error: Invalid argument 'commit'. Must be a name of a commit",
        ),
        (
            "git.reflog",
            "[{ commit: 24de7ab09e55b75957048e988b4411467cfa5278, comment: clone: from /tmp/origin }, { commit: \
             3b434f696c848140d2137675bf2dc7b174f0e8ff, comment: commit: Second commit }]",
        ),
        (
            "git.stash.list",
            "[{ name: stash@{0}, message: On master: try c, time: 2026-10-14T17:46:40+00:00 }, { name: stash@{1}, \
             message: WIP on master: 3b434f6 Second, time: 2026-10-14T15:00:00+00:00 }]",
        ),
        ("git.stash.list.first.time.getClass.name", "DateTime"),
        (
            "git.Identity.new 'Ann' 'ann@example.com' | .toString",
            "Ann <ann@example.com>",
        ),
    ]);
}

#[test]
fn branches_tags_and_remotes() {
    check(&[
        (
            "git.branches",
            "[{ name: feature, commit: 3b434f696c848140d2137675bf2dc7b174f0e8ff, upstreamBranch: null, aheadCount: 0, \
             behindCount: 0 }, { name: master, commit: 3b434f696c848140d2137675bf2dc7b174f0e8ff, upstreamBranch: \
             origin/master, aheadCount: 1, behindCount: 0 }]",
        ),
        ("git.branch.localBranches | length", "2"),
        ("git.branch.current.name", "master"),
        ("git.branch.get 'feature' | .upstreamBranch", "null"),
        (
            "git.branch.get 'nope'",
            "error: Invalid argument 'branch'. Must be a local branch",
        ),
        (
            "git.branch.remoteBranches",
            "[{ remote: origin, name: master, commit: 24de7ab09e55b75957048e988b4411467cfa5278 }]",
        ),
        ("git.branch.remoteBranches.first.fullName", "origin/master"),
        ("git.branch.remoteBranches.first.log | length", "1"),
        ("git.branch.remoteBranches.first.isAncestorOf 'master'", "true"),
        ("git.branches.first.isAncestorOf 'origin/master'", "false"),
        ("git.branches.first.log | length", "2"),
        ("git.branches.first.name.info.commit", H2),
        ("git.branches.first.name.upstreamBranch", "null"),
        // Annotated tags give the commit they point to (Scala gives the tag object's hash, here 1ec17d8…)
        (
            "git.tags",
            "[{ name: v1, commit: 24de7ab09e55b75957048e988b4411467cfa5278 }, { name: v2, commit: \
             3b434f696c848140d2137675bf2dc7b174f0e8ff }]",
        ),
        ("git.tag.list.last.name.isA git.tag.TagName", "true"),
        ("git.tag.create 'v2' | .commit", H2),
        (
            "git.remotes",
            "[{ name: origin, fetchUrls: [/tmp/origin], pushUrls: [] }]",
        ),
        ("git.remote.list.first.name.isA git.branch.RemoteName", "true"),
    ]);
}

#[test]
fn errors() {
    check(&[
        ("git.stage", "error: Must provide either 'paths' or 'all'"),
        (
            "git.stage --all 'a.txt'",
            "error: Invalid argument 'all'. Cannot provide both 'paths' and 'all'",
        ),
        ("git.unstage", "error: Must provide either 'paths' or 'all'"),
        ("git.restore", "error: Must provide either 'paths' or 'all'"),
        (
            "git.switch 'nope'",
            "error: Invalid argument 'branch'. Must be a local branch",
        ),
        (
            "git.branch.delete 'nope'",
            "error: Invalid argument 'branches'. Must be a local branch",
        ),
        ("git.tag.delete 'nope'", "error: Invalid argument 'tags'. Must be a tag"),
        (
            "git.merge 'nope'",
            "error: Invalid argument 'commit'. Must be the name of a commit",
        ),
        (
            "git.branch.create",
            "error: Must provide at least one of 'branch' and 'fromRemote'",
        ),
        (
            "git.branch.create --fromRemote='origin/nope'",
            "error: Invalid argument 'fromRemote'. origin/nope is not a valid remote branch",
        ),
        (
            "git.branch.create --fromRemote=42",
            "error: Invalid argument 'fromRemote'. Must be a remote branch, but was Number",
        ),
        (
            "git.branch.setCommit 'master' 'nope'",
            "error: Invalid argument 'commit'. Must be the name of a commit",
        ),
        // As `git branch -f`, not the checked-out branch
        (
            "git.branch.setCommit 'master' 'origin/master'",
            "error: Cannot set the commit of 'master': it is the current branch",
        ),
        (
            "git.branch.current.setCommit 'origin/master'",
            "error: Cannot set the commit of 'master': it is the current branch",
        ),
    ]);
}

#[test]
fn outside_a_repository() {
    with_big_stack(|| {
        let platform = Rc::new(TestPlatform::default().with_command("git", fake_git));
        let mut ctx = Ctx::new(platform.clone());
        for code in ["git.status", "git.log", "git.branches", "git.stash.create"] {
            let error = ctx.run("test", code).expect_err(code);
            assert!(
                error
                    .to_string()
                    .contains("Not a git repository (or any of the parent directories)"),
                "{code}: {error}"
            );
        }
        assert_eq!(render(&ctx.run("test", "git.isRepo '.'").expect("runs")), "false");
        assert!(
            platform.processes.borrow().is_empty(),
            "git isn't run outside a repository"
        );
    })
}

/// The git commands that change things, run by `code` (queries are left out)
fn changes(code: &str) -> Vec<String> {
    const QUERIES: &[&str] = &[
        "status",
        "log",
        "for-each-ref",
        "rev-parse",
        "symbolic-ref",
        "merge-base",
        "reflog",
        "diff-tree",
        "remote",
    ];
    let (result, platform) = run(code);
    assert!(!result.starts_with("error"), "{code}: {result}");
    platform
        .processes
        .borrow()
        .iter()
        .filter(|p| !QUERIES.contains(&p.argv[1].as_str()) && !p.argv.contains(&"--get-regexp".to_string()))
        .map(|p| p.argv[1..].join(" "))
        .collect()
}

#[test]
fn commands_run_for_changes() {
    with_big_stack(|| {
        let cases: &[(&str, &[&str])] = &[
            ("git.add 'a.txt' ['b', 'c']", &["add -- a.txt b c"]),
            ("git.stage 'notes.txt'", &["add -- notes.txt"]),
            (
                "git.stage --all",
                &["add -- :(top,literal)sub/y.txt :(top,literal)u.txt :(top,literal)keep.txt :(top,literal)del.txt"],
            ),
            ("git.unstage 'new.txt'", &["reset --quiet HEAD -- new.txt"]),
            ("git.unstage --all", &["reset --quiet HEAD -- :(top,literal)new.txt"]),
            // As `git restore`: from the index, unless --staged
            (
                "git.restore -a",
                &["restore --quiet --worktree -- :(top,literal)keep.txt :(top,literal)del.txt"],
            ),
            (
                "git.restore --staged 'keep.txt'",
                &["restore --quiet --staged --worktree -- keep.txt"],
            ),
            (
                "git.commit 'Fix it'",
                &["commit --quiet --allow-empty --allow-empty-message --cleanup=verbatim --message Fix it"],
            ),
            (
                "git.amend",
                &[
                    "commit --quiet --allow-empty --allow-empty-message --cleanup=verbatim --message Second commit\n --amend",
                ],
            ),
            (
                "git.commit --all --amend 'X'",
                &[
                    "add -- :(top,literal)sub/y.txt :(top,literal)u.txt :(top,literal)keep.txt :(top,literal)del.txt",
                    "commit --quiet --allow-empty --allow-empty-message --cleanup=verbatim --message X --amend",
                ],
            ),
            (
                "git.merge --squash 'feature'",
                &["merge --no-edit --squash 3b434f696c848140d2137675bf2dc7b174f0e8ff"],
            ),
            ("git.switch 'feature'", &["checkout --quiet feature --"]),
            (
                "git.branch.switch (git.branch.get 'feature')",
                &["checkout --quiet feature --"],
            ),
            ("git.branches.first.switch", &["checkout --quiet feature --"]),
            ("git.status.branch.switch", &["checkout --quiet master --"]),
            ("git.branch.create 'master'", &["branch --quiet master"]),
            (
                "git.branch.create -s 'feature'",
                &["branch --quiet feature", "checkout --quiet feature --"],
            ),
            (
                "git.branch.create --fromRemote='origin/master'",
                &["branch --quiet --track master origin/master"],
            ),
            (
                "git.branch.remoteBranches.first.createLocal",
                &["branch --quiet --track master origin/master"],
            ),
            (
                "git.branch.delete 'feature' (git.branch.get 'master')",
                &["branch --quiet -d -- feature master"],
            ),
            ("git.branches.first.delete", &["branch --quiet -d -- feature"]),
            ("git.branch.delete --force 'feature'", &["branch --quiet -D -- feature"]),
            ("git.branches.first.delete -f", &["branch --quiet -D -- feature"]),
            (
                "git.branch.setCommit 'feature' 'origin/master'",
                &["update-ref refs/heads/feature 24de7ab09e55b75957048e988b4411467cfa5278"],
            ),
            (
                "git.branches.first.setCommit git.log.first",
                &["update-ref refs/heads/feature 24de7ab09e55b75957048e988b4411467cfa5278"],
            ),
            (
                "git.branch.remoteBranches.first.delete",
                &["push --quiet origin :master"],
            ),
            ("git.tag.create 'v2'", &["tag --annotate --message= -- v2"]),
            ("git.tag.delete 'v1' git.tags.last", &["tag --delete -- v1 v2"]),
            ("git.tags.first.delete", &["tag --delete -- v1"]),
            ("git.stash.create", &["stash push --quiet"]),
            ("git.stash.apply", &["stash apply --quiet"]),
            ("git.stash.pop", &["stash pop --quiet"]),
            ("git.stash.drop", &["stash drop --quiet"]),
            ("git.init", &["init --quiet"]),
            (
                "git.clone 'https://example.com/x/repo.git'",
                &["clone --quiet -- https://example.com/x/repo.git repo"],
            ),
            ("git.fetch", &["fetch"]),
            ("git.pull", &["pull --no-edit"]),
            ("git.push", &["push --porcelain"]),
            (
                "git.push --setUpstream --force 'feature'",
                &[
                    "config --get branch.feature.pushRemote",
                    "config --get remote.pushDefault",
                    "config --get branch.feature.remote",
                    "push --porcelain --force origin feature",
                    "config branch.feature.remote origin",
                    "config branch.feature.merge refs/heads/feature",
                ],
            ),
            ("git.push --remote='upstream'", &["push --porcelain upstream"]),
            (
                "git.branches.first.push -u",
                &[
                    "config --get branch.feature.pushRemote",
                    "config --get remote.pushDefault",
                    "config --get branch.feature.remote",
                    "push --porcelain origin feature",
                    "config branch.feature.remote origin",
                    "config branch.feature.merge refs/heads/feature",
                ],
            ),
        ];
        let failures: Vec<String> = cases
            .iter()
            .filter_map(|(code, expected)| {
                let actual = changes(code);
                (actual != *expected).then(|| format!("{code:?}: ran {actual:?}, expected {expected:?}"))
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
fn results_of_changes() {
    check(&[
        ("git.clone 'https://example.com/x/repo.git'", "repo"),
        (
            "git.clone 'https://example.com/x/repo.git' 'dir' | .isA os.Path",
            "true",
        ),
        ("git.commit 'Fix it' | .getClass", "git.Commit"),
        ("git.fetch", "[]"),
        (
            "git.push 'feature'",
            "[{ remote: refs/heads/feature, status: OK, message: null, isFastForward: true, isForceUpdate: false }]",
        ),
        ("git.branch.create 'feature' | .name", "feature"),
    ]);
}

fn completions(input: &str) -> Vec<String> {
    let pos = input.find('▶').expect("cursor marker");
    let text = input.replacen('▶', "", 1);
    let mut ctx = Ctx::new(Rc::new(platform()));
    complete::complete(&mut ctx, &text, pos, false)
        .map(|r| r.completions.iter().map(|c| c.insert_text().to_string()).collect())
        .unwrap_or_default()
}

#[test]
fn completion() {
    with_big_stack(|| {
        let cases: &[(&str, &[&str])] = &[
            (r#"git.switch "▶""#, &["feature", "master"]),
            (r#"git.switch "f▶""#, &["feature"]),
            (r#"git.branch.get "▶""#, &["feature", "master"]),
            (r#"git.branch.delete "m▶""#, &["master"]),
            (r#"git.log "▶""#, &["feature", "master", "origin/master"]),
            (r#"git.merge "or▶""#, &["origin/master"]),
            (r#"git.branch.setCommit "f▶" "x""#, &["feature"]),
            (r#"git.branch.setCommit "feature" "orig▶""#, &["origin/master"]),
            (r#"git.branch.create --fromRemote="▶""#, &["origin/master"]),
            (r#"git.tag.delete "▶""#, &["v1", "v2"]),
            (r#"git.push --remote="▶""#, &["origin"]),
            (r#"git.push "fe▶""#, &["feature"]),
            (r#"git.stage "▶""#, &["del.txt", "keep.txt", "sub/y.txt", "u.txt"]),
            (r#"git.unstage "▶""#, &["new.txt"]),
            (r#"git.restore "▶""#, &["del.txt", "keep.txt"]),
            (r#"git.add "no▶""#, &["notes.txt"]),
            (r#"git.log.first.isAncestorOf "fe▶""#, &["feature"]),
            (r#"git.status.branch.isAncestorOf "fe▶""#, &["feature"]),
            (r#"git.status.branch.sw▶"#, &["switch"]),
            (r#"git.log.first.hash.inf▶"#, &["info"]),
            (r#"git.branches.first.upstreamBranch.createL▶"#, &["createLocal"]),
            (r#"git.status.mod▶"#, &["modified"]),
        ];
        let failures: Vec<String> = cases
            .iter()
            .filter_map(|(input, expected)| {
                let actual = completions(input);
                (actual != *expected).then(|| format!("{input:?}: got {actual:?}, expected {expected:?}"))
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
fn completion_outside_a_repository_falls_back_to_files() {
    with_big_stack(|| {
        let platform = TestPlatform {
            files: MemoryFiles::of(&["/home/alice/notes.txt"]),
            ..TestPlatform::default()
        };
        let mut ctx = Ctx::new(Rc::new(platform));
        let text = r#"git.stage "no""#;
        let result = complete::complete(&mut ctx, text, text.len() - 1, false).expect("completions");
        let names: Vec<&str> = result.completions.iter().map(|c| c.insert_text()).collect();
        assert_eq!(names, ["notes.txt"]);
    })
}

#[test]
fn type_inference() {
    with_big_stack(|| {
        let cases = [
            ("git.status", "git.Status"),
            ("git.status.branch", "core.String tagged git.branch.LocalBranchName"),
            ("git.log", "[git.Commit]"),
            ("git.log.first.hash.author", "git.Identity"),
            ("git.log.first.hash.info", "git.Commit"),
            ("git.log.first.diff", "[git.FileDiff]"),
            ("git.log.first.parents", "[core.String tagged git.CommitHash]"),
            ("git.status.branch.info", "git.branch.Branch"),
            ("git.status.branch.log", "[git.Commit]"),
            ("git.branches.first.upstreamBranch.info", "git.branch.RemoteBranch"),
            ("git.branch.remoteBranches.first.createLocal", "git.branch.Branch"),
            ("git.branch.create 'x'", "git.branch.Branch"),
            ("git.tags.first.name", "core.String tagged git.tag.TagName"),
            ("git.remotes.first.fetchUrls", "[core.String]"),
            ("git.fetch", "[git.FetchBranchUpdate]"),
            ("git.reflog", "[git.ReflogEntry]"),
            ("git.isRepo '.'", "core.Boolean"),
            ("git.clone 'x'", "core.String tagged os.Path"),
            ("git.status.isClean", "core.Boolean"),
        ];
        let mut failures = vec![];
        for (input, expected) in cases {
            let mut ctx = Ctx::new(Rc::new(platform()));
            let compiled = ctx.compile_forgiving(SourceUnit::new("test", input));
            let inferred = infer::infer(&ctx, &compiled.program.body);
            let actual = inferred
                .types
                .get(&compiled.program.body.id)
                .map_or("<no type>".to_string(), |t| format!("{t:?}"));
            if actual != expected {
                failures.push(format!("{input:?}: got {actual}, expected {expected}"));
            }
        }
        assert!(
            failures.is_empty(),
            "{} failing:\n  {}",
            failures.len(),
            failures.join("\n  ")
        );
    })
}
