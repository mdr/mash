//! Printing git results: `git.Status` like `git status`, and commits as a table of their main fields.
//! Expected output is what Scala Mash's `Viewer` printed for a scratch repository with the same contents
//! (`git` is faked with that repository's output), except where a comment says otherwise.

use std::rc::Rc;

use mash_core::eval::Ctx;
use mash_core::platform::{FakeOutcome, MemoryFiles, TestPlatform};
use mash_repl::screen::{BasicColour, Dimensions, Style};
use mash_repl::view::{ViewConfig, view_value};

/// `git status --porcelain=v2 --branch -z …` output: ahead of origin/master by one commit, with a staged new
/// file, a modified file, a deleted file and two untracked files
const STATUS: &str = "# branch.oid 3b434f696c848140d2137675bf2dc7b174f0e8ff\0# branch.head master\0\
    # branch.upstream origin/master\0# branch.ab +1 -0\0\
    1 .D N... 100644 100644 000000 abaddc0b9edd523c69166a2c9f3a9e31a4c873e3 abaddc0b9edd523c69166a2c9f3a9e31a4c873e3 del.txt\0\
    1 .M N... 100644 100644 100644 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 keep.txt\0\
    1 A. N... 000000 100644 100644 0000000000000000000000000000000000000000 19d9cc8584ac2c7dcf57d2680375e80f099dc481 new.txt\0\
    ? sub/y.txt\0? u.txt\0";

/// `git log -z --format=…` output: two commits
const LOG: &str = "24de7ab09e55b75957048e988b4411467cfa5278\0\x001767323045\0Alice Example\0alice@example.com\0\
    Bob Committer\0bob@example.com\0First commit\0First commit\n\nWith a body\nover two lines\n\0\
    3b434f696c848140d2137675bf2dc7b174f0e8ff\x0024de7ab09e55b75957048e988b4411467cfa5278\x001767323045\0\
    Alice Example\0alice@example.com\0Bob Committer\0bob@example.com\0Second commit\0Second commit\n\0";

/// A repository in the home directory, with git giving `status` as its status
fn platform(status: &'static str) -> TestPlatform {
    TestPlatform {
        files: MemoryFiles::of(&["/home/alice/.git/"]),
        ..TestPlatform::default()
    }
    .with_command("git", move |p| match p.args.first().map(String::as_str) {
        Some("status") => FakeOutcome::output(status),
        Some("log") => FakeOutcome::output(LOG),
        _ => FakeOutcome::status(1),
    })
}

fn view_lines(status: &'static str, columns: usize, source: &str) -> Vec<mash_repl::screen::StyledString> {
    let mut ctx = Ctx::new(Rc::new(platform(status)));
    let value = ctx.run("test", source).unwrap_or_else(|e| panic!("{source}: {e}"));
    let config = ViewConfig {
        fuzzy_time: false,
        ..ViewConfig::default()
    };
    view_value(&mut ctx, &value, Dimensions::new(1000, columns), &config).unwrap_or_else(|e| panic!("{source}: {e}"))
}

fn view(status: &'static str, columns: usize, source: &str) -> String {
    let lines = view_lines(status, columns, source);
    let text: Vec<String> = lines.iter().map(|l| l.forget_styling()).collect();
    text.join("\n")
}

#[test]
fn status_is_shown_like_git_status() {
    // Scala lists untracked files in hash-set order (`u.txt` first); they're sorted here
    assert_eq!(
        view(STATUS, 100, "git.status"),
        r#"On branch master
Your branch is ahead of 'origin/master' by 1 commit.
  (use "git.push" to publish your local commits)
Changes to be committed:
(use "git.unstage <file>..." to unstage)

        new file:   new.txt

Changed not staged for commit:
  (use "git.stage <file>..." to update what will be committed)
  (use "git.restore <file>..." to discard changes in working directory)

        modified:   keep.txt
        deleted:    del.txt

Untracked files:
  (use "git.stage <file>..." to include in what will be committed)

        sub/y.txt
        u.txt
"#
    );
}

#[test]
fn status_colours_staged_green_and_unstaged_red() {
    let lines = view_lines(STATUS, 100, "git.status");
    let style_of = |text: &str| {
        let line = lines
            .iter()
            .find(|l| l.forget_styling().trim() == text)
            .unwrap_or_else(|| panic!("no line {text}"));
        line.chars.last().map(|c| c.style).unwrap_or_default()
    };
    assert_eq!(style_of("new file:   new.txt"), Style::fg(BasicColour::Green));
    assert_eq!(style_of("modified:   keep.txt"), Style::fg(BasicColour::Red));
    assert_eq!(style_of("u.txt"), Style::fg(BasicColour::Red));
}

#[test]
fn clean_diverged_and_behind_statuses() {
    let clean = "# branch.oid abc\0# branch.head main\0# branch.upstream origin/main\0# branch.ab +2 -3\0";
    assert_eq!(
        view(clean, 100, "git.status"),
        r#"On branch main
Your branch and 'origin/main' have diverged,
and have 2 and 3 different commits each, respectively.
  (use "git.pull" to merge the remote branch into yours)
nothing to commit, working directory clean"#
    );
    // Scala says "by 1 commits" here: it pluralises by the ahead count. (As in Scala, conflicts alone count as
    // nothing to commit.)
    let behind = "# branch.oid abc\0# branch.head main\0# branch.upstream origin/main\0# branch.ab +0 -1\0\
        u UU N... 100644 100644 100644 100644 aaaa bbbb cccc both.txt\0";
    assert_eq!(
        view(behind, 100, "git.status"),
        r#"On branch main
Your branch is behind 'origin/main' by 1 commit, and can be fast-forwarded.
  (use "git.pull" to update your local branch)
Unmerged paths:
  (use "git.stage <file>..." to mark resolution)

        both modified:   both.txt

nothing to commit, working directory clean"#
    );
}

#[test]
fn status_in_a_tree_or_raw_view_is_an_object() {
    assert!(view(STATUS, 100, "view.raw git.status").contains("upstreamBranch"));
}

#[test]
fn commits_are_a_table_of_hash_time_author_and_summary() {
    // Times are ISO-style (a deliberate difference, as for all date-times): Scala shows `2 Jan 2026, 03:04:05`,
    // one character wider, so the hash column there is one narrower
    assert_eq!(
        view(STATUS, 100, "git.log"),
        r#"╔═╤════════════════════════════╤═══════════════════╤═════════════════════════════════╤═════════════╗
║#│hash                        │commitTime         │author                           │summary      ║
╟─┼────────────────────────────┼───────────────────┼─────────────────────────────────┼─────────────╢
║0│24de7ab09e55b75957048e988b4…│2026-01-02 03:04:05│Alice Example <alice@example.com>│First commit ║
║1│3b434f696c848140d2137675bf2…│2026-01-02 03:04:05│Alice Example <alice@example.com>│Second commit║
╚═╧════════════════════════════╧═══════════════════╧═════════════════════════════════╧═════════════╝"#
    );
    assert_eq!(
        view(STATUS, 60, "git.log"),
        r#"╔═╤═══╤═══════════════════╤════════════════════════╤═══════╗
║#│ha…│commitTime         │author                  │summary║
╟─┼───┼───────────────────┼────────────────────────┼───────╢
║0│24…│2026-01-02 03:04:05│Alice Example <alice@ex…│First …║
║1│3b…│2026-01-02 03:04:05│Alice Example <alice@ex…│Second…║
╚═╧═══╧═══════════════════╧════════════════════════╧═══════╝"#
    );
}

#[test]
fn an_identity_is_an_object_table() {
    assert_eq!(
        view(STATUS, 100, "git.log.first.author"),
        r#"╔════════════════════════╗
║        Identity        ║
╟──────┬─────────────────╢
║author│Alice Example    ║
║email │alice@example.com║
╚══════╧═════════════════╝"#
    );
}
