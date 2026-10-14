//! The `git` namespace against the real `git` command, in temporary repositories (with no global or system
//! git configuration, and a fixed identity). Results were compared with Scala Mash's (JGit) for the same
//! repositories.
//!
//! git runs in the process's working directory, which `cd` changes: so this is one test, in its own test
//! binary.

use std::rc::Rc;

use mash_core::eval::{Ctx, render, with_big_stack};
use mash_os::NativePlatform;

struct Session {
    ctx: Ctx,
}

impl Session {
    /// Evaluate `code`, rendering the result
    fn eval(&mut self, code: &str) -> String {
        match self.ctx.run("test", code) {
            Ok(value) => render(&value),
            Err(e) => panic!("{code}: {e}"),
        }
    }

    /// Evaluate `code`, expecting an error
    fn error(&mut self, code: &str) -> String {
        match self.ctx.run("test", code) {
            Ok(value) => panic!("{code}: expected an error, got {}", render(&value)),
            Err(e) => e.to_string(),
        }
    }

    fn check(&mut self, code: &str, expected: &str) {
        assert_eq!(self.eval(code), expected, "{code}");
    }
}

/// A Mash string literal
fn quoted(s: &str) -> String {
    format!("'{}'", s.replace('\'', "''"))
}

#[test]
fn git_in_temporary_repositories() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path().canonicalize().expect("canonical").display().to_string();
    with_big_stack(move || {
        let mut s = Session {
            ctx: Ctx::new(Rc::new(NativePlatform)),
        };
        // An isolated, deterministic git
        for (name, value) in [
            ("HOME", root.as_str()),
            ("GIT_CONFIG_GLOBAL", "/dev/null"),
            ("GIT_CONFIG_NOSYSTEM", "1"),
            ("GIT_CONFIG_COUNT", "1"),
            ("GIT_CONFIG_KEY_0", "init.defaultBranch"),
            ("GIT_CONFIG_VALUE_0", "main"),
            ("GIT_AUTHOR_NAME", "Alice Example"),
            ("GIT_AUTHOR_EMAIL", "alice@example.com"),
            ("GIT_COMMITTER_NAME", "Bob Committer"),
            ("GIT_COMMITTER_EMAIL", "bob@example.com"),
            ("GIT_AUTHOR_DATE", "2026-01-02T03:04:05Z"),
            ("GIT_COMMITTER_DATE", "2026-01-02T03:04:05Z"),
        ] {
            s.eval(&format!("env.{name} = {}", quoted(value)));
        }
        let origin = format!("{root}/origin");
        let work = format!("{root}/work");

        // Outside a repository
        s.eval(&format!("cd {}", quoted(&root)));
        s.check("git.isRepo '.'", "false");
        assert!(s.error("git.status").contains("Not a git repository"));

        // A repository with one commit and a tag
        s.eval(&format!("mkdir {0}; cd {0}", quoted(&origin)));
        s.eval("git.init");
        s.check("git.isRepo '.'", "true");
        // No history yet
        s.check("git.log", "[]");
        s.check("git.reflog", "[]");
        s.eval("write 'a.txt' 'one'; write 'keep.txt' 'keep'; write 'del.txt' 'del'");
        s.check("git.status.untracked", "[a.txt, del.txt, keep.txt]");
        s.eval("git.stage --all");
        s.check("git.status.added", "[a.txt, del.txt, keep.txt]");
        s.check("git.commit 'First commit' | .summary", "First commit");
        s.check("git.log | length", "1");
        s.check("git.log.first.diff | (.type)", "[Add, Add, Add]");
        s.check("git.tag.create 'v1' | .commit | .summary", "First commit");
        s.check("git.status.isClean", "true");

        // A clone of it, with history, branches and a dirty working tree
        s.eval(&format!("cd {}", quoted(&root)));
        s.check(&format!("git.clone {} 'work'", quoted(&origin)), "work");
        s.eval(&format!("cd {}", quoted(&work)));
        s.check("git.remotes | (.name)", "[origin]");
        s.check(&format!("git.remotes.first.fetchUrls == [{}]", quoted(&origin)), "true");
        s.check("git.branch.remoteBranches | (.name)", "[main]");
        s.eval("write --append 'a.txt' 'two'");
        s.check("git.status.modified", "[a.txt]");
        s.check("git.commit --all 'Second commit' | .parents | length", "1");
        s.check("git.log | (.summary)", "[First commit, Second commit]");
        let first = s.eval("git.log.first.hash");
        let second = s.eval("git.log.last.hash");
        s.check("git.log.last.hash.parent", &first);
        s.check(
            "git.log.last.diff",
            "[{ type: Modify, oldPath: a.txt, newPath: a.txt }]",
        );
        s.check("git.log.last.author.toString", "Alice Example <alice@example.com>");
        s.check("git.log.last.committer.author", "Bob Committer");
        s.check("git.log.last.commitTime", "2026-01-02T03:04:05+00:00");
        s.check("git.log.last.message", "Second commit\n");
        s.check("git.log 'v1' | length", "1");
        s.check(
            "git.status | select 'branch' 'upstreamBranch' 'aheadCount' 'behindCount'",
            "{ branch: main, upstreamBranch: origin/main, aheadCount: 1, behindCount: 0 }",
        );
        s.check(
            "git.branches",
            &format!(
                "[{{ name: main, commit: {second}, upstreamBranch: origin/main, aheadCount: 1, behindCount: 0 }}]"
            ),
        );
        s.check("git.status.upstreamBranch.isAncestorOf 'main'", "true");
        s.check("git.status.branch.isAncestorOf 'origin/main'", "false");
        s.check("git.tags | (.name)", "[v1]");
        s.check("git.reflog | (.comment) | last", "commit: Second commit");

        // Staging, unstaging and restoring
        s.eval(
            "write 'new.txt' 'new'; write --append 'keep.txt' 'more'; rm 'del.txt'; mkdir 'sub'; write 'sub/y.txt' 'y'",
        );
        s.check(
            "git.status | select 'added' 'modified' 'missing' 'untracked'",
            "{ added: [], modified: [keep.txt], missing: [del.txt], untracked: [new.txt, sub/y.txt] }",
        );
        s.eval("git.stage 'new.txt' 'del.txt'");
        s.check(
            "git.status | select 'added' 'removed' 'untracked'",
            "{ added: [new.txt], removed: [del.txt], untracked: [sub/y.txt] }",
        );
        s.eval("git.unstage --all");
        s.check(
            "git.status | select 'added' 'missing' 'untracked'",
            "{ added: [], missing: [del.txt], untracked: [new.txt, sub/y.txt] }",
        );
        // From a subdirectory, paths are relative to it (as `git status` shows them), so they can be staged
        s.check(
            "cd 'sub'; git.status | select 'modified' 'untracked'",
            "{ modified: [../keep.txt], untracked: [../new.txt, y.txt] }",
        );
        s.check("git.status.untracked | map (.exists)", "[true, true]");
        s.eval("git.stage git.status.untracked; cd '..'");
        s.check("git.status.added", "[new.txt, sub/y.txt]");
        s.eval("git.unstage --all");
        // From a subdirectory, --all still means the whole working tree
        s.eval("cd 'sub'; git.stage --all; cd '..'");
        s.check(
            "git.status | select 'added' 'changed' 'removed' 'modified' 'untracked'",
            "{ added: [new.txt, sub/y.txt], changed: [keep.txt], removed: [del.txt], modified: [], untracked: [] }",
        );
        // As `git restore`, restoring keeps staged changes, unless --staged
        s.eval("write --append 'keep.txt' 'unstaged'");
        s.check(
            "git.status | select 'changed' 'modified'",
            "{ changed: [keep.txt], modified: [keep.txt] }",
        );
        s.eval("git.restore 'keep.txt'");
        s.check(
            "git.status | select 'changed' 'modified'",
            "{ changed: [keep.txt], modified: [] }",
        );
        s.eval("git.restore --staged 'keep.txt'");
        s.check("git.status.changed + git.status.modified", "[]");

        // Stashing
        s.eval("git.stash.create");
        s.check("git.status.isClean", "true");
        s.check("git.stash.list | map (.name)", "[\"stash@{0}\"]");
        s.eval("git.stash.apply");
        s.check("git.status.isClean", "false");
        // Applying keeps the stash; popping drops it
        s.check("git.stash.list | length", "1");
        s.eval("git.stash.drop");
        s.check("git.stash.list", "[]");
        s.eval("git.stash.create");
        s.check("git.status.isClean", "true");
        s.eval("git.stash.pop");
        s.check("[git.status.isClean, git.stash.list]", "[false, []]");
        s.eval("git.restore --all; git.unstage --all; rm 'new.txt'; rm 'sub'");
        s.check("git.status.isClean", "true");

        // A file added with intent to add (`git add -N`) is a change not yet staged
        s.eval("write 'ita.txt' 'i'; run 'git' 'add' '-N' 'ita.txt'");
        s.check(
            "git.status | select 'added' 'modified'",
            "{ added: [], modified: [ita.txt] }",
        );
        s.check("git.status.isClean", "false");
        s.eval("git.stage --all");
        s.check("git.status.added", "[ita.txt]");
        s.eval("git.unstage --all; rm 'ita.txt'");
        s.check("git.status.isClean", "true");

        // Branches
        s.check("git.branch.create 'feature' | .name", "feature");
        s.eval("git.switch 'feature'");
        s.check("git.branch.current.name", "feature");
        s.eval("write 'f.txt' 'f'; git.stage 'f.txt'; git.commit 'Feature commit'");
        s.check("'main'.isA git.branch.LocalBranchName", "false");
        s.check("git.status.branch.log | length", "3");
        s.eval("git.status.branch.info.switch");
        s.eval("git.switch 'main'");
        // As `git branch -d`, an unmerged branch isn't deleted (without --force)
        assert!(s.error("git.branch.delete 'feature'").contains("git branch failed"));
        assert!(
            s.error("git.branch.get 'feature' | .delete")
                .contains("git branch failed")
        );
        s.check("git.branches | (.name)", "[feature, main]");
        s.check("git.branches.last.isAncestorOf 'feature'", "true");
        s.eval("git.merge 'feature'");
        s.check("git.log.last.summary", "Feature commit");
        // As `git branch -f`, not the checked-out branch
        assert!(
            s.error("git.branch.setCommit 'main' 'v1'")
                .contains("it is the current branch")
        );
        s.eval("git.branch.setCommit 'feature' 'v1'");
        s.check("git.branch.get 'feature' | .commit | .summary", "First commit");
        s.eval("git.branch.delete 'feature'");
        s.check("git.branches | (.name)", "[main]");

        // Amending
        s.check(
            "git.amend 'Feature commit, amended' | .summary",
            "Feature commit, amended",
        );
        s.check("git.amend | .message", "Feature commit, amended\n");

        // Pushing a new branch, fetching and pulling
        s.eval("git.branch.create --switch 'topic'");
        s.check(
            "git.push --setUpstream 'topic' | first | select 'remote' 'status'",
            "{ remote: refs/heads/topic, status: OK }",
        );
        s.check("git.branch.current.upstreamBranch", "origin/topic");
        s.check("git.fetch", "[]");
        s.check("git.pull", "[]");
        s.eval(&format!(
            "cd {}; write 'o.txt' 'o'; git.stage 'o.txt'; git.commit 'Origin commit'; cd {}",
            quoted(&origin),
            quoted(&work)
        ));
        s.check("git.fetch | (.branch) | (.toString)", "[origin/main]");
        s.check("git.fetch", "[]");
        // main and origin/main have diverged, so pushing main is rejected: an error
        assert_eq!(s.error("git.push 'main'"), "Push rejected: refs/heads/main");
        s.check("git.branch.remoteBranches | (.name)", "[main, topic]");
        s.check(
            "git.branch.remoteBranches.last.delete; git.branch.remoteBranches | (.name)",
            "[main]",
        );
        s.eval("git.tag.delete 'v1'");
        s.check("git.tags", "[]");

        // A branch from a remote other than origin is named without its remote, as `git checkout --track`
        s.eval(&format!(
            "cd {}; git.branch.create 'side'; cd {}",
            quoted(&origin),
            quoted(&work)
        ));
        s.eval(&format!(
            "run 'git' 'remote' 'add' 'upstream' {}; run 'git' 'fetch' '--quiet' 'upstream'",
            quoted(&origin)
        ));
        s.check("git.branch.create --fromRemote='upstream/side' | .name", "side");
        // A named branch is pushed to the remote it tracks, not origin
        s.eval("git.switch 'side'; write 'side.txt' 's'; git.stage 'side.txt'; git.commit 'Side commit'");
        s.check("git.push 'side' | first | .status", "OK");
        s.check("git.log 'upstream/side' | last | .summary", "Side commit");
        assert!(
            s.error("git.log 'origin/side'")
                .contains("Must be the name of a commit")
        );
        // An upstream that's a local branch is named the same way by status and by branches
        s.eval("git.switch 'side'; run 'git' 'branch' '--quiet' '-u' 'main'");
        s.check("git.status.upstreamBranch", "main");
        s.check("git.branch.current.upstreamBranch", "main");
        assert!(s.error("git.switch 'nope'").contains("Must be a local branch"));
    });
}
