//! Parsing the machine-readable output of `git` commands into plain Rust data. Pure, so it's unit-tested on
//! canned output.

use std::collections::BTreeMap;

/// The `--format` for `git log` (with `-z`) that [`commits`] parses: one NUL-separated field per item, each
/// commit ending with a NUL.
pub(crate) const LOG_FORMAT: &str = "--format=%H%x00%P%x00%ct%x00%an%x00%ae%x00%cn%x00%ce%x00%s%x00%B";

/// The `--format` for `git for-each-ref` that [`branches`] parses
pub(crate) const BRANCH_FORMAT: &str =
    "--format=%(refname)%00%(objectname)%00%(upstream)%00%(upstream:track,nobracket)";

/// The `--format` for `git for-each-ref` that [`tags`] parses (`*objectname` is the commit an annotated tag
/// points to)
pub(crate) const TAG_FORMAT: &str = "--format=%(refname)%00%(objectname)%00%(*objectname)";

/// The `--format` for `git for-each-ref` that [`refs`] parses
pub(crate) const REF_FORMAT: &str = "--format=%(refname)%00%(objectname)";

/// The `--format` for `git reflog` (with `-z`) that [`reflog`] parses
pub(crate) const REFLOG_FORMAT: &str = "--format=%H%x00%gs";

/// The `--format` for `git stash list` (with `-z`) that [`stash_list`] parses
pub(crate) const STASH_LIST_FORMAT: &str = "--format=%gd%x00%gs%x00%ct";

/// A person and their email address.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Identity {
    pub name: String,
    pub email: String,
}

/// A commit, from `git log`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Commit {
    pub hash: String,
    pub parents: Vec<String>,
    /// Seconds since the epoch
    pub commit_time: i64,
    pub author: Identity,
    pub committer: Identity,
    /// The first paragraph of the message, on one line
    pub summary: String,
    /// The whole message, as stored
    pub message: String,
}

/// How a branch compares with the branch it tracks.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Tracking {
    /// e.g. `origin/master`
    pub upstream: String,
    pub ahead: u64,
    pub behind: u64,
}

/// A branch (local or remote), from `git for-each-ref`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Branch {
    /// The full ref name, e.g. `refs/heads/master`
    pub ref_name: String,
    pub commit: String,
    /// None if the branch tracks nothing, or its upstream no longer exists
    pub tracking: Option<Tracking>,
}

/// The working tree status, from `git status --porcelain=v2`. Paths are relative to the top of the working tree.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Status {
    /// The current branch, or the commit hash if HEAD is detached
    pub branch: String,
    pub tracking: Option<Tracking>,
    /// New files that have been staged
    pub added: Vec<String>,
    /// Changed files that have been staged
    pub changed: Vec<String>,
    /// Files that have been deleted, but not staged
    pub missing: Vec<String>,
    /// Modified files that have not been staged
    pub modified: Vec<String>,
    /// Files that have been deleted and staged
    pub removed: Vec<String>,
    pub untracked: Vec<String>,
    pub conflicting: Vec<String>,
}

/// A change to a file between two commits, from `git diff-tree --name-status`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FileDiff {
    /// `Add`, `Modify` or `Delete` (as JGit's change types)
    pub kind: &'static str,
    pub old_path: Option<String>,
    pub new_path: Option<String>,
}

/// An entry in the reflog.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ReflogEntry {
    pub commit: String,
    pub comment: String,
}

/// A stash, from `git stash list`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StashEntry {
    /// As git names it: `stash@{0}` is the newest
    pub name: String,
    pub message: String,
    /// Seconds since the epoch
    pub time: i64,
}

/// The result of pushing one ref, from `git push --porcelain` (named as JGit's `RemoteRefUpdate`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PushUpdate {
    /// The remote ref, e.g. `refs/heads/master`
    pub remote_ref: String,
    /// `OK`, `UP_TO_DATE`, `REJECTED_NONFASTFORWARD`, `REJECTED_REMOTE_CHANGED` or `REJECTED_OTHER_REASON`
    pub status: &'static str,
    pub message: Option<String>,
    pub fast_forward: bool,
    pub forced: bool,
}

/// A change to a remote-tracking branch made by a fetch: None for a branch that didn't exist before (`old`) or
/// was pruned (`new`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RefUpdate {
    /// e.g. `origin/master`
    pub branch: String,
    pub old: Option<String>,
    pub new: Option<String>,
}

/// NUL-terminated items (a final unterminated item is kept too)
fn nul_items(output: &str) -> Vec<&str> {
    let mut items: Vec<&str> = output.split('\0').collect();
    if items.last() == Some(&"") {
        items.pop();
    }
    items
}

/// Commits from `git log -z` with [`LOG_FORMAT`]
pub(crate) fn commits(output: &str) -> Vec<Commit> {
    nul_items(output)
        .as_chunks::<9>()
        .0
        .iter()
        .map(|fields| Commit {
            hash: fields[0].to_string(),
            parents: fields[1].split_whitespace().map(str::to_string).collect(),
            commit_time: fields[2].trim().parse().unwrap_or_default(),
            author: Identity {
                name: fields[3].to_string(),
                email: fields[4].to_string(),
            },
            committer: Identity {
                name: fields[5].to_string(),
                email: fields[6].to_string(),
            },
            summary: fields[7].to_string(),
            message: fields[8].to_string(),
        })
        .collect()
}

/// `refs/remotes/origin/master` → `origin/master` (Scala `GitCommon.trimRemoteBranchPrefix`)
pub(crate) fn trim_remote_prefix(ref_name: &str) -> &str {
    ref_name.strip_prefix("refs/remotes/").unwrap_or(ref_name)
}

/// An upstream branch as `git status` names it: `origin/master`, or a local branch's name (`refs/heads/b` →
/// `b`)
fn upstream_name(ref_name: &str) -> &str {
    let name = trim_remote_prefix(ref_name);
    name.strip_prefix("refs/heads/").unwrap_or(name)
}

/// Ahead/behind counts from `%(upstream:track,nobracket)` (`ahead 1, behind 2`); None if the upstream is gone.
/// (Needs the C locale: the words are translated.)
fn track_counts(track: &str) -> Option<(u64, u64)> {
    if track.trim() == "gone" {
        return None;
    }
    let (mut ahead, mut behind) = (0, 0);
    for part in track.split(',') {
        match part.split_whitespace().collect::<Vec<_>>().as_slice() {
            ["ahead", n] => ahead = n.parse().unwrap_or_default(),
            ["behind", n] => behind = n.parse().unwrap_or_default(),
            _ => {}
        }
    }
    Some((ahead, behind))
}

/// Branches from `git for-each-ref` with [`BRANCH_FORMAT`] (a line per ref)
pub(crate) fn branches(output: &str) -> Vec<Branch> {
    output
        .lines()
        .filter_map(|line| {
            let mut fields = line.split('\0');
            let ref_name = fields.next().filter(|s| !s.is_empty())?.to_string();
            let commit = fields.next().unwrap_or_default().to_string();
            let upstream = fields.next().unwrap_or_default();
            let track = fields.next().unwrap_or_default();
            let tracking = if upstream.is_empty() {
                None
            } else {
                track_counts(track).map(|(ahead, behind)| Tracking {
                    upstream: upstream_name(upstream).to_string(),
                    ahead,
                    behind,
                })
            };
            Some(Branch {
                ref_name,
                commit,
                tracking,
            })
        })
        .collect()
}

/// `(name, commit)` of tags from `git for-each-ref refs/tags` with [`TAG_FORMAT`]: annotated tags give the
/// commit they point to
pub(crate) fn tags(output: &str) -> Vec<(String, String)> {
    output
        .lines()
        .filter_map(|line| {
            let mut fields = line.split('\0');
            let ref_name = fields.next().filter(|s| !s.is_empty())?;
            let object = fields.next().unwrap_or_default();
            let peeled = fields.next().unwrap_or_default();
            let name = ref_name.strip_prefix("refs/tags/").unwrap_or(ref_name).to_string();
            let commit = if peeled.is_empty() { object } else { peeled };
            Some((name, commit.to_string()))
        })
        .collect()
}

/// `ref name → object` from `git for-each-ref` with [`REF_FORMAT`]
pub(crate) fn refs(output: &str) -> BTreeMap<String, String> {
    output
        .lines()
        .filter_map(|line| {
            let (name, object) = line.split_once('\0')?;
            Some((name.to_string(), object.to_string()))
        })
        .collect()
}

/// The remote-tracking branches that changed between two snapshots of `refs/remotes` (from [`refs`]), as
/// JGit's tracking ref updates (symbolic `…/HEAD` refs are left out)
pub(crate) fn ref_updates(before: &BTreeMap<String, String>, after: &BTreeMap<String, String>) -> Vec<RefUpdate> {
    let names: std::collections::BTreeSet<&String> = before.keys().chain(after.keys()).collect();
    names
        .into_iter()
        .filter(|name| !name.ends_with("/HEAD"))
        .filter_map(|name| {
            let old = before.get(name);
            let new = after.get(name);
            (old != new).then(|| RefUpdate {
                branch: trim_remote_prefix(name).to_string(),
                old: old.cloned(),
                new: new.cloned(),
            })
        })
        .collect()
}

/// The working tree status from `git status --porcelain=v2 --branch -z --no-renames`. Paths are sorted.
pub(crate) fn status(output: &str) -> Status {
    let mut status = Status::default();
    let mut oid = String::new();
    let mut head = String::new();
    let mut upstream = None;
    let mut ahead_behind = None;
    for item in nul_items(output) {
        if let Some(header) = item.strip_prefix("# ") {
            match header.split_once(' ') {
                Some(("branch.oid", value)) => oid = value.to_string(),
                Some(("branch.head", value)) => head = value.to_string(),
                Some(("branch.upstream", value)) => upstream = Some(value.to_string()),
                Some(("branch.ab", value)) => {
                    let counts: Vec<u64> = value
                        .split_whitespace()
                        .map(|n| n.trim_start_matches(['+', '-']).parse().unwrap_or_default())
                        .collect();
                    if let [ahead, behind] = counts[..] {
                        ahead_behind = Some((ahead, behind));
                    }
                }
                _ => {}
            }
        } else if let Some(path) = item.strip_prefix("? ") {
            status.untracked.push(path.to_string());
        } else if let Some(entry) = item.strip_prefix("1 ") {
            // XY sub mH mI mW hH hI path
            let fields: Vec<&str> = entry.splitn(8, ' ').collect();
            if let [xy, .., path] = fields[..]
                && fields.len() == 8
            {
                let mut codes = xy.chars();
                let (x, y) = (codes.next().unwrap_or('.'), codes.next().unwrap_or('.'));
                let path = path.to_string();
                match x {
                    'A' => status.added.push(path.clone()),
                    'M' | 'T' => status.changed.push(path.clone()),
                    'D' => status.removed.push(path.clone()),
                    _ => {}
                }
                match y {
                    // `A`: added with intent to add (`git add -N`), so not yet staged
                    'M' | 'T' | 'A' => status.modified.push(path),
                    'D' => status.missing.push(path),
                    _ => {}
                }
            }
        } else if let Some(entry) = item.strip_prefix("u ") {
            // XY sub m1 m2 m3 mW h1 h2 h3 path
            if let Some(path) = entry.splitn(10, ' ').nth(9) {
                status.conflicting.push(path.to_string());
            }
        }
    }
    status.branch = if head == "(detached)" { oid } else { head };
    status.tracking = upstream.zip(ahead_behind).map(|(upstream, (ahead, behind))| Tracking {
        upstream,
        ahead,
        behind,
    });
    for paths in [
        &mut status.added,
        &mut status.changed,
        &mut status.missing,
        &mut status.modified,
        &mut status.removed,
        &mut status.untracked,
        &mut status.conflicting,
    ] {
        paths.sort();
    }
    status
}

/// Changes from `git diff-tree -r -z --no-renames --name-status`
pub(crate) fn file_diffs(output: &str) -> Vec<FileDiff> {
    nul_items(output)
        .as_chunks::<2>()
        .0
        .iter()
        .map(|pair| {
            let path = Some(pair[1].to_string());
            match pair[0].chars().next() {
                Some('A') => FileDiff {
                    kind: "Add",
                    old_path: None,
                    new_path: path,
                },
                Some('D') => FileDiff {
                    kind: "Delete",
                    old_path: path,
                    new_path: None,
                },
                _ => FileDiff {
                    kind: "Modify",
                    old_path: path.clone(),
                    new_path: path,
                },
            }
        })
        .collect()
}

/// Reflog entries from `git reflog -z` with [`REFLOG_FORMAT`], newest first
pub(crate) fn reflog(output: &str) -> Vec<ReflogEntry> {
    nul_items(output)
        .as_chunks::<2>()
        .0
        .iter()
        .map(|pair| ReflogEntry {
            commit: pair[0].to_string(),
            comment: pair[1].to_string(),
        })
        .collect()
}

/// Stashes from `git stash list -z` with [`STASH_LIST_FORMAT`], newest first
pub(crate) fn stash_list(output: &str) -> Vec<StashEntry> {
    nul_items(output)
        .as_chunks::<3>()
        .0
        .iter()
        .map(|item| StashEntry {
            name: item[0].to_string(),
            message: item[1].to_string(),
            time: item[2].parse().unwrap_or(0),
        })
        .collect()
}

/// `(name, URLs, push URLs)` of the remotes listed by `git remote`, with URLs from `git config -z
/// --get-regexp` (`key\nvalue` items)
pub(crate) fn remotes(names: &str, config: &str) -> Vec<(String, Vec<String>, Vec<String>)> {
    let mut remotes: Vec<(String, Vec<String>, Vec<String>)> = names
        .lines()
        .filter(|name| !name.is_empty())
        .map(|name| (name.to_string(), vec![], vec![]))
        .collect();
    for item in nul_items(config) {
        let (key, value) = item.split_once('\n').unwrap_or((item, ""));
        let Some((name, variable)) = key.strip_prefix("remote.").and_then(|k| k.rsplit_once('.')) else {
            continue;
        };
        if let Some(remote) = remotes.iter_mut().find(|(n, _, _)| n == name) {
            match variable {
                "url" => remote.1.push(value.to_string()),
                "pushurl" => remote.2.push(value.to_string()),
                _ => {}
            }
        }
    }
    remotes
}

/// Ref updates from `git push --porcelain`: lines `<flag>\t<from>:<to>\t<summary>`, between `To <url>` and
/// `Done`
pub(crate) fn push_updates(output: &str) -> Vec<PushUpdate> {
    output
        .lines()
        .filter_map(|line| {
            let mut fields = line.split('\t');
            let flag = fields.next()?;
            let refs = fields.next()?;
            let summary = fields.next().unwrap_or_default();
            let (_, remote_ref) = refs.split_once(':')?;
            let reason = summary
                .find('(')
                .map(|i| summary[i + 1..].trim_end_matches(')').to_string());
            let (status, fast_forward, forced) = match flag {
                " " | "*" => ("OK", true, false),
                "+" => ("OK", false, true),
                "-" => ("OK", false, false),
                "=" => ("UP_TO_DATE", false, false),
                "!" => match reason.as_deref() {
                    Some("non-fast-forward" | "fetch first") => ("REJECTED_NONFASTFORWARD", false, false),
                    Some(r) if r.starts_with("stale info") => ("REJECTED_REMOTE_CHANGED", false, false),
                    _ => ("REJECTED_OTHER_REASON", false, false),
                },
                _ => return None,
            };
            Some(PushUpdate {
                remote_ref: remote_ref.to_string(),
                status,
                message: reason,
                fast_forward,
                forced,
            })
        })
        .collect()
}

/// The directory `git clone` makes for a repository URL (git's "humanish" part): the last path segment,
/// without a trailing `/`, `/.git` or `.git` (Scala used JGit's `URIish.getHumanishName`)
pub(crate) fn humanish_name(url: &str) -> String {
    let trimmed = url.trim_end_matches('/');
    let trimmed = trimmed.strip_suffix("/.git").unwrap_or(trimmed);
    let trimmed = trimmed.strip_suffix(".git").unwrap_or(trimmed);
    let name = trimmed.rsplit(['/', ':']).next().unwrap_or(trimmed);
    name.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Output captured from git 2.54 for a scratch repository
    const STATUS: &str = "# branch.oid 3b434f696c848140d2137675bf2dc7b174f0e8ff\0# branch.head master\0\
        # branch.upstream origin/master\0# branch.ab +1 -0\0\
        1 .D N... 100644 100644 000000 abaddc0b9edd523c69166a2c9f3a9e31a4c873e3 abaddc0b9edd523c69166a2c9f3a9e31a4c873e3 del.txt\0\
        1 .M N... 100644 100644 100644 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 keep.txt\0\
        1 A. N... 000000 100644 100644 0000000000000000000000000000000000000000 19d9cc8584ac2c7dcf57d2680375e80f099dc481 new file.txt\0\
        1 MM N... 100644 100644 100644 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 both.txt\0\
        1 D. N... 100644 000000 000000 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 0000000000000000000000000000000000000000 gone.txt\0\
        1 .A N... 000000 000000 100644 0000000000000000000000000000000000000000 0000000000000000000000000000000000000000 intent.txt\0\
        u UU N... 100644 100644 100644 100644 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 2fa992c0b8b5c6acd2bdd4fa31de29d29799bdd5 conflict.txt\0\
        ? sub/y.txt\0? u.txt\0";

    #[test]
    fn parses_status() {
        let status = super::status(STATUS);
        assert_eq!(status.branch, "master");
        assert_eq!(
            status.tracking,
            Some(Tracking {
                upstream: "origin/master".into(),
                ahead: 1,
                behind: 0
            })
        );
        assert_eq!(status.added, ["new file.txt"]);
        assert_eq!(status.changed, ["both.txt"]);
        assert_eq!(status.removed, ["gone.txt"]);
        assert_eq!(status.missing, ["del.txt"]);
        assert_eq!(status.modified, ["both.txt", "intent.txt", "keep.txt"]);
        assert_eq!(status.untracked, ["sub/y.txt", "u.txt"]);
        assert_eq!(status.conflicting, ["conflict.txt"]);
    }

    #[test]
    fn status_of_detached_head_and_gone_upstream() {
        let detached = status("# branch.oid abc123\0# branch.head (detached)\0");
        assert_eq!(detached.branch, "abc123");
        assert_eq!(detached.tracking, None);
        // An upstream with no ahead/behind counts no longer exists
        let gone = status("# branch.oid abc\0# branch.head main\0# branch.upstream origin/main\0");
        assert_eq!(gone.tracking, None);
    }

    #[test]
    fn parses_log() {
        let output = "24de\0\x001767323045\0Alice Example\0alice@example.com\0Bob\0bob@example.com\0First commit\0\
            First commit\n\nWith a body\n\x003b43\x0024de\x001767323045\0Alice Example\0alice@example.com\0Bob\0\
            bob@example.com\0Second commit\0Second commit\n\0";
        let commits = commits(output);
        assert_eq!(commits.len(), 2);
        assert_eq!(commits[0].hash, "24de");
        assert!(commits[0].parents.is_empty());
        assert_eq!(commits[0].message, "First commit\n\nWith a body\n");
        assert_eq!(commits[1].parents, ["24de"]);
        assert_eq!(commits[1].commit_time, 1767323045);
        assert_eq!(commits[1].author.name, "Alice Example");
        assert_eq!(commits[1].committer.email, "bob@example.com");
        assert_eq!(commits[1].summary, "Second commit");
    }

    #[test]
    fn parses_branches() {
        let output = "refs/heads/feature\x003b43\0\0\nrefs/heads/master\x003b43\0refs/remotes/origin/master\0ahead 1\n\
            refs/heads/old\x00aaaa\0refs/remotes/origin/old\0gone\n\
            refs/heads/x\x00bbbb\0refs/remotes/origin/x\0ahead 2, behind 3\n";
        let branches = branches(output);
        assert_eq!(branches.len(), 4);
        assert_eq!(branches[0].tracking, None);
        assert_eq!(
            branches[1].tracking,
            Some(Tracking {
                upstream: "origin/master".into(),
                ahead: 1,
                behind: 0
            })
        );
        assert_eq!(branches[2].tracking, None);
        assert_eq!(branches[3].tracking.as_ref().map(|t| (t.ahead, t.behind)), Some((2, 3)));
    }

    #[test]
    fn parses_tags() {
        let output = "refs/tags/v1\x00aaaa\0\nrefs/tags/v2\x00tttt\x00cccc\n";
        assert_eq!(
            tags(output),
            [
                ("v1".to_string(), "aaaa".to_string()),
                ("v2".to_string(), "cccc".to_string())
            ]
        );
    }

    #[test]
    fn finds_ref_updates() {
        let before =
            refs("refs/remotes/origin/HEAD\x00aaaa\nrefs/remotes/origin/a\x00aaaa\nrefs/remotes/origin/b\x00bbbb\n");
        let after =
            refs("refs/remotes/origin/HEAD\x00cccc\nrefs/remotes/origin/a\x00cccc\nrefs/remotes/origin/c\x00dddd\n");
        assert_eq!(
            ref_updates(&before, &after),
            [
                RefUpdate {
                    branch: "origin/a".into(),
                    old: Some("aaaa".into()),
                    new: Some("cccc".into())
                },
                RefUpdate {
                    branch: "origin/b".into(),
                    old: Some("bbbb".into()),
                    new: None
                },
                RefUpdate {
                    branch: "origin/c".into(),
                    old: None,
                    new: Some("dddd".into())
                },
            ]
        );
    }

    #[test]
    fn parses_diffs() {
        assert_eq!(
            file_diffs("A\0new.txt\0M\0a.txt\0D\0old.txt\0T\0link\0"),
            [
                FileDiff {
                    kind: "Add",
                    old_path: None,
                    new_path: Some("new.txt".into())
                },
                FileDiff {
                    kind: "Modify",
                    old_path: Some("a.txt".into()),
                    new_path: Some("a.txt".into())
                },
                FileDiff {
                    kind: "Delete",
                    old_path: Some("old.txt".into()),
                    new_path: None
                },
                FileDiff {
                    kind: "Modify",
                    old_path: Some("link".into()),
                    new_path: Some("link".into())
                },
            ]
        );
    }

    #[test]
    fn parses_reflog() {
        let entries = reflog("3b43\0commit: Second commit\x0024de\0clone: from /tmp/x\0");
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[1].comment, "clone: from /tmp/x");
    }

    #[test]
    fn parses_remotes() {
        let remotes = remotes(
            "origin\nup.stream\n",
            "remote.origin.url\nhttps://example.com/a.git\0remote.origin.fetch\n+refs/heads/*\0\
             remote.up.stream.url\n/tmp/b\0remote.up.stream.pushurl\n/tmp/c\0",
        );
        assert_eq!(
            remotes,
            [
                (
                    "origin".to_string(),
                    vec!["https://example.com/a.git".to_string()],
                    vec![]
                ),
                (
                    "up.stream".to_string(),
                    vec!["/tmp/b".to_string()],
                    vec!["/tmp/c".to_string()]
                ),
            ]
        );
    }

    #[test]
    fn parses_push_output() {
        let output = "To /tmp/origin\n \trefs/heads/master:refs/heads/master\t3b43..4a47\n\
            *\trefs/heads/new:refs/heads/new\t[new branch]\n\
            !\trefs/heads/x:refs/heads/x\t[rejected] (non-fast-forward)\n\
            =\trefs/heads/y:refs/heads/y\t[up to date]\n\
            -\t:refs/heads/z\t[deleted]\nDone\n";
        let updates = push_updates(output);
        let summary: Vec<(&str, &str, bool, bool)> = updates
            .iter()
            .map(|u| (u.remote_ref.as_str(), u.status, u.fast_forward, u.forced))
            .collect();
        assert_eq!(
            summary,
            [
                ("refs/heads/master", "OK", true, false),
                ("refs/heads/new", "OK", true, false),
                ("refs/heads/x", "REJECTED_NONFASTFORWARD", false, false),
                ("refs/heads/y", "UP_TO_DATE", false, false),
                ("refs/heads/z", "OK", false, false),
            ]
        );
        assert_eq!(updates[2].message.as_deref(), Some("non-fast-forward"));
    }

    #[test]
    fn humanish_names() {
        assert_eq!(humanish_name("https://github.com/github/testrepo.git"), "testrepo");
        assert_eq!(humanish_name("/srv/repos/project/.git"), "project");
        assert_eq!(humanish_name("git@host:team/tool.git/"), "tool");
        assert_eq!(humanish_name("host:repo"), "repo");
    }
}
