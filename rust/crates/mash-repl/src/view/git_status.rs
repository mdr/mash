//! A `git.Status` shown like the output of `git status` (Scala `GitStatusPrinter`).

use mash_core::value::{Object, Value};

use crate::screen::{BasicColour, Style, StyledString};

const INDENT: &str = "        ";

fn string_field(status: &Object, name: &str) -> Option<String> {
    status.get_str(name).and_then(|v| v.as_str().map(str::to_string))
}

fn count_field(status: &Object, name: &str) -> i64 {
    status
        .get_str(name)
        .and_then(|v| v.as_number())
        .and_then(|n| n.as_i64())
        .unwrap_or(0)
}

/// The strings in a list field
fn paths(status: &Object, name: &str) -> Vec<String> {
    match status.get_str(name) {
        Some(Value::List(xs)) => xs
            .snapshot()
            .iter()
            .map(|x| x.as_str().map_or_else(|| mash_core::eval::render(x), str::to_string))
            .collect(),
        _ => vec![],
    }
}

fn commits(n: i64) -> &'static str {
    if n == 1 { "commit" } else { "commits" }
}

/// The lines describing a `git.Status` object
pub(crate) fn status_lines(status: &Object) -> Vec<StyledString> {
    let mut lines = vec![];
    let mut plain = |s: &str| lines.push(StyledString::plain(s));
    plain(&format!(
        "On branch {}",
        string_field(status, "branch").unwrap_or_default()
    ));
    let upstream = string_field(status, "upstreamBranch").unwrap_or_default();
    let (ahead, behind) = (count_field(status, "aheadCount"), count_field(status, "behindCount"));
    if ahead > 0 && behind > 0 {
        plain(&format!("Your branch and '{upstream}' have diverged,"));
        plain(&format!(
            "and have {ahead} and {behind} different commits each, respectively."
        ));
        plain(r#"  (use "git.pull" to merge the remote branch into yours)"#);
    } else if ahead > 0 {
        plain(&format!(
            "Your branch is ahead of '{upstream}' by {ahead} {}.",
            commits(ahead)
        ));
        plain(r#"  (use "git.push" to publish your local commits)"#);
    } else if behind > 0 {
        plain(&format!(
            "Your branch is behind '{upstream}' by {behind} {}, and can be fast-forwarded.",
            commits(behind)
        ));
        plain(r#"  (use "git.pull" to update your local branch)"#);
    }

    let [added, changed, removed, modified, missing, conflicting, untracked] = [
        "added",
        "changed",
        "removed",
        "modified",
        "missing",
        "conflicting",
        "untracked",
    ]
    .map(|f| paths(status, f));
    let to_be_committed = !(added.is_empty() && changed.is_empty() && removed.is_empty());
    let unstaged = !(modified.is_empty() && missing.is_empty());
    let green = Style::fg(BasicColour::Green);
    let red = Style::fg(BasicColour::Red);
    let entries = |lines: &mut Vec<StyledString>, label: &str, paths: &[String], style: Style| {
        for path in paths {
            lines.push(StyledString::styled(&format!("{INDENT}{label}{path}"), style));
        }
    };
    if to_be_committed {
        lines.push(StyledString::plain("Changes to be committed:"));
        lines.push(StyledString::plain(r#"(use "git.unstage <file>..." to unstage)"#));
        lines.push(StyledString::plain(""));
        entries(&mut lines, "new file:   ", &added, green);
        entries(&mut lines, "modified:   ", &changed, green);
        entries(&mut lines, "deleted:    ", &removed, green);
        lines.push(StyledString::plain(""));
    }
    if unstaged {
        lines.push(StyledString::plain("Changed not staged for commit:"));
        lines.push(StyledString::plain(
            r#"  (use "git.stage <file>..." to update what will be committed)"#,
        ));
        lines.push(StyledString::plain(
            r#"  (use "git.restore <file>..." to discard changes in working directory)"#,
        ));
        lines.push(StyledString::plain(""));
        entries(&mut lines, "modified:   ", &modified, red);
        entries(&mut lines, "deleted:    ", &missing, red);
        lines.push(StyledString::plain(""));
    }
    if !conflicting.is_empty() {
        lines.push(StyledString::plain("Unmerged paths:"));
        lines.push(StyledString::plain(
            r#"  (use "git.stage <file>..." to mark resolution)"#,
        ));
        lines.push(StyledString::plain(""));
        entries(&mut lines, "both modified:   ", &conflicting, red);
        lines.push(StyledString::plain(""));
    }
    if !untracked.is_empty() {
        lines.push(StyledString::plain("Untracked files:"));
        lines.push(StyledString::plain(
            r#"  (use "git.stage <file>..." to include in what will be committed)"#,
        ));
        lines.push(StyledString::plain(""));
        entries(&mut lines, "", &untracked, red);
        lines.push(StyledString::plain(""));
    }
    if !to_be_committed && !unstaged && untracked.is_empty() {
        lines.push(StyledString::plain("nothing to commit, working directory clean"));
    }
    lines
}
