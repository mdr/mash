//! Running the `git` command (Scala's `GitHelper`, which used JGit instead), and the queries several builtins
//! share: commits, branches, tags and remotes.
//!
//! git runs in the current directory, with the environment of the `env` global (as other commands do). Its
//! standard error always reaches the terminal, so git's own messages (errors, progress, credential prompts)
//! are seen as when running git directly.

use std::collections::BTreeMap;

use super::parse::{self, Branch, Commit};
use crate::builtins::os::process::child_environment;
use crate::eval::{Ctx, MashError, R};
use crate::platform::{ProcessInput, ProcessOutcome, ProcessOutput, ProcessRequest, resolve};
use crate::value::Value;

const NOT_A_REPOSITORY: &str = "Not a git repository (or any of the parent directories)";

/// How a git command is run.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Mode {
    /// A read-only query whose output is parsed: no standard input, the C locale (some formats are
    /// translated), and no optional locks (so a status doesn't contend with the user's own git)
    Query,
    /// A change, with output captured (to parse, or because Scala printed nothing); standard input stays the
    /// terminal (for hooks and credential prompts)
    Quiet,
    /// A change whose output the user sees (e.g. a merge's summary or conflicts)
    Terminal,
}

/// Whether `dir` is within a git repository: it or one of its parents has a `.git`, or is itself a git
/// directory (Scala used JGit's `FileRepositoryBuilder.findGitDir`)
pub(crate) fn is_repository(ctx: &Ctx, dir: &str) -> bool {
    let platform = ctx.platform();
    let Some(cwd) = platform.current_dir() else {
        return false;
    };
    if !platform.metadata(dir).is_ok_and(|m| m.is_dir()) {
        return false;
    }
    let mut path = resolve(&cwd, dir);
    loop {
        let git = if path == "/" {
            "/.git".to_string()
        } else {
            format!("{path}/.git")
        };
        let is_git_dir = |p: &str| {
            ["HEAD", "objects", "refs"]
                .iter()
                .all(|f| platform.metadata(&format!("{p}/{f}")).is_ok())
        };
        if platform.metadata(&git).is_ok() || is_git_dir(&path) {
            return true;
        }
        match path.rfind('/') {
            Some(0) if path.len() > 1 => path = "/".to_string(),
            Some(i) if i > 0 => path.truncate(i),
            _ => return false,
        }
    }
}

/// `GIT_DIR` is set in the `env` global, so git finds the repository itself
fn git_dir_set(ctx: &Ctx) -> bool {
    matches!(ctx.globals().get_str("env"), Some(Value::Object(env)) if env.contains_str("GIT_DIR"))
}

/// Fail unless the current directory is within a repository
pub(crate) fn require_repository(ctx: &Ctx) -> R<()> {
    if is_repository(ctx, ".") || git_dir_set(ctx) {
        Ok(())
    } else {
        Err(MashError::new(NOT_A_REPOSITORY))
    }
}

/// Run git with `args`, wherever we are (for `clone` and `init`). A process interrupted by Ctrl-C interrupts
/// evaluation. The exit status isn't checked.
pub(crate) fn run_anywhere(ctx: &mut Ctx, mode: Mode, args: &[&str]) -> R<ProcessOutcome> {
    let mut argv = vec!["git".to_string()];
    argv.extend(args.iter().map(|a| a.to_string()));
    let mut env = child_environment(ctx)?;
    if mode == Mode::Query {
        env.retain(|(name, _)| name != "LC_ALL" && name != "GIT_OPTIONAL_LOCKS");
        env.push(("LC_ALL".into(), "C".into()));
        env.push(("GIT_OPTIONAL_LOCKS".into(), "0".into()));
    }
    let request = ProcessRequest {
        argv: &argv,
        env: &env,
        stdin: if mode == Mode::Query {
            ProcessInput::Text("")
        } else {
            ProcessInput::Inherit
        },
        stdout: if mode == Mode::Terminal {
            ProcessOutput::Inherit
        } else {
            ProcessOutput::Capture
        },
        time_limit: ctx.time_left(),
    };
    let outcome = ctx.platform().run_process(&request).map_err(|e| {
        MashError::new(format!(
            "Could not run git: {}",
            crate::builtins::os::paths::describe(&e)
        ))
    })?;
    ctx.check_deadline()?;
    if outcome.interrupted {
        return Err(MashError::interrupted());
    }
    Ok(outcome)
}

/// Run git in the current repository; the exit status isn't checked
pub(crate) fn run(ctx: &mut Ctx, mode: Mode, args: &[&str]) -> R<ProcessOutcome> {
    require_repository(ctx)?;
    run_anywhere(ctx, mode, args)
}

/// The error for a git command that failed (git has explained why on standard error)
pub(crate) fn failed(args: &[&str], outcome: &ProcessOutcome) -> MashError {
    let command = args.first().copied().unwrap_or_default();
    MashError::new(format!("git {command} failed (exit status {})", outcome.exit_status))
}

/// Run git in the current repository, failing if it fails; its captured standard output
pub(crate) fn git(ctx: &mut Ctx, mode: Mode, args: &[&str]) -> R<String> {
    let outcome = run(ctx, mode, args)?;
    if outcome.exit_status != 0 {
        return Err(failed(args, &outcome));
    }
    Ok(outcome.stdout)
}

// ------------------------------------------------------------------------------------------------ queries

/// The commits reachable from `start` (HEAD if None), oldest first (as Scala reversed JGit's log)
pub(crate) fn log(ctx: &mut Ctx, start: Option<&str>) -> R<Vec<Commit>> {
    let mut args = vec!["log", "-z", "--reverse", parse::LOG_FORMAT];
    args.extend(start);
    args.push("--");
    Ok(parse::commits(&git(ctx, Mode::Query, &args)?))
}

/// The commit with this hash (or other name)
pub(crate) fn commit(ctx: &mut Ctx, name: &str) -> R<Commit> {
    let output = git(ctx, Mode::Query, &["log", "-z", "-1", parse::LOG_FORMAT, name, "--"])?;
    parse::commits(&output)
        .into_iter()
        .next()
        .ok_or_else(|| MashError::new(format!("No commit found: {name}")))
}

/// The full hash of the commit a name (hash, branch, tag, …) refers to, if it names one
pub(crate) fn resolve_commit(ctx: &mut Ctx, name: &str) -> R<Option<String>> {
    if name.is_empty() || name.starts_with('-') {
        return Ok(None);
    }
    let spec = format!("{name}^{{commit}}");
    let outcome = run(ctx, Mode::Query, &["rev-parse", "--verify", "--quiet", &spec])?;
    Ok((outcome.exit_status == 0).then(|| outcome.stdout.trim().to_string()))
}

/// Whether commit `ancestor` is an ancestor of (or the same as) `descendant`
pub(crate) fn is_ancestor(ctx: &mut Ctx, ancestor: &str, descendant: &str) -> R<bool> {
    let args = ["merge-base", "--is-ancestor", ancestor, descendant];
    let outcome = run(ctx, Mode::Query, &args)?;
    match outcome.exit_status {
        0 => Ok(true),
        1 => Ok(false),
        _ => Err(failed(&args, &outcome)),
    }
}

/// Branches under `refs/heads` (local) or `refs/remotes` (remote-tracking, without the symbolic `…/HEAD`),
/// sorted by name
pub(crate) fn branches(ctx: &mut Ctx, remote: bool) -> R<Vec<Branch>> {
    let prefix = if remote { "refs/remotes" } else { "refs/heads" };
    let output = git(ctx, Mode::Query, &["for-each-ref", parse::BRANCH_FORMAT, prefix])?;
    let mut branches = parse::branches(&output);
    branches.retain(|b| !b.ref_name.ends_with("/HEAD"));
    Ok(branches)
}

/// Names of local branches; empty if not in a repository (for completion and validation, as Scala's
/// `SwitchFunction.getLocalBranches`)
pub(crate) fn local_branch_names(ctx: &mut Ctx) -> Vec<String> {
    branches(ctx, false)
        .map(|bs| {
            bs.iter()
                .map(|b| b.ref_name.trim_start_matches("refs/heads/").to_string())
                .collect()
        })
        .unwrap_or_default()
}

/// Names of remote-tracking branches (`origin/master`); empty if not in a repository
pub(crate) fn remote_branch_names(ctx: &mut Ctx) -> Vec<String> {
    branches(ctx, true)
        .map(|bs| {
            bs.iter()
                .map(|b| parse::trim_remote_prefix(&b.ref_name).to_string())
                .collect()
        })
        .unwrap_or_default()
}

/// Local branch names, then remote-tracking branch names: what a commit argument completes to
pub(crate) fn branch_names(ctx: &mut Ctx) -> Vec<String> {
    let mut names = local_branch_names(ctx);
    names.extend(remote_branch_names(ctx));
    names
}

/// `(name, commit)` of each tag
pub(crate) fn tags(ctx: &mut Ctx) -> R<Vec<(String, String)>> {
    let output = git(ctx, Mode::Query, &["for-each-ref", parse::TAG_FORMAT, "refs/tags"])?;
    Ok(parse::tags(&output))
}

/// Names of tags; empty if not in a repository
pub(crate) fn tag_names(ctx: &mut Ctx) -> Vec<String> {
    tags(ctx)
        .map(|tags| tags.into_iter().map(|(name, _)| name).collect())
        .unwrap_or_default()
}

/// Names of remotes; empty if not in a repository
pub(crate) fn remote_names(ctx: &mut Ctx) -> Vec<String> {
    git(ctx, Mode::Query, &["remote"])
        .map(|output| output.lines().map(str::to_string).collect())
        .unwrap_or_default()
}

/// The remote-tracking refs and the commits they point to (to see what a fetch changed)
pub(crate) fn remote_refs(ctx: &mut Ctx) -> R<BTreeMap<String, String>> {
    let output = git(ctx, Mode::Query, &["for-each-ref", parse::REF_FORMAT, "refs/remotes"])?;
    Ok(parse::refs(&output))
}

/// The full name of the current branch's ref (`refs/heads/master`), or None if HEAD is detached
pub(crate) fn current_branch_ref(ctx: &mut Ctx) -> R<Option<String>> {
    let outcome = run(ctx, Mode::Query, &["symbolic-ref", "--quiet", "HEAD"])?;
    Ok((outcome.exit_status == 0).then(|| outcome.stdout.trim().to_string()))
}
