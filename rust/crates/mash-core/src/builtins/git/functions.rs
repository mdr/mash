//! Functions directly in `git` (status, the log, committing, staging, merging, pushing, pulling, …) and the
//! methods of commits, commit hashes, identities and statuses.

use mash_macros::{builtin, builtin_method};

use super::objects::{self, string_field};
use super::parse;
use super::run::{self, Mode, git};
use super::{COMMIT_CLASS, FETCH_BRANCH_UPDATE_CLASS, FILE_DIFF_CLASS, REFLOG_ENTRY_CLASS, STATUS_CLASS};
use crate::builtins::os::paths::{path_of, path_value};
use crate::complete::CompletionSpec;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, Param, Params};
use crate::infer::{Type, TypedArgs};
use crate::value::{List, Value};

use super::branch::{local_branch_name, set_upstream_config};

fn commit_type() -> Type {
    Type::Instance(&COMMIT_CLASS)
}

/// The name of the parameter that the argument at `arg_pos` is bound to
pub(super) fn param_at<'p>(params: &'p Params, args: &TypedArgs, arg_pos: usize) -> Option<&'p str> {
    let index = (*args.bind(params).arg_params.get(arg_pos)?)?;
    params.params[index].name.as_deref()
}

/// Branch names, local then remote: what a commit can be completed to
pub(super) fn branch_name_specs(ctx: &mut Ctx) -> Vec<CompletionSpec> {
    vec![CompletionSpec::Items(run::branch_names(ctx))]
}

/// Complete a `commit` argument with branch names
pub(super) fn complete_commit(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    match param_at(params, args, arg_pos) {
        Some("commit") => vec![CompletionSpec::ComputedSpecs(branch_name_specs)],
        _ => vec![],
    }
}

/// The hash of the commit an argument names: a string (hash, branch, tag, …), or a `Branch`, `RemoteBranch`
/// or `Commit` object (Scala `MergeFunction.validateCommit`)
pub(super) fn validate_commit(ctx: &mut Ctx, args: &Args, param: &str, value: &Value) -> R<String> {
    let name = match value {
        Value::Str(s) => s.as_str().to_string(),
        _ if objects::has_class(value, &super::BRANCH_CLASS) => string_field(value, "name")?,
        _ if objects::has_class(value, &super::REMOTE_BRANCH_CLASS) => objects::remote_branch_full_name(value)?,
        _ if objects::has_class(value, &COMMIT_CLASS) => string_field(value, "hash")?,
        _ => return Err(args.invalid(param, "Must be a name of a commit")),
    };
    run::resolve_commit(ctx, &name)?.ok_or_else(|| args.invalid(param, "Must be the name of a commit"))
}

/// Paths from a variadic, flattened parameter (Scala `FunctionHelpers.interpretAsPaths`)
fn paths(paths: &List) -> R<Vec<String>> {
    paths
        .snapshot()
        .iter()
        .map(|p| {
            path_of(p).ok_or_else(|| {
                MashError::new(format!(
                    "Could not interpret value of type {} as a path.",
                    p.type_name()
                ))
            })
        })
        .collect()
}

/// A path from `git status` (relative to the top of the working tree) as a pathspec that works from any
/// subdirectory
fn top_pathspec(path: &str) -> String {
    format!(":(top,literal){path}")
}

/// Run a git command on some paths (none: do nothing)
fn on_paths(ctx: &mut Ctx, command: &[&str], paths: &[String]) -> R<()> {
    if paths.is_empty() {
        return Ok(());
    }
    let mut args = command.to_vec();
    args.push("--");
    args.extend(paths.iter().map(String::as_str));
    git(ctx, Mode::Quiet, &args).map(drop)
}

/// The current directory's path within the working tree (`sub/dir/`, or empty at the top)
fn tree_prefix(ctx: &mut Ctx) -> R<String> {
    Ok(git(ctx, Mode::Query, &["rev-parse", "--show-prefix"])?
        .trim_end()
        .to_string())
}

/// A path relative to the top of the working tree, relative to the directory at `prefix` instead (as
/// `git status` shows paths)
fn relative_to(prefix: &str, path: &str) -> String {
    let directory: Vec<&str> = prefix.split('/').filter(|part| !part.is_empty()).collect();
    let parts: Vec<&str> = path.split('/').collect();
    let common = directory
        .iter()
        .zip(&parts[..parts.len().saturating_sub(1)])
        .take_while(|(a, b)| a == b)
        .count();
    let mut relative = vec![".."; directory.len() - common];
    relative.extend(&parts[common..]);
    relative.join("/")
}

/// The working tree status, with paths relative to the current directory: so that they are paths from here,
/// as other git functions take
fn status_here(ctx: &mut Ctx) -> R<parse::Status> {
    let mut status = status(ctx)?;
    let prefix = tree_prefix(ctx)?;
    if !prefix.is_empty() {
        for paths in [
            &mut status.added,
            &mut status.changed,
            &mut status.missing,
            &mut status.modified,
            &mut status.removed,
            &mut status.untracked,
            &mut status.conflicting,
        ] {
            for path in paths.iter_mut() {
                *path = relative_to(&prefix, path);
            }
        }
    }
    Ok(status)
}

fn status(ctx: &mut Ctx) -> R<parse::Status> {
    let output = git(
        ctx,
        Mode::Query,
        &[
            "status",
            "--porcelain=v2",
            "--branch",
            "-z",
            "--untracked-files=all",
            "--no-renames",
        ],
    )?;
    Ok(parse::status(&output))
}

/// `paths` and the (distinct) status lists chosen by `lists`, as pathspecs
fn status_pathspecs(
    ctx: &mut Ctx,
    given: Vec<String>,
    all: bool,
    lists: fn(&parse::Status) -> Vec<&String>,
) -> R<Vec<String>> {
    let mut files = vec![];
    if all {
        let status = status(ctx)?;
        for path in lists(&status) {
            let spec = top_pathspec(path);
            if !files.contains(&spec) {
                files.push(spec);
            }
        }
    }
    files.extend(given);
    Ok(files)
}

fn unstaged(status: &parse::Status) -> Vec<&String> {
    status
        .untracked
        .iter()
        .chain(&status.modified)
        .chain(&status.missing)
        .chain(&status.conflicting)
        .collect()
}

fn staged(status: &parse::Status) -> Vec<&String> {
    status
        .added
        .iter()
        .chain(&status.changed)
        .chain(&status.removed)
        .collect()
}

/// Changes in the working tree, not staged
fn restorable(status: &parse::Status) -> Vec<&String> {
    status.modified.iter().chain(&status.missing).collect()
}

/// Changes staged or not, except new files (restoring those would delete them)
fn restorable_staged(status: &parse::Status) -> Vec<&String> {
    status
        .modified
        .iter()
        .chain(&status.missing)
        .chain(&status.removed)
        .chain(&status.changed)
        .collect()
}

/// Paths from `git status`, or None outside a repository
fn status_paths(ctx: &mut Ctx, lists: fn(&parse::Status) -> Vec<&String>) -> Option<Vec<String>> {
    let status = status_here(ctx).ok()?;
    let mut paths: Vec<String> = vec![];
    for path in lists(&status) {
        if !paths.contains(path) {
            paths.push(path.clone());
        }
    }
    Some(paths)
}

/// The files of a `paths` argument: some from `git status`, or (outside a repository) any file
fn complete_status_paths(
    params: &Params,
    arg_pos: usize,
    args: &TypedArgs,
    specs: fn(&mut Ctx) -> Vec<CompletionSpec>,
) -> Vec<CompletionSpec> {
    match param_at(params, args, arg_pos) {
        Some("paths") => vec![CompletionSpec::ComputedSpecs(specs)],
        _ => vec![],
    }
}

fn items_or_files(paths: Option<Vec<String>>) -> Vec<CompletionSpec> {
    match paths {
        Some(paths) => vec![CompletionSpec::Items(paths)],
        None => vec![CompletionSpec::File],
    }
}

fn require_paths_or_all(paths: &[String], all: bool) -> R<()> {
    if paths.is_empty() && !all {
        return Err(MashError::new("Must provide either 'paths' or 'all'"));
    }
    Ok(())
}

// ------------------------------------------------------------------------------------------------ status

/// Return the working tree status
///
/// Paths are relative to the current directory, as `git status` shows them.
#[builtin(namespace = "git", returns = Type::Instance(&STATUS_CLASS))]
pub(super) fn status_(ctx: &mut Ctx) -> R<Value> {
    Ok(objects::status(&status_here(ctx)?))
}

/// Return true if a directory is within a Git repository
#[builtin(namespace = "git", name = "isRepo", returns = Type::BOOLEAN)]
pub(super) fn is_repo(ctx: &mut Ctx, #[param("Directory to test")] dir: crate::builtins::os::PathArg) -> R<bool> {
    let dir = if dir.0.is_empty() { ".".to_string() } else { dir.0 };
    Ok(run::is_repository(ctx, &dir))
}

// ------------------------------------------------------------------------------------------------ staging

fn complete_files(_: &Params, _: usize, _: &TypedArgs) -> Vec<CompletionSpec> {
    vec![CompletionSpec::File]
}

/// Record changes to the repository
#[builtin(namespace = "git", returns = Type::UNIT, completion = complete_files)]
pub(super) fn add(
    ctx: &mut Ctx,
    #[param("Add paths to the index", variadic, at_least_one, flatten)] paths: List,
) -> R<()> {
    on_paths(ctx, &["add"], &self::paths(&paths)?)
}

fn stage_all_param() -> Param {
    Param::named("all")
        .summary("Stage all unstaged files (default false)")
        .short('a')
        .boolean_flag()
}

fn complete_unstaged(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    complete_status_paths(params, arg_pos, args, |ctx| items_or_files(status_paths(ctx, unstaged)))
}

/// Stage files
#[builtin(namespace = "git", returns = Type::UNIT, completion = complete_unstaged)]
pub(super) fn stage(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = stage_all_param)] all: bool,
    #[param("Stage the given paths", variadic, flatten)] paths: List,
) -> R<()> {
    let paths = self::paths(&paths)?;
    require_paths_or_all(&paths, all)?;
    if !paths.is_empty() && all {
        return Err(args.invalid("all", "Cannot provide both 'paths' and 'all'"));
    }
    stage_paths(ctx, paths, all)
}

/// Stage paths, or all unstaged files (including deletions)
fn stage_paths(ctx: &mut Ctx, paths: Vec<String>, all: bool) -> R<()> {
    let files = status_pathspecs(ctx, paths, all, unstaged)?;
    on_paths(ctx, &["add"], &files)
}

fn complete_staged(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    complete_status_paths(params, arg_pos, args, |ctx| items_or_files(status_paths(ctx, staged)))
}

/// Unstage files
#[builtin(namespace = "git", returns = Type::UNIT, completion = complete_staged)]
pub(super) fn unstage(
    ctx: &mut Ctx,
    #[param("Unstage all staged files (default false)", name = "all", short = 'a', boolean_flag)] all: bool,
    #[param("Unstage the given paths", variadic, flatten)] paths: List,
) -> R<()> {
    let paths = self::paths(&paths)?;
    require_paths_or_all(&paths, all)?;
    let files = status_pathspecs(ctx, paths, all, staged)?;
    if run::resolve_commit(ctx, "HEAD")?.is_some() {
        on_paths(ctx, &["reset", "--quiet", "HEAD"], &files)
    } else {
        // Nothing committed yet: unstaging is removing from the index
        on_paths(ctx, &["rm", "--cached", "--quiet", "-r"], &files)
    }
}

fn complete_restorable(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    complete_status_paths(params, arg_pos, args, |ctx| {
        items_or_files(status_paths(ctx, restorable))
    })
}

/// Restore modified or deleted files in the working directory
///
/// As `git restore`: files are restored from the index, so staged changes are kept. With --staged, staged
/// changes are discarded too, restoring files from the last commit (`git restore --staged --worktree`).
#[builtin(namespace = "git", returns = Type::UNIT, completion = complete_restorable)]
pub(super) fn restore(
    ctx: &mut Ctx,
    #[param(
        "Restore all modified paths (default false)",
        name = "all",
        short = 'a',
        boolean_flag
    )]
    all: bool,
    #[param(
        "Also discard staged changes, restoring from the last commit (default false)",
        name = "staged",
        short = 's',
        boolean_flag
    )]
    staged: bool,
    #[param("Restore the given paths", variadic, flatten)] paths: List,
) -> R<()> {
    let paths = self::paths(&paths)?;
    require_paths_or_all(&paths, all)?;
    if staged {
        let files = status_pathspecs(ctx, paths, all, restorable_staged)?;
        on_paths(ctx, &["restore", "--quiet", "--staged", "--worktree"], &files)
    } else {
        let files = status_pathspecs(ctx, paths, all, restorable)?;
        on_paths(ctx, &["restore", "--quiet", "--worktree"], &files)
    }
}

// ------------------------------------------------------------------------------------------------ committing

fn commit_all_param() -> Param {
    Param::named("all")
        .summary("Automatically stage any modified, untracked or missing files (default false)")
        .short('a')
        .boolean_flag()
}

/// Commit (or amend the last commit), returning the new commit (Scala `CommitFunction.doCommit`). As JGit's,
/// empty commits and messages are allowed and the message is kept as given.
fn do_commit(ctx: &mut Ctx, all: bool, amend: bool, message: Option<String>) -> R<Value> {
    if all {
        stage_paths(ctx, vec![], true)?;
    }
    let message = match message {
        Some(message) => message,
        None => match run::resolve_commit(ctx, "HEAD")? {
            Some(head) => run::commit(ctx, &head)?.message,
            None => return Err(MashError::new("No previous commit to amend")),
        },
    };
    let mut args = vec![
        "commit",
        "--quiet",
        "--allow-empty",
        "--allow-empty-message",
        "--cleanup=verbatim",
        "--message",
        message.as_str(),
    ];
    if amend {
        args.push("--amend");
    }
    git(ctx, Mode::Quiet, &args)?;
    Ok(objects::commit(&run::commit(ctx, "HEAD")?))
}

/// Record changes to the repository
#[builtin(namespace = "git", returns = commit_type())]
pub(super) fn commit(
    ctx: &mut Ctx,
    #[param(def = commit_all_param)] all: bool,
    #[param(
        "Replace the tip of the current branch by creating a new commit (default false)",
        boolean_flag
    )]
    amend: bool,
    #[param("Message")] message: String,
) -> R<Value> {
    do_commit(ctx, all, amend, Some(message))
}

/// Amend the previous commit
#[builtin(namespace = "git", returns = commit_type())]
pub(super) fn amend(
    ctx: &mut Ctx,
    #[param(def = commit_all_param)] all: bool,
    #[param("Message (defaults to last commit message)")] message: Option<String>,
) -> R<Value> {
    do_commit(ctx, all, true, message)
}

// ------------------------------------------------------------------------------------------------ history

/// Return a sequence of commit objects from the current Git repository
#[builtin(namespace = "git", returns = Type::seq(commit_type()), completion = complete_commit)]
pub(super) fn log(ctx: &mut Ctx, args: &Args, #[param("Commit to find the log of")] commit: Option<Value>) -> R<List> {
    let start = match commit {
        Some(commit) => Some(validate_commit(ctx, args, "commit", &commit)?),
        None => None,
    };
    // Before the first commit, there's no history (rather than git's error)
    if start.is_none() && run::resolve_commit(ctx, "HEAD")?.is_none() {
        return Ok(List::new(vec![]));
    }
    Ok(objects::commits(&run::log(ctx, start.as_deref())?))
}

/// The Git reference log
#[builtin(namespace = "git", returns = Type::seq(Type::Instance(&REFLOG_ENTRY_CLASS)))]
pub(super) fn reflog(ctx: &mut Ctx) -> R<List> {
    if run::resolve_commit(ctx, "HEAD")?.is_none() {
        return Ok(List::new(vec![]));
    }
    let output = git(ctx, Mode::Query, &["reflog", "-z", parse::REFLOG_FORMAT])?;
    let mut entries = parse::reflog(&output);
    entries.reverse();
    Ok(List::new(entries.iter().map(objects::reflog_entry).collect()))
}

/// Merge another commit into this branch
#[builtin(namespace = "git", returns = Type::UNIT, completion = complete_commit)]
pub(super) fn merge(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Squash commits (default false)", short = 's', boolean_flag)] squash: bool,
    #[param("Name of a commit to merge")] commit: Value,
) -> R<()> {
    let commit = validate_commit(ctx, args, "commit", &commit)?;
    let mut git_args = vec!["merge", "--no-edit"];
    if squash {
        git_args.push("--squash");
    }
    git_args.push(&commit);
    git(ctx, Mode::Terminal, &git_args).map(drop)
}

// ------------------------------------------------------------------------------------------------ repositories

/// Create an empty Git repository
#[builtin(namespace = "git", returns = Type::UNIT)]
pub(super) fn init(ctx: &mut Ctx) -> R<()> {
    let args = ["init", "--quiet"];
    let outcome = run::run_anywhere(ctx, Mode::Quiet, &args)?;
    if outcome.exit_status != 0 {
        return Err(run::failed(&args, &outcome));
    }
    Ok(())
}

/// Clone a Git repository into a new directory.
///
/// Returns the path to the new directory.
///
/// Examples:
/// <mash>
///   git.clone "https://github.com/github/testrepo.git"       # clones into "testRepo"
///   git.clone "https://github.com/github/testrepo.git" "dir" # clones into "dir"
/// </mash>
#[builtin(namespace = "git", returns = Type::PATH)]
pub(super) fn clone(
    ctx: &mut Ctx,
    #[param("Repository URL to clone")] repository: String,
    #[param("Name to give to new repository directory")] directory: Option<String>,
) -> R<Value> {
    let directory = directory.unwrap_or_else(|| parse::humanish_name(&repository));
    let args = ["clone", "--quiet", "--", &repository, &directory];
    let outcome = run::run_anywhere(ctx, Mode::Quiet, &args)?;
    if outcome.exit_status != 0 {
        return Err(run::failed(&args, &outcome));
    }
    Ok(path_value(directory))
}

/// Run a fetch (or pull), returning how it changed the remote-tracking branches
fn fetch_updates(ctx: &mut Ctx, mode: Mode, args: &[&str]) -> R<List> {
    let before = run::remote_refs(ctx)?;
    git(ctx, mode, args)?;
    let after = run::remote_refs(ctx)?;
    Ok(List::new(
        parse::ref_updates(&before, &after)
            .iter()
            .map(objects::fetch_update)
            .collect(),
    ))
}

/// Download objects and refs from another repository.
#[builtin(namespace = "git", returns = Type::seq(Type::Instance(&FETCH_BRANCH_UPDATE_CLASS)))]
pub(super) fn fetch(ctx: &mut Ctx) -> R<List> {
    fetch_updates(ctx, Mode::Quiet, &["fetch"])
}

/// Fetch from and integrate with another repository or a local branch
#[builtin(namespace = "git", returns = Type::seq(Type::Instance(&FETCH_BRANCH_UPDATE_CLASS)))]
pub(super) fn pull(ctx: &mut Ctx) -> R<List> {
    fetch_updates(ctx, Mode::Terminal, &["pull", "--no-edit"])
}

pub(super) fn set_upstream_param() -> Param {
    Param::named("setUpstream")
        .summary("Add upstream tracking branch (default false)")
        .short('u')
        .boolean_flag()
}

pub(super) fn force_param() -> Param {
    Param::named("force")
        .summary("Force push (default false)")
        .short('f')
        .boolean_flag()
}

pub(super) fn remote_param() -> Param {
    Param::named("remote")
        .summary("Remote to push to")
        .flag()
        .mandatory_flag_value()
        .optional()
}

fn current_branch_name(ctx: &mut Ctx) -> R<String> {
    let current = run::current_branch_ref(ctx)?.ok_or_else(|| MashError::new("Not on a branch"))?;
    Ok(current.trim_start_matches("refs/heads/").to_string())
}

/// The remote git pushes `branch` to: its `pushRemote`, then `remote.pushDefault`, then
/// the remote it tracks, then origin
fn push_remote(ctx: &mut Ctx, branch: &str) -> R<String> {
    let keys = [
        format!("branch.{branch}.pushRemote"),
        "remote.pushDefault".to_string(),
        format!("branch.{branch}.remote"),
    ];
    for key in keys {
        // An unset key: git config exits with 1
        let value = run::git(ctx, Mode::Query, &["config", "--get", &key]).unwrap_or_default();
        let value = value.trim();
        if !value.is_empty() {
            return Ok(value.to_string());
        }
    }
    Ok("origin".to_string())
}

/// Push branches (none: git's default) to a remote, returning the result for each ref (Scala `PushFunction`)
pub(super) fn push_branches(
    ctx: &mut Ctx,
    branches: &[String],
    remote: Option<&str>,
    force: bool,
    set_upstream: bool,
) -> R<List> {
    let mut args = vec!["push", "--porcelain"];
    if force {
        args.push("--force");
    }
    // Tracking needs a branch: the current one, if none is named
    let current;
    let branches = if set_upstream && branches.is_empty() {
        current = [current_branch_name(ctx)?];
        &current[..]
    } else {
        branches
    };
    // Named branches need a remote: the first one's, as git would choose for it
    let resolved;
    let remote = match (remote, branches.first()) {
        (None, Some(branch)) => {
            resolved = push_remote(ctx, branch)?;
            Some(resolved.as_str())
        }
        _ => remote,
    };
    args.extend(remote);
    args.extend(branches.iter().map(String::as_str));
    let outcome = run::run(ctx, Mode::Quiet, &args)?;
    let updates = parse::push_updates(&outcome.stdout);
    if updates.is_empty() && outcome.exit_status != 0 {
        return Err(MashError::new(format!(
            "Error pushing: git push failed (exit status {})",
            outcome.exit_status
        )));
    }
    // A rejected ref is a failure (and isn't tracked): git's explanation has been shown
    let rejected: Vec<&str> = updates
        .iter()
        .filter(|u| u.status.starts_with("REJECTED"))
        .map(|u| u.remote_ref.as_str())
        .collect();
    if !rejected.is_empty() {
        return Err(MashError::new(format!("Push rejected: {}", rejected.join(", "))));
    }
    if set_upstream {
        set_upstream_config(ctx, branches, remote)?;
    }
    Ok(List::new(updates.iter().map(objects::push_update).collect()))
}

fn complete_push(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    match param_at(params, args, arg_pos) {
        Some("remote") => vec![CompletionSpec::ComputedSpecs(|ctx| {
            vec![CompletionSpec::Items(run::remote_names(ctx))]
        })],
        Some("branches") => vec![CompletionSpec::ComputedSpecs(|ctx| {
            vec![CompletionSpec::Items(run::local_branch_names(ctx))]
        })],
        _ => vec![],
    }
}

/// Update remote refs along with associated objects
///
/// Returns the result for each ref pushed. If the remote rejects any, it is an error. Without `--remote`, the
/// branches go where git would push the first of them: its `pushRemote`, `remote.pushDefault`, the remote it
/// tracks, or else origin.
#[builtin(namespace = "git", returns = Type::seq(Type::OBJECT), completion = complete_push)]
pub(super) fn push(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = set_upstream_param)] set_upstream: bool,
    #[param(def = force_param)] force: bool,
    #[param(def = remote_param)] remote: Option<String>,
    #[param("Local branch to push", variadic)] branches: List,
) -> R<List> {
    let mut names = vec![];
    for branch in branches.snapshot() {
        names.push(local_branch_name(ctx, args, "branches", &branch)?);
    }
    push_branches(ctx, &names, remote.as_deref(), force, set_upstream)
}

// ------------------------------------------------------------------------------------------------ Commit methods

/// Return the difference between the first parent of this commit, and this commit
#[builtin_method(name = "diff", returns = Type::seq(Type::Instance(&FILE_DIFF_CLASS)))]
pub(super) fn commit_diff(ctx: &mut Ctx, target: &Value) -> R<List> {
    let hash = string_field(target, "hash")?;
    let parents = objects::parents(target)?;
    let mut args = vec![
        "diff-tree",
        "-r",
        "-z",
        "--no-renames",
        "--no-commit-id",
        "--name-status",
    ];
    match parents.first() {
        Some(parent) => args.push(parent),
        // A root commit: everything was added
        None => args.push("--root"),
    }
    args.extend([hash.as_str(), "--"]);
    let output = git(ctx, Mode::Query, &args)?;
    Ok(List::new(
        parse::file_diffs(&output).iter().map(objects::file_diff).collect(),
    ))
}

/// Return true if this is an ancestor of the given commit
#[builtin_method(
    name = "isAncestorOf",
    aliases = ["isMergedInto"],
    returns = Type::BOOLEAN,
    completion = complete_ancestor_commit
)]
pub(super) fn commit_is_ancestor_of(
    ctx: &mut Ctx,
    target: &Value,
    args: &Args,
    #[param("Name of a commit to test if it is a descendant")] commit: Value,
) -> R<bool> {
    let this = string_field(target, "hash")?;
    is_ancestor_of(ctx, args, &this, &commit)
}

/// Whether the commit named `this` is an ancestor of the `commit` argument (Scala
/// `AbstractIsAncestorOfMethod`)
pub(super) fn is_ancestor_of(ctx: &mut Ctx, args: &Args, this: &str, commit: &Value) -> R<bool> {
    let that = validate_commit(ctx, args, "commit", commit)?;
    let this = run::resolve_commit(ctx, this)?.ok_or_else(|| MashError::new(format!("Not a commit: {this}")))?;
    run::is_ancestor(ctx, &this, &that)
}

/// Complete an `isAncestorOf` commit with branch names
pub(super) fn complete_ancestor_commit(
    params: &Params,
    arg_pos: usize,
    _: Option<&Type>,
    args: &TypedArgs,
) -> Vec<CompletionSpec> {
    complete_commit(params, arg_pos, args)
}

/// Return the first parent, if there is one, else null
#[builtin_method(name = "parent", returns = Type::tagged_string(&super::COMMIT_HASH_CLASS))]
pub(super) fn commit_parent(target: &Value) -> R<Value> {
    Ok(objects::parents(target)?
        .first()
        .map_or(Value::Null, |p| objects::commit_hash(p.as_str())))
}

/// Get information about the commit with this hash
#[builtin_method(name = "info", returns = commit_type())]
pub(super) fn commit_info(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let hash = target.as_str().unwrap_or_default().to_string();
    Ok(Value::Object(objects::commit_by_hash(ctx, &hash)?))
}

// ------------------------------------------------------------------------------------------------ other methods

/// Represent this object as a string
#[builtin_method(name = "toString", returns = Type::STRING)]
pub(super) fn identity_to_string(target: &Value) -> R<String> {
    Ok(format!(
        "{} <{}>",
        string_field(target, "author")?,
        string_field(target, "email")?
    ))
}

/// Return true if and only if the repository is in a clean state (no modified or untracked files)
#[builtin_method(name = "isClean", returns = Type::BOOLEAN)]
pub(super) fn is_clean(target: &Value) -> R<bool> {
    let Value::Object(o) = target else {
        return Err(MashError::new(format!(
            "Expected an Object, but was a {}",
            target.type_name()
        )));
    };
    let non_empty = |field: &str| matches!(o.get_str(field), Some(Value::List(xs)) if !xs.is_empty());
    let dirty = [
        "added",
        "changed",
        "removed",
        "modified",
        "missing",
        "untracked",
        "conflicting",
    ]
    .iter()
    .any(|field| non_empty(field));
    Ok(!dirty)
}
