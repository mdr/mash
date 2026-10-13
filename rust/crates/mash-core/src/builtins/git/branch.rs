//! `git.branch`: local and remote-tracking branches, and the methods of `Branch`, `RemoteBranch` and the branch
//! name tag classes.

use mash_macros::{builtin, builtin_method};

use super::functions::{
    complete_ancestor_commit, force_param, is_ancestor_of, param_at, push_branches, remote_param, set_upstream_param,
    validate_commit,
};
use super::objects::{self, string_field};
use super::run::{self, Mode, git};
use super::{BRANCH_CLASS, COMMIT_CLASS, REMOTE_BRANCH_CLASS, REMOTE_BRANCH_NAME_CLASS};
use crate::complete::CompletionSpec;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, Param, Params};
use crate::infer::{Type, TypedArgs};
use crate::value::{List, Str, Value};

fn branch_type() -> Type {
    Type::Instance(&BRANCH_CLASS)
}

fn commits_type() -> Type {
    Type::seq(Type::Instance(&COMMIT_CLASS))
}

fn local_branch_specs(ctx: &mut Ctx) -> Vec<CompletionSpec> {
    vec![CompletionSpec::Items(run::local_branch_names(ctx))]
}

fn remote_branch_specs(ctx: &mut Ctx) -> Vec<CompletionSpec> {
    vec![CompletionSpec::Items(run::remote_branch_names(ctx))]
}

/// The name of a local branch given as an argument: a string or a `Branch` (Scala
/// `DeleteFunction.validateBranch`)
pub(super) fn local_branch_name(ctx: &mut Ctx, args: &Args, param: &str, value: &Value) -> R<String> {
    let name = match value {
        Value::Str(s) => s.as_str().to_string(),
        _ if objects::has_class(value, &BRANCH_CLASS) => string_field(value, "name")?,
        _ => return Err(args.invalid(param, "Must be a local branch")),
    };
    if !run::local_branch_names(ctx).contains(&name) {
        return Err(args.invalid(param, "Must be a local branch"));
    }
    Ok(name)
}

/// Make each branch (none: the current branch) track the branch of the same name on `remote` (default
/// origin) (Scala `PushFunction.setUpstreamConfig`)
pub(super) fn set_upstream_config(ctx: &mut Ctx, branches: &[String], remote: Option<&str>) -> R<()> {
    let branches = if branches.is_empty() {
        let current = run::current_branch_ref(ctx)?.ok_or_else(|| MashError::new("Not on a branch"))?;
        vec![current.trim_start_matches("refs/heads/").to_string()]
    } else {
        branches.to_vec()
    };
    for branch in branches {
        let remote_key = format!("branch.{branch}.remote");
        git(ctx, Mode::Quiet, &["config", &remote_key, remote.unwrap_or("origin")])?;
        let merge_key = format!("branch.{branch}.merge");
        let merge = format!("refs/heads/{branch}");
        git(ctx, Mode::Quiet, &["config", &merge_key, &merge])?;
    }
    Ok(())
}

/// Switch the working tree to a local branch
fn switch_to(ctx: &mut Ctx, branch: &str) -> R<()> {
    git(ctx, Mode::Quiet, &["checkout", "--quiet", branch, "--"]).map(drop)
}

/// The log of a branch (or other commit name)
fn log_of(ctx: &mut Ctx, name: &str) -> R<List> {
    let hash = run::resolve_commit(ctx, name)?.ok_or_else(|| MashError::new(format!("No commit found: {name}")))?;
    Ok(objects::commits(&run::log(ctx, Some(&hash))?))
}

/// Create a local branch, tracking `remote` if given, and switch to it if asked; the new `Branch`
fn create_branch(ctx: &mut Ctx, name: &str, remote: Option<&str>, switch: bool) -> R<Value> {
    let mut args = vec!["branch", "--quiet"];
    if remote.is_some() {
        args.push("--track");
    }
    args.push(name);
    args.extend(remote);
    git(ctx, Mode::Quiet, &args)?;
    let branch = objects::local_branch_by_name(ctx, name)?;
    if switch {
        switch_to(ctx, name)?;
    }
    Ok(Value::Object(branch))
}

fn switch_param() -> Param {
    Param::named("switch")
        .summary("Switch to the new branch after creating it (default false)")
        .short('s')
        .boolean_flag()
}

// ------------------------------------------------------------------------------------------------ functions

fn complete_create(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    match param_at(params, args, arg_pos) {
        Some("fromRemote") => vec![CompletionSpec::ComputedSpecs(remote_branch_specs)],
        _ => vec![],
    }
}

/// Create a new local branch
///
/// If 'fromRemote' is provided, 'branch' can be omitted, with the same name used locally.
#[builtin(namespace = "git.branch", returns = branch_type(), completion = complete_create)]
pub(super) fn create(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "Name to give the new local branch",
        description = "Can be omitted if 'fromRemote' is provided"
    )]
    branch: Option<String>,
    #[param(def = switch_param)] switch: bool,
    #[param(
        "Create the new branch as a local tracking branch of the given remote branch",
        flag,
        name = "fromRemote"
    )]
    from_remote: Option<Value>,
) -> R<Value> {
    let from_remote = match from_remote {
        None => None,
        Some(Value::Str(s)) => Some(s.as_str().to_string()),
        Some(value) if objects::has_class(&value, &REMOTE_BRANCH_CLASS) => {
            Some(objects::remote_branch_full_name(&value)?)
        }
        Some(other) => {
            return Err(args.invalid(
                "fromRemote",
                &format!("Must be a remote branch, but was {}", other.type_name()),
            ));
        }
    };
    if let Some(remote) = &from_remote
        && !run::remote_branch_names(ctx).contains(remote)
    {
        return Err(args.invalid("fromRemote", &format!("{remote} is not a valid remote branch")));
    }
    let local_name = match (&branch, &from_remote) {
        (Some(branch), _) => branch.clone(),
        (None, Some(remote)) => remote.strip_prefix("origin/").unwrap_or(remote).to_string(),
        (None, None) => return Err(MashError::new("Must provide at least one of 'branch' and 'fromRemote'")),
    };
    create_branch(ctx, &local_name, from_remote.as_deref(), switch)
}

/// The current branch
#[builtin(namespace = "git.branch", returns = branch_type())]
pub(super) fn current(ctx: &mut Ctx) -> R<Value> {
    let current = run::current_branch_ref(ctx)?.ok_or_else(|| MashError::new("Not on a branch"))?;
    run::branches(ctx, false)?
        .iter()
        .find(|b| b.ref_name == current)
        .map(objects::local_branch)
        .ok_or_else(|| MashError::new("The current branch has no commits"))
}

fn complete_local_branches(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    match param_at(params, args, arg_pos) {
        Some("branches" | "branch") => vec![CompletionSpec::ComputedSpecs(local_branch_specs)],
        _ => vec![],
    }
}

/// Delete a local branch
///
/// As `git branch -d`, a branch whose commits aren't merged (into its upstream, or the current branch) isn't
/// deleted, unless --force is given.
#[builtin(namespace = "git.branch", returns = Type::UNIT, completion = complete_local_branches)]
pub(super) fn delete(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = force_delete_param)] force: bool,
    #[param("Local branches to delete", variadic, at_least_one)] branches: List,
) -> R<()> {
    let mut names = vec![];
    for branch in branches.snapshot() {
        names.push(local_branch_name(ctx, args, "branches", &branch)?);
    }
    let mut git_args = vec!["branch", "--quiet", delete_flag(force), "--"];
    git_args.extend(names.iter().map(String::as_str));
    git(ctx, Mode::Quiet, &git_args).map(drop)
}

/// Get a local branch by name
#[builtin(namespace = "git.branch", returns = branch_type(), completion = complete_local_branches)]
pub(super) fn get(ctx: &mut Ctx, args: &Args, #[param("Local branch to get")] branch: Value) -> R<Value> {
    let name = local_branch_name(ctx, args, "branch", &branch)?;
    let ref_name = format!("refs/heads/{name}");
    Ok(run::branches(ctx, false)?
        .iter()
        .find(|b| b.ref_name == ref_name)
        .map_or(Value::Null, objects::local_branch))
}

/// List branches in the repository
#[builtin(namespace = "git.branch", aliases = ["git.branches"], returns = Type::seq(branch_type()))]
pub(super) fn local_branches(ctx: &mut Ctx) -> R<List> {
    Ok(List::new(
        run::branches(ctx, false)?.iter().map(objects::local_branch).collect(),
    ))
}

/// List remote branches in the repository
#[builtin(namespace = "git.branch", returns = Type::seq(Type::Instance(&REMOTE_BRANCH_CLASS)))]
pub(super) fn remote_branches(ctx: &mut Ctx) -> R<List> {
    Ok(List::new(
        run::branches(ctx, true)?.iter().map(objects::remote_branch).collect(),
    ))
}

fn complete_set_commit(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    match param_at(params, args, arg_pos) {
        Some("branch") => vec![CompletionSpec::ComputedSpecs(local_branch_specs)],
        Some("commit") => vec![CompletionSpec::ComputedSpecs(super::functions::branch_name_specs)],
        _ => vec![],
    }
}

/// Point a local branch at a commit (as JGit's forced ref update: the working tree isn't touched)
fn force_delete_param() -> Param {
    Param::named("force")
        .summary("Delete even if not merged (default false)")
        .short('f')
        .boolean_flag()
}

/// As `git branch`: `-d` refuses to delete a branch whose commits aren't in its upstream or HEAD
fn delete_flag(force: bool) -> &'static str {
    if force { "-D" } else { "-d" }
}

/// Point a branch at a commit. Not the checked-out branch, as `git branch -f` refuses: that would leave the
/// working tree and index describing a different commit.
fn set_commit_of(ctx: &mut Ctx, branch: &str, commit: &str) -> R<()> {
    let ref_name = format!("refs/heads/{branch}");
    let current = run::run(ctx, Mode::Query, &["symbolic-ref", "--quiet", "HEAD"])?;
    if current.exit_status == 0 && current.stdout.trim_end() == ref_name {
        return Err(MashError::new(format!(
            "Cannot set the commit of '{branch}': it is the current branch"
        )));
    }
    git(ctx, Mode::Quiet, &["update-ref", &ref_name, commit]).map(drop)
}

/// Update a branch to point to a different commit
#[builtin(namespace = "git.branch", returns = Type::UNIT, completion = complete_set_commit)]
pub(super) fn set_commit(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Local branch to update")] branch: Value,
    #[param("Commit to point to")] commit: Value,
) -> R<()> {
    let branch = local_branch_name(ctx, args, "branch", &branch)?;
    let commit = validate_commit(ctx, args, "commit", &commit)?;
    set_commit_of(ctx, &branch, &commit)
}

fn complete_switch(_: &Params, _: usize, _: &TypedArgs) -> Vec<CompletionSpec> {
    vec![CompletionSpec::ComputedSpecs(local_branch_specs)]
}

/// Switch to a local branch
#[builtin(namespace = "git.branch", aliases = ["git.switch"], returns = Type::UNIT, completion = complete_switch)]
pub(super) fn switch(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Name of a local branch to switch to")] branch: Value,
) -> R<()> {
    let branch = local_branch_name(ctx, args, "branch", &branch)?;
    switch_to(ctx, &branch)
}

// ------------------------------------------------------------------------------------------------ Branch methods

fn name(target: &Value) -> R<String> {
    string_field(target, "name")
}

/// Delete this branch (if merged, unless --force is given)
#[builtin_method(name = "delete", returns = Type::UNIT)]
pub(super) fn branch_delete(ctx: &mut Ctx, target: &Value, #[param(def = force_delete_param)] force: bool) -> R<()> {
    let name = name(target)?;
    git(
        ctx,
        Mode::Quiet,
        &["branch", "--quiet", delete_flag(force), "--", &name],
    )
    .map(drop)
}

/// Return a list of commits from this branch
#[builtin_method(name = "log", returns = commits_type())]
pub(super) fn branch_log(ctx: &mut Ctx, target: &Value) -> R<List> {
    log_of(ctx, &name(target)?)
}

/// Return true if this is an ancestor of the given commit
#[builtin_method(
    name = "isAncestorOf",
    aliases = ["isMergedInto"],
    returns = Type::BOOLEAN,
    completion = complete_ancestor_commit
)]
pub(super) fn branch_is_ancestor_of(
    ctx: &mut Ctx,
    target: &Value,
    args: &Args,
    #[param("Name of a commit to test if it is a descendant")] commit: Value,
) -> R<bool> {
    is_ancestor_of(ctx, args, &name(target)?, &commit)
}

/// Push this branch
#[builtin_method(name = "push", returns = Type::UNIT)]
pub(super) fn branch_push(
    ctx: &mut Ctx,
    target: &Value,
    #[param(def = force_param)] force: bool,
    #[param(def = set_upstream_param)] set_upstream: bool,
    #[param(def = remote_param)] remote: Option<String>,
) -> R<()> {
    push_branches(ctx, &[name(target)?], remote.as_deref(), force, set_upstream).map(drop)
}

/// Update this branch to point to a given commit
#[builtin_method(name = "setCommit", returns = Type::UNIT, completion = complete_ancestor_commit)]
pub(super) fn branch_set_commit(
    ctx: &mut Ctx,
    target: &Value,
    args: &Args,
    #[param("Commit to point to")] commit: Value,
) -> R<()> {
    let commit = validate_commit(ctx, args, "commit", &commit)?;
    set_commit_of(ctx, &name(target)?, &commit)
}

/// Switch to this branch
#[builtin_method(name = "switch", returns = Type::UNIT)]
pub(super) fn branch_switch(ctx: &mut Ctx, target: &Value) -> R<()> {
    switch_to(ctx, &name(target)?)
}

/// Get information about the local branch with this name
#[builtin_method(name = "info", returns = branch_type())]
pub(super) fn local_branch_info(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let name = target.as_str().unwrap_or_default().to_string();
    Ok(Value::Object(objects::local_branch_by_name(ctx, &name)?))
}

// ------------------------------------------------------------------------------------------------ RemoteBranch methods

/// Create a local branch tracking this remote branch
#[builtin_method(returns = branch_type())]
pub(super) fn create_local(ctx: &mut Ctx, target: &Value, #[param(def = switch_param)] switch: bool) -> R<Value> {
    let full_name = objects::remote_branch_full_name(target)?;
    create_branch(ctx, &name(target)?, Some(&full_name), switch)
}

/// Delete this remote branch
#[builtin_method(name = "delete", returns = Type::UNIT)]
pub(super) fn remote_branch_delete(ctx: &mut Ctx, target: &Value) -> R<()> {
    let remote = string_field(target, "remote")?;
    let refspec = format!(":{}", name(target)?);
    git(ctx, Mode::Quiet, &["push", "--quiet", &remote, &refspec]).map(drop)
}

/// Full name of the remote branch, e.g. origin/master
#[builtin_method(returns = Type::STRING)]
pub(super) fn full_name(target: &Value) -> R<Value> {
    Ok(Value::Str(Str::tagged(
        objects::remote_branch_full_name(target)?,
        &REMOTE_BRANCH_NAME_CLASS,
    )))
}

/// Return true if this is an ancestor of the given commit
#[builtin_method(
    name = "isAncestorOf",
    aliases = ["isMergedInto"],
    returns = Type::BOOLEAN,
    completion = complete_ancestor_commit
)]
pub(super) fn remote_branch_is_ancestor_of(
    ctx: &mut Ctx,
    target: &Value,
    args: &Args,
    #[param("Name of a commit to test if it is a descendant")] commit: Value,
) -> R<bool> {
    is_ancestor_of(ctx, args, &objects::remote_branch_full_name(target)?, &commit)
}

/// Return a list of commits from this branch
#[builtin_method(name = "log", returns = commits_type())]
pub(super) fn remote_branch_log(ctx: &mut Ctx, target: &Value) -> R<List> {
    log_of(ctx, &objects::remote_branch_full_name(target)?)
}

/// Get information about the local branch with this name
#[builtin_method(name = "info", returns = Type::Instance(&REMOTE_BRANCH_CLASS))]
pub(super) fn remote_branch_info(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let name = target.as_str().unwrap_or_default().to_string();
    Ok(Value::Object(objects::remote_branch_by_name(ctx, &name)?))
}
