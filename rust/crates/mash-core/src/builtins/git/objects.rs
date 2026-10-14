//! Mash objects for git data (commits, branches, …), reading their fields back, and finding the object for a
//! name (for the tag classes' lifted members).

use super::parse::{self, Commit, Identity, Status, Tracking};
use super::run;
use super::{
    BRANCH_CLASS, COMMIT_CLASS, COMMIT_HASH_CLASS, FETCH_BRANCH_UPDATE_CLASS, FILE_DIFF_CLASS, IDENTITY_CLASS,
    LOCAL_BRANCH_NAME_CLASS, REFLOG_ENTRY_CLASS, REMOTE_BRANCH_CLASS, REMOTE_BRANCH_NAME_CLASS, REMOTE_CLASS,
    REMOTE_NAME_CLASS, STASH_ENTRY_CLASS, STATUS_CLASS, TAG_CLASS, TAG_NAME_CLASS,
};
use crate::builtins::os::paths::path_value;
use crate::eval::{Ctx, MashError, R};
use crate::value::{BuiltinClass, ClassRef, DateTime, List, Object, Str, Value};

fn tagged(text: impl Into<std::rc::Rc<str>>, tag: &'static BuiltinClass) -> Value {
    Value::Str(Str::tagged(text, tag))
}

fn object(class: &'static BuiltinClass, fields: Vec<(&str, Value)>) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(class)));
    for (name, value) in fields {
        object.set_str(name, value);
    }
    Value::Object(object)
}

pub(crate) fn commit_hash(hash: impl Into<std::rc::Rc<str>>) -> Value {
    tagged(hash, &COMMIT_HASH_CLASS)
}

fn optional_hash(hash: Option<&String>) -> Value {
    hash.map_or(Value::Null, |h| commit_hash(h.as_str()))
}

fn identity(identity: &Identity) -> Value {
    object(
        &IDENTITY_CLASS,
        vec![
            ("author", Value::str(&identity.name)),
            ("email", Value::str(&identity.email)),
        ],
    )
}

/// A `git.Commit`
pub(crate) fn commit(commit: &Commit) -> Value {
    let time = jiff::Timestamp::from_second(commit.commit_time).unwrap_or(jiff::Timestamp::UNIX_EPOCH);
    object(
        &COMMIT_CLASS,
        vec![
            ("hash", commit_hash(commit.hash.as_str())),
            ("commitTime", Value::DateTime(DateTime(time))),
            ("author", identity(&commit.author)),
            ("committer", identity(&commit.committer)),
            ("summary", Value::str(&commit.summary)),
            ("message", Value::str(&commit.message)),
            (
                "parents",
                Value::list(commit.parents.iter().map(|p| commit_hash(p.as_str())).collect()),
            ),
        ],
    )
}

/// A list of `git.Commit`s
pub(crate) fn commits(commits: &[Commit]) -> List {
    List::new(commits.iter().map(commit).collect())
}

/// Upstream branch, ahead and behind counts (null, 0, 0 without an upstream)
fn tracking_fields(tracking: Option<&Tracking>) -> [(&'static str, Value); 3] {
    match tracking {
        Some(t) => [
            ("upstreamBranch", tagged(t.upstream.as_str(), &REMOTE_BRANCH_NAME_CLASS)),
            ("aheadCount", Value::num(t.ahead as f64)),
            ("behindCount", Value::num(t.behind as f64)),
        ],
        None => [
            ("upstreamBranch", Value::Null),
            ("aheadCount", Value::num(0.0)),
            ("behindCount", Value::num(0.0)),
        ],
    }
}

/// A `git.branch.Branch`, from a branch under `refs/heads`
pub(crate) fn local_branch(branch: &parse::Branch) -> Value {
    let name = branch.ref_name.strip_prefix("refs/heads/").unwrap_or(&branch.ref_name);
    let mut fields = vec![
        ("name", tagged(name, &LOCAL_BRANCH_NAME_CLASS)),
        ("commit", commit_hash(branch.commit.as_str())),
    ];
    fields.extend(tracking_fields(branch.tracking.as_ref()));
    object(&BRANCH_CLASS, fields)
}

/// A `git.branch.RemoteBranch`, from a branch under `refs/remotes`
pub(crate) fn remote_branch(branch: &parse::Branch) -> Value {
    let full_name = parse::trim_remote_prefix(&branch.ref_name);
    let (remote, name) = full_name.split_once('/').unwrap_or((full_name, ""));
    object(
        &REMOTE_BRANCH_CLASS,
        vec![
            ("remote", tagged(remote, &REMOTE_NAME_CLASS)),
            ("name", tagged(name, &REMOTE_BRANCH_NAME_CLASS)),
            ("commit", commit_hash(branch.commit.as_str())),
        ],
    )
}

/// A `git.Status`
pub(crate) fn status(status: &Status) -> Value {
    let paths = |paths: &[String]| Value::list(paths.iter().map(|p| path_value(p.as_str())).collect());
    let mut fields = vec![("branch", tagged(status.branch.as_str(), &LOCAL_BRANCH_NAME_CLASS))];
    fields.extend(tracking_fields(status.tracking.as_ref()));
    fields.extend([
        ("added", paths(&status.added)),
        ("changed", paths(&status.changed)),
        ("missing", paths(&status.missing)),
        ("modified", paths(&status.modified)),
        ("removed", paths(&status.removed)),
        ("untracked", paths(&status.untracked)),
        ("conflicting", paths(&status.conflicting)),
    ]);
    object(&STATUS_CLASS, fields)
}

/// A `git.FileDiff`
pub(crate) fn file_diff(diff: &parse::FileDiff) -> Value {
    let path = |p: &Option<String>| p.as_deref().map_or(Value::Null, path_value);
    object(
        &FILE_DIFF_CLASS,
        vec![
            ("type", Value::str(diff.kind)),
            ("oldPath", path(&diff.old_path)),
            ("newPath", path(&diff.new_path)),
        ],
    )
}

/// A `git.ReflogEntry`
pub(crate) fn reflog_entry(entry: &parse::ReflogEntry) -> Value {
    object(
        &REFLOG_ENTRY_CLASS,
        vec![
            ("commit", commit_hash(entry.commit.as_str())),
            ("comment", Value::str(&entry.comment)),
        ],
    )
}

/// A `git.StashEntry`
pub(crate) fn stash_entry(entry: &parse::StashEntry) -> Value {
    let time = jiff::Timestamp::from_second(entry.time).unwrap_or(jiff::Timestamp::UNIX_EPOCH);
    object(
        &STASH_ENTRY_CLASS,
        vec![
            ("name", Value::str(&entry.name)),
            ("message", Value::str(&entry.message)),
            ("time", Value::DateTime(DateTime(time))),
        ],
    )
}

/// A `git.FetchBranchUpdate`
pub(crate) fn fetch_update(update: &parse::RefUpdate) -> Value {
    object(
        &FETCH_BRANCH_UPDATE_CLASS,
        vec![
            ("branch", tagged(update.branch.as_str(), &REMOTE_BRANCH_NAME_CLASS)),
            ("oldCommit", optional_hash(update.old.as_ref())),
            ("newCommit", optional_hash(update.new.as_ref())),
        ],
    )
}

/// A `git.tag.Tag`
pub(crate) fn tag(name: &str, commit: &str) -> Value {
    object(
        &TAG_CLASS,
        vec![("name", tagged(name, &TAG_NAME_CLASS)), ("commit", commit_hash(commit))],
    )
}

/// A `git.remote.Remote`
pub(crate) fn remote(name: &str, fetch_urls: &[String], push_urls: &[String]) -> Value {
    let strings = |xs: &[String]| Value::list(xs.iter().map(|x| Value::str(x)).collect());
    object(
        &REMOTE_CLASS,
        vec![
            ("name", tagged(name, &REMOTE_NAME_CLASS)),
            ("fetchUrls", strings(fetch_urls)),
            ("pushUrls", strings(push_urls)),
        ],
    )
}

/// The result of pushing one ref (an object of no class, as in Scala)
pub(crate) fn push_update(update: &parse::PushUpdate) -> Value {
    Value::Object(Object::from_pairs([
        ("remote", Value::str(&update.remote_ref)),
        ("status", Value::str(update.status)),
        ("message", update.message.as_deref().map_or(Value::Null, Value::str)),
        ("isFastForward", Value::Bool(update.fast_forward)),
        ("isForceUpdate", Value::Bool(update.forced)),
    ]))
}

// ------------------------------------------------------------------------------------------------ reading fields

/// Whether a value is an object of a builtin class
pub(crate) fn has_class(value: &Value, class: &'static BuiltinClass) -> bool {
    matches!(value, Value::Object(o) if o.class().is_some_and(|c| c.is_builtin(class)))
}

/// A string field of an object (Scala `AbstractObjectWrapper.getStringField`)
pub(crate) fn string_field(target: &Value, name: &str) -> R<String> {
    match target {
        Value::Object(o) => match o.get_str(name) {
            Some(Value::Str(s)) => Ok(s.as_str().to_string()),
            Some(other) => Err(MashError::new(format!(
                "Field '{name}' must be a String, but was a {}",
                other.type_name()
            ))),
            None => Err(MashError::new(format!("No field '{name}' found in object"))),
        },
        other => Err(MashError::new(format!(
            "Expected an Object, but was a {}",
            other.type_name()
        ))),
    }
}

/// The `remote/name` of a `RemoteBranch` object
pub(crate) fn remote_branch_full_name(target: &Value) -> R<String> {
    Ok(format!(
        "{}/{}",
        string_field(target, "remote")?,
        string_field(target, "name")?
    ))
}

/// The hashes in a commit's `parents` field
pub(crate) fn parents(target: &Value) -> R<Vec<String>> {
    match target {
        Value::Object(o) => match o.get_str("parents") {
            Some(Value::List(xs)) => xs
                .snapshot()
                .iter()
                .map(|x| match x {
                    Value::Str(s) => Ok(s.as_str().to_string()),
                    other => Err(MashError::new(format!(
                        "Expected parents to be of type String, but one was of type '{}'",
                        other.type_name()
                    ))),
                })
                .collect(),
            _ => Err(MashError::new("Field 'parents' must be a List")),
        },
        other => Err(MashError::new(format!(
            "Expected an Object, but was a {}",
            other.type_name()
        ))),
    }
}

// ------------------------------------------------------------------------------------------------ lookups

fn into_object(value: Value) -> Object {
    match value {
        Value::Object(o) => o,
        _ => unreachable!("git objects are objects"),
    }
}

/// The commit with this hash, as a `git.Commit`
pub(crate) fn commit_by_hash(ctx: &mut Ctx, hash: &str) -> R<Object> {
    Ok(into_object(commit(&run::commit(ctx, hash)?)))
}

/// The local branch with this name, as a `git.branch.Branch`
pub(crate) fn local_branch_by_name(ctx: &mut Ctx, name: &str) -> R<Object> {
    let ref_name = format!("refs/heads/{name}");
    run::branches(ctx, false)?
        .iter()
        .find(|b| b.ref_name == ref_name)
        .map(|b| into_object(local_branch(b)))
        .ok_or_else(|| MashError::new(format!("No branch with name {name}")))
}

/// The remote-tracking branch with this name (`origin/master`), as a `git.branch.RemoteBranch`
pub(crate) fn remote_branch_by_name(ctx: &mut Ctx, name: &str) -> R<Object> {
    let ref_name = format!("refs/remotes/{name}");
    run::branches(ctx, true)?
        .iter()
        .find(|b| b.ref_name == ref_name)
        .map(|b| into_object(remote_branch(b)))
        .ok_or_else(|| MashError::new(format!("No branch with name {name}")))
}
