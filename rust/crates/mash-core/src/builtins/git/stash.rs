//! `git.stash`: stashing changes in the working directory, listing the stashes, and applying or dropping the
//! newest.

use mash_macros::builtin;

use super::STASH_ENTRY_CLASS;
use super::objects;
use super::parse;
use super::run::{Mode, git};
use crate::eval::{Ctx, R};
use crate::infer::Type;
use crate::value::List;

/// Stash changes in the working directory
#[builtin(namespace = "git.stash", returns = Type::UNIT)]
pub(super) fn create(ctx: &mut Ctx) -> R<()> {
    git(ctx, Mode::Quiet, &["stash", "push", "--quiet"]).map(drop)
}

/// Apply stashed changes to the current working directory
///
/// Applies the newest stash, and keeps it: see `git.stash.pop` to also drop it.
#[builtin(namespace = "git.stash", returns = Type::UNIT)]
pub(super) fn apply(ctx: &mut Ctx) -> R<()> {
    git(ctx, Mode::Quiet, &["stash", "apply", "--quiet"]).map(drop)
}

/// Apply the newest stash to the current working directory, then drop it
///
/// If applying it conflicts, the stash is kept (as `git stash pop`).
#[builtin(namespace = "git.stash", returns = Type::UNIT)]
pub(super) fn pop(ctx: &mut Ctx) -> R<()> {
    git(ctx, Mode::Quiet, &["stash", "pop", "--quiet"]).map(drop)
}

/// Delete the newest stash, without applying it
#[builtin(name = "drop", namespace = "git.stash", returns = Type::UNIT)]
pub(super) fn drop_(ctx: &mut Ctx) -> R<()> {
    git(ctx, Mode::Quiet, &["stash", "drop", "--quiet"]).map(drop)
}

/// The stashes, newest first
#[builtin(namespace = "git.stash", returns = Type::seq(Type::Instance(&STASH_ENTRY_CLASS)))]
pub(super) fn list(ctx: &mut Ctx) -> R<List> {
    let output = git(ctx, Mode::Query, &["stash", "list", "-z", parse::STASH_LIST_FORMAT])?;
    Ok(List::new(parse::stash_list(&output).iter().map(objects::stash_entry).collect()))
}
