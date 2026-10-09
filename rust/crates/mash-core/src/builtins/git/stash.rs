//! `git.stash`: stashing changes in the working directory and applying them again.

use mash_macros::builtin;

use super::run::{Mode, git};
use crate::eval::{Ctx, R};
use crate::infer::Type;

/// Stash changes in the working directory
#[builtin(namespace = "git.stash", returns = Type::UNIT)]
pub(super) fn create(ctx: &mut Ctx) -> R<()> {
    git(ctx, Mode::Quiet, &["stash", "push", "--quiet"]).map(drop)
}

/// Apply stashed changes to the current working directory
#[builtin(namespace = "git.stash", returns = Type::UNIT)]
pub(super) fn apply(ctx: &mut Ctx) -> R<()> {
    git(ctx, Mode::Quiet, &["stash", "apply", "--quiet"]).map(drop)
}
