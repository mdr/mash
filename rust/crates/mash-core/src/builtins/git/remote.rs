//! `git.remote`: the repository's remotes.

use mash_macros::builtin;

use super::REMOTE_CLASS;
use super::objects;
use super::parse;
use super::run::{Mode, git};
use crate::eval::{Ctx, R};
use crate::infer::Type;
use crate::value::List;

/// Get a list of Git remotes
#[builtin(namespace = "git.remote", aliases = ["git.remotes"], returns = Type::seq(Type::Instance(&REMOTE_CLASS)))]
pub(super) fn list(ctx: &mut Ctx) -> R<List> {
    let names = git(ctx, Mode::Query, &["remote"])?;
    // No remotes configured: git config exits with 1
    let config = git(
        ctx,
        Mode::Query,
        &["config", "-z", "--get-regexp", r"^remote\..*\.(url|pushurl)$"],
    )
    .unwrap_or_default();
    Ok(List::new(
        parse::remotes(&names, &config)
            .iter()
            .map(|(name, fetch, push)| objects::remote(name, fetch, push))
            .collect(),
    ))
}
