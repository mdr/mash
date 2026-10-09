//! `git.tag`: listing, creating and deleting tags.

use mash_macros::{builtin, builtin_method};

use super::TAG_CLASS;
use super::functions::param_at;
use super::objects::{self, has_class, string_field};
use super::run::{self, Mode, git};
use crate::complete::CompletionSpec;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, Params};
use crate::infer::{Type, TypedArgs};
use crate::value::{List, Value};

/// Get a list of tags
#[builtin(namespace = "git.tag", aliases = ["git.tags"], returns = Type::seq(Type::Instance(&TAG_CLASS)))]
pub(super) fn list(ctx: &mut Ctx) -> R<List> {
    Ok(List::new(
        run::tags(ctx)?
            .iter()
            .map(|(name, commit)| objects::tag(name, commit))
            .collect(),
    ))
}

/// Create a Git tag
///
/// (As Scala Mash, with JGit, an annotated tag with an empty message.)
#[builtin(namespace = "git.tag", returns = Type::Instance(&TAG_CLASS))]
pub(super) fn create(ctx: &mut Ctx, #[param("Name to give the tag")] name: String) -> R<Value> {
    git(ctx, Mode::Quiet, &["tag", "--annotate", "--message=", "--", &name])?;
    run::tags(ctx)?
        .iter()
        .find(|(tag, _)| *tag == name)
        .map(|(name, commit)| objects::tag(name, commit))
        .ok_or_else(|| MashError::new(format!("Could not find the new tag '{name}'")))
}

fn complete_tags(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    match param_at(params, args, arg_pos) {
        Some("tags") => vec![CompletionSpec::ComputedSpecs(|ctx| {
            vec![CompletionSpec::Items(run::tag_names(ctx))]
        })],
        _ => vec![],
    }
}

fn delete_tags(ctx: &mut Ctx, names: &[String]) -> R<()> {
    let mut args = vec!["tag", "--delete", "--"];
    args.extend(names.iter().map(String::as_str));
    git(ctx, Mode::Quiet, &args).map(drop)
}

/// Delete a tag
#[builtin(namespace = "git.tag", returns = Type::UNIT, completion = complete_tags)]
pub(super) fn delete(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Tags to delete", variadic, at_least_one)] tags: List,
) -> R<()> {
    let existing = run::tag_names(ctx);
    let mut names = vec![];
    for tag in tags.snapshot() {
        let name = match &tag {
            Value::Str(s) => s.as_str().to_string(),
            _ if has_class(&tag, &TAG_CLASS) => string_field(&tag, "name")?,
            _ => return Err(args.invalid("tags", "Must be a tag")),
        };
        if !existing.contains(&name) {
            return Err(args.invalid("tags", "Must be a tag"));
        }
        names.push(name);
    }
    delete_tags(ctx, &names)
}

/// Delete this tag
#[builtin_method(name = "delete", returns = Type::UNIT)]
pub(super) fn tag_delete(ctx: &mut Ctx, target: &Value) -> R<()> {
    delete_tags(ctx, &[string_field(target, "name")?])
}
