//! Completing text as a string: an argument with completion specs (e.g. a field name or directory), one side of
//! an equality with an enumerated type, or a path (Scala `StringCompleter`, `ArgCompleter`,
//! `EqualityCompleter`).

use mash_syntax::{SyntaxKind, TextRange};

use super::find::{contiguous_region, find, find_invocation_with_literal_arg};
use super::members::complete_string_member;
use super::paths::complete_paths;
use super::{
    Completer, Completion, CompletionLocation, CompletionResult, CompletionSpec, analyse, at, replace, tokenise,
};
use crate::ast::{Arg, BinOp, ExprKind};
use crate::eval::Ctx;
use crate::infer::{BuiltinFunctionType, Inferred, Type, TypedArg, TypedArgs, ValueInfo};
use crate::value::BuiltinClass;

pub(crate) struct StringCompletion {
    /// The completions are paths (and so can be merged with binding completions)
    pub(crate) path_completion: bool,
    pub(crate) result: Option<CompletionResult>,
}

impl Completer<'_> {
    /// Reinterpret the text around `region` as a string literal, and complete that
    pub(crate) fn complete_as_string(&self, ctx: &mut Ctx, region: TextRange) -> StringCompletion {
        for (liberal, substring) in [(true, false), (false, false), (true, true), (false, true)] {
            let contiguous = contiguous_region(self.text, region, self.mish, liberal);
            let contents: String = self.text[contiguous].chars().filter(|c| *c != '"').collect();
            let quoted = format!("\"{contents}\"");
            let replaced = replace(self.text, contiguous, &quoted);
            let string_region = at(usize::from(contiguous.start()), quoted.len());
            let completion = self.complete_string(ctx, &replaced, string_region, substring);
            if let Some(result) = completion.result {
                return StringCompletion {
                    path_completion: completion.path_completion,
                    result: Some(CompletionResult {
                        replace: contiguous,
                        ..result
                    }),
                };
            }
        }
        StringCompletion {
            path_completion: false,
            result: None,
        }
    }

    fn complete_string(
        &self,
        ctx: &mut Ctx,
        text: &str,
        string_region: TextRange,
        substring: bool,
    ) -> StringCompletion {
        if let Some(result) = complete_arg(ctx, text, self.mish, string_region) {
            return StringCompletion {
                path_completion: false,
                result: Some(result),
            };
        }
        if let Some(result) = complete_equality(ctx, text, self.mish, string_region) {
            return StringCompletion {
                path_completion: false,
                result: Some(result),
            };
        }
        let without_quotes: String = text[string_region].chars().filter(|c| *c != '"').collect();
        let result = complete_paths(ctx.platform(), &without_quotes, string_region, false, substring);
        StringCompletion {
            path_completion: result.is_some(),
            result,
        }
    }
}

fn string_literal_at(text: &str, mish: bool, region: TextRange) -> bool {
    tokenise(text, mish)
        .iter()
        .any(|t| t.kind == SyntaxKind::STRING_LITERAL && t.range == region)
}

/// What's known about each argument of a call
pub(crate) fn typed_args(inferred: &Inferred, args: &[Arg]) -> TypedArgs {
    let info = |e: &crate::ast::E| ValueInfo {
        value: inferred.constants.get(&e.id).cloned(),
        ty: inferred.types.get(&e.id).cloned(),
    };
    TypedArgs {
        args: args
            .iter()
            .map(|arg| match arg {
                Arg::Positional(e) => TypedArg::Positional(info(e)),
                Arg::ShortFlags { flags, .. } => TypedArg::ShortFlags(flags.clone()),
                Arg::LongFlag { name, value, .. } => TypedArg::LongFlag(name.clone(), value.as_ref().map(info)),
            })
            .collect(),
    }
}

/// Complete a string argument of a builtin from its completion specs
fn complete_arg(ctx: &mut Ctx, text: &str, mish: bool, literal: TextRange) -> Option<CompletionResult> {
    if !string_literal_at(text, mish, literal) {
        return None;
    }
    let analysis = analyse(ctx, text, mish);
    let info = find_invocation_with_literal_arg(&analysis.program.body, literal)?;
    let ExprKind::Invocation { function, args, .. } = &info.invocation.kind else {
        return None;
    };
    let args = typed_args(&analysis.inferred, args);
    let specs = match analysis.inferred.types.get(&function.id)? {
        Type::BuiltinFunction(BuiltinFunctionType::Builtin(f)) => f.completion_specs(info.arg_pos, &args),
        Type::BoundBuiltinMethod(target, m) => m.completion_specs(info.arg_pos, Some(target), &args),
        _ => return None,
    };
    let without_quotes: String = text[literal].chars().filter(|c| *c != '"').collect();
    specs
        .into_iter()
        .map(|spec| complete_from_spec(ctx, &spec, &without_quotes, literal))
        .fold(None, CompletionResult::merge)
}

fn complete_from_spec(
    ctx: &mut Ctx,
    spec: &CompletionSpec,
    prefix: &str,
    literal: TextRange,
) -> Option<CompletionResult> {
    match spec {
        CompletionSpec::Directory | CompletionSpec::File => {
            let directories_only = matches!(spec, CompletionSpec::Directory);
            complete_paths(ctx.platform(), prefix, literal, directories_only, false)
                .or_else(|| complete_paths(ctx.platform(), prefix, literal, directories_only, true))
        }
        CompletionSpec::Members(ty) => CompletionResult::of(complete_string_member(ty, prefix), literal),
        CompletionSpec::Items(items) => complete_items(items, prefix, literal),
        CompletionSpec::ComputedSpecs(specs) => specs(ctx)
            .iter()
            .map(|spec| complete_from_spec(ctx, spec, prefix, literal))
            .fold(None, CompletionResult::merge),
        CompletionSpec::Computed(compute) => {
            let items = compute(ctx, prefix);
            complete_items(&items, prefix, literal)
        }
    }
}

/// Items starting with the prefix or, failing that, containing it
fn complete_items(items: &[String], prefix: &str, literal: TextRange) -> Option<CompletionResult> {
    let starting: Vec<Completion> = items
        .iter()
        .filter(|item| item.starts_with(prefix))
        .map(|item| Completion {
            quoted: true,
            ..Completion::new(item)
        })
        .collect();
    CompletionResult::of(starting, literal).or_else(|| {
        let containing = items
            .iter()
            .filter_map(|item| {
                let index = item.find(prefix)?;
                let pos = item[..index].chars().count();
                let location = CompletionLocation {
                    display_pos: pos,
                    insert_pos: pos,
                    ..Default::default()
                };
                Some(Completion {
                    quoted: true,
                    location,
                    ..Completion::new(item)
                })
            })
            .collect();
        CompletionResult::of(containing, literal)
    })
}

/// `x == "fo▶"` where `x` is of a tag type with known values
fn complete_equality(ctx: &mut Ctx, text: &str, mish: bool, literal: TextRange) -> Option<CompletionResult> {
    if !string_literal_at(text, mish, literal) {
        return None;
    }
    let analysis = analyse(ctx, text, mish);
    let is_literal = |e: &crate::ast::E| {
        e.range == Some(literal) && matches!(e.kind, ExprKind::String { .. } | ExprKind::Literal(_))
    };
    let mut other = None;
    find(&analysis.program.body, &mut |e| match &e.kind {
        ExprKind::BinOp {
            op: BinOp::Equals | BinOp::NotEquals,
            lhs,
            rhs,
        } => {
            if is_literal(lhs) {
                other = Some(rhs.clone());
            } else if is_literal(rhs) {
                other = Some(lhs.clone());
            }
            other.is_some()
        }
        _ => false,
    })?;
    let Some(Type::Tagged { base, tag }) = analysis.inferred.types.get(&other?.id) else {
        return None;
    };
    let quoted = std::ptr::eq(*base, &crate::builtins::core::STRING_CLASS);
    let prefix: String = text[literal].chars().filter(|c| *c != '"').collect();
    let completions = enumeration(ctx, tag)
        .iter()
        .filter(|v| v.starts_with(&prefix))
        .map(|v| Completion {
            quoted,
            ..Completion::new(v.as_str())
        })
        .collect();
    CompletionResult::of(completions, literal)
}

/// The values of a tag class with a known set of them
fn enumeration(ctx: &Ctx, class: &'static BuiltinClass) -> Vec<String> {
    match class.enumerate {
        Some(enumerate) => enumerate(ctx.platform()),
        None => class.enumeration.iter().map(|v| v.to_string()).collect(),
    }
}
