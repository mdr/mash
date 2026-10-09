//! Completing flags: `--fo▶` and `-▶` (Scala `FlagCompleter`).

use mash_syntax::lexer::Token;
use mash_syntax::{SyntaxKind, TextRange};

use super::find::find_invocation_with_flag_arg;
use super::{Completer, Completion, CompletionResult, CompletionType, analyse, replace, tokenise};
use crate::ast::ExprKind;
use crate::eval::Ctx;
use crate::function::Params;
use crate::infer::{BuiltinFunctionType, Type};

/// A flag a function accepts: its description, short and long forms
struct Flag {
    description: Option<String>,
    short: Option<char>,
    long: Option<String>,
}

fn flags(params: &Params) -> Vec<Flag> {
    params
        .params
        .iter()
        .map(|p| Flag {
            description: p.summary.as_deref().or(p.name.as_deref()).map(str::to_string),
            short: p.short_flag,
            long: p.name.as_deref().map(str::to_string),
        })
        .collect()
}

/// The parameters of a function of the given type
fn params_of(ty: &Type) -> Option<&Params> {
    Some(match ty {
        Type::BuiltinFunction(BuiltinFunctionType::Builtin(f)) => f.params(),
        Type::BuiltinFunction(BuiltinFunctionType::Constructor(c)) | Type::UserClass(c) => &c.params,
        Type::BoundBuiltinMethod(_, m) => m.params(),
        Type::UserFunction(f) | Type::BoundUserMethod(_, f) => &f.params,
        _ => return None,
    })
}

/// The flags of the function invoked with the flag at `flag` in `text`
fn invocation_flags(ctx: &mut Ctx, text: &str, mish: bool, flag: TextRange) -> Option<Vec<Flag>> {
    let analysis = analyse(ctx, text, mish);
    let info = find_invocation_with_flag_arg(&analysis.program.body, flag)?;
    let ExprKind::Invocation { function, .. } = &info.invocation.kind else {
        return None;
    };
    let ty = analysis.inferred.types.get(&function.id)?;
    params_of(ty).map(flags)
}

/// Complete a long flag (`--fo▶`) from the names of the function's parameters
pub(crate) fn complete_long_flag(ctx: &mut Ctx, completer: &Completer, flag: Token) -> Option<CompletionResult> {
    let flags = invocation_flags(ctx, completer.text, completer.mish, flag.range)?;
    let prefix = completer.text[flag.range].trim_start_matches('-');
    let completions = flags
        .into_iter()
        .filter_map(|f| {
            let long = f.long.filter(|l| l.starts_with(prefix))?;
            Some(Completion {
                description: f.description,
                kind: Some(CompletionType::Flag),
                ..Completion::new(format!("--{long}"))
            })
        })
        .collect();
    CompletionResult::of(completions, flag.range)
}

/// Complete a lone `-` with all the function's flags, long and short
pub(crate) fn complete_all_flags(ctx: &mut Ctx, completer: &Completer, minus: Token) -> Option<CompletionResult> {
    let replaced = replace(completer.text, minus.range, "--dummyFlag ");
    let flag = tokenise(&replaced, completer.mish)
        .into_iter()
        .find(|t| t.kind == SyntaxKind::LONG_FLAG && t.range.start() == minus.range.start())?;
    let flags = invocation_flags(ctx, &replaced, completer.mish, flag.range)?;
    let mut completions = vec![];
    for flag in flags {
        for text in flag
            .long
            .iter()
            .map(|l| format!("--{l}"))
            .chain(flag.short.map(|c| format!("-{c}")))
        {
            completions.push(Completion {
                description: flag.description.clone(),
                ..Completion::new(text)
            });
        }
    }
    CompletionResult::of(completions, minus.range)
}
