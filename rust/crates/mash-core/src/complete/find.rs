//! Finding things in trees and token streams for completion (Scala `InvocationFinder`,
//! `ContiguousRegionFinder`).

use mash_syntax::{SyntaxKind, TextRange, TextSize};

use super::tokenise;
use crate::ast::{Arg, E, ExprKind};

/// The first expression (deepest first, then left to right — as Scala's `AstNode.find`) satisfying `f`
pub(crate) fn find(e: &E, f: &mut dyn FnMut(&E) -> bool) -> Option<E> {
    let mut children = vec![];
    e.for_each_child(&mut |child| children.push(child.clone()));
    for child in &children {
        if let Some(found) = find(child, f) {
            return Some(found);
        }
    }
    f(e).then(|| e.clone())
}

/// All expressions satisfying `f`
pub(crate) fn find_all(e: &E, f: &mut dyn FnMut(&E) -> bool, found: &mut Vec<E>) {
    e.for_each_child(&mut |child| find_all(child, f, found));
    if f(e) {
        found.push(e.clone());
    }
}

/// An invocation, and the index of one of its arguments
#[derive(Debug, Clone)]
pub struct InvocationInfo {
    pub invocation: E,
    pub arg_pos: usize,
}

/// The invocation that has the literal at `literal` as an argument (positional, or a flag's value)
pub fn find_invocation_with_literal_arg(program: &E, literal: TextRange) -> Option<InvocationInfo> {
    find_invocation(program, &|arg| match arg {
        Arg::Positional(e) | Arg::LongFlag { value: Some(e), .. } => {
            e.range == Some(literal) && matches!(e.kind, ExprKind::String { .. } | ExprKind::Literal(_))
        }
        _ => false,
    })
}

/// The invocation that has the flag at `flag` as an argument
pub fn find_invocation_with_flag_arg(program: &E, flag: TextRange) -> Option<InvocationInfo> {
    find_invocation(program, &|arg| match arg {
        Arg::ShortFlags { range, .. } | Arg::LongFlag { range, .. } => range.is_some_and(|r| r.start() == flag.start()),
        Arg::Positional(_) => false,
    })
}

fn find_invocation(program: &E, is_arg: &dyn Fn(&Arg) -> bool) -> Option<InvocationInfo> {
    let mut arg_pos = 0;
    let invocation = find(program, &mut |e| match &e.kind {
        ExprKind::Invocation { args, .. } => match args.iter().position(is_arg) {
            Some(pos) => {
                arg_pos = pos;
                true
            }
            None => false,
        },
        _ => false,
    })?;
    Some(InvocationInfo { invocation, arg_pos })
}

/// Expand `region` to take in the tokens either side of it up to whitespace (and, unless `liberal`, up to
/// brackets and `=`) — Scala `ContiguousRegionFinder`
pub fn contiguous_region(text: &str, region: TextRange, mish: bool, liberal: bool) -> TextRange {
    use SyntaxKind::*;
    let tokens = tokenise(text, mish);
    let keep = |kind: SyntaxKind| {
        kind != WHITESPACE
            && (liberal
                || !matches!(
                    kind,
                    LPAREN
                        | LPAREN_INVOKE
                        | RPAREN
                        | LBRACE
                        | RBRACE
                        | LSQUARE
                        | LSQUARE_LOOKUP
                        | RSQUARE
                        | MISH_INTERPOLATION_START
                        | SHORT_EQUALS
                ))
    };
    let containing = |pos: TextSize| {
        tokens
            .iter()
            .position(|t| t.range.start() <= pos && pos < t.range.end())
    };
    // Scala's `lastPos` of an empty region is the position before it
    let last = if region.is_empty() {
        region.start().checked_sub(1.into())
    } else {
        Some(region.end() - TextSize::from(1))
    };
    let (Some(mut start), Some(mut end)) = (containing(region.start()), last.and_then(containing)) else {
        return region;
    };
    while start > 0 && keep(tokens[start - 1].kind) {
        start -= 1;
    }
    while end + 1 < tokens.len() && keep(tokens[end + 1].kind) {
        end += 1;
    }
    TextRange::new(tokens[start].range.start(), tokens[end].range.end())
}
