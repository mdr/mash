//! Invocation assistance: which function's calling syntax to show while the user types a call (Scala
//! `InvocationAssistance`).

use std::rc::Rc;

use mash_syntax::{SyntaxKind, TextRange, TextSize};

use super::find::find_all;
use super::{analyse, tokenise};
use crate::ast::{E, ExprKind};
use crate::eval::Ctx;
use crate::function::{BuiltinFunction, BuiltinMethod};
use crate::infer::{BuiltinFunctionType, Inferred, Type, UserClassType, UserFunctionType};

/// Something whose calling syntax can be shown
#[derive(Clone)]
pub enum Assistable {
    Function(&'static dyn BuiltinFunction),
    UserFunction(Rc<UserFunctionType>),
    Method(&'static dyn BuiltinMethod),
    UserMethod(Rc<UserFunctionType>),
    Constructor(Rc<UserClassType>),
}

impl std::fmt::Debug for Assistable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Assistable::Function(b) => write!(f, "Function({})", b.full_name()),
            Assistable::UserFunction(u) => write!(f, "UserFunction({:?})", u.name),
            Assistable::Method(m) => write!(f, "Method({})", m.name()),
            Assistable::UserMethod(u) => write!(f, "UserMethod({:?})", u.name),
            Assistable::Constructor(c) => write!(f, "Constructor({})", c.name),
        }
    }
}

/// What to show assistance for, with the cursor at byte offset `pos`
pub fn assistance(ctx: &mut Ctx, text: &str, pos: usize, mish: bool) -> Option<Assistable> {
    assistable(&type_of_nearest_function(ctx, text, pos, mish)?)
}

fn assistable(ty: &Type) -> Option<Assistable> {
    Some(match ty {
        Type::Seq(element) => return assistable(element),
        Type::BuiltinFunction(BuiltinFunctionType::Builtin(f)) => Assistable::Function(*f),
        Type::BuiltinFunction(BuiltinFunctionType::Constructor(c)) | Type::UserClass(c) => {
            Assistable::Constructor(c.clone())
        }
        Type::UserFunction(f) => Assistable::UserFunction(f.clone()),
        Type::BoundBuiltinMethod(_, m) => Assistable::Method(*m),
        Type::BoundUserMethod(_, m) => Assistable::UserMethod(m.clone()),
        _ => return None,
    })
}

/// The type of the function in the innermost call (or reference to a function) around the cursor
pub fn type_of_nearest_function(ctx: &mut Ctx, text: &str, pos: usize, mish: bool) -> Option<Type> {
    let pos = TextSize::from(pos as u32);
    let analysis = analyse(ctx, text, mish);
    let inferred = &analysis.inferred;
    let tokens = tokenise(text, mish);
    let text_end = TextSize::of(text);
    let contains = |e: &E| {
        e.range
            .is_some_and(|range| expanded(range, &tokens, text_end).contains(pos))
    };
    let mut candidates = vec![];
    find_all(
        &analysis.program.body,
        &mut |e| {
            let relevant = matches!(e.kind, ExprKind::Invocation { .. })
                || inferred.pre_invocation.contains_key(&e.id)
                || inferred.types.get(&e.id).is_some_and(is_function_type);
            relevant && contains(e)
        },
        &mut candidates,
    );
    let innermost = candidates
        .iter()
        .min_by_key(|e| e.range.map_or(u32::MAX, |r| u32::from(r.len())))?;
    function_type(inferred, innermost)
}

fn function_type(inferred: &Inferred, e: &E) -> Option<Type> {
    match &e.kind {
        ExprKind::Invocation { function, .. } => inferred.types.get(&function.id).cloned(),
        _ => inferred
            .pre_invocation
            .get(&e.id)
            .or_else(|| inferred.types.get(&e.id))
            .cloned(),
    }
}

fn is_function_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::BuiltinFunction(_) | Type::BoundBuiltinMethod(..) | Type::UserFunction(_) | Type::BoundUserMethod(..)
    )
}

/// A node's range extended over following whitespace and comments; at the end of the text, one past it (so
/// a cursor at the very end counts as inside)
fn expanded(range: TextRange, tokens: &[mash_syntax::lexer::Token], text_end: TextSize) -> TextRange {
    let mut end = range.end();
    for token in tokens.iter().filter(|t| t.range.start() >= range.end()) {
        if token.kind.is_trivia() || token.kind == SyntaxKind::ERROR_TOKEN && token.range.is_empty() {
            end = token.range.end();
        } else {
            return TextRange::new(range.start(), end);
        }
    }
    let end = if end == text_end { end + TextSize::from(1) } else { end };
    TextRange::new(range.start(), end)
}
