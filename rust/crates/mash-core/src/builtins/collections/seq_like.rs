//! Sequence-like arguments: lists, strings (as sequences of characters) and objects (Scala
//! `SequenceLikeAnalyser` and `ToListHelper`).

use crate::eval::{Ctx, MemberLookup, R};
use crate::function::{Args, Sequence};
use crate::value::{Object, Str, Value};

/// A list, string or object argument (Scala `SequenceLike`)
pub(crate) enum SequenceLike {
    List(Vec<Value>),
    Str(Str),
    Object(Object),
}

/// Interpret a parameter as a list, string or object (Scala `SequenceLikeAnalyser`). An object with a
/// nullary `toList` method returning a list counts as that list (Scala `ToListHelper`).
pub(crate) fn sequence_like(ctx: &mut Ctx, args: &Args, name: &str) -> R<SequenceLike> {
    match args.value(name).clone() {
        Value::List(xs) => Ok(SequenceLike::List(xs.snapshot())),
        Value::Str(s) => Ok(SequenceLike::Str(s)),
        Value::Object(obj) => Ok(match try_to_list(ctx, &obj)? {
            Some(items) => SequenceLike::List(items),
            None => SequenceLike::Object(obj),
        }),
        other => Err(args.invalid(
            name,
            &format!("Must be a List, String, or Object, but was a {}", other.type_name()),
        )),
    }
}

/// The result of an object's `toList` method, if it has a nullary one that returns a list
fn try_to_list(ctx: &mut Ctx, obj: &Object) -> R<Option<Vec<Value>>> {
    let target = Value::Object(obj.clone());
    let Some(method) = ctx.lookup_member(&target, "toList", MemberLookup::DEFAULT) else {
        return Ok(None);
    };
    let nullary = matches!(method, Value::Function(_) | Value::Builtin(_) | Value::BoundMethod(_))
        && ctx.params_of(&method).is_some_and(|p| p.allows_nullary());
    if !nullary {
        return Ok(None);
    }
    Ok(match ctx.call_values(&method, vec![])? {
        Value::List(xs) => Some(xs.snapshot()),
        _ => None,
    })
}

/// A string's characters, as `Character`-tagged strings
pub(crate) fn characters(s: &Str) -> Vec<Value> {
    Sequence::Str(s.clone()).items()
}

/// Turn items that came from string `s` back into a string with `s`'s tag, if they are all strings;
/// otherwise a list (Scala `SequenceLike.String.reassemble`)
pub(crate) fn reassemble_string(s: &Str, items: Vec<Value>) -> Value {
    Sequence::Str(s.clone()).rebuild(items)
}

/// Rebuild a sequence of the same kind as `input` (Scala `WhereFunction.reassembleSequence`)
pub(crate) fn reassemble(input: &Value, items: Vec<Value>) -> Value {
    match input {
        Value::Str(s) => reassemble_string(s, items),
        _ => Value::list(items),
    }
}
