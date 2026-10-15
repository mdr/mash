//! `rename`: renaming the fields of an object, or of each of a list of objects (as Nushell's `rename`).

use std::collections::HashSet;
use std::rc::Rc;

use mash_macros::builtin;

use super::Methodised;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, Params};
use crate::infer::{Inferencer, Type, TypedArgs, object_fields};
use crate::value::{Object, Value};

/// Rename fields of an object, or of each of a list of objects
///
/// The renaming is either an object giving the new name for each old one, or a function from a field's name
/// to its new name. Fields keep their places. An object without a field to rename is left as it is, but it's
/// an error if none of them has it (a mistyped name, most likely), or if a new name is one an object already
/// has.
///
/// Examples:
/// <mash>
///   rename { size: 'bytes' } (ls)
///   !{df -h} | parseTable | rename { 'Mounted on': 'mount', 'Use%': 'used' }
///   json.read "api.json" | rename (.toLower)
/// </mash>
#[builtin(namespace = "collections", infer = infer_rename)]
pub(crate) fn rename(
    ctx: &mut Ctx,
    args: &Args,
    #[param("The new name for each old one (an object), or a function from a name to its new name")] renaming: Value,
    #[param("An object, or a sequence of objects, whose fields to rename")] target: Value,
) -> R<Value> {
    let renaming = match &renaming {
        Value::Object(o) => {
            let mut pairs = vec![];
            for (old, new) in o.entries() {
                pairs.push((ctx.stringify(&old)?, ctx.stringify(&new)?));
            }
            Renaming::Names(pairs)
        }
        Value::Function(_) | Value::Builtin(_) | Value::BoundMethod(_) => Renaming::Function(renaming.clone()),
        other => {
            return Err(args.invalid(
                "renaming",
                &format!("Must be an Object or a Function, but was a {}", other.type_name()),
            ));
        }
    };
    let objects: Vec<Object> = match &target {
        Value::Object(o) => vec![o.clone()],
        Value::List(xs) => xs
            .snapshot()
            .into_iter()
            .map(|x| match x {
                Value::Object(o) => Ok(o),
                other => Err(args.invalid(
                    "target",
                    &format!("Must be a sequence of objects, but contained a {}", other.type_name()),
                )),
            })
            .collect::<R<_>>()?,
        other => {
            return Err(args.invalid(
                "target",
                &format!(
                    "Must be an Object or a sequence of objects, but was a {}",
                    other.type_name()
                ),
            ));
        }
    };
    if let Renaming::Names(pairs) = &renaming
        && !objects.is_empty()
    {
        for (old, _) in pairs {
            if !objects.iter().any(|o| o.get_str(old).is_some()) {
                return Err(MashError::new(format!("No field '{old}' to rename")));
            }
        }
    }
    let mut renamed = vec![];
    for object in &objects {
        renamed.push(Value::Object(rename_object(ctx, object, &renaming)?));
    }
    Ok(match target {
        Value::Object(_) => renamed.pop().unwrap_or(Value::Null),
        _ => Value::list(renamed),
    })
}

pub(crate) static RENAME_METHOD: Methodised = Methodised::new(&RenameFunction, "target", &[], false);

enum Renaming {
    /// Old name, new name
    Names(Vec<(String, String)>),
    Function(Value),
}

/// A new object with the fields of `object` renamed, in the same order
fn rename_object(ctx: &mut Ctx, object: &Object, renaming: &Renaming) -> R<Object> {
    let result = Object::new();
    let mut names: HashSet<String> = HashSet::new();
    for (key, value) in object.entries() {
        let name = ctx.stringify(&key)?;
        let new_name = match renaming {
            Renaming::Names(pairs) => pairs.iter().find(|(old, _)| *old == name).map(|(_, new)| new.clone()),
            Renaming::Function(f) => {
                let new_name = ctx.call_values(f, vec![Value::str(&name)])?;
                Some(ctx.stringify(&new_name)?)
            }
        };
        let (key, name) = match new_name {
            Some(new_name) => (Value::str(&new_name), new_name),
            None => (key, name),
        };
        if !names.insert(name.clone()) {
            return Err(MashError::new(format!(
                "Cannot rename to '{name}': there would be two fields of that name"
            )));
        }
        result.set(key, value);
    }
    Ok(result)
}

/// The target's type, with its fields renamed when both its fields and the renaming are known
fn infer_rename(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let target = bound.ty(params, "target")?;
    let pairs: Option<Vec<(Rc<str>, Rc<str>)>> = match bound.value(params, "renaming") {
        Some(Value::Object(o)) => o
            .entries()
            .into_iter()
            .map(|(old, new)| match (old, new) {
                (Value::Str(old), Value::Str(new)) => Some((Rc::from(old.as_str()), Rc::from(new.as_str()))),
                _ => None,
            })
            .collect(),
        _ => None,
    };
    let rename_fields = |ty: &Type| -> Type {
        match (object_fields(ty), &pairs) {
            (Some(fields), Some(pairs)) => Type::object(fields.into_iter().map(|(name, ty)| {
                let new = pairs.iter().find(|(old, _)| *old == name).map(|(_, new)| new.clone());
                (new.unwrap_or(name), ty)
            })),
            _ => Type::OBJECT,
        }
    };
    Some(match &target {
        Type::Seq(element) => Type::seq(rename_fields(element)),
        other => rename_fields(other),
    })
}
