//! Changing the fields of an object, or of each of a list of objects: `rename`, `update` and `default` (as
//! Nushell's commands of those names).

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
        f if is_function(f) => Renaming::Function(f.clone()),
        other => {
            return Err(args.invalid(
                "renaming",
                &format!("Must be an Object or a Function, but was a {}", other.type_name()),
            ));
        }
    };
    let objects = objects_of(args, &target)?;
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

/// Change the value of fields of an object, or of each of a list of objects
///
/// Give a field's name and either a function, which is given the field's value and returns its new one, or
/// a new value; or give an object of names and their functions or values, to change several fields at once.
/// A function isn't called on null: the field stays null (filling nulls is a different job). An object
/// without the field is left as it is, but it's an error if none of them has it. Fields keep their places;
/// the results are plain objects, as their fields may no longer suit their class.
///
/// Examples:
/// <mash>
///   !{ps aux} | parseTable | update 'PID' (.toNumber)
///   !{ps aux} | parseTable | update { PID: (.toNumber), '%CPU': (.toNumber) }
///   csv.read "orders.csv" | update 'placed' (.toDateTime)
///   tasks | update 'status' 'done'
/// </mash>
#[builtin(namespace = "collections", infer = infer_update)]
pub(crate) fn update(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Name of the field to change, or an object of names and their changes")] field: Value,
    #[param("A function from the field's value to its new one, or the new value (with a field name)")] change: Option<
        Value,
    >,
    #[param("An object, or a sequence of objects, whose fields to change")] target: Value,
) -> R<Value> {
    let changes: Vec<(String, Value)> = match (&field, change) {
        (Value::Object(o), None) => {
            let mut changes = vec![];
            for (name, change) in o.entries() {
                changes.push((ctx.stringify(&name)?, change));
            }
            changes
        }
        (Value::Object(_), Some(_)) => {
            return Err(args.invalid("change", "Not needed when the fields to change are given as an object"));
        }
        (Value::Str(name), Some(change)) => vec![(name.as_str().to_string(), change)],
        (Value::Str(name), None) => {
            return Err(args.invalid(
                "change",
                &format!("Give a function or a new value for '{}'", name.as_str()),
            ));
        }
        (other, _) => {
            return Err(args.invalid(
                "field",
                &format!("Must be a String or an Object, but was a {}", other.type_name()),
            ));
        }
    };
    let objects = objects_of(args, &target)?;
    if !objects.is_empty() {
        for (name, _) in &changes {
            if !objects.iter().any(|o| o.get_str(name).is_some()) {
                return Err(MashError::new(format!("No field '{name}' to update")));
            }
        }
    }
    let mut updated = vec![];
    for object in &objects {
        let result = Object::new();
        for (key, value) in object.entries() {
            let name = ctx.stringify(&key)?;
            let value = match changes.iter().find(|(n, _)| *n == name) {
                Some((_, change)) if is_function(change) => {
                    if value.is_null() {
                        value
                    } else {
                        ctx.call_values(change, vec![value])?
                    }
                }
                Some((_, new_value)) => new_value.clone(),
                None => value,
            };
            result.set(key, value);
        }
        updated.push(Value::Object(result));
    }
    Ok(match target {
        Value::Object(_) => updated.pop().unwrap_or(Value::Null),
        _ => Value::list(updated),
    })
}

/// Fill in missing values of fields of an object, or of each of a list of objects
///
/// Give a field's name and its default value, or an object of names and default values to fill in several at
/// once. A field that's null gets the default value, and so does one that's missing, which is added at the
/// end. With --empty, empty strings, lists and objects are filled in too (as reading CSV gives an empty string
/// for a blank value). The results are plain objects.
///
/// Examples:
/// <mash>
///   json.read "users.json" | default { role: 'user', active: true }
///   !{df -h} | parseTable | default 'Mounted on' '-'
///   csv.read "orders.csv" | default --empty 'country' 'unknown'
/// </mash>
#[builtin(namespace = "collections", infer = infer_default)]
pub(crate) fn default(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "Also fill in empty strings, lists and objects (default false)",
        short = 'e',
        boolean_flag
    )]
    empty: bool,
    #[param("Name of the field to fill in, or an object of names and their default values")] field: Value,
    #[param("The default value (with a field name)")] value: Option<Value>,
    #[param("An object, or a sequence of objects, whose fields to fill in")] target: Value,
) -> R<Value> {
    let defaults: Vec<(String, Value)> = match (&field, value) {
        (Value::Object(o), None) => {
            let mut defaults = vec![];
            for (name, value) in o.entries() {
                defaults.push((ctx.stringify(&name)?, value));
            }
            defaults
        }
        (Value::Object(_), Some(_)) => {
            return Err(args.invalid("value", "Not needed when the fields to fill in are given as an object"));
        }
        (Value::Str(name), Some(value)) => vec![(name.as_str().to_string(), value)],
        (Value::Str(name), None) => {
            return Err(args.invalid("value", &format!("Give a default value for '{}'", name.as_str())));
        }
        (other, _) => {
            return Err(args.invalid(
                "field",
                &format!("Must be a String or an Object, but was a {}", other.type_name()),
            ));
        }
    };
    let missing = |value: &Value| match value {
        Value::Null => true,
        Value::Str(s) => empty && s.as_str().is_empty(),
        Value::List(xs) => empty && xs.is_empty(),
        Value::Object(o) => empty && o.is_empty(),
        _ => false,
    };
    let objects = objects_of(args, &target)?;
    let mut filled = vec![];
    for object in &objects {
        let result = Object::new();
        for (key, value) in object.entries() {
            let name = ctx.stringify(&key)?;
            let value = match defaults.iter().find(|(n, _)| *n == name) {
                Some((_, default)) if missing(&value) => default.clone(),
                _ => value,
            };
            result.set(key, value);
        }
        for (name, default) in &defaults {
            if result.get_str(name).is_none() {
                result.set_str(name, default.clone());
            }
        }
        filled.push(Value::Object(result));
    }
    Ok(match target {
        Value::Object(_) => filled.pop().unwrap_or(Value::Null),
        _ => Value::list(filled),
    })
}

pub(crate) static DEFAULT_METHOD: Methodised = Methodised::new(&DefaultFunction, "target", &[], false);

/// The target's type, with the fields filled in added (of unknown type)
fn infer_default(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let target = bound.ty(params, "target")?;
    let added: Option<Vec<Rc<str>>> = match bound.value(params, "field") {
        Some(Value::Str(name)) => Some(vec![Rc::from(name.as_str())]),
        Some(Value::Object(o)) => o
            .entries()
            .into_iter()
            .map(|(name, _)| match name {
                Value::Str(name) => Some(Rc::from(name.as_str())),
                _ => None,
            })
            .collect(),
        _ => None,
    };
    let add_fields = |ty: &Type| -> Type {
        match (object_fields(ty), &added) {
            (Some(mut fields), Some(added)) => {
                for name in added {
                    match fields.iter_mut().find(|(n, _)| n == name) {
                        Some(field) => field.1 = Type::ANY,
                        None => fields.push((name.clone(), Type::ANY)),
                    }
                }
                Type::object(fields)
            }
            _ => Type::OBJECT,
        }
    };
    Some(match &target {
        Type::Seq(element) => Type::seq(add_fields(element)),
        other => add_fields(other),
    })
}

pub(crate) static UPDATE_METHOD: Methodised = Methodised::new(&UpdateFunction, "target", &[], false);

fn is_function(value: &Value) -> bool {
    matches!(value, Value::Function(_) | Value::Builtin(_) | Value::BoundMethod(_))
}

/// The objects of `target`: it, or each of a list of them
fn objects_of(args: &Args, target: &Value) -> R<Vec<Object>> {
    match target {
        Value::Object(o) => Ok(vec![o.clone()]),
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
            .collect(),
        other => Err(args.invalid(
            "target",
            &format!(
                "Must be an Object or a sequence of objects, but was a {}",
                other.type_name()
            ),
        )),
    }
}

/// The target's type, as plain objects; changed fields' types are unknown
fn infer_update(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let target = bound.ty(params, "target")?;
    let changed: Option<Vec<Rc<str>>> = match bound.value(params, "field") {
        Some(Value::Str(name)) => Some(vec![Rc::from(name.as_str())]),
        Some(Value::Object(o)) => o
            .entries()
            .into_iter()
            .map(|(name, _)| match name {
                Value::Str(name) => Some(Rc::from(name.as_str())),
                _ => None,
            })
            .collect(),
        _ => None,
    };
    let change_fields = |ty: &Type| -> Type {
        match (object_fields(ty), &changed) {
            (Some(fields), Some(changed)) => Type::object(fields.into_iter().map(|(name, ty)| {
                let ty = if changed.contains(&name) { Type::ANY } else { ty };
                (name, ty)
            })),
            _ => Type::OBJECT,
        }
    };
    Some(match &target {
        Type::Seq(element) => Type::seq(change_fields(element)),
        other => change_fields(other),
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
