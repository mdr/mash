//! Methods of `List` that aren't collection functions, and its static `new`.

use mash_macros::{builtin, builtin_method};

use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, Callable, Sequence};
use crate::value::{List, Object, Str, Value};

fn items(target: &Value) -> Vec<Value> {
    match target {
        Value::List(xs) => xs.snapshot(),
        _ => vec![],
    }
}

/// Append the given item to the end of this List
#[builtin_method(infer = super::inference::infer_append)]
pub(crate) fn append(target: &Value, #[param("An item to place at the end of the List")] item: Value) -> R<Value> {
    let mut items = items(target);
    items.push(item);
    Ok(Value::list(items))
}

/// Prepend the given item to the start of this List
#[builtin_method(infer = super::inference::infer_append)]
pub(crate) fn prepend(target: &Value, #[param("An item to place at the start of the List")] item: Value) -> R<Value> {
    let mut items = items(target);
    items.insert(0, item);
    Ok(Value::list(items))
}

/// Compute the cartesian product with another sequence
#[builtin_method]
pub(crate) fn cartesian_product(
    target: &Value,
    #[param("Other sequence in the product")] sequence: Sequence,
) -> R<Value> {
    let others = sequence.items();
    let mut pairs = vec![];
    for x in items(target) {
        for y in &others {
            pairs.push(Value::list(vec![x.clone(), y.clone()]));
        }
    }
    Ok(Value::list(pairs))
}

/// Compute the multiset intersection between this and another sequence
#[builtin_method(infer = super::inference::infer_intersect)]
pub(crate) fn intersect(
    target: &Value,
    #[param("Other sequence to intersect with this")] sequence: Sequence,
) -> R<Value> {
    let mut remaining = sequence.items();
    let mut result = vec![];
    for x in items(target) {
        if let Some(i) = remaining.iter().position(|y| *y == x) {
            remaining.remove(i);
            result.push(x);
        }
    }
    Ok(Value::list(result))
}

fn target_objects(target: &Value) -> R<Vec<Object>> {
    items(target)
        .into_iter()
        .map(|v| match v {
            Value::Object(o) => Ok(o),
            other => Err(MashError::new(format!(
                "Can only call this on a list of Objects, but a value was {}",
                other.type_name()
            ))),
        })
        .collect()
}

fn argument_objects(args: &Args, sequence: &Sequence) -> R<Vec<Object>> {
    sequence
        .items()
        .into_iter()
        .map(|v| match v {
            Value::Object(o) => Ok(o),
            other => Err(args.invalid(
                "sequence",
                &format!(
                    "Can only call this on a list of Objects, but a value was {}",
                    other.type_name()
                ),
            )),
        })
        .collect()
}

/// Fields of both objects (the second's win), without a class
fn merge(a: &Object, b: &Object) -> Object {
    let result = Object::new();
    for (k, v) in a.entries().into_iter().chain(b.entries()) {
        result.set(k, v);
    }
    result
}

/// Inner join with a list of other objects
#[builtin_method]
pub(crate) fn inner_join(
    ctx: &mut Ctx,
    args: &Args,
    target: &Value,
    #[param("A list of other Objects to join against")] sequence: Sequence,
    #[param("A predicate to join on")] on: Callable,
    #[param("Prefix to use for fields from this list of objects", flag, default = "")] prefix1: Str,
    #[param("Prefix to use for fields from the given list of objects", flag, default = "")] prefix2: Str,
) -> R<Value> {
    let objects1 = target_objects(target)?;
    let objects2 = argument_objects(args, &sequence)?;
    let mut result = vec![];
    for o1 in &objects1 {
        for o2 in &objects2 {
            let matched = on.call(ctx, vec![Value::Object(o1.clone()), Value::Object(o2.clone())])?;
            if matched.is_truthy() {
                let joined = merge(
                    &add_prefix(ctx, o1, prefix1.as_str())?,
                    &add_prefix(ctx, o2, prefix2.as_str())?,
                );
                result.push(Value::Object(joined));
            }
        }
    }
    Ok(Value::list(result))
}

fn add_prefix(ctx: &mut Ctx, obj: &Object, prefix: &str) -> R<Object> {
    let result = Object::new();
    for (k, v) in obj.entries() {
        let name = format!("{prefix}{}", ctx.stringify(&k)?);
        result.set_str(&name, v);
    }
    Ok(result)
}

/// Natural join with a list of other objects
#[builtin_method]
pub(crate) fn natural_join(
    args: &Args,
    target: &Value,
    #[param("A list of other Objects to join against")] sequence: Sequence,
) -> R<Value> {
    let objects1 = target_objects(target)?;
    let objects2 = argument_objects(args, &sequence)?;
    let fields = |objects: &[Object]| {
        let mut fields: Vec<Value> = vec![];
        for o in objects {
            for k in o.keys() {
                if !fields.contains(&k) {
                    fields.push(k);
                }
            }
        }
        fields
    };
    let fields2 = fields(&objects2);
    let common: Vec<Value> = fields(&objects1).into_iter().filter(|f| fields2.contains(f)).collect();
    if common.is_empty() {
        return Ok(Value::list(vec![]));
    }
    // Objects with every common field, grouped by the values of those fields (in order of first appearance)
    let group = |objects: &[Object]| {
        let mut groups: Vec<(Vec<Value>, Vec<Object>)> = vec![];
        for o in objects {
            let Some(key) = common.iter().map(|f| o.get(f)).collect::<Option<Vec<Value>>>() else {
                continue;
            };
            match groups.iter_mut().find(|(k, _)| *k == key) {
                Some((_, members)) => members.push(o.clone()),
                None => groups.push((key, vec![o.clone()])),
            }
        }
        groups
    };
    let groups2 = group(&objects2);
    let mut result = vec![];
    for (key, members1) in group(&objects1) {
        if let Some((_, members2)) = groups2.iter().find(|(k, _)| *k == key) {
            for o1 in &members1 {
                for o2 in members2 {
                    result.push(Value::Object(merge(o1, o2)));
                }
            }
        }
    }
    Ok(Value::list(result))
}

/// Construct a new List with the given elements
#[builtin(namespace = "collections.List", name = "new", infer = super::inference::infer_list_new)]
pub(crate) fn list_new(#[param("Elements of the list", variadic)] elements: List) -> R<Value> {
    Ok(Value::List(elements))
}
