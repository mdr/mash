//! Sorting, minimum and maximum: `sort`, `sortBy`, `min`, `max`, `minBy`, `maxBy`.
//!
//! Comparators never call back into Mash: keys (attribute values, or strings for natural ordering) are
//! computed first, then sorted with the total order on values.

use std::cmp::Ordering;

use mash_macros::builtin;

use super::seq_like::{SequenceLike, characters, reassemble_string, sequence_like};
use crate::eval::{Ctx, R};
use crate::function::{Args, Callable, Param, Sequence};
use crate::value::{List, Object, Value, compare, compare_lists};

pub(crate) fn descending_param() -> Param {
    Param::named("descending")
        .short('d')
        .summary("Sort results from highest value to lowest")
        .boolean_flag()
}

pub(crate) fn natural_order_param() -> Param {
    Param::named("naturalOrder")
        .short('n')
        .summary("Use natural sort order ('alphanum') when comparing items")
        .boolean_flag()
}

/// Sort the elements of a sequence
///
/// If called on an object, the object fields are sorted.
///
/// Examples:
/// <mash>
///   sort [3, 1, 2]                                      # [1, 2, 3]
///   sort --descending [3, 1, 2]                         # [3, 2, 1]
///   sort --naturalOrder ["a2.txt", "a10.txt", "a1.txt"] # ["a1.txt", "a2.txt", "a10.txt"]
/// </mash>
#[builtin(namespace = "collections")]
pub(crate) fn sort(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = descending_param)] descending: bool,
    #[param(def = natural_order_param)] natural_order: bool,
    #[param("Sequence to sort")] sequence: Value,
) -> R<Value> {
    let _ = sequence;
    Ok(match sequence_like(ctx, args, "sequence")? {
        SequenceLike::List(items) => Value::list(sort_values(ctx, items, descending, natural_order)?),
        SequenceLike::Str(s) => {
            let sorted = sort_values(ctx, characters(&s), descending, natural_order)?;
            reassemble_string(&s, sorted)
        }
        SequenceLike::Object(obj) => {
            let entries = obj.entries();
            let keys: Vec<Value> = entries.iter().map(|(k, _)| k.clone()).collect();
            let order = sort_order(ctx, keys.iter().map(|k| vec![k.clone()]).collect(), natural_order)?;
            Value::Object(object_from(arrange(&entries, order, descending)))
        }
    })
}

/// Sort the elements of a sequence by one or more attributes
///
/// Examples:
/// <mash>
///   ["aa", "b", "ccc"] sortBy length                # ["b", "aa", "ccc"]
///   ["aa", "b", "ccc"] | sortBy length --descending # ["ccc", "aa", "b"]
///   ["az", "b", "aa"] | sortBy first last           # ["aa", "az", "b"]
/// </mash>
#[builtin(namespace = "collections")]
pub(crate) fn sort_by(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = descending_param)] descending: bool,
    #[param(def = natural_order_param)] natural_order: bool,
    #[param(
        "One or more functions to apply to elements to extract a value to compare",
        description = "If multiple attribute functions are supplied, elements are compared by the first attribute.\n  If they are equal, then the second attribute is used to order them, and so on.",
        variadic,
        at_least_one
    )]
    attributes: List,
    #[param("Sequence to sort")] sequence: Value,
) -> R<Value> {
    let _ = sequence;
    let attributes = attribute_functions(args, &attributes)?;
    Ok(match sequence_like(ctx, args, "sequence")? {
        SequenceLike::List(items) => {
            Value::list(sort_by_attributes(ctx, items, &attributes, descending, natural_order)?)
        }
        SequenceLike::Str(s) => {
            let sorted = sort_by_attributes(ctx, characters(&s), &attributes, descending, natural_order)?;
            reassemble_string(&s, sorted)
        }
        SequenceLike::Object(obj) => Value::Object(sort_object_by(ctx, &obj, &attributes, descending, natural_order)?),
    })
}

/// Validate the attribute functions of `sortBy` (strings, functions and bound methods; not characters of a
/// string)
pub(crate) fn attribute_functions(args: &Args, attributes: &List) -> R<Vec<Callable>> {
    attributes
        .snapshot()
        .into_iter()
        .map(|a| match a {
            Value::Str(_) | Value::Function(_) | Value::Builtin(_) | Value::BoundMethod(_) => Ok(Callable(a)),
            other => Err(args.invalid(
                "attributes",
                &format!("Must be a function, but was a {}", other.type_name()),
            )),
        })
        .collect()
}

fn sort_by_attributes(
    ctx: &mut Ctx,
    items: Vec<Value>,
    attributes: &[Callable],
    descending: bool,
    natural_order: bool,
) -> R<Vec<Value>> {
    let mut keys = Vec::with_capacity(items.len());
    for item in &items {
        let mut key = Vec::with_capacity(attributes.len());
        for attribute in attributes {
            key.push(attribute.call1(ctx, item.clone())?);
        }
        keys.push(key);
    }
    let order = sort_order(ctx, keys, natural_order)?;
    Ok(arrange(&items, order, descending))
}

/// Sort an object's fields by attributes of each field. An attribute function taking two arguments is given
/// the field name and value; otherwise just the name (Scala `ObjectClass.SortByMethod`).
pub(crate) fn sort_object_by(
    ctx: &mut Ctx,
    obj: &Object,
    attributes: &[Callable],
    descending: bool,
    natural_order: bool,
) -> R<Object> {
    let entries = obj.entries();
    let mut keys = Vec::with_capacity(entries.len());
    for (field, value) in &entries {
        let mut key = Vec::with_capacity(attributes.len());
        for attribute in attributes {
            let args = if attribute.accepts_positional(ctx, 2) {
                vec![field.clone(), value.clone()]
            } else {
                vec![field.clone()]
            };
            key.push(attribute.call(ctx, args)?);
        }
        keys.push(key);
    }
    let order = sort_order(ctx, keys, natural_order)?;
    Ok(object_from(arrange(&entries, order, descending)))
}

/// Sort values with the standard (or natural) ordering
pub(crate) fn sort_values(ctx: &mut Ctx, items: Vec<Value>, descending: bool, natural_order: bool) -> R<Vec<Value>> {
    let keys = items.iter().map(|v| vec![v.clone()]).collect();
    let order = sort_order(ctx, keys, natural_order)?;
    Ok(arrange(&items, order, descending))
}

/// The indexes of `keys` in sorted order (a stable sort). Natural ordering compares the keys' string forms.
fn sort_order(ctx: &mut Ctx, keys: Vec<Vec<Value>>, natural_order: bool) -> R<Vec<usize>> {
    let mut order: Vec<usize> = (0..keys.len()).collect();
    if natural_order {
        let mut strings = Vec::with_capacity(keys.len());
        for key in &keys {
            let mut parts = Vec::with_capacity(key.len());
            for v in key {
                parts.push(ctx.stringify(v)?);
            }
            strings.push(parts);
        }
        order.sort_by(|&a, &b| {
            let (xs, ys) = (&strings[a], &strings[b]);
            xs.iter()
                .zip(ys)
                .map(|(x, y)| natural_cmp(x, y))
                .find(|o| o.is_ne())
                .unwrap_or_else(|| xs.len().cmp(&ys.len()))
        });
    } else {
        let tz = ctx.time_zone();
        order.sort_by(|&a, &b| compare_lists(&keys[a], &keys[b], &tz));
    }
    Ok(order)
}

/// The items in `order`, reversed if `descending` (Scala sorts, then reverses)
fn arrange<T: Clone>(items: &[T], order: Vec<usize>, descending: bool) -> Vec<T> {
    let mut sorted: Vec<T> = order.into_iter().map(|i| items[i].clone()).collect();
    if descending {
        sorted.reverse();
    }
    sorted
}

fn object_from(entries: Vec<(Value, Value)>) -> Object {
    let obj = Object::new();
    for (k, v) in entries {
        obj.set(k, v);
    }
    obj
}

/// Natural ("alphanum") string ordering: runs of digits compare numerically, everything else character by
/// character.
pub(crate) fn natural_cmp(a: &str, b: &str) -> Ordering {
    let (a, b): (Vec<char>, Vec<char>) = (a.chars().collect(), b.chars().collect());
    let (mut i, mut j) = (0, 0);
    while i < a.len() && j < b.len() {
        if a[i].is_ascii_digit() && b[j].is_ascii_digit() {
            let start_a = i;
            while i < a.len() && a[i].is_ascii_digit() {
                i += 1;
            }
            let start_b = j;
            while j < b.len() && b[j].is_ascii_digit() {
                j += 1;
            }
            let digits_a: String = a[start_a..i].iter().collect();
            let digits_b: String = b[start_b..j].iter().collect();
            let (trimmed_a, trimmed_b) = (digits_a.trim_start_matches('0'), digits_b.trim_start_matches('0'));
            // Numbers differing only in leading zeros are equal (as in Scala Mash's comparator)
            let ordering = trimmed_a
                .len()
                .cmp(&trimmed_b.len())
                .then_with(|| trimmed_a.cmp(trimmed_b));
            if ordering.is_ne() {
                return ordering;
            }
        } else {
            match a[i].cmp(&b[j]) {
                Ordering::Equal => {
                    i += 1;
                    j += 1;
                }
                other => return other,
            }
        }
    }
    (a.len() - i).cmp(&(b.len() - j))
}

fn items_param() -> Param {
    Param::named("items").variadic().at_least_one().flatten()
}

fn default_param() -> Param {
    Param::named("default")
        .summary("Default value to return, if the items are empty")
        .default_value(|| Value::Null)
        .flag()
        .mandatory_flag_value()
}

/// The first extreme element (by `better`) among the non-null items, or `default` if there are none
fn extreme(ctx: &Ctx, items: Vec<Value>, default: Value, better: Ordering) -> Value {
    let tz = ctx.time_zone();
    let mut best: Option<Value> = None;
    for item in items.into_iter().filter(|v| !v.is_null()) {
        if best.as_ref().is_none_or(|b| compare(&item, b, &tz) == better) {
            best = Some(item);
        }
    }
    best.unwrap_or(default)
}

/// Find the largest element of a sequence
///
/// Examples:
/// <mash>
///   max [1, 2, 3]      # 3
///   max 1 2 3          # 3
///   max [] --default=0 # 0
/// </mash>
#[builtin(namespace = "collections")]
pub(crate) fn max(
    ctx: &mut Ctx,
    #[param(
        def = items_param,
        "Items to find the maximum of",
        description = "If a single argument is provided, it must be a sequence; the largest element of the sequence is returned.\nIf multiple arguments are provided, the largest argument is returned."
    )]
    items: Sequence,
    #[param(def = default_param)] default: Value,
) -> R<Value> {
    Ok(extreme(ctx, items.items(), default, Ordering::Greater))
}

/// Find the smallest element of a sequence
///
/// Examples:
/// <mash>
///   min [1, 2, 3]      # 1
///   min 1 2 3          # 1
///   min [] --default=0 # 0
/// </mash>
#[builtin(namespace = "collections")]
pub(crate) fn min(
    ctx: &mut Ctx,
    #[param(
        def = items_param,
        "Items to find the minimum of",
        description = "If a single argument is provided, it must be a sequence; the smallest element of the sequence is returned.\nIf multiple arguments are provided, the smallest argument is returned."
    )]
    items: Sequence,
    #[param(def = default_param)] default: Value,
) -> R<Value> {
    Ok(extreme(ctx, items.items(), default, Ordering::Less))
}

/// The first element whose attribute is the extreme (by `better`), ignoring null attributes
fn extreme_by(ctx: &mut Ctx, items: Vec<Value>, attribute: &Callable, default: Value, better: Ordering) -> R<Value> {
    let tz = ctx.time_zone();
    let mut best: Option<(Value, Value)> = None;
    for item in items {
        let value = attribute.call1(ctx, item.clone())?;
        if value.is_null() {
            continue;
        }
        if best.as_ref().is_none_or(|(_, b)| compare(&value, b, &tz) == better) {
            best = Some((item, value));
        }
    }
    Ok(best.map_or(default, |(item, _)| item))
}

/// Find the largest element of a sequence by an attribute
///
/// The given function is applied to each element of the input sequence to compute a value, and the element
/// with the largest value is returned. If there are multiple elements with the maximum value, the first is
/// returned.
///
/// Examples:
/// <mash>
///   maxBy length ["a", "bbb", "cc"] # "bbb"
/// </mash>
#[builtin(namespace = "collections")]
pub(crate) fn max_by(
    ctx: &mut Ctx,
    #[param("Function to extract a value to compare elements")] attribute: Callable,
    #[param("Sequence to find the maximum of")] sequence: Sequence,
    #[param(def = default_param)] default: Value,
) -> R<Value> {
    extreme_by(ctx, sequence.items(), &attribute, default, Ordering::Greater)
}

/// Find the smallest element of a sequence by an attribute
///
/// The given function is applied to each element of the input sequence to compute a value, and the element
/// with the smallest value is returned. If there are multiple elements with the minimum value, the first is
/// returned.
///
/// Examples:
/// <mash>
///   minBy length ["a", "bbb", "cc"] # "a"
/// </mash>
#[builtin(namespace = "collections")]
pub(crate) fn min_by(
    ctx: &mut Ctx,
    #[param("Function to extract a value to compare elements")] attribute: Callable,
    #[param("Sequence to find the minimum of")] sequence: Sequence,
    #[param(def = default_param)] default: Value,
) -> R<Value> {
    extreme_by(ctx, sequence.items(), &attribute, default, Ordering::Less)
}

#[cfg(test)]
mod tests {
    use super::natural_cmp;
    use std::cmp::Ordering::*;

    #[test]
    fn natural_ordering() {
        assert_eq!(natural_cmp("a2.txt", "a10.txt"), Less);
        assert_eq!(natural_cmp("a10", "a2"), Greater);
        assert_eq!(natural_cmp("a01", "a1"), Equal);
        assert_eq!(natural_cmp("a10b2", "a10b10"), Less);
        assert_eq!(natural_cmp("", "1"), Less);
        assert_eq!(natural_cmp("abc", "abd"), Less);
        assert_eq!(natural_cmp("a", "a1"), Less);
        assert_eq!(natural_cmp("x10y", "x10y"), Equal);
    }
}
