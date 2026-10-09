//! `transpose`: swap the nesting of lists of lists, lists of objects, objects of lists and objects of objects.

use mash_macros::builtin;

use crate::eval::R;
use crate::function::Args;
use crate::value::{Object, Value};

/// Transpose a nested structure, such as a list of lists
///
/// Examples:
///   <mash>[["a", "b"], ["c", "d"], ["e", "f"]]</mash>
///   ╔═╤═╤═╗
///   ║#│0│1║
///   ╟─┼─┼─╢
///   ║0│a│b║
///   ║1│c│d║
///   ║2│e│f║
///   ╚═╧═╧═╝
///   <mash>[["a", "b"], ["c", "d"], ["e", "f"]] | transpose</mash>
///   ╔═╤═╤═╤═╗
///   ║#│0│1│2║
///   ╟─┼─┼─┼─╢
///   ║0│a│c│e║
///   ║1│b│d│f║
///   ╚═╧═╧═╧═╝
#[builtin(namespace = "collections", infer = super::inference::infer_transpose)]
pub(crate) fn transpose(
    args: &Args,
    #[param(
        "If true, skip gaps in lists where an entry isn't available; if false, use null (default false)",
        short = 's',
        boolean_flag
    )]
    skip_gaps: bool,
    #[param("Sequence or Object to transpose")] sequence: Value,
) -> R<Value> {
    match &sequence {
        Value::List(xs) => {
            let items = xs.snapshot();
            if items.iter().all(|x| matches!(x, Value::List(_))) {
                let lists: Vec<Vec<Value>> = items
                    .iter()
                    .map(|x| if let Value::List(l) = x { l.snapshot() } else { vec![] })
                    .collect();
                Ok(Value::list(
                    transpose_lists(lists, skip_gaps).into_iter().map(Value::list).collect(),
                ))
            } else if items.iter().all(|x| matches!(x, Value::Object(_))) {
                let objects: Vec<Object> = items
                    .iter()
                    .filter_map(|x| {
                        if let Value::Object(o) = x {
                            Some(o.clone())
                        } else {
                            None
                        }
                    })
                    .collect();
                Ok(Value::Object(transpose_list_of_objects(&objects, skip_gaps)))
            } else {
                Err(args.invalid("sequence", "List must contain either all Lists or Objects"))
            }
        }
        Value::Object(obj) => {
            let entries = obj.entries();
            if entries.iter().all(|(_, v)| matches!(v, Value::Object(_))) {
                Ok(Value::Object(transpose_object_of_objects(&entries)))
            } else if entries.iter().all(|(_, v)| matches!(v, Value::List(_))) {
                let fields: Vec<(Value, Vec<Value>)> = entries
                    .into_iter()
                    .map(|(k, v)| (k, if let Value::List(l) = v { l.snapshot() } else { vec![] }))
                    .collect();
                Ok(Value::list(
                    transpose_object_of_lists(fields)
                        .into_iter()
                        .map(Value::Object)
                        .collect(),
                ))
            } else {
                Err(args.invalid("sequence", "Object values must be either all Lists or Objects"))
            }
        }
        _ => Err(args.invalid("sequence", "Must be a List or Object")),
    }
}

fn transpose_lists(lists: Vec<Vec<Value>>, skip_gaps: bool) -> Vec<Vec<Value>> {
    let longest = lists.iter().map(Vec::len).max().unwrap_or(0);
    (0..longest)
        .map(|i| {
            lists
                .iter()
                .filter_map(|list| list.get(i).cloned().or_else(|| (!skip_gaps).then_some(Value::Null)))
                .collect()
        })
        .collect()
}

fn distinct_keys<'a>(objects: impl Iterator<Item = &'a Object>) -> Vec<Value> {
    let mut keys: Vec<Value> = vec![];
    for obj in objects {
        for k in obj.keys() {
            if !keys.contains(&k) {
                keys.push(k);
            }
        }
    }
    keys
}

fn transpose_list_of_objects(objects: &[Object], skip_gaps: bool) -> Object {
    let result = Object::new();
    for field in distinct_keys(objects.iter()) {
        let values: Vec<Value> = objects
            .iter()
            .filter_map(|o| o.get(&field).or_else(|| (!skip_gaps).then_some(Value::Null)))
            .collect();
        result.set(field, Value::list(values));
    }
    result
}

fn transpose_object_of_objects(entries: &[(Value, Value)]) -> Object {
    let inner: Vec<(Value, Object)> = entries
        .iter()
        .filter_map(|(k, v)| {
            if let Value::Object(o) = v {
                Some((k.clone(), o.clone()))
            } else {
                None
            }
        })
        .collect();
    let result = Object::new();
    for inner_field in distinct_keys(inner.iter().map(|(_, o)| o)) {
        let obj = Object::new();
        for (outer_field, inner_object) in &inner {
            if let Some(value) = inner_object.get(&inner_field) {
                obj.set(outer_field.clone(), value);
            }
        }
        result.set(inner_field, Value::Object(obj));
    }
    result
}

fn transpose_object_of_lists(fields: Vec<(Value, Vec<Value>)>) -> Vec<Object> {
    let longest = fields.iter().map(|(_, vs)| vs.len()).max().unwrap_or(0);
    (0..longest)
        .map(|i| {
            let obj = Object::new();
            for (field, values) in &fields {
                if let Some(value) = values.get(i) {
                    obj.set(field.clone(), value.clone());
                }
            }
            obj
        })
        .collect()
}
