//! `joinBy`: joining two tables on keys (as Nushell's `join`), keeping unmatched rows if asked.

use std::collections::HashMap;

use mash_macros::builtin;

use super::Methodised;
use crate::eval::{Ctx, MashError, R};
use crate::function::Args;
use crate::infer::Type;
use crate::value::{Object, Value};

/// Join two lists of objects on keys
///
/// Each row of this table is joined with each row of the other whose key is equal (null keys match nothing),
/// giving an object with the fields of both: this row's, then the other's. Each key is a field name (a row
/// without the field has a null key) or a function of a row; the other table's key defaults to this one's.
///
/// By default only rows that match are kept (an inner join). --left also keeps this table's rows that match
/// nothing, --right the other's, and --outer both, with the fields of the other side null; the other table's
/// unmatched rows come at the end. Rows are in this table's order, each followed by its matches in the other
/// table's order.
///
/// A key field with the same name in both tables appears once. Another field of the other table whose name
/// this table has too gets a _ suffix (name_); or --prefix puts a prefix on all the other table's fields.
///
/// Examples:
/// <mash>
///   orders | joinBy customers 'customerId' 'id'
///   people | joinBy --left managers 'managerId' 'id' --prefix='manager_'
/// </mash>
#[builtin(namespace = "collections", name = "joinBy", returns = Type::seq(Type::OBJECT))]
#[allow(clippy::too_many_arguments)]
pub(crate) fn join_by(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "Also keep rows of this table that match nothing (default false)",
        short = 'l',
        boolean_flag
    )]
    left: bool,
    #[param(
        "Also keep rows of the other table that match nothing (default false)",
        short = 'r',
        boolean_flag
    )]
    right: bool,
    #[param(
        "Also keep rows of both tables that match nothing (default false)",
        short = 'o',
        boolean_flag
    )]
    outer: bool,
    #[param(
        "Prefix for all the other table's fields (default: a _ suffix for those whose names clash)",
        flag,
        mandatory_flag_value
    )]
    prefix: Option<Value>,
    #[param("The other table: a list of objects")] others: Value,
    #[param("Key of this table's rows: a field name, or a function of a row")] key: Value,
    #[param("Key of the other table's rows (default: the same as this table's)")] other_key: Option<Value>,
    #[param("This table: a list of objects")] table: Value,
) -> R<Value> {
    if [left, right, outer].iter().filter(|&&f| f).count() > 1 {
        return Err(MashError::new("Give only one of --left, --right and --outer"));
    }
    let (keep_left, keep_right) = (left || outer, right || outer);
    let rows = objects(args, "table", &table)?;
    let other_rows = objects(args, "others", &others)?;
    let other_key = other_key.unwrap_or_else(|| key.clone());
    let prefix = match &prefix {
        Some(p) => Some(ctx.stringify(p)?),
        None => None,
    };
    // A key field with the same name in both tables appears once
    let shared_key = match (&key, &other_key) {
        (Value::Str(a), Value::Str(b)) if a.as_str() == b.as_str() => Some(a.as_str().to_string()),
        _ => None,
    };

    let fields = |ctx: &mut Ctx, rows: &[Object]| -> R<Vec<String>> {
        let mut names: Vec<String> = vec![];
        for row in rows {
            for k in row.keys() {
                let name = ctx.stringify(&k)?;
                if !names.contains(&name) {
                    names.push(name);
                }
            }
        }
        Ok(names)
    };
    let left_fields = fields(ctx, &rows)?;
    // The other table's fields, and what they're called in the result
    let mut taken: Vec<String> = left_fields.clone();
    let mut right_fields: Vec<(String, String)> = vec![];
    for name in fields(ctx, &other_rows)? {
        if shared_key.as_deref() == Some(name.as_str()) {
            continue;
        }
        let mut new_name = match &prefix {
            Some(p) => format!("{p}{name}"),
            None => name.clone(),
        };
        while taken.contains(&new_name) {
            new_name.push('_');
        }
        taken.push(new_name.clone());
        right_fields.push((name, new_name));
    }

    let key_of = |ctx: &mut Ctx, key: &Value, row: &Object| -> R<Value> {
        match key {
            Value::Str(name) => Ok(row.get_str(name.as_str()).unwrap_or(Value::Null)),
            f => ctx.call_values(f, vec![Value::Object(row.clone())]),
        }
    };
    // The other table's rows by key
    #[allow(clippy::mutable_key_type)]
    let mut by_key: HashMap<Value, Vec<usize>> = HashMap::new();
    for (i, row) in other_rows.iter().enumerate() {
        let k = key_of(ctx, &other_key, row)?;
        if !k.is_null() {
            by_key.entry(k).or_default().push(i);
        }
    }

    let joined = |left: Option<&Object>, right: Option<&Object>| -> Object {
        let result = Object::new();
        match left {
            Some(row) => {
                for (k, v) in row.entries() {
                    result.set(k, v);
                }
            }
            None => {
                for name in &left_fields {
                    result.set_str(name, Value::Null);
                }
            }
        }
        for (name, new_name) in &right_fields {
            let value = right.and_then(|row| row.get_str(name)).unwrap_or(Value::Null);
            result.set_str(new_name, value);
        }
        // The shared key, for a row from the other table alone
        if let (None, Some(row), Some(shared)) = (left, right, &shared_key) {
            result.set_str(shared, row.get_str(shared).unwrap_or(Value::Null));
        }
        result
    };

    let mut matched_right = vec![false; other_rows.len()];
    let mut result = vec![];
    for row in &rows {
        let k = key_of(ctx, &key, row)?;
        let matches = if k.is_null() { None } else { by_key.get(&k) };
        match matches {
            Some(indexes) => {
                for &i in indexes {
                    matched_right[i] = true;
                    result.push(Value::Object(joined(Some(row), Some(&other_rows[i]))));
                }
            }
            None if keep_left => result.push(Value::Object(joined(Some(row), None))),
            None => {}
        }
    }
    if keep_right {
        for (i, row) in other_rows.iter().enumerate() {
            if !matched_right[i] {
                result.push(Value::Object(joined(None, Some(row))));
            }
        }
    }
    Ok(Value::list(result))
}

pub(crate) static JOIN_BY_METHOD: Methodised = Methodised::new(&JoinByFunction, "table", &[], false);

fn objects(args: &Args, param: &str, value: &Value) -> R<Vec<Object>> {
    let Value::List(xs) = value else {
        return Err(args.invalid(
            param,
            &format!("Must be a List of objects, but was a {}", value.type_name()),
        ));
    };
    xs.snapshot()
        .into_iter()
        .map(|x| match x {
            Value::Object(o) => Ok(o),
            other => Err(args.invalid(
                param,
                &format!("Must be a List of objects, but contained a {}", other.type_name()),
            )),
        })
        .collect()
}
