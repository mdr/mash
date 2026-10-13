//! `groupBy`, `countBy` and the `Group` class.

use crate::infer::Type;
use indexmap::IndexMap;
use mash_macros::{builtin, builtin_method};

use crate::builtins::core::OBJECT_CLASS;
use crate::eval::Ctx;
use crate::eval::{MashError, R};
use crate::function::{Callable, Sequence};
use crate::value::{BuiltinClass, ClassRef, Field, List, Object, Value};

pub static GROUP_CLASS: BuiltinClass = BuiltinClass {
    summary: "A group of values with a common key, as produced by the groupBy function",
    fields: &[
        Field {
            name: "key",
            summary: "Key that each element of the group shares",
            ty: || Type::ANY,
        },
        Field {
            name: "values",
            summary: "Values of the group",
            ty: || Type::seq(Type::ANY),
        },
    ],
    methods: &[&CountMethod],
    static_methods: &[&GroupNewFunction],
    ..BuiltinClass::new("collections", "Group", Some(&OBJECT_CLASS))
};

fn group(key: Value, values: Vec<Value>) -> Value {
    let obj = Object::with_class(Some(ClassRef::Builtin(&GROUP_CLASS)));
    obj.set_str("key", key);
    obj.set_str("values", Value::list(values));
    Value::Object(obj)
}

/// The number of values in this group
#[builtin_method(returns = crate::infer::Type::NUMBER)]
pub(crate) fn count(target: &Value) -> R<usize> {
    match target {
        Value::Object(obj) => match obj.get_str("values") {
            Some(Value::List(xs)) => Ok(xs.len()),
            Some(other) => Err(MashError::new(format!(
                "Field 'values' should have type List, but was '{}'",
                other.type_name()
            ))),
            None => Err(MashError::new("No field 'values' found in object")),
        },
        other => Err(MashError::new(format!(
            "target is of type {}, not Object",
            other.type_name()
        ))),
    }
}

/// Construct a new Group object
#[builtin(namespace = "collections.Group", name = "new")]
pub(crate) fn group_new(
    #[param("Key that each element of the group shares")] key: Value,
    #[param("Values of the group")] values: Value,
) -> R<Value> {
    let obj = Object::with_class(Some(ClassRef::Builtin(&GROUP_CLASS)));
    obj.set_str("key", key);
    obj.set_str("values", values);
    Ok(Value::Object(obj))
}

const DEFAULT_ALL_KEY: &str = "All";

/// Group together the elements of a sequence sharing a common key
///
/// Collects groups from the input sequence, where a group is a subset of the sequence sharing the same key,
/// as determined by the given discriminator function. The groups are returned, by default, as an Object, with
/// a field per key, with a list of values as its value.
///
/// If the --groups flag is true, an list of Groups is returned instead, with a field for every group key.
///
/// Example:
///   <mash>groupBy first ["foo", "bar", "baz"]</mash>
///   ╔═╤═══╤═══╗
///   ║#│0  │1  ║
///   ╟─┼───┼───╢
///   ║f│foo│   ║
///   ║b│bar│baz║
///   ╚═╧═══╧═══╝
#[builtin(namespace = "collections", infer = super::inference::infer_group_by, completion = super::inference::complete_map_function)]
#[allow(clippy::too_many_arguments)]
pub(crate) fn group_by(
    ctx: &mut Ctx,
    #[param(
        "Output a List of Group objects (default false)",
        short = 'g',
        boolean_flag,
        description = "If true, output a list of Group objects, one for each group. If false, output a single\n  object with a field for each group."
    )]
    groups: bool,
    #[param(
        "Include a group containing all the results (default false)",
        short = 't',
        flag,
        default = false,
        flag_value_name = "key",
        description = "If true, include an additional group containing all the elements.\nIf false, this group is not included.\nIf a non-boolean argument is given, that is used as the key for the additional group.\nOtherwise, a default key of \"All\" is used. "
    )]
    all: Value,
    #[param(
        "Include groups that have null keys (default false)",
        short = 'n',
        flag,
        default = false,
        flag_value_name = "key",
        description = "If true, include a group with null keys, if any elements exist for such a group.\nIf false, exclude a group with a null key.\nIf a non-boolean argument is given, that will be used as the key for the null group instead of null."
    )]
    include_null: Value,
    #[param("Function to apply to elements of the sequence to determine a key")] discriminator: Callable,
    #[param("Sequence from which to form groups")] sequence: Sequence,
    #[param(
        "Function to apply to elements of the sequence to determine the final values of the output groups",
        flag,
        mandatory_flag_value
    )]
    select: Option<Callable>,
) -> R<Value> {
    let mut result = vec![];
    for (key, values) in keyed_groups(ctx, &discriminator, sequence.items(), &include_null, &all)? {
        let values = match &select {
            Some(f) => values.into_iter().map(|v| f.call1(ctx, v)).collect::<R<Vec<_>>>()?,
            None => values,
        };
        result.push((key, values));
    }
    Ok(if groups {
        Value::List(result.into_iter().map(|(k, vs)| group(k, vs)).collect::<List>())
    } else {
        let obj = Object::new();
        for (k, vs) in result {
            obj.set(k, Value::list(vs));
        }
        Value::Object(obj)
    })
}

/// The elements of `items` grouped by key, in the order each key is first seen. Null keys are left out unless
/// `include_null` is given: true, or another value to use as their key. `all` (true for the key "All", or another
/// value as the key) adds a group of everything. A key given for either that is also a key of the data is an
/// error, rather than one group silently replacing another. Shared by `groupBy` and `countBy`.
fn keyed_groups(
    ctx: &mut Ctx,
    discriminator: &Callable,
    items: Vec<Value>,
    include_null: &Value,
    all: &Value,
) -> R<Vec<(Value, Vec<Value>)>> {
    let mut by_key: IndexMap<Value, Vec<Value>> = IndexMap::new();
    for item in &items {
        let key = discriminator.call1(ctx, item.clone())?;
        by_key.entry(key).or_default().push(item.clone());
    }
    // A flag is off only when false: a falsy key (0, '') is still a key
    let given = |flag: &Value| !matches!(flag, Value::Bool(false));
    let clash = |flag: &str, key: &Value, ctx: &mut Ctx| -> R<MashError> {
        Ok(MashError::new(format!(
            "The {flag} key '{}' is also the key of a group: give another with {flag}=<key>",
            ctx.stringify(key)?
        )))
    };
    let mut result: Vec<(Value, Vec<Value>)> = vec![];
    let null_key = match include_null {
        Value::Bool(_) => Value::Null,
        other => other.clone(),
    };
    for (key, values) in by_key {
        if key.is_null() {
            if given(include_null) {
                result.push((null_key.clone(), values));
            }
        } else {
            result.push((key, values));
        }
    }
    if !null_key.is_null() && result.iter().filter(|(k, _)| k.mash_equals(&null_key)).count() > 1 {
        return Err(clash("--includeNull", &null_key, ctx)?);
    }
    if given(all) {
        let all_key = match all {
            Value::Bool(_) => Value::str(DEFAULT_ALL_KEY),
            other => other.clone(),
        };
        if result.iter().any(|(k, _)| k.mash_equals(&all_key)) {
            return Err(clash("--all", &all_key, ctx)?);
        }
        result.push((all_key, items));
    }
    Ok(result)
}

/// Count the elements of a sequence sharing a common key
///
/// Counts the elements of the input sequence by key, as determined by the given discriminator function. The
/// counts are returned, by default, as an Object, with a field per key (in the order each key is first seen),
/// with the number of elements as its value.
///
/// If the --groups flag is true, a list of objects with fields key and count is returned instead, one for each
/// key: a table, ready to sort or filter.
///
/// Examples:
/// <mash>
///   countBy identity ["a", "b", "a", "c", "a"]                    # { a: 3, b: 1, c: 1 }
///   "mississippi".countBy identity                                # { m: 1, i: 4, s: 4, p: 2 }
///   ls | countBy (.extension) --groups | sortBy (.count) --descending
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_count_by, completion = super::inference::complete_map_function)]
pub(crate) fn count_by(
    ctx: &mut Ctx,
    #[param(
        "Output a List of objects with key and count fields (default false)",
        short = 'g',
        boolean_flag,
        description = "If true, output a list of objects, one for each key, with fields key and count. If false,\n  output a single object with a field for each key."
    )]
    groups: bool,
    #[param(
        "Include a count of all the elements (default false)",
        short = 't',
        flag,
        default = false,
        flag_value_name = "key",
        description = "If true, include an additional count of all the elements.\nIf false, this count is not included.\nIf a non-boolean argument is given, that is used as its key.\nOtherwise, a default key of \"All\" is used. "
    )]
    all: Value,
    #[param(
        "Count elements that have null keys (default false)",
        short = 'n',
        flag,
        default = false,
        flag_value_name = "key",
        description = "If true, count the elements with null keys, if there are any.\nIf false, leave them out.\nIf a non-boolean argument is given, that will be used as their key instead of null."
    )]
    include_null: Value,
    #[param("Function to apply to elements of the sequence to determine a key")] discriminator: Callable,
    #[param("Sequence whose elements to count")] sequence: Sequence,
) -> R<Value> {
    let counts = keyed_groups(ctx, &discriminator, sequence.items(), &include_null, &all)?
        .into_iter()
        .map(|(key, values)| (key, Value::from(values.len())));
    Ok(if groups {
        Value::List(
            counts
                .map(|(key, count)| {
                    let row = Object::new();
                    row.set_str("key", key);
                    row.set_str("count", count);
                    Value::Object(row)
                })
                .collect::<List>(),
        )
    } else {
        let obj = Object::new();
        for (key, count) in counts {
            obj.set(key, count);
        }
        Value::Object(obj)
    })
}
