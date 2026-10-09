//! `groupBy` and the `Group` class.

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
    let items = sequence.items();
    let mut by_key: IndexMap<Value, Vec<Value>> = IndexMap::new();
    for item in &items {
        let key = discriminator.call1(ctx, item.clone())?;
        by_key.entry(key).or_default().push(item.clone());
    }
    let null_key = match &include_null {
        Value::Bool(true) => Value::Null,
        other => other.clone(),
    };
    let all_key = match &all {
        Value::Bool(true) => Value::str(DEFAULT_ALL_KEY),
        other => other.clone(),
    };
    let select_all = |ctx: &mut Ctx, values: Vec<Value>| -> R<Vec<Value>> {
        match &select {
            Some(f) => values.into_iter().map(|v| f.call1(ctx, v)).collect(),
            None => Ok(values),
        }
    };
    let mut result: Vec<(Value, Vec<Value>)> = vec![];
    for (key, values) in by_key {
        if key.is_null() && !include_null.is_truthy() {
            continue;
        }
        let key = if key.is_null() { null_key.clone() } else { key };
        result.push((key, select_all(ctx, values)?));
    }
    if all.is_truthy() {
        result.push((all_key, select_all(ctx, items)?));
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
