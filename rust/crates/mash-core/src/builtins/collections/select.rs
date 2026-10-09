//! `select` and `deselect`: choosing fields of objects.

use mash_macros::{builtin, builtin_method};

use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, Argument, Suspended};
use crate::value::{List, Object, Value};

/// Select members of an object or sequence of objects
///
/// If the input is a sequence, the selection is applied to every element and a
///   sequence of new objects is returned.
/// If the input is a single object, a single new object is returned with the selection applied.
///
/// The arguments are interpreted as follows:
///   1) A positional string argument -- the input object member with that name is copied to an output field with
///     the same name.
///   2) A flag with no argument -- the input member with the same name as the flag is copied to an output field with
///    the same name.
///   3) A flag with an argument -- the flag argument is applied as a function to the input object, and the result
///     is copied to an output field with the flag name.
///
/// The order of fields in the argument list is retained in the new output objects.
///
/// Examples:
/// <mash>
///   select "foo" { foo: 42, bar: 24 }              # { foo: 42 }
///   select --foo { foo: 42, bar: 24 }              # { foo: 42 }
///   select --baz=(_.foo * 2) { foo: 42, bar: 24 }  # { baz: 84 }
///   select "bar" "foo" { foo: 42, bar: 24 }        # { bar: 24, foo: 42 }
///   select --add --baz=(_.foo * 2) { foo: 42 }     # { foo: 42, baz: 84 }
///   select "foo" [{ foo: 42, bar: 24 }, { foo: 12, bar: 18 }] # [{ foo: 42 }, { foo: 12 }]
/// </mash>
#[builtin(namespace = "collections")]
pub(crate) fn select(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "Add the fields to the existing set of members, rather than replacing",
        short = 'a',
        flag,
        default = false
    )]
    add: bool,
    #[param("The selection of members to take from the object", all_args)] _selectors: Value,
    #[param("Either an object or sequence of objects to select fields from")] target: Value,
) -> R<Value> {
    do_select(ctx, args, &target, add)
}

/// Select members of an object or sequence of objects
///
/// If the input is a sequence, the selection is applied to every element and a
///   sequence of new objects is returned.
/// If the input is a single object, a single new object is returned with the selection applied.
///
/// The arguments are interpreted as follows:
///   1) A positional string argument -- the input object member with that name is copied to an output field with
///     the same name.
///   2) A flag with no argument -- the input member with the same name as the flag is copied to an output field with
///    the same name.
///   3) A flag with an argument -- the flag argument is applied as a function to the input object, and the result
///     is copied to an output field with the flag name.
///
/// The order of fields in the argument list is retained in the new output objects.
///
/// Examples:
/// <mash>
///   select "foo" { foo: 42, bar: 24 }              # { foo: 42 }
///   select --foo { foo: 42, bar: 24 }              # { foo: 42 }
///   select --baz=(_.foo * 2) { foo: 42, bar: 24 }  # { baz: 84 }
///   select "bar" "foo" { foo: 42, bar: 24 }        # { bar: 24, foo: 42 }
///   select --add --baz=(_.foo * 2) { foo: 42 }     # { foo: 42, baz: 84 }
///   select "foo" [{ foo: 42, bar: 24 }, { foo: 12, bar: 18 }] # [{ foo: 42 }, { foo: 12 }]
/// </mash>
#[builtin_method(name = "select")]
pub(crate) fn select_on_list(
    ctx: &mut Ctx,
    args: &Args,
    target: &Value,
    #[param(
        "Add the fields to the existing set of members, rather than replacing",
        short = 'a',
        flag,
        default = false
    )]
    add: bool,
    #[param("The selection of members to take from the object", all_args)] _selectors: Value,
) -> R<Value> {
    do_select(ctx, args, target, add)
}

fn do_select(ctx: &mut Ctx, args: &Args, target: &Value, add: bool) -> R<Value> {
    let mut selections: Vec<(String, Value)> = vec![];
    for argument in args.all_args() {
        let value = |v: &Suspended| match v {
            Suspended::Value(v) => v.clone(),
            Suspended::Expr { .. } => unreachable!("all-args arguments are resolved"),
        };
        match argument {
            Argument::Positional { value: v, location } => match value(v) {
                Value::Str(s) => selections.push((s.as_str().to_string(), Value::Str(s))),
                other => {
                    let message = format!("Positional arguments must be a String, but was a {}", other.type_name());
                    return Err(MashError::argument(message, location.clone()));
                }
            },
            Argument::ShortFlags { flags, location } => {
                if flags != &['a'] {
                    let flags: Vec<String> = flags.iter().map(|f| format!("-{f}")).collect();
                    let message = format!("Short flags not supported by select: {}", flags.join(", "));
                    return Err(MashError::argument(message, location.clone()));
                }
            }
            Argument::LongFlag { name, value: None, .. } => {
                if &**name != "add" {
                    selections.push((name.to_string(), Value::str(name)));
                }
            }
            Argument::LongFlag {
                name, value: Some(v), ..
            } => selections.push((name.to_string(), value(v))),
        }
    }
    match target {
        Value::List(xs) => {
            let mut results = vec![];
            for x in xs.snapshot() {
                results.push(select_from(ctx, &x, &selections, add)?);
            }
            Ok(Value::list(results))
        }
        x => select_from(ctx, x, &selections, add),
    }
}

fn select_from(ctx: &mut Ctx, target: &Value, selections: &[(String, Value)], add: bool) -> R<Value> {
    let result = match (add, target) {
        (true, Value::Object(obj)) => obj.copy_with_class(obj.class()),
        _ => Object::new(),
    };
    for (field, f) in selections {
        let value = ctx.call_values(f, vec![target.clone()])?;
        result.set_str(field, value);
    }
    Ok(Value::Object(result))
}

/// Remove fields from an object or sequence of objects
#[builtin(namespace = "collections")]
pub(crate) fn deselect(
    #[param("Fields from the object", variadic, at_least_one)] fields: List,
    #[param("Object or sequence of objects to remove fields from")] target: Value,
) -> R<Value> {
    let fields = fields.snapshot();
    let deselect = |value: Value| match value {
        Value::Object(obj) => {
            let result = Object::new();
            for (k, v) in obj.entries() {
                if !fields.contains(&k) {
                    result.set(k, v);
                }
            }
            Value::Object(result)
        }
        other => other,
    };
    Ok(match target {
        Value::List(xs) => Value::list(xs.snapshot().into_iter().map(deselect).collect()),
        other => deselect(other),
    })
}
