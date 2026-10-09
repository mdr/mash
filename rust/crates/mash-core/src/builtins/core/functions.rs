//! General-purpose functions in `core`.

use mash_macros::{builtin, builtin_method};

use crate::eval::{Ctx, MashError, R};
use crate::function::Callable;
use crate::value::{ClassRef, DateTime, Number, Object, Str, Value};

/// Return the argument unchanged
///
/// Examples:
/// <mash>
///   identity 42 # 42
/// </mash>
#[builtin(namespace = "core", infer = super::infer_rules::identity)]
pub fn identity(#[param("Item to return")] item: Value) -> R<Value> {
    Ok(item)
}

/// Logically negate the given item
///
/// The given argument is interpreted as truthy or falsey, then negated.
///
/// Examples:
/// <mash>
///   not true  # false
///   not false # true
///   not 0     # true
///   not 1     # false
/// </mash>
#[builtin(namespace = "core", returns = crate::infer::Type::BOOLEAN)]
pub fn not(#[param("Item to negate")] item: Value) -> R<bool> {
    Ok(!item.is_truthy())
}

/// Check whether or not the given argument is null
///
/// Examples:
/// <mash>
///   isNull null # true
///   isNull 0    # false
/// </mash>
#[builtin(namespace = "core")]
pub fn is_null(#[param("Value to test for nullness")] value: Value) -> R<bool> {
    Ok(value.is_null())
}

/// Print the given argument to standard output
#[builtin(namespace = "core", returns = crate::infer::Type::UNIT)]
pub fn print(ctx: &mut Ctx, #[param("Item to print")] item: Value) -> R<()> {
    let mut text = ctx.stringify(&item)?;
    text.push('\n');
    ctx.platform().write_output(&text);
    Ok(())
}

/// Throw an exception with the given message
#[builtin(namespace = "core")]
pub fn error(#[param("Error message to produce")] message: Str) -> R<Value> {
    Err(MashError::new(message.as_str()))
}

/// Value that represents no argument being provided, for use as sentinel values in default arguments
#[builtin(namespace = "core")]
pub fn no_arg() -> R<Value> {
    Ok(no_arg_value())
}

/// The value returned by `noArg`: an empty object of class `NoArg`
pub(crate) fn no_arg_value() -> Value {
    Value::Object(Object::with_class(Some(ClassRef::Builtin(&super::NO_ARG_CLASS))))
}

/// Parse the given string as a number
///
/// Examples:
/// <mash>
///   parseNumber "42" # 42
/// </mash>
#[builtin(namespace = "core", returns = crate::infer::Type::NUMBER)]
pub fn parse_number(#[param("String to parse as a number")] string: Str) -> R<Number> {
    Number::parse(string.as_str())
        .ok_or_else(|| MashError::new(format!("Could not parse '{}' as a number", string.as_str())))
}

/// Access the argument without triggering nullary evaluation
///
/// Examples:
/// <mash>
///   safe ls               # Returns the function value, rather than executing it
///   safe "string".reverse # Returns the bound method value, rather than executing it
/// </mash>
#[builtin(namespace = "core")]
pub fn safe(#[param(safe)] value: Value) -> R<Value> {
    Ok(value)
}

/// Apply a function to a value, ignore the result, and return the original value.
#[builtin(namespace = "core", returns = crate::infer::Type::BOOLEAN)]
pub fn tap(
    ctx: &mut Ctx,
    #[param("Function to apply to the item")] f: Callable,
    #[param("Value to tap")] value: Value,
) -> R<Value> {
    f.call1(ctx, value.clone())?;
    Ok(value)
}

/// Execute the given code, catching any exceptions
#[builtin(namespace = "core", name = "try", infer = super::infer_rules::try_)]
pub fn try_(
    ctx: &mut Ctx,
    #[param("Code to execute", lazy)] body: Value,
    #[param("Code to execute if an exception is thrown in the body", lazy)] catch: Option<Value>,
    #[param(
        "Code to execute after execution of the body, regardless of whether an exception is thrown or not",
        lazy
    )]
    finally: Option<Value>,
) -> R<Value> {
    let result = match ctx.call_values(&body, vec![]) {
        Err(error) if !error.is_interrupted() => match &catch {
            Some(catch) => ctx.call_values(catch, vec![]),
            None => Ok(Value::Unit),
        },
        other => other,
    };
    if let Some(finally) = &finally {
        ctx.call_values(finally, vec![])?;
    }
    result
}

/// Loop executing the given block while the condition remains true
#[builtin(namespace = "core", name = "while", returns = crate::infer::Type::UNIT)]
pub fn while_(
    ctx: &mut Ctx,
    #[param("Condition of the while loop", lazy)] condition: Value,
    #[param("Code to execute", lazy)] block: Value,
) -> R<()> {
    while ctx.call_values(&condition, vec![])?.is_truthy() {
        ctx.call_values(&block, vec![])?;
    }
    Ok(())
}

/// Measure how long it takes to execute a piece of code
///
/// Examples:
/// <mash>
///    timeTaken (ls --recursive) | .duration # 854
/// </mash>
#[builtin(namespace = "core", infer = super::infer_rules::time_taken)]
pub fn time_taken(ctx: &mut Ctx, #[param("Code to execute", lazy)] block: Value) -> R<Object> {
    let start = ctx.platform().now();
    let result = ctx.call_values(&block, vec![])?;
    let finish = ctx.platform().now();
    let millis = finish.duration_since(start).as_millis();
    let object = Object::with_class(Some(ClassRef::Builtin(&super::TIMED_RESULT_CLASS)));
    object.set_str("result", result);
    object.set_str("started", Value::DateTime(DateTime(start)));
    object.set_str(
        "duration",
        Value::Number(
            Number::new(millis as f64),
            Some(&crate::builtins::time::MILLISECONDS_CLASS),
        ),
    );
    Ok(object)
}

/// When the timed code finished
#[builtin_method(returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_TIME_CLASS))]
pub fn finished(target: Object) -> R<Value> {
    let started = match target.get_str("started") {
        Some(Value::DateTime(t)) => t.0,
        _ => return Err(MashError::new("Field 'started' must be a DateTime")),
    };
    let duration = target
        .get_str("duration")
        .and_then(|d| d.as_number())
        .and_then(|n| n.as_i64())
        .unwrap_or(0);
    let finished = started
        .checked_add(jiff::SignedDuration::from_millis(duration))
        .map_err(|e| MashError::new(e.to_string()))?;
    Ok(Value::DateTime(DateTime(finished)))
}
