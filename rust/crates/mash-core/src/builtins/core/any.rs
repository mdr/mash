//! Methods every value has (Scala `AnyClass`).

use mash_macros::builtin_method;

use crate::eval::{R, is_a, primary_class, render};
use crate::function::{BuiltinMethod, Sequence};
use crate::value::{ClassRef, Str, Value};

pub(super) const METHODS: &[&dyn BuiltinMethod] = &[
    &GetClassMethod,
    &InMethod,
    &IsAMethod,
    &IsNullMethod,
    &IsTruthyMethod,
    &ToStringMethod,
];

/// Check whether or not the given value is null
///
/// Examples:
/// <mash>
///   null.isNull # true
///   0.isNull    # false
/// </mash>
#[builtin_method(shy)]
fn is_null(target: &Value) -> R<bool> {
    Ok(target.is_null())
}

/// Check whether or not the given value is truthy
///
/// Examples:
/// <mash>
///   true.isTruthy  # true
///   false.isTruthy # false
///   null.isTruthy  # false
///   10.isTruthy    # true
/// </mash>
#[builtin_method(shy)]
fn is_truthy(target: &Value) -> R<bool> {
    Ok(target.is_truthy())
}

/// Return true if and only if this value is an instance of the given class
///
/// Examples:
/// <mash>
///   42.isA Number  # true
///   42.isA Boolean # false
///   42.isA Any     # true
/// </mash>
#[builtin_method(name = "isA", shy)]
fn is_a_(target: &Value, #[param("Class to check")] class: ClassRef) -> R<bool> {
    Ok(is_a(target, &class))
}

/// The class of this object
#[builtin_method(shy)]
fn get_class(target: &Value) -> R<ClassRef> {
    Ok(primary_class(target))
}

/// Represent this object as a string
///
/// Examples:
/// <mash>
///   42.toString   # "42"
///   null.toString # "null"
/// </mash>
#[builtin_method(shy)]
fn to_string(target: &Value) -> R<Value> {
    Ok(match target {
        Value::Str(_) => target.clone(),
        _ => Value::Str(Str::new(render(target))),
    })
}

/// Check whether this element is contained in a sequence
///
/// Examples:
/// <mash>
///   2.in [1, 2, 3] # true
///   4.in [1, 2, 3] # false
///   "i".in "team"  # false
/// </mash>
#[builtin_method(shy)]
fn in_(target: &Value, #[param("Sequence to check if element is contained in")] sequence: Sequence) -> R<bool> {
    Ok(sequence.items().iter().any(|item| item == target))
}
