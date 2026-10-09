//! General-purpose functions in `core`.

use mash_macros::builtin;

use crate::eval::{MashError, R};
use crate::value::{Str, Value};

/// Return the argument unchanged
///
/// Examples:
/// <mash>
///   identity 42 # 42
/// </mash>
#[builtin(namespace = "core")]
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
#[builtin(namespace = "core")]
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

/// Throw an exception with the given message
#[builtin(namespace = "core")]
pub fn error(#[param("Error message to produce")] message: Str) -> R<Value> {
    Err(MashError::new(message.as_str()))
}
