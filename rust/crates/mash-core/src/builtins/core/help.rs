//! Help for functions, methods, classes and fields (`foo?`).

use crate::eval::{Ctx, MashError, R};
use crate::value::Value;

/// The help object for a value (Scala `HelpCreator.getHelp`)
pub(crate) fn help_for(_ctx: &mut Ctx, value: &Value) -> R<Value> {
    Err(MashError::new(format!(
        "No help available for value of type {}",
        value.type_name()
    )))
}
