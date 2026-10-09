//! Displaying values: the result of each command is printed as a table, an object, text lines, help, or a
//! plain value (Scala `Viewer` and the `view.printer` package).

use mash_core::eval::{Ctx, R};
use mash_core::value::Value;

use crate::screen::{Dimensions, StyledString};

/// Options for viewing values (from `config.view`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ViewConfig {
    /// Show times relative to now ("3 hours ago")
    pub fuzzy_time: bool,
}

impl Default for ViewConfig {
    fn default() -> Self {
        ViewConfig { fuzzy_time: true }
    }
}

/// The lines to print for `value`, the result of a command, on a terminal of the given size. Unit (and lists
/// of units) print nothing.
pub fn view_value(ctx: &mut Ctx, value: &Value, _size: Dimensions, _config: &ViewConfig) -> R<Vec<StyledString>> {
    // Placeholder until the printers are ported
    if matches!(value, Value::Unit) {
        return Ok(vec![]);
    }
    Ok(ctx.stringify(value)?.lines().map(StyledString::plain).collect())
}
