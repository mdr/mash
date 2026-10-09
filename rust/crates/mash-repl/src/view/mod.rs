//! Displaying values: the result of each command is printed as a table, an object, text lines, help, or a
//! plain value (Scala `Viewer` and the `view.printer` package).
//!
//! Output is always printed for now, however large; the object browser (for output too big for the screen,
//! and `view.browse`) arrives in M5.

pub mod boxes;
pub mod calling_syntax;
pub mod columns;
pub mod field;
pub mod help;
pub mod highlight;
mod object;
mod table;
mod text;
mod tree;

use mash_core::builtins::view::{ViewDirectives, as_view};
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
///
/// * Lists of objects (or of lists), and objects whose values are similar objects (or lists), are tables.
/// * Lists of strings (and nulls) are a line each.
/// * Functions, bound methods, classes and help objects (from `help`/`?`) show their help.
/// * Other non-empty objects are a table of fields.
/// * `view.View` objects (from `view.tree`, `view.raw`, …) show their data as they direct.
/// * Anything else is shown as text.
pub fn view_value(ctx: &mut Ctx, value: &Value, size: Dimensions, config: &ViewConfig) -> R<Vec<StyledString>> {
    view(ctx, value, size, config, ViewDirectives::default())
}

fn view(
    ctx: &mut Ctx,
    value: &Value,
    size: Dimensions,
    config: &ViewConfig,
    directives: ViewDirectives,
) -> R<Vec<StyledString>> {
    let custom_views = !directives.disable_custom_views;
    if directives.use_tree {
        return tree::tree_lines(ctx, value, size, config);
    }
    if table::is_suitable(value) {
        return table::table_lines(ctx, value, size, config);
    }
    if let Value::List(xs) = value {
        let items = xs.snapshot();
        if !items.is_empty() && items.iter().all(|x| matches!(x, Value::Str(_) | Value::Null)) {
            return Ok(items.iter().flat_map(text_lines).collect());
        }
    }
    let is_help = matches!(
        value,
        Value::Function(_) | Value::Builtin(_) | Value::BoundMethod(_) | Value::Class(_)
    ) || help::is_help_object(value);
    if custom_views
        && is_help
        && let Some(lines) = help::help_lines(value)
    {
        return Ok(lines);
    }
    if let Some((data, directives)) = as_view(value) {
        return view(ctx, &data, size, config, directives);
    }
    match value {
        Value::Object(o) if !o.is_empty() => object::object_lines(ctx, o, size, config),
        Value::Unit => Ok(vec![]),
        // Scala Mash means not to print a list of units either (but its check never matches)
        Value::List(xs) if !xs.is_empty() && xs.snapshot().iter().all(|x| matches!(x, Value::Unit)) => Ok(vec![]),
        _ => Ok(text::lines(&field::render_field(ctx, value, false, config)?)
            .map(StyledString::plain)
            .collect()),
    }
}

/// The lines of a string (or `null`) in a list of text lines
fn text_lines(value: &Value) -> Vec<StyledString> {
    match value {
        Value::Str(s) => text::lines(s.as_str()).map(StyledString::plain).collect(),
        _ => vec![StyledString::plain("null")],
    }
}
