//! Displaying values: the result of each command is printed as a table, an object, text lines, help, or a
//! plain value (Scala `Viewer` and the `view.printer` package), or, if it's too large for the screen (or
//! the user asks with `view.browse`), shown in the object browser ([`crate::browser`]).

pub mod boxes;
pub mod calling_syntax;
pub mod columns;
pub mod field;
mod git_status;
pub mod help;
pub mod highlight;
pub(crate) mod object;
pub(crate) mod table;
mod text;
pub(crate) mod tree;

use std::collections::BTreeSet;

use mash_core::builtins::git::STATUS_CLASS;
use mash_core::builtins::view::{ViewDirectives, as_view};
use mash_core::eval::{Ctx, R};
use mash_core::value::Value;

use crate::browser::model::{self, DisplayModel};
use crate::browser::search::SearchState;
use crate::screen::{Dimensions, StyledString};

/// Options for viewing values (from `config.view`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ViewConfig {
    /// Show times relative to now ("3 hours ago")
    pub fuzzy_time: bool,
    /// Show output too large for the screen in the object browser
    pub browse_large_output: bool,
}

impl Default for ViewConfig {
    fn default() -> Self {
        ViewConfig {
            fuzzy_time: true,
            browse_large_output: true,
        }
    }
}

/// How a command's result is shown (Scala `ViewResult`).
#[derive(Clone, PartialEq, Debug)]
pub enum Viewed {
    /// Print these lines
    Print(Vec<StyledString>),
    /// Open the object browser
    Browse(DisplayModel),
}

/// How the object browser decorates a table: the mark column (if marking is supported) and marked rows, the
/// cursor, and search hits.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct Decorations<'a> {
    pub(crate) marked: Option<&'a BTreeSet<usize>>,
    pub(crate) current_row: Option<usize>,
    /// The selected column (0 is the row labels); None highlights the whole current row
    pub(crate) current_column: Option<usize>,
    pub(crate) search: Option<&'a SearchState>,
}

/// The lines to print for `value`, the result of a command, on a terminal of the given size. Unit (and lists
/// of units) print nothing.
///
/// * Lists of objects (or of lists), and objects whose values are similar objects (or lists), are tables.
/// * Lists of strings (and nulls) are a line each.
/// * Functions, bound methods, classes and help objects (from `help`/`?`) show their help.
/// * Other non-empty objects are a table of fields.
/// * `view.View` objects (from `view.tree`, `view.raw`, …) show their data as they direct.
/// * `git.Status` objects are shown like `git status` output.
/// * Anything else is shown as text.
///
/// Everything is printed, however large, and `view.browse` is ignored; see [`view_or_browse`] for the REPL.
pub fn view_value(ctx: &mut Ctx, value: &Value, size: Dimensions, config: &ViewConfig) -> R<Vec<StyledString>> {
    match view(ctx, value, size, config, ViewDirectives::default(), false)? {
        Viewed::Print(lines) => Ok(lines),
        Viewed::Browse(_) => unreachable!("browsing is off"),
    }
}

/// How to show `value`, the result of a command (Scala `Viewer.view`): as [`view_value`] prints it, unless
/// it's too large for the screen (and `config.browse_large_output` is set, and it isn't `view.print`ed), or
/// it's a `view.browse`, in which case it's shown in the object browser
pub fn view_or_browse(ctx: &mut Ctx, value: &Value, size: Dimensions, config: &ViewConfig) -> R<Viewed> {
    view(ctx, value, size, config, ViewDirectives::default(), true)
}

fn view(
    ctx: &mut Ctx,
    value: &Value,
    size: Dimensions,
    config: &ViewConfig,
    directives: ViewDirectives,
    can_browse: bool,
) -> R<Viewed> {
    let custom_views = !directives.disable_custom_views;
    // Whether output too large for the screen goes to the browser
    let browse_large = can_browse && config.browse_large_output && !directives.print;
    let too_big = |lines: usize| browse_large && lines >= size.rows;
    if can_browse && directives.use_browser {
        if directives.use_tree
            && let Some(tree) = model::tree_model(ctx, value, config)?
        {
            return Ok(Viewed::Browse(DisplayModel::Tree(tree)));
        }
        return Ok(Viewed::Browse(model::display_model(ctx, value, size, config)?));
    }
    if directives.use_tree {
        let lines = tree::tree_lines(ctx, value, size, config)?;
        if too_big(lines.len())
            && let Some(tree) = model::tree_model(ctx, value, config)?
        {
            return Ok(Viewed::Browse(DisplayModel::Tree(tree)));
        }
        return Ok(Viewed::Print(lines));
    }
    if table::is_suitable(value) {
        let rows = match value {
            Value::List(xs) => xs.len(),
            Value::Object(o) => o.len(),
            _ => 0,
        };
        // Three header rows and a footer
        if too_big(rows + 4) {
            let model = table::create_model(ctx, value, size, config, true, &[])?;
            return Ok(Viewed::Browse(DisplayModel::TwoDTable(model)));
        }
        return Ok(Viewed::Print(table::table_lines(ctx, value, size, config)?));
    }
    if let Value::List(xs) = value {
        let items = xs.snapshot();
        if !items.is_empty() && items.iter().all(|x| matches!(x, Value::Str(_) | Value::Null)) {
            if too_big(items.len()) {
                return Ok(Viewed::Browse(DisplayModel::TextLines(model::text_lines_model(
                    ctx, xs,
                )?)));
            }
            return Ok(Viewed::Print(items.iter().flat_map(text_lines).collect()));
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
        if too_big(lines.len())
            && let Some(help) = model::help_model(ctx, value)
        {
            return Ok(Viewed::Browse(DisplayModel::Help(help)));
        }
        return Ok(Viewed::Print(lines));
    }
    if let Some((data, directives)) = as_view(value) {
        return view(ctx, &data, size, config, directives, can_browse);
    }
    if let Value::Object(o) = value
        && custom_views
        && o.class().is_some_and(|c| c.is_builtin(&STATUS_CLASS))
    {
        return Ok(Viewed::Print(git_status::status_lines(o)));
    }
    let lines = match value {
        Value::Object(o) if !o.is_empty() => {
            // A header row and a footer
            if too_big(o.len() + 2) {
                let model = object::create_model(ctx, o, size, config, true)?;
                return Ok(Viewed::Browse(DisplayModel::SingleObject(model)));
            }
            object::object_lines(ctx, o, size, config)?
        }
        Value::Unit => vec![],
        // Scala Mash means not to print a list of units either (but its check never matches)
        Value::List(xs) if !xs.is_empty() && xs.snapshot().iter().all(|x| matches!(x, Value::Unit)) => vec![],
        _ => text::lines(&field::render_field(ctx, value, false, config)?)
            .map(StyledString::plain)
            .collect(),
    };
    Ok(Viewed::Print(lines))
}

/// The lines of a string (or `null`) in a list of text lines
fn text_lines(value: &Value) -> Vec<StyledString> {
    match value {
        Value::Str(s) => text::lines(s.as_str()).map(StyledString::plain).collect(),
        _ => vec![StyledString::plain("null")],
    }
}
