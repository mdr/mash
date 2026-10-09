//! What the object browser shows for a value (Scala `DisplayModel` and its creators): a 2D table, a table
//! of an object's fields, a tree, text lines, help, or a single value.

use mash_core::eval::{Ctx, R};
use mash_core::value::{List, Value};

use crate::screen::{Dimensions, StyledString};
use crate::view::help::{HelpLink, help_value, help_with_links, is_help_object};
use crate::view::object::{self, SingleObjectTableModel};
use crate::view::table::{self, TwoDTableModel};
use crate::view::tree::{self, TreeNode};
use crate::view::{ViewConfig, field};

/// A value as the browser shows it.
#[derive(Clone, PartialEq, Debug)]
pub enum DisplayModel {
    TwoDTable(TwoDTableModel),
    SingleObject(SingleObjectTableModel),
    Tree(TreeModel),
    TextLines(TextLinesModel),
    Help(HelpModel),
    Value(ValueModel),
}

/// A list or object as a tree.
#[derive(Clone, PartialEq, Debug)]
pub struct TreeModel {
    pub root: TreeNode,
    pub raw_value: Value,
}

/// A list, a line per item.
#[derive(Clone, PartialEq, Debug)]
pub struct TextLinesModel {
    pub lines: Vec<String>,
    /// The list (as a value)
    pub raw_value: Value,
}

/// Help for a function, method, class or field, with links to other help.
#[derive(Clone, PartialEq, Debug)]
pub struct HelpModel {
    pub lines: Vec<StyledString>,
    pub links: Vec<HelpLink>,
    pub raw_value: Value,
}

/// Any other value, as text.
#[derive(Clone, PartialEq, Debug)]
pub struct ValueModel {
    pub rendered: String,
    pub raw_value: Value,
}

/// How the browser shows `value` (Scala `DisplayModel.getDisplayModel`): help for functions, classes and
/// help objects; a 2D table if suitable; a table of fields for other non-empty objects; a line per item for
/// lists; otherwise the value as text
pub fn display_model(ctx: &mut Ctx, value: &Value, size: Dimensions, config: &ViewConfig) -> R<DisplayModel> {
    let help = matches!(value, Value::Function(_) | Value::Builtin(_) | Value::Class(_)) || is_help_object(value);
    if help && let Some(model) = help_model(ctx, value) {
        return Ok(DisplayModel::Help(model));
    }
    if table::is_suitable(value) {
        return Ok(DisplayModel::TwoDTable(table::create_model(
            ctx,
            value,
            size,
            config,
            true,
            &[],
        )?));
    }
    match value {
        Value::Object(o) if !o.is_empty() => Ok(DisplayModel::SingleObject(object::create_model(
            ctx, o, size, config, true,
        )?)),
        Value::List(xs) => Ok(DisplayModel::TextLines(text_lines_model(ctx, xs)?)),
        _ => Ok(DisplayModel::Value(value_model(ctx, value, config)?)),
    }
}

/// The help for `value`, if it has any
pub(crate) fn help_model(ctx: &Ctx, value: &Value) -> Option<HelpModel> {
    let value = help_value(value);
    let (lines, links) = help_with_links(ctx, &value)?;
    Some(HelpModel {
        lines,
        links,
        raw_value: value,
    })
}

/// A tree of a list or object; None for other values, and for empty ones (which the tree browser can't show)
pub(crate) fn tree_model(ctx: &mut Ctx, value: &Value, config: &ViewConfig) -> R<Option<TreeModel>> {
    if !matches!(value, Value::List(_) | Value::Object(_)) {
        return Ok(None);
    }
    let root = tree::create_tree(ctx, value, config)?;
    Ok((!root.is_empty_container()).then(|| TreeModel {
        root,
        raw_value: value.clone(),
    }))
}

/// A line per item: its string form, with tabs expanded (Scala `TextLinesModelCreator`)
pub(crate) fn text_lines_model(ctx: &mut Ctx, list: &List) -> R<TextLinesModel> {
    let lines = list
        .snapshot()
        .iter()
        .map(|item| Ok(ctx.stringify(item)?.replace('\t', &" ".repeat(8))))
        .collect::<R<_>>()?;
    Ok(TextLinesModel {
        lines,
        raw_value: Value::List(list.clone()),
    })
}

/// The value as text (Scala `ValueModelCreator`)
pub(crate) fn value_model(ctx: &mut Ctx, value: &Value, config: &ViewConfig) -> R<ValueModel> {
    Ok(ValueModel {
        rendered: field::render_field(ctx, value, false, config)?,
        raw_value: value.clone(),
    })
}
