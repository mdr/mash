//! Nested lists and objects as a tree (Scala `ObjectTreeModelCreator`, `ObjectTreeCommonRenderer` and
//! `ObjectTreePrinter`), for `view.tree` and the object browser's tree view.
//!
//! ```text
//! ┌─ a: 1
//! ├─ b:
//! │  ├─ 1
//! │  └─ 2
//! └─ c: {}
//! ```

use mash_core::eval::{Ctx, R};
use mash_core::value::Value;

use super::ViewConfig;
use super::field::render_field;
use super::text::ellipsisise;
use crate::screen::{Colour, Dimensions, Style, StyledString, default_colours};

/// A value as a tree, with leaves and field names rendered as they would be in a table cell (Scala
/// `ObjectTreeNode`).
#[derive(Clone, PartialEq, Debug)]
pub enum TreeNode {
    Leaf(String, Value),
    List(Vec<TreeNode>, Value),
    Object(Vec<(String, TreeNode)>, Value),
}

impl TreeNode {
    pub fn raw_value(&self) -> &Value {
        match self {
            TreeNode::Leaf(_, value) | TreeNode::List(_, value) | TreeNode::Object(_, value) => value,
        }
    }

    /// An empty list or object
    pub(crate) fn is_empty_container(&self) -> bool {
        match self {
            TreeNode::List(items, _) => items.is_empty(),
            TreeNode::Object(fields, _) => fields.is_empty(),
            TreeNode::Leaf(..) => false,
        }
    }
}

/// One step on the way to a part of a tree (Scala `ObjectTreeChoice`).
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TreeChoice {
    /// The field of an object (by its rendered name)
    Field(String),
    /// The item of a list
    Index(usize),
    /// From a field to its label
    OntoFieldLabel,
    /// From a node to its (leaf or empty) value
    OntoValue,
}

/// A selectable part of a tree: the steps from the root (Scala `ObjectTreePath`).
#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct TreePath(pub Vec<TreeChoice>);

impl TreePath {
    pub fn with(&self, choice: TreeChoice) -> TreePath {
        let mut choices = self.0.clone();
        choices.push(choice);
        TreePath(choices)
    }

    pub fn descend_index(&self, index: usize) -> TreePath {
        self.with(TreeChoice::Index(index))
    }

    pub fn descend_field(&self, field: &str) -> TreePath {
        self.with(TreeChoice::Field(field.to_string()))
    }

    pub fn onto_value(&self) -> TreePath {
        self.with(TreeChoice::OntoValue)
    }

    pub fn onto_field_label(&self) -> TreePath {
        self.with(TreeChoice::OntoFieldLabel)
    }

    /// The path without its last step
    pub fn ascend(&self) -> TreePath {
        let mut choices = self.0.clone();
        choices.pop();
        TreePath(choices)
    }
}

/// The lines of a tree of `value`
pub(crate) fn tree_lines(ctx: &mut Ctx, value: &Value, size: Dimensions, config: &ViewConfig) -> R<Vec<StyledString>> {
    let root = create_tree(ctx, value, config)?;
    Ok(render_tree(&root, None, size))
}

/// The tree of `value` (Scala `ObjectTreeModelCreator`)
pub(crate) fn create_tree(ctx: &mut Ctx, value: &Value, config: &ViewConfig) -> R<TreeNode> {
    create_node(ctx, value, config, &mut vec![])
}

/// `stack` holds the containers being converted, so that a cycle becomes a leaf (`[…]` or `{…}` inside)
/// rather than an infinite tree
fn create_node(ctx: &mut Ctx, value: &Value, config: &ViewConfig, stack: &mut Vec<Value>) -> R<TreeNode> {
    let is_container = matches!(value, Value::List(_) | Value::Object(_));
    if is_container {
        let same = |other: &Value| match (value, other) {
            (Value::List(a), Value::List(b)) => a.ptr_eq(b),
            (Value::Object(a), Value::Object(b)) => a.ptr_eq(b),
            _ => false,
        };
        if stack.iter().any(same) {
            return Ok(TreeNode::Leaf(render_field(ctx, value, true, config)?, value.clone()));
        }
        stack.push(value.clone());
    }
    let node = match value {
        Value::List(xs) => TreeNode::List(
            xs.snapshot()
                .iter()
                .map(|x| create_node(ctx, x, config, stack))
                .collect::<R<_>>()?,
            value.clone(),
        ),
        Value::Object(o) => {
            let mut fields = vec![];
            for (k, v) in o.entries() {
                fields.push((
                    render_field(ctx, &k, true, config)?,
                    create_node(ctx, &v, config, stack)?,
                ));
            }
            TreeNode::Object(fields, value.clone())
        }
        _ => TreeNode::Leaf(render_field(ctx, value, true, config)?, value.clone()),
    };
    if is_container {
        stack.pop();
    }
    Ok(node)
}

/// The lines of a tree, with the part at `selection` (if any) highlighted
pub(crate) fn render_tree(root: &TreeNode, selection: Option<&TreePath>, size: Dimensions) -> Vec<StyledString> {
    let mut printer = Printer {
        lines: vec![],
        line: StyledString::new(),
        columns: size.columns,
        selection,
    };
    let path = TreePath::default();
    match root {
        TreeNode::List(items, _) if items.is_empty() => printer.println("[]"),
        TreeNode::Object(fields, _) if fields.is_empty() => printer.println("{}"),
        TreeNode::Leaf(value, _) => printer.println(value),
        TreeNode::List(items, _) => print_list(&mut printer, items, "", &path, true),
        TreeNode::Object(fields, _) => print_object(&mut printer, fields, "", &path, true),
    }
    printer.lines
}

struct Printer<'a> {
    lines: Vec<StyledString>,
    line: StyledString,
    columns: usize,
    selection: Option<&'a TreePath>,
}

impl Printer<'_> {
    fn print(&mut self, s: &str) {
        self.line.push_str(s, Style::DEFAULT);
    }

    /// Print text that's highlighted if `path` is selected
    fn print_part(&mut self, s: &str, path: &TreePath, foreground: Colour) {
        let style = Style {
            foreground,
            inverse: self.selection == Some(path),
            ..Style::DEFAULT
        };
        self.line.push_str(s, style);
    }

    /// Print a leaf value, cut short to fit on the line
    fn print_value(&mut self, s: &str, path: &TreePath) {
        let available = self.columns.saturating_sub(self.line.width());
        self.print_part(&ellipsisise(s, available), path, Colour::Default);
    }

    fn end_line(&mut self) {
        self.lines.push(std::mem::take(&mut self.line));
    }

    fn println(&mut self, s: &str) {
        self.print(s);
        self.end_line();
    }
}

fn print_list(printer: &mut Printer, nodes: &[TreeNode], prefix: &str, path: &TreePath, connect_up: bool) {
    for (index, node) in nodes.iter().enumerate() {
        let item_path = path.descend_index(index);
        let is_last = index == nodes.len() - 1;
        let (node_prefix, connector) = prefix_and_connector(index, nodes.len(), connect_up, prefix);
        printer.print(node_prefix);
        printer.print_part(connector, &item_path, Colour::Default);
        let nesting_prefix = format!("{prefix}{}", if is_last { "   " } else { "│  " });
        match node {
            TreeNode::Leaf(value, _) => {
                printer.print("─ ");
                printer.print_value(value, &item_path.onto_value());
                printer.end_line();
            }
            TreeNode::List(items, _) if items.is_empty() => {
                printer.print("─ ");
                printer.print_part("[]", &item_path.onto_value(), Colour::Default);
                printer.end_line();
            }
            TreeNode::Object(fields, _) if fields.is_empty() => {
                printer.print("─ ");
                printer.print_part("{}", &item_path.onto_value(), Colour::Default);
                printer.end_line();
            }
            TreeNode::List(items, _) => {
                printer.print("──");
                print_list(printer, items, &nesting_prefix, &item_path, false);
            }
            TreeNode::Object(fields, _) => {
                printer.print("──");
                print_object(printer, fields, &nesting_prefix, &item_path, false);
            }
        }
    }
}

fn print_object(printer: &mut Printer, fields: &[(String, TreeNode)], prefix: &str, path: &TreePath, connect_up: bool) {
    for (index, (field, node)) in fields.iter().enumerate() {
        let item_path = path.descend_field(field);
        let is_last = index == fields.len() - 1;
        let (node_prefix, connector) = prefix_and_connector(index, fields.len(), connect_up, prefix);
        printer.print(node_prefix);
        printer.print_part(connector, &item_path, Colour::Default);
        let field_path = item_path.onto_field_label();
        printer.print("─ ");
        printer.print_part(field, &field_path, default_colours::YELLOW);
        printer.print(":");
        let nesting_prefix = format!("{prefix}{}", if is_last { "   " } else { "│  " });
        match node {
            TreeNode::Leaf(value, _) => {
                printer.print(" ");
                printer.print_value(value, &field_path.onto_value());
                printer.end_line();
            }
            TreeNode::List(items, _) if items.is_empty() => {
                printer.print(" ");
                printer.print_part("[]", &field_path.onto_value(), Colour::Default);
                printer.end_line();
            }
            TreeNode::Object(fields, _) if fields.is_empty() => {
                printer.print(" ");
                printer.print_part("{}", &field_path.onto_value(), Colour::Default);
                printer.end_line();
            }
            TreeNode::List(items, _) => {
                printer.end_line();
                printer.print(&nesting_prefix);
                print_list(printer, items, &nesting_prefix, &field_path, true);
            }
            TreeNode::Object(fields, _) => {
                printer.end_line();
                printer.print(&nesting_prefix);
                print_object(printer, fields, &nesting_prefix, &field_path, true);
            }
        }
    }
}

/// What to print before the item at `index`: the nesting prefix (except for the first item, which continues
/// the current line) and the connector
fn prefix_and_connector(index: usize, count: usize, connect_up: bool, prefix: &str) -> (&str, &'static str) {
    let is_last = index == count - 1;
    if index > 0 {
        return (prefix, if is_last { "└" } else { "├" });
    }
    let connector = if prefix.is_empty() {
        if count > 1 { "┌" } else { "─" }
    } else if connect_up {
        if is_last { "└" } else { "├" }
    } else if is_last {
        "─"
    } else {
        "┬"
    };
    ("", connector)
}
