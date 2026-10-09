//! Nested lists and objects as a tree (Scala `ObjectTreeModelCreator`, `ObjectTreeCommonRenderer` and
//! `ObjectTreePrinter`), for `view.tree`.
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
use crate::screen::{Dimensions, Style, StyledString, default_colours};

/// A value as a tree, with leaves and field names rendered as they would be in a table cell.
#[derive(Debug)]
enum Node {
    Leaf(String),
    List(Vec<Node>),
    Object(Vec<(String, Node)>),
}

/// The lines of a tree of `value`
pub(crate) fn tree_lines(ctx: &mut Ctx, value: &Value, size: Dimensions, config: &ViewConfig) -> R<Vec<StyledString>> {
    let root = create_node(ctx, value, config, &mut vec![])?;
    let mut printer = Printer {
        lines: vec![],
        line: StyledString::new(),
        columns: size.columns,
    };
    match &root {
        Node::List(items) if items.is_empty() => printer.println("[]"),
        Node::Object(fields) if fields.is_empty() => printer.println("{}"),
        Node::Leaf(value) => printer.println(value),
        Node::List(items) => print_list(&mut printer, items, "", true),
        Node::Object(fields) => print_object(&mut printer, fields, "", true),
    }
    Ok(printer.lines)
}

/// `stack` holds the containers being converted, so that a cycle becomes a leaf (`[…]` or `{…}` inside)
/// rather than an infinite tree
fn create_node(ctx: &mut Ctx, value: &Value, config: &ViewConfig, stack: &mut Vec<Value>) -> R<Node> {
    let is_container = matches!(value, Value::List(_) | Value::Object(_));
    if is_container {
        let same = |other: &Value| match (value, other) {
            (Value::List(a), Value::List(b)) => a.ptr_eq(b),
            (Value::Object(a), Value::Object(b)) => a.ptr_eq(b),
            _ => false,
        };
        if stack.iter().any(same) {
            return Ok(Node::Leaf(render_field(ctx, value, true, config)?));
        }
        stack.push(value.clone());
    }
    let node = match value {
        Value::List(xs) => Node::List(
            xs.snapshot()
                .iter()
                .map(|x| create_node(ctx, x, config, stack))
                .collect::<R<_>>()?,
        ),
        Value::Object(o) => {
            let mut fields = vec![];
            for (k, v) in o.entries() {
                fields.push((
                    render_field(ctx, &k, true, config)?,
                    create_node(ctx, &v, config, stack)?,
                ));
            }
            Node::Object(fields)
        }
        _ => Node::Leaf(render_field(ctx, value, true, config)?),
    };
    if is_container {
        stack.pop();
    }
    Ok(node)
}

struct Printer {
    lines: Vec<StyledString>,
    line: StyledString,
    columns: usize,
}

impl Printer {
    fn print(&mut self, s: &str) {
        self.line.push_str(s, Style::DEFAULT);
    }

    fn print_field_name(&mut self, s: &str) {
        self.line.push_str(s, Style::fg(default_colours::YELLOW));
    }

    /// Print a leaf value, cut short to fit on the line
    fn print_value(&mut self, s: &str) {
        let available = self.columns.saturating_sub(self.line.width());
        self.print(&ellipsisise(s, available));
    }

    fn end_line(&mut self) {
        self.lines.push(std::mem::take(&mut self.line));
    }

    fn println(&mut self, s: &str) {
        self.print(s);
        self.end_line();
    }
}

fn print_list(printer: &mut Printer, nodes: &[Node], prefix: &str, connect_up: bool) {
    for (index, node) in nodes.iter().enumerate() {
        let is_last = index == nodes.len() - 1;
        let (node_prefix, connector) = prefix_and_connector(index, nodes.len(), connect_up, prefix);
        printer.print(node_prefix);
        printer.print(connector);
        let nesting_prefix = format!("{prefix}{}", if is_last { "   " } else { "│  " });
        match node {
            Node::Leaf(value) => {
                printer.print("─ ");
                printer.print_value(value);
                printer.end_line();
            }
            Node::List(items) if items.is_empty() => printer.println("─ []"),
            Node::Object(fields) if fields.is_empty() => printer.println("─ {}"),
            Node::List(items) => {
                printer.print("──");
                print_list(printer, items, &nesting_prefix, false);
            }
            Node::Object(fields) => {
                printer.print("──");
                print_object(printer, fields, &nesting_prefix, false);
            }
        }
    }
}

fn print_object(printer: &mut Printer, fields: &[(String, Node)], prefix: &str, connect_up: bool) {
    for (index, (field, node)) in fields.iter().enumerate() {
        let is_last = index == fields.len() - 1;
        let (node_prefix, connector) = prefix_and_connector(index, fields.len(), connect_up, prefix);
        printer.print(node_prefix);
        printer.print(connector);
        printer.print("─ ");
        printer.print_field_name(field);
        printer.print(":");
        let nesting_prefix = format!("{prefix}{}", if is_last { "   " } else { "│  " });
        match node {
            Node::Leaf(value) => {
                printer.print(" ");
                printer.print_value(value);
                printer.end_line();
            }
            Node::List(items) if items.is_empty() => printer.println(" []"),
            Node::Object(fields) if fields.is_empty() => printer.println(" {}"),
            Node::List(items) => {
                printer.end_line();
                printer.print(&nesting_prefix);
                print_list(printer, items, &nesting_prefix, true);
            }
            Node::Object(fields) => {
                printer.end_line();
                printer.print(&nesting_prefix);
                print_object(printer, fields, &nesting_prefix, true);
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
