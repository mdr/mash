//! Two-dimensional tables: a list of objects (or lists) as rows, or an object whose values are objects (or
//! lists) with a row per field (Scala `TwoDTableModelCreator`, `TwoDTableCommonRenderer` and
//! `TwoDTablePrinter`).
//!
//! ```text
//! ╔═╤═══╤═════╗
//! ║#│a  │b    ║
//! ╟─┼───┼─────╢
//! ║0│1  │hello║
//! ╚═╧═══╧═════╝
//! ```

use std::collections::HashSet;

use mash_core::builtins::collections::GROUP_CLASS;
use mash_core::eval::{Ctx, MemberLookup, R, is_a};
use mash_core::value::{ClassRef, Value};

use super::ViewConfig;
use super::boxes::BoxChars;
use super::columns::{ColumnRequest, allocate_columns};
use super::field::render_field;
use super::text::{self, ELLIPSIS, ellipsisise_styled, fit_to_width};
use crate::screen::{Dimensions, Style, StyledChar, StyledString, default_colours};

/// The header of the row label column
const ROW_LABEL_COLUMN_NAME: &str = "#";

/// How many rows are sampled to decide the columns
const COLUMN_SAMPLE_SIZE: usize = 50;

/// Whether `value` is shown as a 2D table: a non-empty list of objects or of lists, or a non-empty object
/// whose values are all lists, or all objects with mostly the same fields (Scala `isSuitableForTwoDTable`)
pub(crate) fn is_suitable(value: &Value) -> bool {
    match value {
        Value::List(xs) => {
            let items = xs.snapshot();
            !items.is_empty()
                && (items.iter().all(|x| matches!(x, Value::Object(_)))
                    || items.iter().all(|x| matches!(x, Value::List(_))))
        }
        Value::Object(o) => {
            let values = o.values();
            !values.is_empty()
                && ((values.iter().all(|x| matches!(x, Value::Object(_))) && schemas_are_similar(&values))
                    || values.iter().all(|x| matches!(x, Value::List(_))))
        }
        _ => false,
    }
}

/// More than three quarters of the fields of the first ten objects are shared by all of them
fn schemas_are_similar(values: &[Value]) -> bool {
    let field_sets: Vec<HashSet<Value>> = values
        .iter()
        .filter_map(|v| match v {
            Value::Object(o) => Some(o.keys().into_iter().collect()),
            _ => None,
        })
        .take(10)
        .collect();
    let all_fields: HashSet<&Value> = field_sets.iter().flatten().collect();
    let shared_count = all_fields
        .iter()
        .filter(|field| field_sets.iter().all(|set| set.contains(**field)))
        .count();
    !all_fields.is_empty() && shared_count as f64 / all_fields.len() as f64 > 0.75
}

/// How a column gets its value from a row.
#[derive(Clone, Debug)]
enum Fetch {
    /// A member (a field, or a method bound to the row, which is called if `invoke` is set)
    Member { member: Value, invoke: bool },
    /// An element of a list
    Index(usize),
}

impl Fetch {
    fn member(name: &str) -> Fetch {
        Fetch::Member {
            member: Value::str(name),
            invoke: false,
        }
    }

    fn name(&self, ctx: &mut Ctx) -> R<String> {
        match self {
            Fetch::Member { member, .. } => ctx.stringify(member),
            Fetch::Index(i) => Ok(i.to_string()),
        }
    }

    fn lookup(&self, ctx: &mut Ctx, row: &Value) -> R<Option<Value>> {
        match self {
            Fetch::Member { member, invoke } => {
                let found = match (row, member) {
                    (_, Value::Str(name)) => ctx.lookup_member(row, name.as_str(), MemberLookup::DEFAULT),
                    (Value::Object(o), key) => o.get(key),
                    _ => None,
                };
                match found {
                    Some(value) if *invoke => Ok(Some(ctx.invoke_nullary(value, None)?)),
                    found => Ok(found),
                }
            }
            Fetch::Index(i) => Ok(match row {
                Value::List(xs) => xs.snapshot().get(*i).cloned(),
                _ => None,
            }),
        }
    }
}

#[derive(Clone, Debug)]
struct ColumnSpec {
    fetch: Fetch,
    weight: f64,
}

impl ColumnSpec {
    fn new(fetch: Fetch, weight: f64) -> ColumnSpec {
        ColumnSpec { fetch, weight }
    }
}

/// A table ready to draw: the first column is the row labels.
#[derive(Debug)]
struct TableModel {
    names: Vec<String>,
    widths: Vec<usize>,
    /// The rendered cells of each row, starting with its label
    rows: Vec<Vec<String>>,
}

/// The lines of a 2D table of `value` (which must be [`is_suitable`])
pub(crate) fn table_lines(ctx: &mut Ctx, value: &Value, size: Dimensions, config: &ViewConfig) -> R<Vec<StyledString>> {
    let model = create_model(ctx, value, size, config)?;
    Ok(render(&model, size))
}

fn create_model(ctx: &mut Ctx, value: &Value, size: Dimensions, config: &ViewConfig) -> R<TableModel> {
    // Each row's label and value
    let row_infos: Vec<(String, Value)> = match value {
        Value::List(xs) => xs
            .snapshot()
            .into_iter()
            .enumerate()
            .map(|(i, x)| (i.to_string(), x))
            .collect(),
        Value::Object(o) => o
            .entries()
            .into_iter()
            .map(|(k, v)| Ok((render_field(ctx, &k, true, config)?, v)))
            .collect::<R<_>>()?,
        _ => vec![],
    };
    let row_values: Vec<Value> = row_infos.iter().map(|(_, v)| v.clone()).collect();
    let specs = column_specs(&row_values);

    let mut rows = vec![];
    for (label, row) in &row_infos {
        let mut cells = vec![label.clone()];
        for spec in &specs {
            cells.push(match spec.fetch.lookup(ctx, row)? {
                Some(cell) => render_field(ctx, &cell, true, config)?,
                None => String::new(),
            });
        }
        rows.push(cells);
    }

    let mut names = vec![ROW_LABEL_COLUMN_NAME.to_string()];
    for spec in &specs {
        names.push(spec.fetch.name(ctx)?);
    }
    let requests: Vec<ColumnRequest> = specs
        .iter()
        .enumerate()
        .map(|(i, spec)| {
            let widest_cell = rows.iter().map(|cells| text::width(&cells[i + 1])).max().unwrap_or(0);
            ColumnRequest {
                weight: spec.weight,
                requested: widest_cell.max(text::width(&names[i + 1])),
            }
        })
        .collect();
    let row_label_width = rows.iter().map(|cells| text::width(&cells[0])).max().unwrap_or(0);
    // The table's sides and the borders between columns
    let borders = 1 + specs.len() + 1;
    let available = size.columns.saturating_sub(row_label_width + borders);
    let mut widths = vec![row_label_width];
    widths.extend(allocate_columns(&requests, available));
    Ok(TableModel { names, widths, rows })
}

fn column_specs(rows: &[Value]) -> Vec<ColumnSpec> {
    let sample = &rows[..rows.len().min(COLUMN_SAMPLE_SIZE)];
    if sample.is_empty() {
        return vec![];
    }
    let group_class = ClassRef::Builtin(&GROUP_CLASS);
    if sample.iter().all(|v| is_a(v, &group_class)) {
        return vec![
            ColumnSpec::new(Fetch::member("key"), 10.0),
            ColumnSpec::new(
                Fetch::Member {
                    member: Value::str("count"),
                    invoke: true,
                },
                3.0,
            ),
            ColumnSpec::new(Fetch::member("values"), 1.0),
        ];
    }
    // (git commits get their own columns when git is ported)
    if !sample.iter().all(|v| matches!(v, Value::Object(_) | Value::List(_))) {
        return vec![];
    }
    let mut fields: Vec<Value> = vec![];
    let mut seen = HashSet::new();
    let mut max_list_size = 0;
    for row in sample {
        match row {
            Value::Object(o) => {
                for key in o.keys() {
                    if seen.insert(key.clone()) {
                        fields.push(key);
                    }
                }
            }
            Value::List(xs) => max_list_size = max_list_size.max(xs.len()),
            _ => {}
        }
    }
    let field_specs = fields.into_iter().map(|member| Fetch::Member { member, invoke: false });
    let index_specs = (0..max_list_size).map(Fetch::Index);
    field_specs
        .chain(index_specs)
        .map(|fetch| ColumnSpec::new(fetch, 1.0))
        .collect()
}

fn label_style() -> Style {
    Style::fg(default_colours::YELLOW)
}

fn render(model: &TableModel, size: Dimensions) -> Vec<StyledString> {
    let b = BoxChars::UNICODE;
    let mut lines = vec![
        border_row(
            model,
            size,
            b.double_top_left,
            b.double_horizontal,
            b.double_horizontal_single_down,
            b.double_top_right,
        ),
        header_row(model, size),
        border_row(
            model,
            size,
            b.double_vertical_single_right,
            b.single_horizontal,
            b.single_intersect,
            b.double_vertical_single_left,
        ),
    ];
    for cells in &model.rows {
        lines.push(data_row(model, cells, size));
    }
    lines.push(border_row(
        model,
        size,
        b.double_bottom_left,
        b.double_horizontal,
        b.double_horizontal_single_up,
        b.double_bottom_right,
    ));
    lines
}

fn border_row(
    model: &TableModel,
    size: Dimensions,
    first: char,
    internal: char,
    divider: char,
    last: char,
) -> StyledString {
    let segments: Vec<String> = model.widths.iter().map(|&w| internal.to_string().repeat(w)).collect();
    let line = format!("{first}{}{last}", segments.join(&divider.to_string()));
    truncate_if_required(
        StyledString::plain(&line),
        size,
        StyledChar::new(internal, Style::DEFAULT),
    )
}

fn header_row(model: &TableModel, size: Dimensions) -> StyledString {
    let cells = model
        .names
        .iter()
        .zip(&model.widths)
        .map(|(name, &width)| label_style().apply(&fit_to_width(name, width)));
    row_line(cells, size)
}

fn data_row(model: &TableModel, cells: &[String], size: Dimensions) -> StyledString {
    let cells = cells.iter().zip(&model.widths).enumerate().map(|(i, (cell, &width))| {
        let style = if i == 0 { label_style() } else { Style::DEFAULT };
        style.apply(&fit_to_width(cell, width))
    });
    row_line(cells, size)
}

/// Cells between the table's sides, separated by single lines
fn row_line(cells: impl Iterator<Item = StyledString>, size: Dimensions) -> StyledString {
    let b = BoxChars::UNICODE;
    let side = StyledString::plain(&b.double_vertical.to_string());
    let line = side.clone() + &StyledString::join(cells, &StyledString::plain(&b.single_vertical.to_string())) + &side;
    truncate_if_required(line, size, StyledChar::new(ELLIPSIS, Style::DEFAULT))
}

/// A line wider than the terminal is cut short, keeping its last character (the table's side)
fn truncate_if_required(line: StyledString, size: Dimensions, ellipsis: StyledChar) -> StyledString {
    let columns = size.columns;
    if line.width() <= columns {
        return line;
    }
    if columns < 3 {
        return ellipsisise_styled(&line, columns, ellipsis);
    }
    let n = line.len();
    let middle = StyledString::from_iter(line.chars[1..n - 1].iter().copied());
    let mut result = StyledString::from_iter([line.chars[0]]);
    result.append(&ellipsisise_styled(&middle, columns - 2, ellipsis));
    result.push(line.chars[n - 1]);
    result
}
