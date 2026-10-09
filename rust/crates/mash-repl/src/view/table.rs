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
use std::ops::Range;

use mash_core::builtins::collections::GROUP_CLASS;
use mash_core::builtins::git::COMMIT_CLASS;
use mash_core::eval::{Ctx, MemberLookup, R, is_a};
use mash_core::value::{ClassRef, Value};
use mash_syntax::expression_combiner::combine_safely;

use super::boxes::BoxChars;
use super::columns::{ColumnRequest, allocate_columns};
use super::field::render_field;
use super::text::{self, ELLIPSIS, ellipsisise_styled, fit_to_width};
use super::{Decorations, ViewConfig};
use crate::browser::safe_property;
use crate::screen::{Colour, Dimensions, Style, StyledChar, StyledString, default_colours};

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

/// How a column gets its value from a row (or a row from the table's value).
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Fetch {
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

    /// The member name or index, as a value
    pub(crate) fn value(&self) -> Value {
        match self {
            Fetch::Member { member, .. } => member.clone(),
            Fetch::Index(i) => Value::num(*i as f64),
        }
    }

    /// An expression for the value fetched from the value of `parent` (Scala `ValueFetch.fetchPath`)
    pub(crate) fn path(&self, parent: &str) -> String {
        match self {
            Fetch::Member { member, invoke } => safe_property(parent, member, *invoke),
            Fetch::Index(i) => combine_safely(parent, &format!("[{i}]")),
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

/// A table ready to draw (Scala `TwoDTableModel`): the first column is the row labels.
#[derive(Clone, PartialEq, Debug)]
pub struct TwoDTableModel {
    pub(crate) columns: Vec<Column>,
    pub(crate) rows: Vec<Row>,
    /// The list or object the table shows
    pub(crate) raw_value: Value,
}

/// A column of a table.
#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Column {
    pub(crate) name: String,
    pub(crate) width: usize,
    /// Which of the table's column specs this is (None for the row labels), for hiding columns
    pub(crate) id: Option<usize>,
    /// How cells get their values from rows (None for the row labels)
    pub(crate) fetch: Option<Fetch>,
}

/// A row of a table: a cell for each column.
#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Row {
    pub(crate) cells: Vec<Cell>,
    pub(crate) raw_value: Value,
    /// How the row is found in the table's value
    pub(crate) fetch: Fetch,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Cell {
    pub(crate) rendered: String,
    /// The value shown (the field name or index for a row label), if the row has one for this column
    pub(crate) raw_value: Option<Value>,
}

impl TwoDTableModel {
    /// The text of each row's cells, starting with its label
    pub fn rendered_rows(&self) -> Vec<Vec<String>> {
        self.rows
            .iter()
            .map(|row| row.cells.iter().map(|cell| cell.rendered.clone()).collect())
            .collect()
    }

    /// The columns' names, starting with the row labels'
    pub fn column_names(&self) -> Vec<String> {
        self.columns.iter().map(|column| column.name.clone()).collect()
    }

    pub fn number_of_rows(&self) -> usize {
        self.rows.len()
    }

    /// The columns' widths, starting with the row labels'
    pub fn column_widths(&self) -> Vec<usize> {
        self.columns.iter().map(|column| column.width).collect()
    }

    /// Including the row label column
    pub(crate) fn number_of_columns(&self) -> usize {
        self.columns.len()
    }
}

/// The lines of a 2D table of `value` (which must be [`is_suitable`])
pub(crate) fn table_lines(ctx: &mut Ctx, value: &Value, size: Dimensions, config: &ViewConfig) -> R<Vec<StyledString>> {
    let model = create_model(ctx, value, size, config, false, &[])?;
    Ok(render_table(
        &model,
        size,
        &Decorations::default(),
        0,
        model.number_of_rows(),
    ))
}

/// The model of a 2D table of `value` (which must be [`is_suitable`]). `support_marking` leaves room for the
/// object browser's mark column; `hidden` lists the ids of columns not to show.
pub(crate) fn create_model(
    ctx: &mut Ctx,
    value: &Value,
    size: Dimensions,
    config: &ViewConfig,
    support_marking: bool,
    hidden: &[usize],
) -> R<TwoDTableModel> {
    // Each row's label, value, and how it's found
    let row_infos: Vec<(String, Value, Fetch)> = match value {
        Value::List(xs) => xs
            .snapshot()
            .into_iter()
            .enumerate()
            .map(|(i, x)| (i.to_string(), x, Fetch::Index(i)))
            .collect(),
        Value::Object(o) => o
            .entries()
            .into_iter()
            .map(|(k, v)| {
                let label = render_field(ctx, &k, true, config)?;
                Ok((
                    label,
                    v,
                    Fetch::Member {
                        member: k,
                        invoke: false,
                    },
                ))
            })
            .collect::<R<_>>()?,
        _ => vec![],
    };
    let row_values: Vec<Value> = row_infos.iter().map(|(_, v, _)| v.clone()).collect();
    let all_specs = column_specs(&row_values);
    let specs: Vec<(usize, &ColumnSpec)> = all_specs
        .iter()
        .enumerate()
        .filter(|(id, _)| !hidden.contains(id))
        .collect();

    let mut rows = vec![];
    for (label, row, fetch) in row_infos {
        let mut cells = vec![Cell {
            rendered: label,
            raw_value: Some(fetch.value()),
        }];
        for (_, spec) in &specs {
            let raw_value = spec.fetch.lookup(ctx, &row)?;
            let rendered = match &raw_value {
                Some(cell) => render_field(ctx, cell, true, config)?,
                None => String::new(),
            };
            cells.push(Cell { rendered, raw_value });
        }
        rows.push(Row {
            cells,
            raw_value: row,
            fetch,
        });
    }

    let mut names = vec![];
    for (_, spec) in &specs {
        names.push(spec.fetch.name(ctx)?);
    }
    let requests: Vec<ColumnRequest> = specs
        .iter()
        .enumerate()
        .map(|(i, (_, spec))| {
            let widest_cell = rows
                .iter()
                .map(|row| text::width(&row.cells[i + 1].rendered))
                .max()
                .unwrap_or(0);
            ColumnRequest {
                weight: spec.weight,
                requested: widest_cell.max(text::width(&names[i])),
            }
        })
        .collect();
    let row_label_width = rows
        .iter()
        .map(|row| text::width(&row.cells[0].rendered))
        .max()
        .unwrap_or(0);
    // The table's sides and the borders between columns (counting hidden columns, as Scala Mash does), and
    // the mark column
    let borders = 1 + all_specs.len() + 1 + if support_marking { 2 } else { 0 };
    let available = size.columns.saturating_sub(row_label_width + borders);
    let widths = allocate_columns(&requests, available);
    let mut columns = vec![Column {
        name: ROW_LABEL_COLUMN_NAME.to_string(),
        width: row_label_width,
        id: None,
        fetch: None,
    }];
    for (((id, spec), name), width) in specs.iter().zip(names).zip(widths) {
        columns.push(Column {
            name,
            width,
            id: Some(*id),
            fetch: Some(spec.fetch.clone()),
        });
    }
    Ok(TwoDTableModel {
        columns,
        rows,
        raw_value: value.clone(),
    })
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
    let commit_class = ClassRef::Builtin(&COMMIT_CLASS);
    if sample.iter().all(|v| is_a(v, &commit_class)) {
        return vec![
            ColumnSpec::new(Fetch::member("hash"), 1.0),
            ColumnSpec::new(Fetch::member("commitTime"), 10.0),
            ColumnSpec::new(Fetch::member("author"), 10.0),
            ColumnSpec::new(Fetch::member("summary"), 3.0),
        ];
    }
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

/// The lines of a table showing `row_count` rows from `first_row`, with arrows on the borders if there are
/// more rows above or below (Scala `TwoDTableCommonRenderer`)
pub(crate) fn render_table(
    model: &TwoDTableModel,
    size: Dimensions,
    decorations: &Decorations,
    first_row: usize,
    row_count: usize,
) -> Vec<StyledString> {
    let renderer = Renderer {
        model,
        size,
        decorations,
    };
    let b = BoxChars::UNICODE;
    let mut lines = vec![
        renderer.border_row(
            [
                b.double_top_left,
                b.double_horizontal,
                b.double_horizontal_single_down,
                b.double_top_right,
            ],
            None,
        ),
        renderer.header_row(),
        renderer.border_row(
            [
                b.double_vertical_single_right,
                b.single_horizontal,
                b.single_intersect,
                b.double_vertical_single_left,
            ],
            (first_row > 0).then_some(UP_ARROW),
        ),
    ];
    let end = model.rows.len().min(first_row + row_count);
    for index in first_row.min(end)..end {
        lines.push(renderer.data_row(index));
    }
    lines.push(renderer.border_row(
        [
            b.double_bottom_left,
            b.double_horizontal,
            b.double_horizontal_single_up,
            b.double_bottom_right,
        ],
        (first_row + row_count < model.rows.len()).then_some(DOWN_ARROW),
    ));
    lines
}

/// Draws a table, decorated for the object browser.
struct Renderer<'a> {
    model: &'a TwoDTableModel,
    size: Dimensions,
    decorations: &'a Decorations<'a>,
}

impl Renderer<'_> {
    fn shows_marks(&self) -> bool {
        self.decorations.marked.is_some()
    }

    /// A border: its first, internal, divider and last characters, and any arrow for its middle
    fn border_row(&self, [first, internal, divider, last]: [char; 4], arrow: Option<char>) -> StyledString {
        let mut line = String::from(first);
        if self.shows_marks() {
            line.push(internal);
            line.push(divider);
        }
        let segments: Vec<String> = self
            .model
            .columns
            .iter()
            .map(|c| internal.to_string().repeat(c.width))
            .collect();
        line.push_str(&segments.join(&divider.to_string()));
        line.push(last);
        if let Some(arrow) = arrow {
            line = set_middle_char(&line, arrow);
        }
        truncate_if_required(
            StyledString::plain(&line),
            self.size,
            StyledChar::new(internal, Style::DEFAULT),
        )
    }

    fn header_row(&self) -> StyledString {
        let b = BoxChars::UNICODE;
        let mut line = StyledString::plain(&b.double_vertical.to_string());
        if self.shows_marks() {
            line.push_str(&format!(" {}", b.single_vertical), Style::DEFAULT);
        }
        let cells = self
            .model
            .columns
            .iter()
            .map(|column| label_style().apply(&fit_to_width(&column.name, column.width)));
        line.append(&StyledString::join(
            cells,
            &StyledString::plain(&b.single_vertical.to_string()),
        ));
        line.push_str(&b.double_vertical.to_string(), Style::DEFAULT);
        truncate_if_required(line, self.size, StyledChar::new(ELLIPSIS, Style::DEFAULT))
    }

    fn data_row(&self, index: usize) -> StyledString {
        let b = BoxChars::UNICODE;
        let decorations = self.decorations;
        let is_cursor_row = decorations.current_row == Some(index);
        let row_style = cell_style(is_cursor_row && decorations.current_column.is_none(), false, false);
        let mut line = StyledString::plain(&b.double_vertical.to_string());
        if let Some(marked) = decorations.marked {
            let mark = if marked.contains(&index) { '◈' } else { ' ' };
            line.push_str(&format!("{mark}{}", b.single_vertical), row_style);
        }
        let row = &self.model.rows[index];
        let cells = self.model.columns.iter().enumerate().map(|(column_index, column)| {
            let highlight = is_cursor_row && decorations.current_column.is_none_or(|c| c == column_index);
            let matches = decorations
                .search
                .map_or(&[][..], |s| s.cell_matches(index, column_index));
            styled_cell(
                &fit_to_width(&row.cells[column_index].rendered, column.width),
                matches,
                |is_match| cell_style(highlight, is_match, column.id.is_none()),
            )
        });
        line.append(&StyledString::join(
            cells,
            &row_style.apply(&b.single_vertical.to_string()),
        ));
        line.push_str(&b.double_vertical.to_string(), Style::DEFAULT);
        truncate_if_required(line, self.size, StyledChar::new(ELLIPSIS, Style::DEFAULT))
    }
}

/// The style of a table cell: inverse if highlighted (by the cursor), cyan and bold in a search hit, yellow
/// for a label
pub(crate) fn cell_style(highlight: bool, search_match: bool, label: bool) -> Style {
    let foreground = if search_match {
        default_colours::CYAN
    } else if label {
        default_colours::YELLOW
    } else {
        Colour::Default
    };
    Style {
        foreground,
        inverse: highlight,
        bold: search_match,
        ..Style::DEFAULT
    }
}

/// Cell text, each character styled by whether it's in a search hit (`matches` are character ranges)
pub(crate) fn styled_cell(text: &str, matches: &[Range<usize>], style: impl Fn(bool) -> Style) -> StyledString {
    text.chars()
        .enumerate()
        .map(|(i, c)| StyledChar::new(c, style(matches.iter().any(|m| m.contains(&i)))))
        .collect()
}

/// Shown in a border when there are more rows above
pub(crate) const UP_ARROW: char = '↑';
/// Shown in a border when there are more rows below
pub(crate) const DOWN_ARROW: char = '↓';

/// `s` with its middle character replaced (Scala `ArrowHelper`)
pub(crate) fn set_middle_char(s: &str, c: char) -> String {
    let mut chars: Vec<char> = s.chars().collect();
    if !chars.is_empty() {
        let middle = chars.len() / 2;
        chars[middle] = c;
    }
    chars.into_iter().collect()
}

fn label_style() -> Style {
    Style::fg(default_colours::YELLOW)
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
