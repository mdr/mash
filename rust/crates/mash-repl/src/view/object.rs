//! A single object as a two-column table of fields and values, headed by its class name if it has a class
//! (Scala `SingleObjectTableModelCreator`, `SingleObjectTableCommonRenderer` and `SingleObjectTablePrinter`).
//!
//! ```text
//! ╔═══════════╗
//! ║   Stats   ║
//! ╟─────────┬─╢
//! ║min      │1║
//! ╚═════════╧═╝
//! ```

use mash_core::eval::{Ctx, R};
use mash_core::value::{Object, Value};

use super::boxes::BoxChars;
use super::columns::{ColumnRequest, allocate_columns};
use super::field::render_field;
use super::table::{DOWN_ARROW, cell_style, set_middle_char, styled_cell};
use super::text::{self, centre, ellipsisise, fit_to_width};
use super::{Decorations, ViewConfig};
use crate::screen::{Dimensions, Style, StyledString, default_colours};

/// A table of an object's fields, ready to draw (Scala `SingleObjectTableModel`).
#[derive(Clone, PartialEq, Debug)]
pub struct SingleObjectTableModel {
    pub(crate) class_name: Option<String>,
    /// Each field and its value, rendered
    pub(crate) fields: Vec<(String, String)>,
    pub(crate) field_width: usize,
    pub(crate) value_width: usize,
    /// The object (as a value)
    pub(crate) raw_value: Value,
    /// Each field and its value
    pub(crate) raw_fields: Vec<(Value, Value)>,
}

impl SingleObjectTableModel {
    /// Each field and its value, as text
    pub fn rendered_fields(&self) -> &[(String, String)] {
        &self.fields
    }

    pub(crate) fn number_of_rows(&self) -> usize {
        self.fields.len()
    }
}

/// The lines of a table of a non-empty object's fields
pub(crate) fn object_lines(
    ctx: &mut Ctx,
    object: &Object,
    size: Dimensions,
    config: &ViewConfig,
) -> R<Vec<StyledString>> {
    let model = create_model(ctx, object, size, config, false)?;
    Ok(render_object_table(
        &model,
        &Decorations::default(),
        0,
        model.number_of_rows(),
    ))
}

/// The model of a table of a non-empty object's fields. `support_marking` leaves room for the object
/// browser's mark column.
pub(crate) fn create_model(
    ctx: &mut Ctx,
    object: &Object,
    size: Dimensions,
    config: &ViewConfig,
    support_marking: bool,
) -> R<SingleObjectTableModel> {
    let class_name = object.class().map(|c| c.name());
    let raw_fields = object.entries();
    let mut fields = vec![];
    let mut widest_key = 0;
    for (key, value) in &raw_fields {
        // As in Scala, the field column's requested width is that of the key's plain string form
        widest_key = widest_key.max(text::width(&ctx.stringify(key)?));
        fields.push((
            render_field(ctx, key, true, config)?,
            render_field(ctx, value, true, config)?,
        ));
    }
    let widest_value = fields.iter().map(|(_, v)| text::width(v)).max().unwrap_or(0);
    let requests = [
        ColumnRequest {
            weight: 10.0,
            requested: widest_key,
        },
        ColumnRequest {
            weight: 1.0,
            requested: widest_value,
        },
    ];
    // Allow for the table's sides and the divider, and the mark column
    let marking = if support_marking { 2 } else { 0 };
    let widths = allocate_columns(&requests, size.columns.saturating_sub(3 + marking));
    let (field_width, mut value_width) = (widths[0], widths[1]);
    // Widen the value column if the class name needs more room
    if let Some(name) = &class_name {
        let needed = text::width(name).saturating_sub(field_width + value_width + 1);
        let spare = size.columns.saturating_sub(field_width + value_width + 3);
        value_width += needed.min(spare);
    }
    Ok(SingleObjectTableModel {
        class_name,
        fields,
        field_width,
        value_width,
        raw_value: Value::Object(object.clone()),
        raw_fields,
    })
}

/// The lines of a table showing `row_count` fields from `first_row`, with arrows on the borders if there
/// are more fields above or below
pub(crate) fn render_object_table(
    model: &SingleObjectTableModel,
    decorations: &Decorations,
    first_row: usize,
    row_count: usize,
) -> Vec<StyledString> {
    let b = BoxChars::UNICODE;
    let shows_marks = decorations.marked.is_some();
    // As in Scala Mash, every arrow points down
    let border = |[first, internal, divider, last]: [char; 4], arrow: bool| {
        let mut line = String::from(first);
        if shows_marks {
            line.push(internal);
            line.push(divider);
        }
        line.push_str(&internal.to_string().repeat(model.field_width));
        line.push(divider);
        line.push_str(&internal.to_string().repeat(model.value_width));
        line.push(last);
        if arrow {
            line = set_middle_char(&line, DOWN_ARROW);
        }
        StyledString::plain(&line)
    };
    let side = StyledString::plain(&b.double_vertical.to_string());
    let more_above = first_row > 0;
    let mut lines = vec![];
    match &model.class_name {
        Some(name) => {
            lines.push(border(
                [
                    b.double_top_left,
                    b.double_horizontal,
                    b.double_horizontal,
                    b.double_top_right,
                ],
                false,
            ));
            let row_width = model.field_width + model.value_width + if shows_marks { 3 } else { 1 };
            let title = ellipsisise(&centre(name, row_width), row_width);
            let title_style = Style::fg(default_colours::YELLOW).with_bold();
            lines.push(side.clone() + &title_style.apply(&title) + &side);
            lines.push(border(
                [
                    b.double_vertical_single_right,
                    b.single_horizontal,
                    b.single_horizontal_single_down,
                    b.double_vertical_single_left,
                ],
                more_above,
            ));
        }
        None => lines.push(border(
            [
                b.double_top_left,
                b.double_horizontal,
                b.double_horizontal_single_down,
                b.double_top_right,
            ],
            more_above,
        )),
    }
    let end = model.fields.len().min(first_row + row_count);
    for (index, (field, value)) in model.fields.iter().enumerate().take(end).skip(first_row) {
        let highlight = decorations.current_row == Some(index);
        let divider_style = cell_style(highlight, false, false);
        let matches = |column| decorations.search.map_or(&[][..], |s| s.cell_matches(index, column));
        let mut line = side.clone();
        if let Some(marked) = decorations.marked {
            let mark = if marked.contains(&index) { '◈' } else { ' ' };
            line.push_str(&format!("{mark}{}", b.single_vertical), divider_style);
        }
        line.append(&styled_cell(
            &fit_to_width(field, model.field_width),
            matches(0),
            |is_match| cell_style(highlight, is_match, true),
        ));
        line.push_str(&b.single_vertical.to_string(), divider_style);
        line.append(&styled_cell(
            &fit_to_width(value, model.value_width),
            matches(1),
            |is_match| cell_style(highlight, is_match, false),
        ));
        line.append(&side);
        lines.push(line);
    }
    lines.push(border(
        [
            b.double_bottom_left,
            b.double_horizontal,
            b.double_horizontal_single_up,
            b.double_bottom_right,
        ],
        first_row + row_count < model.fields.len(),
    ));
    lines
}
