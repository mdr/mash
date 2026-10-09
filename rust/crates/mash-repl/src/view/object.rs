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
use mash_core::value::Object;

use super::ViewConfig;
use super::boxes::BoxChars;
use super::columns::{ColumnRequest, allocate_columns};
use super::field::render_field;
use super::text::{self, centre, ellipsisise, fit_to_width};
use crate::screen::{Dimensions, Style, StyledString, default_colours};

/// The lines of a table of a non-empty object's fields
pub(crate) fn object_lines(
    ctx: &mut Ctx,
    object: &Object,
    size: Dimensions,
    config: &ViewConfig,
) -> R<Vec<StyledString>> {
    let class_name = object.class().map(|c| c.name());
    let mut fields = vec![];
    let mut widest_key = 0;
    for (key, value) in object.entries() {
        // As in Scala, the field column's requested width is that of the key's plain string form
        widest_key = widest_key.max(text::width(&ctx.stringify(&key)?));
        fields.push((
            render_field(ctx, &key, true, config)?,
            render_field(ctx, &value, true, config)?,
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
    // Allow for the table's sides and the divider
    let widths = allocate_columns(&requests, size.columns.saturating_sub(3));
    let (field_width, mut value_width) = (widths[0], widths[1]);
    // Widen the value column if the class name needs more room
    if let Some(name) = &class_name {
        let needed = text::width(name).saturating_sub(field_width + value_width + 1);
        let spare = size.columns.saturating_sub(field_width + value_width + 3);
        value_width += needed.min(spare);
    }

    let b = BoxChars::UNICODE;
    let border = |first: char, internal: char, divider: char, last: char| {
        let line = format!(
            "{first}{}{divider}{}{last}",
            internal.to_string().repeat(field_width),
            internal.to_string().repeat(value_width)
        );
        StyledString::plain(&line)
    };
    let side = StyledString::plain(&b.double_vertical.to_string());
    let mut lines = vec![];
    match &class_name {
        Some(name) => {
            lines.push(border(
                b.double_top_left,
                b.double_horizontal,
                b.double_horizontal,
                b.double_top_right,
            ));
            let row_width = field_width + value_width + 1;
            let title = ellipsisise(&centre(name, row_width), row_width);
            let title_style = Style::fg(default_colours::YELLOW).with_bold();
            lines.push(side.clone() + &title_style.apply(&title) + &side);
            lines.push(border(
                b.double_vertical_single_right,
                b.single_horizontal,
                b.single_horizontal_single_down,
                b.double_vertical_single_left,
            ));
        }
        None => lines.push(border(
            b.double_top_left,
            b.double_horizontal,
            b.double_horizontal_single_down,
            b.double_top_right,
        )),
    }
    for (field, value) in &fields {
        let mut line = side.clone();
        line.append(&Style::fg(default_colours::YELLOW).apply(&fit_to_width(field, field_width)));
        line.push_str(&b.single_vertical.to_string(), Style::DEFAULT);
        line.append(&StyledString::plain(&fit_to_width(value, value_width)));
        line.append(&side);
        lines.push(line);
    }
    lines.push(border(
        b.double_bottom_left,
        b.double_horizontal,
        b.double_horizontal_single_up,
        b.double_bottom_right,
    ));
    Ok(lines)
}
