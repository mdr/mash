//! Boxes with a title (Scala `BoxRenderer`), as used for invocation assistance and startup tips:
//!
//! ```text
//! ┌─ reverse ────────────────────────┐
//! │ Reverse a List, String or Object │
//! └──────────────────────────────────┘
//! ```

use super::{ellipsisise, ellipsisise_styled};
use crate::screen::{Dimensions, Line, Style, StyledString, default_colours as colours};

/// What goes in a box.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoxContent {
    pub title: String,
    pub lines: Vec<StyledString>,
}

const INNER_START: &str = "│ ";
const INNER_END: &str = " │";
const TOP_START: &str = "┌─ ";
const TOP_END: &str = "─┐";
const BOTTOM_START: &str = "└─";
const BOTTOM_END: &str = "─┘";

/// Top-line characters other than the title: `┌─ `, a space and `─┐`
const TOP_NON_TITLE: usize = 6;
/// Inner-line characters other than the content
const INNER_NON_CONTENT: usize = 4;
const BOTTOM_NON_FILLER: usize = 4;

/// The box, as wide as its content needs or the terminal allows (but never narrower than its corners)
pub fn render_box(content: &BoxContent, size: Dimensions) -> Vec<Line> {
    let title_width = content.title.chars().count() + TOP_NON_TITLE;
    let widest = content
        .lines
        .iter()
        .map(|l| l.len() + INNER_NON_CONTENT)
        .chain([title_width])
        .max()
        .unwrap_or(0);
    let box_width = widest.min(size.columns).max(TOP_NON_TITLE);

    let available_title = box_width - TOP_NON_TITLE;
    let title = ellipsisise(&content.title, available_title);
    let filler = "─".repeat(available_title - title.chars().count());
    let mut top = StyledString::plain(TOP_START);
    top.push_str(&title, Style::fg(colours::YELLOW).with_bold());
    top.push_str(&format!(" {filler}{TOP_END}"), Style::DEFAULT);

    let mut lines = vec![Line::new(top)];
    let available_inner = box_width - INNER_NON_CONTENT;
    for content_line in &content.lines {
        let content_line = ellipsisise_styled(content_line, available_inner);
        let mut line = StyledString::plain(INNER_START);
        line.append(&content_line);
        line.push_str(&" ".repeat(available_inner - content_line.len()), Style::DEFAULT);
        line.push_str(INNER_END, Style::DEFAULT);
        lines.push(Line::new(line));
    }
    let bottom = format!(
        "{BOTTOM_START}{}{BOTTOM_END}",
        "─".repeat(box_width - BOTTOM_NON_FILLER)
    );
    lines.push(Line::new(StyledString::plain(&bottom)));
    lines
}
