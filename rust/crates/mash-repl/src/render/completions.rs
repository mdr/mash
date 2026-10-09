//! Completion options in columns below the line, with the text the user has typed in bold, and when browsing,
//! the active option highlighted and its description in a box (Scala `CompletionRenderer`).

use mash_core::complete::{Completion, CompletionType};

use super::ellipsisise;
use crate::screen::{Colour, Dimensions, Line, Style, StyledString, default_colours as colours};
use crate::state::CompletionState;

const COLUMN_GAP: usize = 2;

/// The part of the completions' display text that they all share around the completion position: a common
/// prefix (such as a directory), then the text before and after the position (Scala `CompletionFragment`
/// for display)
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CommonFragment {
    pub prefix: String,
    pub before: String,
    pub after: String,
}

impl CommonFragment {
    /// The text the user has typed, as it appears in every completion
    pub fn text(&self) -> String {
        format!("{}{}", self.before, self.after)
    }

    pub fn of(completions: &[Completion]) -> CommonFragment {
        let split = |c: &Completion| {
            let chars: Vec<char> = c.display.chars().collect();
            let prefix_length = c.location.display_prefix_length.min(chars.len());
            let pos = c.location.display_pos.clamp(prefix_length, chars.len());
            (
                chars[..prefix_length].iter().collect::<String>(),
                chars[prefix_length..pos].iter().rev().collect::<String>(),
                chars[pos..].iter().collect::<String>(),
            )
        };
        let common = |a: &str, b: &str| -> String {
            a.chars()
                .zip(b.chars())
                .take_while(|(x, y)| x == y)
                .map(|(x, _)| x)
                .collect()
        };
        let mut fragments = completions.iter().map(split);
        let Some(first) = fragments.next() else {
            return CommonFragment::default();
        };
        let (prefix, before_reversed, after) = fragments.fold(first, |(p, b, a), (p2, b2, a2)| {
            (
                if p == p2 { p } else { String::new() },
                common(&b, &b2),
                common(&a, &a2),
            )
        });
        CommonFragment {
            prefix,
            before: before_reversed.chars().rev().collect(),
            after,
        }
    }
}

fn completion_colour(kind: Option<CompletionType>) -> Colour {
    match kind {
        Some(CompletionType::Directory) => colours::PURPLE,
        Some(CompletionType::File) => colours::YELLOW,
        Some(CompletionType::Flag | CompletionType::Class) => colours::CYAN,
        Some(CompletionType::Binding | CompletionType::Field) => Colour::Default,
        Some(CompletionType::Function | CompletionType::Method) => colours::GREEN,
        None => colours::PINK,
    }
}

fn longest(completions: &[Completion]) -> usize {
    completions.iter().map(|c| c.display.chars().count()).max().unwrap_or(0)
}

/// How many columns of options fit across the terminal
pub fn number_of_columns(completions: &[Completion], size: Dimensions) -> usize {
    let columns = (size.columns + COLUMN_GAP) / (longest(completions) + COLUMN_GAP);
    columns.min(completions.len()).max(1)
}

/// The options (scrolled so the active one is visible), then the description box if browsing
pub fn render_completions(state: &CompletionState, space: Dimensions) -> Vec<Line> {
    let description = match state {
        CompletionState::Browse(browse) => render_description(&browse.completions[browse.active], space),
        CompletionState::Incremental(_) => vec![],
    };
    let rows = space.rows.saturating_sub(description.len());
    let mut lines = render_options(state, Dimensions::new(rows, space.columns));
    lines.extend(description);
    lines
}

fn render_options(state: &CompletionState, space: Dimensions) -> Vec<Line> {
    let completions = state.completions();
    let (fragment, active) = match state {
        CompletionState::Browse(browse) => (CommonFragment::default(), Some(browse.active)),
        CompletionState::Incremental(_) => (CommonFragment::of(completions), None),
    };
    let column_width = longest(completions).min(space.columns);
    let columns = number_of_columns(completions, space);

    let render = |index: usize, completion: &Completion| {
        let text = ellipsisise(&completion.display, column_width);
        let chars: Vec<char> = text.chars().collect();
        let colour = completion_colour(completion.kind);
        let is_active = active == Some(index);
        let plain = Style {
            inverse: is_active,
            ..Style::fg(colour)
        };
        let strong = plain.with_bold();
        let slice = |from: usize, to: usize| -> String {
            chars[from.min(chars.len())..to.clamp(from.min(chars.len()), chars.len())]
                .iter()
                .collect()
        };
        let pos = completion.location.display_pos;
        let prefix_len = fragment.prefix.chars().count();
        let before_len = fragment.before.chars().count();
        let after_len = fragment.after.chars().count();
        let mut s = strong.apply(&fragment.prefix);
        s.push_str(&slice(prefix_len, pos.saturating_sub(before_len)), plain);
        s.push_str(&fragment.text(), strong);
        s.push_str(&slice(pos + after_len, chars.len()), plain);
        s.push_str(&" ".repeat(column_width.saturating_sub(chars.len())), plain);
        s
    };

    let all_lines: Vec<Line> = completions
        .chunks(columns)
        .enumerate()
        .map(|(row, chunk)| {
            let mut line = StyledString::new();
            for (i, completion) in chunk.iter().enumerate() {
                if i > 0 {
                    line.push_str(&" ".repeat(COLUMN_GAP), Style::DEFAULT);
                }
                line.append(&render(row * columns + i, completion));
            }
            Line::new(line)
        })
        .collect();
    let active_row = active.unwrap_or(0) / columns;
    let first_row = (active_row + 1).saturating_sub(space.rows);
    all_lines.into_iter().skip(first_row).take(space.rows).collect()
}

fn render_description(completion: &Completion, space: Dimensions) -> Vec<Line> {
    let description: String = completion
        .description
        .as_deref()
        .unwrap_or("")
        .chars()
        .map(|c| {
            if matches!(c, '\t' | '\n' | '\r' | '\u{8}') {
                ' '
            } else {
                c
            }
        })
        .collect();
    let title = completion
        .kind
        .map(|k| format!(" {} ", type_name(k)))
        .unwrap_or_default();
    let box_width = (description.chars().count() + 4)
        .max(title.chars().count() + 4)
        .min(space.columns);
    let inner = box_width.saturating_sub(4);
    let title = ellipsisise(&title, inner);
    let description = ellipsisise(&description, inner);
    let pad = |s: &str, c: &str| c.repeat(inner.saturating_sub(s.chars().count()));
    vec![
        Line::new(StyledString::plain(&format!("┌─{title}{}─┐", pad(&title, "─")))),
        Line::new(StyledString::plain(&format!(
            "│ {description}{} │",
            pad(&description, " ")
        ))),
        Line::new(StyledString::plain(&format!("└─{}─┘", "─".repeat(inner)))),
    ]
}

fn type_name(kind: CompletionType) -> &'static str {
    match kind {
        CompletionType::Directory => "Directory",
        CompletionType::File => "File",
        CompletionType::Flag => "Flag",
        CompletionType::Field => "Field",
        CompletionType::Method => "Method",
        CompletionType::Binding => "Binding",
        CompletionType::Function => "Function",
        CompletionType::Class => "Class",
    }
}
