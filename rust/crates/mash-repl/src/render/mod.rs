//! Rendering the REPL's state to a [`Screen`] (Scala `view.render`): the prompt and line buffer, then any
//! history search, invocation assistance and completions below it.

pub mod assistance;
pub mod boxes;
pub mod buffer;
pub mod completions;
pub mod highlight;
pub mod history_search;

use mash_core::eval::Ctx;

use crate::screen::{Dimensions, Line, Screen, Style, StyledChar, StyledString};
use crate::state::ReplState;

/// Options for rendering the REPL.
#[derive(Clone, Copy, Debug)]
pub struct RenderOptions {
    pub size: Dimensions,
    /// Unbound identifiers are strings (`config.language.bareWords`)
    pub bare_words: bool,
    /// The line has been accepted: render it without the status indicator
    pub completed: bool,
}

/// The whole REPL (Scala `ReplRenderer.renderRegularRepl`). `ctx` supplies the global names (to show bare
/// words), the working directory (for the prompt and the window title) and the clock.
pub fn render_repl(state: &ReplState, ctx: &mut Ctx, options: RenderOptions) -> Screen {
    let size = options.size;
    let prompt = buffer::prompt(ctx, state.command_number, state.mish);
    let match_range = state.history_search.as_ref().and_then(|s| s.hit.match_range());
    let rendered = buffer::render_line_buffer(
        ctx,
        &state.line_buffer,
        &buffer::BufferOptions {
            columns: size.columns,
            prompt,
            mish: state.mish,
            bare_words: options.bare_words,
            match_range,
            completed: options.completed,
        },
    );
    let mut lines = rendered.lines;
    let mut cursor = rendered.cursor;

    if let Some(search) = &state.history_search {
        let search_lines = history_search::render_history_search(ctx, search);
        if let Some(search_cursor) = search_lines.cursor {
            cursor = Some(crate::screen::Point::new(
                search_cursor.row + lines.len(),
                search_cursor.column,
            ));
        }
        lines.extend(search_lines.lines);
    }

    if let Some(assistance) = &state.assistance {
        lines.extend(assistance::render_assistance(&assistance.0, size));
    }

    if let Some(completion) = &state.completion {
        let remaining = Dimensions::new(size.rows.saturating_sub(lines.len()), size.columns);
        lines.extend(completions::render_completions(completion, remaining));
    }

    let title = match (ctx.platform().current_dir(), ctx.platform().home_dir()) {
        (Some(cwd), Some(home)) => retilde(&cwd, &home),
        (Some(cwd), None) => cwd,
        (None, _) => String::new(),
    };
    Screen {
        lines,
        cursor,
        title,
        alternate: false,
    }
    .truncate(size)
}

/// Lines, and where the cursor goes among them.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct LinesAndCursor {
    pub lines: Vec<Line>,
    pub cursor: Option<crate::screen::Point>,
}

/// `path` with the home directory replaced by `~` (Scala `TildeExpander.retilde`)
pub fn retilde(path: &str, home: &str) -> String {
    if home.is_empty() || home == "/" {
        return path.to_string();
    }
    match path.strip_prefix(home) {
        Some("") => "~".into(),
        Some(rest) if rest.starts_with('/') => format!("~{rest}"),
        _ => path.to_string(),
    }
}

/// `s` cut to at most `max` characters, ending with `…` if anything was cut (Scala `StringUtils.ellipsisise`)
pub fn ellipsisise(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else if max == 0 {
        String::new()
    } else {
        let mut cut: String = s.chars().take(max - 1).collect();
        cut.push('…');
        cut
    }
}

/// `s` cut to at most `max` characters, ending with `…` if anything was cut
pub fn ellipsisise_styled(s: &StyledString, max: usize) -> StyledString {
    if s.len() <= max {
        s.clone()
    } else if max == 0 {
        StyledString::new()
    } else {
        let mut cut = s.take(max - 1);
        cut.push(StyledChar::new('…', Style::DEFAULT));
        cut
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn retilde_replaces_home() {
        assert_eq!(retilde("/home/alice", "/home/alice"), "~");
        assert_eq!(retilde("/home/alice/src", "/home/alice"), "~/src");
        assert_eq!(retilde("/home/alicex", "/home/alice"), "/home/alicex");
        assert_eq!(retilde("/etc", "/home/alice"), "/etc");
    }

    #[test]
    fn ellipsis() {
        assert_eq!(ellipsisise("abcdef", 4), "abc…");
        assert_eq!(ellipsisise("abc", 4), "abc");
        assert_eq!(ellipsisise("abc", 0), "");
    }
}
