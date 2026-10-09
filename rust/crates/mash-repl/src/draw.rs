//! Drawing a [`Screen`] on a terminal as ANSI escape sequences (Scala `ScreenDrawer`, `DrawState` and
//! `terminal.ansi`).
//!
//! The drawer compares the new screen with the one drawn before and writes only what changed. It assumes an
//! xterm-like terminal: lines wrap automatically (with the cursor waiting at the right margin until the next
//! character), and a wide character that doesn't fit at the end of a row wraps to the next. The cursor is
//! hidden while drawing and shown at the end if the screen has one.

use std::collections::HashSet;

use crate::screen::{Colour, Point, Screen, Style, StyledChar, StyledString};

/// ANSI escape sequences (Scala `EscapeSequence`)
pub mod escapes {
    pub const ERASE_LINE_FROM_CURSOR: &str = "\x1b[K";
    pub const MOVE_CURSOR_TO_TOP_LEFT: &str = "\x1b[H";
    pub const CLEAR_SCREEN: &str = "\x1b[2J";
    pub const HIDE_CURSOR: &str = "\x1b[?25l";
    pub const SHOW_CURSOR: &str = "\x1b[?25h";
    pub const SWITCH_TO_ALTERNATE_SCREEN: &str = "\x1b[?1049h";
    pub const RETURN_FROM_ALTERNATE_SCREEN: &str = "\x1b[?1049l";
    pub const RESET: &str = "\x1b[0m";

    pub fn cursor_up(n: usize) -> String {
        format!("\x1b[{n}A")
    }

    pub fn cursor_down(n: usize) -> String {
        format!("\x1b[{n}B")
    }

    pub fn cursor_forward(n: usize) -> String {
        format!("\x1b[{n}C")
    }

    pub fn cursor_backward(n: usize) -> String {
        format!("\x1b[{n}D")
    }

    pub fn set_title(title: &str) -> String {
        format!("\x1b]0;{title}\x07")
    }
}

fn foreground_codes(colour: Colour) -> Vec<u32> {
    use crate::screen::BasicColour::*;
    let basic = |c| match c {
        Black => 30,
        Red => 31,
        Green => 32,
        Yellow => 33,
        Blue => 34,
        Magenta => 35,
        Cyan => 36,
        Grey => 37,
    };
    match colour {
        Colour::Default => vec![39],
        Colour::Basic(c) => vec![basic(c)],
        Colour::Bright(c) => vec![basic(c) + 60],
        Colour::Indexed(n) => vec![38, 5, u32::from(n)],
    }
}

fn background_codes(colour: Colour) -> Vec<u32> {
    let mut codes = foreground_codes(colour);
    codes[0] += 10;
    codes
}

fn sgr(codes: &[u32]) -> String {
    let codes: Vec<String> = codes.iter().map(u32::to_string).collect();
    format!("\x1b[{}m", codes.join(";"))
}

/// The escape sequence that sets every attribute of `style` (Scala `StyleToEscapeSequence`)
pub fn style_sequence(style: Style) -> String {
    let mut codes = foreground_codes(style.foreground);
    codes.extend(background_codes(style.background));
    if style.inverse {
        codes.push(7);
    }
    if style.bold {
        codes.push(1);
    }
    if style.underline {
        codes.push(4);
    }
    sgr(&codes)
}

/// A styled string as text with escape sequences, ending with the style reset (Scala `StyledStringDrawer`)
pub fn styled_to_ansi(s: &StyledString) -> String {
    let mut out = String::new();
    let mut previous = None;
    for c in &s.chars {
        if previous != Some(c.style) {
            out += escapes::RESET;
            out += &style_sequence(c.style);
            previous = Some(c.style);
        }
        out.push(c.c);
    }
    out += escapes::RESET;
    out
}

/// What drawing a screen produced.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ScreenDraw {
    /// Text and escape sequences to write to the terminal
    pub output: String,
    /// The normal screen, while an alternate-screen view (the object browser) is showing
    pub swapped_out: Option<Screen>,
}

/// Draws screens on a terminal of a given width (Scala `ScreenDrawer`).
#[derive(Clone, Copy, Debug)]
pub struct ScreenDrawer {
    columns: usize,
}

impl ScreenDrawer {
    pub fn new(columns: usize) -> ScreenDrawer {
        ScreenDrawer {
            columns: columns.max(1),
        }
    }

    /// Draw `screen`, given the one drawn before (None if the terminal is fresh, e.g. after a command's
    /// output), and the normal screen that's swapped out while the alternate screen is in use.
    pub fn draw(&self, screen: &Screen, previous: Option<&Screen>, swapped_out: Option<&Screen>) -> ScreenDraw {
        let old_alternate = previous.is_some_and(|p| p.alternate);
        let swapping_out = screen.alternate && !old_alternate;
        let swapping_back_in = !screen.alternate && old_alternate;
        let actual_previous = if swapping_back_in { swapped_out } else { previous };
        let current = actual_previous.and_then(|p| p.cursor).unwrap_or_default();
        let mut state = DrawState::new(self.columns, current);

        if swapping_out {
            // Terminal.app inserts a newline on returning from the alternate screen if there's a character in
            // the bottom-right corner, so erase it first (and restore it on the way back).
            if let Some((point, _)) = self.bottom_right_char(actual_previous) {
                state.move_cursor(point);
                state.erase_line_from_cursor();
            }
            state.switch_to_alternate_screen();
        } else if swapping_back_in {
            state.return_from_alternate_screen(current);
            if let Some((point, c)) = self.bottom_right_char(actual_previous) {
                state.move_cursor(point);
                state.add_char(c);
            }
        }

        let previous_lines = actual_previous.map_or(&[][..], |p| &p.lines[..]);
        for row in 0..screen.lines.len().max(previous_lines.len()) {
            let new = screen.lines.get(row);
            let old = previous_lines.get(row);
            match (new, old) {
                _ if new == old => {}
                (Some(new), old) => self.draw_line(&mut state, &screen.lines, new, old, row),
                (None, Some(_)) => {
                    state.move_cursor_to_row(row);
                    state.cr();
                    state.erase_line_from_cursor();
                }
                (None, None) => {}
            }
        }

        state.move_cursor(screen.cursor.unwrap_or_default());
        if previous.map(|p| &p.title) != Some(&screen.title) {
            state.out += &escapes::set_title(&screen.title);
        }
        if screen.cursor.is_some() {
            state.out += escapes::SHOW_CURSOR;
        }
        let swapped_out = if swapping_out {
            previous.cloned()
        } else if swapping_back_in {
            None
        } else {
            swapped_out.cloned()
        };
        ScreenDraw {
            output: state.out,
            swapped_out,
        }
    }

    fn bottom_right_char(&self, screen: Option<&Screen>) -> Option<(Point, StyledChar)> {
        let screen = screen?;
        let last = screen.lines.last()?;
        if last.string.width() != self.columns {
            return None;
        }
        let c = *last.string.chars.last()?;
        Some((Point::new(screen.lines.len() - 1, self.columns - c.width()), c))
    }

    fn draw_line(
        &self,
        state: &mut DrawState,
        lines: &[crate::screen::Line],
        new: &crate::screen::Line,
        old: Option<&crate::screen::Line>,
        row: usize,
    ) {
        state.navigate_up_to_row_or_down_to_just_above(row);
        let mut first_char_drawn = false;
        if row > 0 && state.row == row - 1 {
            // We're on the line above
            let above = &lines[row - 1];
            if above.ends_in_newline {
                state.crlf();
            } else {
                // Rewrite the last character of the line above and the first of this one, to make the terminal
                // wrap (so it knows the lines are one, e.g. for copying)
                if state.column < self.columns {
                    let last = above
                        .string
                        .chars
                        .last()
                        .copied()
                        .unwrap_or(StyledChar::new(' ', Style::DEFAULT));
                    state.move_cursor_to_column(above.string.width().saturating_sub(last.width().max(1)));
                    state.add_char(last);
                }
                let first = new
                    .string
                    .chars
                    .first()
                    .copied()
                    .unwrap_or(StyledChar::new(' ', Style::DEFAULT));
                state.add_char(first);
                first_char_drawn = true;
            }
        }
        let skip = usize::from(first_char_drawn);
        match old {
            Some(old) => {
                let old_cells: HashSet<_> = cells(&old.string).into_iter().collect();
                for (column, c) in cells(&new.string).into_iter().skip(skip) {
                    if !old_cells.contains(&(column, c)) {
                        state.move_cursor_to_column(column);
                        state.add_char(c);
                    }
                }
                let new_width = new.string.width();
                if old.string.width() > new_width {
                    state.move_cursor_to_column(new_width);
                    state.erase_line_from_cursor();
                }
            }
            None => {
                let mut cells = cells(&new.string).into_iter().skip(skip).peekable();
                if let Some(&(column, _)) = cells.peek() {
                    state.move_cursor_to_column(column);
                } else if !first_char_drawn {
                    state.move_cursor_to_column(0);
                }
                for (_, c) in cells {
                    state.add_char(c);
                }
            }
        }
    }
}

/// Each character of a line with the column it starts in
fn cells(s: &StyledString) -> Vec<(usize, StyledChar)> {
    let mut column = 0;
    s.chars
        .iter()
        .map(|&c| {
            let cell = (column, c);
            column += c.width();
            cell
        })
        .collect()
}

/// The text needed to move past a screen, leaving it on the terminal (as when a command is accepted)
pub fn accept_screen(screen: &Screen) -> String {
    let row = screen.cursor.unwrap_or_default().row;
    let mut out = "\r\n".repeat(screen.lines.len().saturating_sub(row));
    out += escapes::ERASE_LINE_FROM_CURSOR;
    out += escapes::RESET;
    out
}

/// The terminal's state while drawing (Scala `DrawState`): where the cursor is, and the current style.
struct DrawState {
    columns: usize,
    row: usize,
    /// May equal `columns`: the cursor is waiting at the right margin to wrap
    column: usize,
    style: Style,
    out: String,
}

impl DrawState {
    fn new(columns: usize, cursor: Point) -> DrawState {
        DrawState {
            columns,
            row: cursor.row,
            column: cursor.column,
            style: Style::DEFAULT,
            out: format!("{}{}", escapes::HIDE_CURSOR, escapes::RESET),
        }
    }

    fn add_char(&mut self, c: StyledChar) {
        if c.style != self.style {
            if self.style.with_foreground(c.style.foreground) == c.style {
                self.out += &sgr(&foreground_codes(c.style.foreground));
            } else {
                self.out += escapes::RESET;
                self.out += &style_sequence(c.style);
            }
            self.style = c.style;
        }
        let width = c.width();
        if self.column + width > self.columns {
            self.column = 0;
            self.row += 1;
        }
        self.out.push(c.c);
        self.column += width;
    }

    /// Move up to `row`, or down to the row just above it
    fn navigate_up_to_row_or_down_to_just_above(&mut self, row: usize) {
        if self.row > row {
            self.out += &escapes::cursor_up(self.row - row);
            self.row = row;
        }
        if self.row + 1 < row {
            self.out += &escapes::cursor_down(row - 1 - self.row);
            self.row = row - 1;
            self.cr();
        }
    }

    fn move_cursor_to_row(&mut self, row: usize) {
        if self.row > row {
            self.out += &escapes::cursor_up(self.row - row);
            self.row = row;
        }
        if self.row < row {
            self.out += &escapes::cursor_down(row - self.row);
            self.row = row;
            self.cr();
        }
    }

    fn move_cursor_to_column(&mut self, column: usize) {
        if self.column >= self.columns {
            // The most reliable way back from the right margin: cursor-backward differs between terminals
            self.cr();
        }
        if self.column > column {
            self.out += &escapes::cursor_backward(self.column - column);
        } else if self.column < column {
            self.out += &escapes::cursor_forward(column - self.column);
        }
        self.column = column;
    }

    fn move_cursor(&mut self, point: Point) {
        self.move_cursor_to_row(point.row);
        self.move_cursor_to_column(point.column);
    }

    fn crlf(&mut self) {
        self.out += "\r\n";
        self.row += 1;
        self.column = 0;
    }

    fn cr(&mut self) {
        self.out.push('\r');
        self.column = 0;
    }

    fn erase_line_from_cursor(&mut self) {
        self.out += escapes::ERASE_LINE_FROM_CURSOR;
    }

    fn switch_to_alternate_screen(&mut self) {
        self.out += escapes::SWITCH_TO_ALTERNATE_SCREEN;
        self.out += escapes::MOVE_CURSOR_TO_TOP_LEFT;
        self.out += escapes::CLEAR_SCREEN;
        self.row = 0;
        self.column = 0;
    }

    fn return_from_alternate_screen(&mut self, cursor: Point) {
        self.out += escapes::RETURN_FROM_ALTERNATE_SCREEN;
        self.row = cursor.row;
        self.column = cursor.column;
    }
}
