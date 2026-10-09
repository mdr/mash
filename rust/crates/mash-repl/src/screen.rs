//! The screen model: styled text, lines and screens (Scala `screen` package). Renderers produce a [`Screen`];
//! a backend draws it (as ANSI escape sequences for a terminal or xterm.js).

use std::fmt;

use unicode_width::UnicodeWidthChar;

/// A colour, as a terminal understands it.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum Colour {
    /// The terminal's own foreground or background colour
    #[default]
    Default,
    Basic(BasicColour),
    /// The bright variant of a basic colour
    Bright(BasicColour),
    /// One of the 256-colour palette
    Indexed(u8),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum BasicColour {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    Grey,
}

impl BasicColour {
    pub fn bright(self) -> Colour {
        Colour::Bright(self)
    }
}

impl From<BasicColour> for Colour {
    fn from(c: BasicColour) -> Self {
        Colour::Basic(c)
    }
}

/// The Dracula theme (https://github.com/dracula/dracula-theme/), as the nearest colours in the 256-colour
/// palette (Scala `DefaultColours`, computed the same way).
pub mod default_colours {
    use super::Colour;

    pub const FOREGROUND: Colour = Colour::Indexed(15);
    pub const COMMENT: Colour = Colour::Indexed(61);
    pub const CYAN: Colour = Colour::Indexed(117);
    pub const GREEN: Colour = Colour::Indexed(84);
    pub const ORANGE: Colour = Colour::Indexed(215);
    pub const PINK: Colour = Colour::Indexed(212);
    pub const PURPLE: Colour = Colour::Indexed(141);
    pub const RED: Colour = Colour::Indexed(203);
    pub const YELLOW: Colour = Colour::Indexed(228);
}

/// How a character is drawn.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Style {
    pub foreground: Colour,
    pub background: Colour,
    pub bold: bool,
    pub inverse: bool,
    pub underline: bool,
}

impl Style {
    pub const DEFAULT: Style = Style {
        foreground: Colour::Default,
        background: Colour::Default,
        bold: false,
        inverse: false,
        underline: false,
    };

    pub fn fg(colour: impl Into<Colour>) -> Style {
        Style {
            foreground: colour.into(),
            ..Style::DEFAULT
        }
    }

    pub fn with_foreground(self, colour: impl Into<Colour>) -> Style {
        Style {
            foreground: colour.into(),
            ..self
        }
    }

    pub fn with_background(self, colour: impl Into<Colour>) -> Style {
        Style {
            background: colour.into(),
            ..self
        }
    }

    pub fn with_bold(self) -> Style {
        Style { bold: true, ..self }
    }

    pub fn with_underline(self) -> Style {
        Style {
            underline: true,
            ..self
        }
    }

    pub fn with_inverse(self) -> Style {
        Style { inverse: true, ..self }
    }

    pub fn invert(self) -> Style {
        Style {
            inverse: !self.inverse,
            ..self
        }
    }

    /// `s` in this style
    pub fn apply(self, s: &str) -> StyledString {
        StyledString::styled(s, self)
    }
}

/// A character and its style. `c` is one Unicode scalar value; its display width is [`StyledChar::width`].
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct StyledChar {
    pub c: char,
    pub style: Style,
}

impl StyledChar {
    pub fn new(c: char, style: Style) -> Self {
        StyledChar { c, style }
    }

    /// Display width in terminal columns: 0 for combining marks and control characters, 2 for wide (East
    /// Asian, most emoji) characters
    pub fn width(&self) -> usize {
        self.c.width().unwrap_or(0)
    }
}

/// A sequence of styled characters (Scala `StyledString`). Indexes and lengths count characters, not
/// columns; use [`StyledString::width`] for the display width.
#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct StyledString {
    pub chars: Vec<StyledChar>,
}

impl StyledString {
    pub fn new() -> Self {
        StyledString { chars: vec![] }
    }

    /// `s` in the default style
    pub fn plain(s: &str) -> Self {
        StyledString::styled(s, Style::DEFAULT)
    }

    pub fn styled(s: &str, style: Style) -> Self {
        StyledString {
            chars: s.chars().map(|c| StyledChar::new(c, style)).collect(),
        }
    }

    pub fn len(&self) -> usize {
        self.chars.len()
    }

    pub fn is_empty(&self) -> bool {
        self.chars.is_empty()
    }

    /// Display width in terminal columns
    pub fn width(&self) -> usize {
        self.chars.iter().map(StyledChar::width).sum()
    }

    pub fn push(&mut self, c: StyledChar) {
        self.chars.push(c);
    }

    pub fn push_str(&mut self, s: &str, style: Style) {
        self.chars.extend(s.chars().map(|c| StyledChar::new(c, style)));
    }

    pub fn append(&mut self, other: &StyledString) {
        self.chars.extend_from_slice(&other.chars);
    }

    /// The first `n` characters
    pub fn take(&self, n: usize) -> StyledString {
        StyledString {
            chars: self.chars.iter().take(n).copied().collect(),
        }
    }

    /// All but the first `n` characters
    pub fn drop_chars(&self, n: usize) -> StyledString {
        StyledString {
            chars: self.chars.iter().skip(n).copied().collect(),
        }
    }

    /// Pad with copies of `c` to `len` characters
    pub fn pad_to(&self, len: usize, c: StyledChar) -> StyledString {
        let mut s = self.clone();
        while s.len() < len {
            s.push(c);
        }
        s
    }

    pub fn join(strings: impl IntoIterator<Item = StyledString>, separator: &StyledString) -> StyledString {
        let mut result = StyledString::new();
        for (i, s) in strings.into_iter().enumerate() {
            if i > 0 {
                result.append(separator);
            }
            result.append(&s);
        }
        result
    }

    /// Change the style of the characters in `range` (character indexes)
    pub fn restyle(&self, range: std::ops::Range<usize>, f: impl Fn(Style) -> Style) -> StyledString {
        StyledString {
            chars: self
                .chars
                .iter()
                .enumerate()
                .map(|(i, c)| {
                    if range.contains(&i) {
                        StyledChar::new(c.c, f(c.style))
                    } else {
                        *c
                    }
                })
                .collect(),
        }
    }

    pub fn invert(&self, range: std::ops::Range<usize>) -> StyledString {
        self.restyle(range, Style::invert)
    }

    /// The text without styles
    pub fn forget_styling(&self) -> String {
        self.chars.iter().map(|c| c.c).collect()
    }
}

impl From<&str> for StyledString {
    fn from(s: &str) -> Self {
        StyledString::plain(s)
    }
}

impl std::ops::Add<&StyledString> for StyledString {
    type Output = StyledString;
    fn add(mut self, rhs: &StyledString) -> StyledString {
        self.append(rhs);
        self
    }
}

impl FromIterator<StyledChar> for StyledString {
    fn from_iter<I: IntoIterator<Item = StyledChar>>(iter: I) -> Self {
        StyledString {
            chars: iter.into_iter().collect(),
        }
    }
}

impl fmt::Debug for StyledString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.forget_styling(), f)
    }
}

impl fmt::Display for StyledString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.forget_styling())
    }
}

/// A size in character cells.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Dimensions {
    pub rows: usize,
    pub columns: usize,
}

impl Dimensions {
    pub fn new(rows: usize, columns: usize) -> Self {
        Dimensions { rows, columns }
    }

    pub fn contains(&self, point: Point) -> bool {
        point.row < self.rows && point.column < self.columns
    }
}

/// A position on the screen (zero-based).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Point {
    pub row: usize,
    pub column: usize,
}

impl Point {
    pub fn new(row: usize, column: usize) -> Self {
        Point { row, column }
    }
}

/// One line of a screen. A line that doesn't end in a newline continues (wraps) onto the next.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Line {
    pub string: StyledString,
    pub ends_in_newline: bool,
}

impl Line {
    pub fn new(string: StyledString) -> Self {
        Line {
            string,
            ends_in_newline: true,
        }
    }

    /// Truncate to `columns` display columns, ending with `…` if anything was cut
    pub fn truncate(&self, columns: usize) -> Line {
        if self.string.width() <= columns {
            return self.clone();
        }
        let mut string = StyledString::new();
        let mut width = 0;
        for c in &self.string.chars {
            if width + c.width() + 1 > columns {
                if columns > 0 {
                    string.push(StyledChar::new('…', c.style));
                }
                break;
            }
            width += c.width();
            string.push(*c);
        }
        Line { string, ..*self }
    }
}

/// What a renderer wants on the screen: its lines, where the cursor goes (hidden if None), and the terminal
/// window title.
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Screen {
    pub lines: Vec<Line>,
    pub cursor: Option<Point>,
    pub title: String,
    /// Drawn on the terminal's alternate screen (full-screen views such as the object browser)
    pub alternate: bool,
}

impl Screen {
    /// Clip to the terminal size
    pub fn truncate(&self, size: Dimensions) -> Screen {
        Screen {
            lines: self
                .lines
                .iter()
                .take(size.rows)
                .map(|line| line.truncate(size.columns))
                .collect(),
            cursor: self.cursor.filter(|&p| size.contains(p)),
            ..self.clone()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn widths_count_columns() {
        assert_eq!(StyledString::plain("ab").width(), 2);
        assert_eq!(StyledString::plain("日本").width(), 4);
        assert_eq!(StyledString::plain("e\u{301}").width(), 1);
    }

    #[test]
    fn truncation_ends_with_an_ellipsis() {
        let line = Line::new("abcdef".into());
        assert_eq!(line.truncate(4).string.forget_styling(), "abc…");
        assert_eq!(line.truncate(6).string.forget_styling(), "abcdef");
        assert_eq!(Line::new("日本語".into()).truncate(4).string.forget_styling(), "日…");
    }
}
