//! The text being edited at the prompt (Scala `LineBuffer`): possibly several lines, a cursor, and an optional
//! selection.
//!
//! Offsets are byte offsets into the text, always on grapheme cluster boundaries: the cursor moves, and
//! characters are deleted, a grapheme at a time (so `e` + combining accent, or an emoji with modifiers, is one
//! step). Columns, as used when moving up and down, also count graphemes.

use std::fmt;
use std::ops::Range;

use unicode_segmentation::UnicodeSegmentation;

/// The contents of the editor: text, cursor and selection. Immutable; every edit returns a new buffer.
#[derive(Clone, PartialEq, Eq, Default)]
pub struct LineBuffer {
    text: String,
    cursor: usize,
    /// The other end of the selection (never equal to the cursor)
    anchor: Option<usize>,
}

impl LineBuffer {
    /// `text` with the cursor at the end
    pub fn new(text: impl Into<String>) -> LineBuffer {
        let text = text.into();
        let cursor = text.len();
        LineBuffer {
            text,
            cursor,
            anchor: None,
        }
    }

    /// `text` with the cursor at byte offset `cursor`
    pub fn with_cursor(text: impl Into<String>, cursor: usize) -> LineBuffer {
        LineBuffer::with_selection(text, cursor, None)
    }

    /// `text` with the cursor at byte offset `cursor` and the selection running from there to `anchor`.
    /// Panics if an offset is out of range or not on a character boundary.
    pub fn with_selection(text: impl Into<String>, cursor: usize, anchor: Option<usize>) -> LineBuffer {
        let text = text.into();
        assert!(
            text.is_char_boundary(cursor),
            "cursor offset {cursor} out of range in {text:?}"
        );
        if let Some(anchor) = anchor {
            assert!(
                text.is_char_boundary(anchor),
                "selection offset {anchor} out of range in {text:?}"
            );
        }
        LineBuffer {
            text,
            cursor,
            anchor: anchor.filter(|&a| a != cursor),
        }
    }

    /// A buffer described with markers, as in tests: `▶` is the cursor, `▷` the other end of the selection.
    /// `◀` and `◁` mark the same just before the preceding character (handy after a closing quote).
    pub fn parse_marked(s: &str) -> LineBuffer {
        let mut text = String::new();
        let (mut cursor, mut anchor) = (None, None);
        for c in s.chars() {
            match c {
                '▶' => cursor = Some(text.len()),
                '◀' => cursor = Some(prev_char(&text, text.len())),
                '▷' => anchor = Some(text.len()),
                '◁' => anchor = Some(prev_char(&text, text.len())),
                c => text.push(c),
            }
        }
        // ◀ and ◁ refer to the character before them in the marked string, which may be followed by more text
        let cursor = cursor.unwrap_or_else(|| panic!("no cursor marker (▶ or ◀) in {s:?}"));
        LineBuffer::with_selection(text, cursor, anchor)
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    /// Byte offset of the cursor
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    /// The other end of the selection, if any
    pub fn anchor(&self) -> Option<usize> {
        self.anchor
    }

    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    pub fn selection(&self) -> Option<Range<usize>> {
        self.anchor
            .map(|anchor| self.cursor.min(anchor)..self.cursor.max(anchor))
    }

    /// The selection, or an empty range at the cursor
    pub fn selection_or_cursor(&self) -> Range<usize> {
        self.selection().unwrap_or(self.cursor..self.cursor)
    }

    pub fn selected_text(&self) -> Option<&str> {
        self.selection().map(|range| &self.text[range])
    }

    /// The byte ranges of each line, not including line endings
    pub fn line_ranges(&self) -> Vec<Range<usize>> {
        line_ranges(&self.text)
    }

    pub fn line_count(&self) -> usize {
        self.text.matches('\n').count() + 1
    }

    pub fn is_multiline(&self) -> bool {
        self.text.contains('\n')
    }

    pub fn cursor_at_end(&self) -> bool {
        self.cursor == self.text.len()
    }

    /// The cursor's row (line index)
    pub fn cursor_row(&self) -> usize {
        self.text[..self.cursor].matches('\n').count()
    }

    /// The cursor's row, and column in graphemes
    pub fn cursor_pos(&self) -> (usize, usize) {
        let row = self.cursor_row();
        let start = self.line_ranges()[row].start;
        (row, self.text[start..self.cursor].graphemes(true).count())
    }

    // ------------------------------------------------------------------------------------------- movement

    fn move_cursor(&self, cursor: usize, extend_selection: bool) -> LineBuffer {
        let anchor = if extend_selection {
            Some(self.anchor.unwrap_or(self.cursor)).filter(|&a| a != cursor)
        } else {
            None
        };
        LineBuffer {
            text: self.text.clone(),
            cursor,
            anchor,
        }
    }

    pub fn cursor_left(&self, extend_selection: bool) -> LineBuffer {
        self.move_cursor(prev_grapheme(&self.text, self.cursor), extend_selection)
    }

    pub fn cursor_right(&self, extend_selection: bool) -> LineBuffer {
        self.move_cursor(next_grapheme(&self.text, self.cursor), extend_selection)
    }

    pub fn cursor_up(&self, extend_selection: bool) -> LineBuffer {
        let (row, _) = self.cursor_pos();
        self.move_to_row(row.saturating_sub(1), extend_selection)
    }

    pub fn cursor_down(&self, extend_selection: bool) -> LineBuffer {
        let (row, _) = self.cursor_pos();
        self.move_to_row((row + 1).min(self.line_count() - 1), extend_selection)
    }

    fn move_to_row(&self, row: usize, extend_selection: bool) -> LineBuffer {
        let (_, column) = self.cursor_pos();
        let range = self.line_ranges()[row].clone();
        self.move_cursor(offset_of_column(&self.text, range, column), extend_selection)
    }

    /// To the start of the line, or if already there, to the start of the buffer
    pub fn move_to_start(&self, extend_selection: bool) -> LineBuffer {
        let line_start = self.line_ranges()[self.cursor_row()].start;
        let target = if self.cursor == line_start { 0 } else { line_start };
        self.move_cursor(target, extend_selection)
    }

    /// To the end of the line, or if already there, to the end of the buffer
    pub fn move_to_end(&self, extend_selection: bool) -> LineBuffer {
        let line_end = self.line_ranges()[self.cursor_row()].end;
        let target = if self.cursor == line_end {
            self.text.len()
        } else {
            line_end
        };
        self.move_cursor(target, extend_selection)
    }

    pub fn move_to_start_of_buffer(&self, extend_selection: bool) -> LineBuffer {
        self.move_cursor(0, extend_selection)
    }

    pub fn move_to_end_of_buffer(&self, extend_selection: bool) -> LineBuffer {
        self.move_cursor(self.text.len(), extend_selection)
    }

    pub fn forward_word(&self, extend_selection: bool) -> LineBuffer {
        self.move_cursor(self.forward_word_offset(), extend_selection)
    }

    pub fn backward_word(&self, extend_selection: bool) -> LineBuffer {
        self.move_cursor(self.backward_word_offset(), extend_selection)
    }

    /// Past any non-word graphemes, then past a word
    fn forward_word_offset(&self) -> usize {
        let mut graphemes = self.text[self.cursor..].grapheme_indices(true).peekable();
        let mut end = self.cursor;
        while let Some(&(i, g)) = graphemes.peek()
            && !is_word(g)
        {
            end = self.cursor + i + g.len();
            graphemes.next();
        }
        while let Some(&(i, g)) = graphemes.peek()
            && is_word(g)
        {
            end = self.cursor + i + g.len();
            graphemes.next();
        }
        end
    }

    /// Back over any non-word graphemes, then back over a word
    fn backward_word_offset(&self) -> usize {
        let mut graphemes = self.text[..self.cursor].grapheme_indices(true).rev().peekable();
        let mut start = self.cursor;
        while let Some(&(i, g)) = graphemes.peek()
            && !is_word(g)
        {
            start = i;
            graphemes.next();
        }
        while let Some(&(i, g)) = graphemes.peek()
            && is_word(g)
        {
            start = i;
            graphemes.next();
        }
        start
    }

    pub fn without_selection(&self) -> LineBuffer {
        LineBuffer {
            anchor: None,
            ..self.clone()
        }
    }

    /// Select `range`, with the cursor at its end
    pub fn select(&self, range: Range<usize>) -> LineBuffer {
        LineBuffer::with_selection(self.text.clone(), range.end, Some(range.start))
    }

    // ------------------------------------------------------------------------------------------- editing

    /// Replace a byte range with `replacement`. The cursor keeps its place in the surrounding text, or goes to
    /// the end of the replacement if it was inside the range; any selection is dropped.
    pub fn replace(&self, range: Range<usize>, replacement: &str) -> LineBuffer {
        let text = format!("{}{replacement}{}", &self.text[..range.start], &self.text[range.end..]);
        let cursor = if self.cursor < range.start {
            self.cursor
        } else if self.cursor >= range.end {
            self.cursor - range.len() + replacement.len()
        } else {
            range.start + replacement.len()
        };
        LineBuffer::with_cursor(text, cursor)
    }

    /// Insert at the cursor, replacing any selection
    pub fn insert_at_cursor(&self, s: &str) -> LineBuffer {
        self.replace(self.selection_or_cursor(), s)
    }

    pub fn insert(&self, s: &str, offset: usize) -> LineBuffer {
        self.replace(offset..offset, s)
    }

    /// Delete the selection, or the grapheme before the cursor
    pub fn backspace(&self) -> LineBuffer {
        let range = self
            .selection()
            .unwrap_or(prev_grapheme(&self.text, self.cursor)..self.cursor);
        self.replace(range, "")
    }

    /// Delete the selection, or the grapheme after the cursor
    pub fn delete(&self) -> LineBuffer {
        let range = self
            .selection()
            .unwrap_or(self.cursor..next_grapheme(&self.text, self.cursor));
        self.replace(range, "")
    }

    pub fn delete_forward_word(&self) -> LineBuffer {
        let range = self.selection().unwrap_or(self.cursor..self.forward_word_offset());
        self.replace(range, "")
    }

    pub fn delete_backward_word(&self) -> LineBuffer {
        let range = self.selection().unwrap_or(self.backward_word_offset()..self.cursor);
        self.replace(range, "")
    }

    pub fn delete_to_end_of_line(&self) -> LineBuffer {
        let line_end = self.line_ranges()[self.cursor_row()].end;
        self.without_selection().replace(self.cursor..line_end, "")
    }

    pub fn delete_to_start_of_line(&self) -> LineBuffer {
        let line_start = self.line_ranges()[self.cursor_row()].start;
        self.without_selection().replace(line_start..self.cursor, "")
    }
}

impl fmt::Debug for LineBuffer {
    /// The text with `▶` at the cursor and `▷` at the other end of the selection
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        let markers = |s: &mut String, i: usize| {
            if i == self.cursor {
                s.push('▶');
            }
            if Some(i) == self.anchor {
                s.push('▷');
            }
        };
        for (i, c) in self.text.char_indices() {
            markers(&mut s, i);
            s.push(c);
        }
        markers(&mut s, self.text.len());
        write!(f, "{s:?}")
    }
}

fn is_word(grapheme: &str) -> bool {
    grapheme.chars().next().is_some_and(char::is_alphanumeric)
}

/// The byte ranges of each line of `text`, not including line endings
pub(crate) fn line_ranges(text: &str) -> Vec<Range<usize>> {
    let mut ranges = vec![];
    let mut start = 0;
    for (i, _) in text.match_indices('\n') {
        ranges.push(start..i);
        start = i + 1;
    }
    ranges.push(start..text.len());
    ranges
}

/// The offset of the start of the grapheme before `pos` (or 0)
pub(crate) fn prev_grapheme(text: &str, pos: usize) -> usize {
    text[..pos].grapheme_indices(true).next_back().map_or(0, |(i, _)| i)
}

/// The offset just after the grapheme at `pos` (or the end)
pub(crate) fn next_grapheme(text: &str, pos: usize) -> usize {
    text[pos..].graphemes(true).next().map_or(text.len(), |g| pos + g.len())
}

fn prev_char(text: &str, pos: usize) -> usize {
    text[..pos].char_indices().next_back().map_or(0, |(i, _)| i)
}

/// The offset `column` graphemes into the line `range` (or its end)
fn offset_of_column(text: &str, range: Range<usize>, column: usize) -> usize {
    text[range.clone()]
        .grapheme_indices(true)
        .nth(column)
        .map_or(range.end, |(i, _)| range.start + i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn graphemes_move_as_one() {
        let b = LineBuffer::new("ae\u{301}");
        assert_eq!(b.cursor_left(false).cursor(), 1);
        assert_eq!(b.backspace().text(), "a");
        let flag = LineBuffer::with_cursor("🇬🇧x", 0);
        assert_eq!(flag.cursor_right(false).cursor(), "🇬🇧".len());
    }

    #[test]
    fn markers_round_trip() {
        let b = LineBuffer::parse_marked("a▷bc▶d");
        assert_eq!((b.text(), b.cursor(), b.anchor()), ("abcd", 3, Some(1)));
        assert_eq!(format!("{b:?}"), "\"a▷bc▶d\"");
        assert_eq!(LineBuffer::parse_marked("\"foo\"◀").cursor(), 4);
    }
}
