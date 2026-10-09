//! Syntax-aware editing operations (Scala `editor` package): toggling quotes, matching brackets and expanding
//! the selection.

use std::ops::Range;

use mash_core::complete::contiguous_region;
use mash_syntax::escapes::{escape_chars, unescape};
use mash_syntax::{Mode, SyntaxKind, TextRange, TextSize, parse};

use crate::line_buffer::LineBuffer;

fn text_range(range: Range<usize>) -> TextRange {
    TextRange::new(TextSize::from(range.start as u32), TextSize::from(range.end as u32))
}

fn byte_range(range: TextRange) -> Range<usize> {
    usize::from(range.start())..usize::from(range.end())
}

fn mode(mish: bool) -> Mode {
    if mish { Mode::Mish } else { Mode::Mash }
}

// ------------------------------------------------------------------------------------------- quote toggling

/// Put double quotes around the selection, or the word at the cursor, escaping as necessary; or, if it's
/// already quoted, remove them (Scala `QuoteToggler`).
pub fn toggle_quotes(line_buffer: &LineBuffer, mish: bool) -> LineBuffer {
    match target_region(line_buffer, mish) {
        Some(region) => {
            let target = &line_buffer.text()[region.clone()];
            if is_quoted(target) {
                unquote(line_buffer, region)
            } else {
                quote(line_buffer, region)
            }
        }
        None => line_buffer.clone(),
    }
}

fn target_region(line_buffer: &LineBuffer, mish: bool) -> Option<Range<usize>> {
    line_buffer.selection().or_else(|| {
        let token = cursor_token(line_buffer.text(), mish, line_buffer.cursor())?;
        Some(byte_range(contiguous_region(line_buffer.text(), token, mish, true)))
    })
}

fn is_quoted(s: &str) -> bool {
    s.len() > 1 && (s.starts_with('"') && s.ends_with('"') || s.starts_with('\'') && s.ends_with('\''))
}

/// A non-whitespace token under the cursor, or else immediately to its left
fn cursor_token(text: &str, mish: bool, cursor: usize) -> Option<TextRange> {
    let tokens = mash_syntax::lexer::tokenise(text, mish).tokens;
    let pos = TextSize::from(cursor as u32);
    let usable = |t: &&mash_syntax::lexer::Token| t.kind != SyntaxKind::WHITESPACE;
    let under = tokens.iter().find(|t| t.range.start() <= pos && pos < t.range.end());
    let before = tokens.iter().find(|t| t.range.end() == pos);
    under.filter(usable).or(before.filter(usable)).map(|t| t.range)
}

fn unquote(line_buffer: &LineBuffer, region: Range<usize>) -> LineBuffer {
    let text = line_buffer.text();
    let cursor = line_buffer.cursor();
    let inner = &text[region.start + 1..region.end - 1];
    let unescaped = unescape(inner);
    let new_text = format!("{}{unescaped}{}", &text[..region.start], &text[region.end..]);
    let new_cursor = if unescaped.len() < inner.len() {
        region.start + unescaped.len()
    } else if cursor == region.start {
        cursor
    } else if region.contains(&cursor) {
        cursor - 1
    } else {
        cursor - 2
    };
    LineBuffer::with_cursor(new_text, new_cursor)
}

fn quote(line_buffer: &LineBuffer, region: Range<usize>) -> LineBuffer {
    let text = line_buffer.text();
    let cursor = line_buffer.cursor();
    let target = &text[region.clone()];
    let (inner, initial_removed) = match target.strip_prefix('"') {
        Some(rest) => (rest, true),
        None => (target, false),
    };
    let (inner, final_removed) = match inner.strip_suffix('"') {
        Some(rest) => (rest, true),
        None => (inner, false),
    };
    let escaped = escape_chars(inner);
    let quoted = format!("\"{escaped}\"");
    let new_text = format!("{}{quoted}{}", &text[..region.start], &text[region.end..]);
    let new_cursor = if escaped.len() > inner.len() {
        // Keeping track of the cursor through escapes is more complex; move it to the end of the string
        region.start + quoted.len()
    } else if cursor < region.end {
        cursor + usize::from(!initial_removed)
    } else {
        cursor + if initial_removed || final_removed { 1 } else { 2 }
    };
    LineBuffer::with_cursor(new_text, new_cursor)
}

// ------------------------------------------------------------------------------------------- brackets

/// The offset of the bracket matching the one at `offset`, if there is one there (Scala `BracketMatcher`).
/// Brackets are `(`, `[` and `{`, including the `{` of `${…}`, `!{…}` and `!!{…}`.
pub fn matching_bracket(text: &str, offset: usize, mish: bool) -> Option<usize> {
    let root = parse(text, mode(mish)).syntax();
    let pos = TextSize::from(offset as u32);
    let token = root
        .descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.text_range().contains(pos))?;
    let siblings: Vec<_> = token
        .parent()?
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .collect();
    let index = siblings.iter().position(|t| t == &token)?;
    let matching = if is_open_bracket(token.kind()) {
        siblings[index + 1..]
            .iter()
            .find(|t| t.kind() == closing_bracket(token.kind()))?
    } else if matches!(
        token.kind(),
        SyntaxKind::RPAREN | SyntaxKind::RSQUARE | SyntaxKind::RBRACE
    ) {
        siblings[..index]
            .iter()
            .rev()
            .find(|t| is_open_bracket(t.kind()) && closing_bracket(t.kind()) == token.kind())?
    } else {
        return None;
    };
    Some(usize::from(matching.text_range().start()) + bracket_offset_in_token(matching.kind()))
}

fn is_open_bracket(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    matches!(
        kind,
        LPAREN
            | LPAREN_INVOKE
            | LSQUARE
            | LSQUARE_LOOKUP
            | LBRACE
            | MISH_INTERPOLATION_START
            | MISH_INTERPOLATION_START_NO_CAPTURE
            | STRING_INTERPOLATION_START_COMPLEX
    )
}

fn closing_bracket(open: SyntaxKind) -> SyntaxKind {
    use SyntaxKind::*;
    match open {
        LPAREN | LPAREN_INVOKE => RPAREN,
        LSQUARE | LSQUARE_LOOKUP => RSQUARE,
        _ => RBRACE,
    }
}

/// Where the bracket character is within a multi-character bracket token (`!{`, `!!{`, `${`)
fn bracket_offset_in_token(kind: SyntaxKind) -> usize {
    match kind {
        SyntaxKind::MISH_INTERPOLATION_START | SyntaxKind::STRING_INTERPOLATION_START_COMPLEX => 1,
        SyntaxKind::MISH_INTERPOLATION_START_NO_CAPTURE => 2,
        _ => 0,
    }
}

// ------------------------------------------------------------------------------------------- selection

/// The next larger piece of syntax around the selection (Scala `SyntaxSelection.expandSelection`), or None
/// if the selection can't grow
pub fn expand_selection(line_buffer: &LineBuffer, mish: bool) -> Option<Range<usize>> {
    let root = parse(line_buffer.text(), mode(mish)).syntax();
    let selection = text_range(line_buffer.selection_or_cursor());
    mash_syntax::selection::expand_selection(&root, selection).map(byte_range)
}
