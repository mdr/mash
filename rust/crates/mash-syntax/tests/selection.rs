//! Syntax-tree selection expansion (M-Up in the REPL). Cases from SyntaxSelectionTest.scala, except that
//! a cursor just after a word selects that word.
//!
//! In the test strings, `▶` is the cursor and `▷` the other end of the selection (if any).

use mash_syntax::selection::expand_selection;
use mash_syntax::{Mode, TextRange, TextSize, parse};

/// Text without markers, and the selection (an empty range at the cursor if there's no `▷`)
fn parse_markers(s: &str) -> (String, TextRange) {
    let mut text = String::new();
    let (mut cursor, mut anchor) = (None, None);
    for c in s.chars() {
        match c {
            '▶' => cursor = Some(text.len()),
            '▷' => anchor = Some(text.len()),
            c => text.push(c),
        }
    }
    let cursor = cursor.expect("no cursor marker");
    let anchor = anchor.unwrap_or(cursor);
    let range = TextRange::new(
        TextSize::from(cursor.min(anchor) as u32),
        TextSize::from(cursor.max(anchor) as u32),
    );
    (text, range)
}

#[track_caller]
fn expands(input: &str, expected: &str) {
    let (text, selection) = parse_markers(input);
    let (expected_text, expected_selection) = parse_markers(expected);
    assert_eq!(text, expected_text);
    let root = parse(&text, Mode::Mash).syntax();
    let actual = expand_selection(&root, selection).unwrap_or(selection);
    assert_eq!(&text[actual], &text[expected_selection], "expanding {input:?}");
    assert_eq!(actual, expected_selection, "expanding {input:?}");
}

#[test]
fn tokens_and_expressions() {
    expands("iden▶tifier", "▷identifier▶");
    expands("foo + ba▶r", "foo + ▷bar▶");
    expands("foo + ▶bar", "foo + ▷bar▶");
    // Scala Mash selected the whole expression here; selecting the word just before the cursor is nicer
    expands("foo + bar▶", "foo + ▷bar▶");
    expands("foo + ▷bar▶", "▷foo + bar▶");
    expands("foo▶.bar", "▷foo▶.bar");
    expands("foo.ba▶r", "foo.▷bar▶");
    expands("foo --ba▶r=baz", "foo ▷--bar▶=baz");
}

#[test]
fn operator_precedence() {
    // (Scala Mash went straight to `2 * 3`; the cursor just after `2` now selects `2` first)
    expands("1 + 2▶ * 3", "1 + ▷2▶ * 3");
    expands("1 + ▷2▶ * 3", "1 + ▷2 * 3▶");
    expands("1 + ▷2▶ * 3 * 4", "1 + ▷2 * 3▶ * 4");
    expands("1 + ▷2 * 3▶ * 4", "1 + ▷2 * 3 * 4▶");
    expands("1 + ▷2 * 3 * 4▶", "▷1 + 2 * 3 * 4▶");
}

#[test]
fn declarations_and_comments() {
    expands("def foo = 4▶2", "def foo = ▷42▶");
    expands("def foo = 42 # comm▶ent", "def foo = 42 ▷# comment▶");
    expands("def foo = 42 ▷# comment▶", "▷def foo = 42 # comment▶");
}

#[test]
fn whitespace_and_empty() {
    expands(" ▶", "▷ ▶");
    expands("▶", "▶");
}

#[test]
fn declarations_include_their_doc_comment() {
    expands(
        "{\n  # Comment\n  ▶def foo▷ = 42\n}",
        "{\n  ▷# Comment\n  def foo = 42▶\n}",
    );
    expands("{\n  # Comment\n  ▶class Foo▷\n}", "{\n  ▷# Comment\n  class Foo▶\n}");
}

/// Expanding repeatedly from a cursor walks out through the tree to the whole input.
#[test]
fn expands_through_the_tree() {
    let s = "ls | where (_.owner == 'root') | first";
    let root = parse(s, Mode::Mash).syntax();
    let mut range = TextRange::empty(TextSize::from((s.find("owner").unwrap() + 2) as u32));
    let mut texts = vec![];
    while let Some(next) = expand_selection(&root, range) {
        assert!(next.contains_range(range) && next.len() > range.len());
        texts.push(&s[next]);
        range = next;
    }
    assert_eq!(
        texts,
        [
            "owner",
            "_.owner",
            "_.owner == 'root'",
            "(_.owner == 'root')",
            "where (_.owner == 'root')",
            "ls | where (_.owner == 'root')",
            "ls | where (_.owner == 'root') | first",
        ]
    );
}
