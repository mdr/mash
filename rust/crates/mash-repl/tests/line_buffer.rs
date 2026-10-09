//! Port of `repl/LineBufferTest.scala`. In the strings, `▶` is the cursor and `▷` the other end of the
//! selection.

use mash_repl::line_buffer::LineBuffer;

fn lb(s: &str) -> LineBuffer {
    LineBuffer::parse_marked(s)
}

#[track_caller]
fn check(actual: LineBuffer, expected: &str) {
    assert_eq!(actual, lb(expected));
}

#[test]
fn cursor_right() {
    check(lb("▶abc").cursor_right(false), "a▶bc");
    check(lb("▶").cursor_right(false), "▶");
    check(lb("a▶").cursor_right(false), "a▶");
    check(lb("▶a").cursor_right(false), "a▶");
    check(lb("abc▶\ndef").cursor_right(false), "abc\n▶def");
    check(lb("abc\ndef▶").cursor_right(false), "abc\ndef▶");
}

#[test]
fn cursor_right_extending_selection() {
    check(lb("▶abc").cursor_right(true), "▷a▶bc");
    check(lb("▷a▶bc").cursor_right(true), "▷ab▶c");
    check(lb("▷ab▶c").cursor_right(true), "▷abc▶");
    check(lb("▷abc▶").cursor_right(true), "▷abc▶");
    check(lb("▶").cursor_right(true), "▶");
    check(lb("a▶").cursor_right(true), "a▶");
    check(lb("▶a").cursor_right(true), "▷a▶");
    check(lb("abc▶\ndef").cursor_right(true), "abc▷\n▶def");
}

#[test]
fn cursor_left() {
    check(lb("abc▶").cursor_left(false), "ab▶c");
    check(lb("▶").cursor_left(false), "▶");
    check(lb("▶a").cursor_left(false), "▶a");
    check(lb("a▶").cursor_left(false), "▶a");
    check(lb("abc\n▶def").cursor_left(false), "abc▶\ndef");
}

#[test]
fn cursor_left_extending_selection() {
    check(lb("abc▶").cursor_left(true), "ab▶c▷");
    check(lb("ab▶c▷").cursor_left(true), "a▶bc▷");
    check(lb("a▶bc▷").cursor_left(true), "▶abc▷");
    check(lb("▶abc▷").cursor_left(true), "▶abc▷");
    check(lb("▶").cursor_left(true), "▶");
    check(lb("▶a").cursor_left(true), "▶a");
    check(lb("a▶").cursor_left(true), "▶a▷");
    check(lb("abc\n▶def").cursor_left(true), "abc▶\n▷def");
}

#[test]
fn cursor_up() {
    check(lb("abc▶").cursor_up(false), "abc▶");
    check(lb("▶").cursor_up(false), "▶");
    check(lb("abc\n▶def").cursor_up(false), "▶abc\ndef");
    check(lb("abcdef\nghi▶").cursor_up(false), "abc▶def\nghi");
    check(lb("abc\ndefghi▶").cursor_up(false), "abc▶\ndefghi");
    check(lb("\n▶def").cursor_up(false), "▶\ndef");
    check(lb("abcdef\nghi▶").cursor_up(true), "abc▶def\nghi▷");
}

#[test]
fn cursor_down() {
    check(lb("abc▶").cursor_down(false), "abc▶");
    check(lb("▶").cursor_down(false), "▶");
    check(lb("▶abc\ndef").cursor_down(false), "abc\n▶def");
    check(lb("abc▶def\nghi").cursor_down(false), "abcdef\nghi▶");
    check(lb("abc▶\ndefghi").cursor_down(false), "abc\ndef▶ghi");
    check(lb("▶\ndef").cursor_down(false), "\n▶def");
    check(lb("abc▶def\nghi").cursor_down(true), "abc▷def\nghi▶");
}

#[test]
fn moving_without_extending_abandons_the_selection() {
    check(lb("▷a▶bc").cursor_right(false), "ab▶c");
    check(lb("▷abc▶").cursor_right(false), "abc▶");
    check(lb("a▶bc▷").cursor_left(false), "▶abc");
    check(lb("▶abc▷").cursor_left(false), "▶abc");
    check(lb("▶abc▷").cursor_up(false), "▶abc");
    check(lb("▶abc▷").cursor_down(false), "▶abc");
}

#[test]
fn insert_at_cursor() {
    check(lb("▶").insert_at_cursor("a"), "a▶");
    check(lb("ab▶").insert_at_cursor("c"), "abc▶");
    check(lb("a▶c").insert_at_cursor("b"), "ab▶c");
    check(lb("abc\n▶").insert_at_cursor("d"), "abc\nd▶");
    check(lb("▶").insert_at_cursor("\n"), "\n▶");
}

#[test]
fn insert_replaces_the_selection() {
    check(lb("a▷bcd▶e").insert_at_cursor("X"), "aX▶e");
    check(lb("a▷bcd▶e").insert_at_cursor("XYZ"), "aXYZ▶e");
    check(lb("a▶bcd▷e").insert_at_cursor("X"), "aX▶e");
    check(lb("▷a▶").insert_at_cursor("X"), "X▶");
    check(lb("▶a▷").insert_at_cursor("X"), "X▶");
}

#[test]
fn delete() {
    check(lb("▶123").delete(), "▶23");
    check(lb("▶").delete(), "▶");
    check(lb("123▶").delete(), "123▶");
    check(lb("1▶23").delete(), "1▶3");
    check(lb("1▷234▶5").delete(), "1▶5");
    check(lb("1▶234▷5").delete(), "1▶5");
    check(lb("1▷2345▶").delete(), "1▶");
}

#[test]
fn backspace() {
    check(lb("123▶").backspace(), "12▶");
    check(lb("▶").backspace(), "▶");
    check(lb("▶123").backspace(), "▶123");
    check(lb("12▶3").backspace(), "1▶3");
    check(lb("abc\n▶").backspace(), "abc▶");
    check(lb("abc\n▶def").backspace(), "abc▶def");
    check(lb("1▷234▶5").backspace(), "1▶5");
    check(lb("1▶234▷5").backspace(), "1▶5");
    check(lb("1▷2345▶").backspace(), "1▶");
}

#[test]
fn delete_forward_word() {
    check(lb("▶foo").delete_forward_word(), "▶");
    check(lb("fo▶o").delete_forward_word(), "fo▶");
    check(lb("foo▶").delete_forward_word(), "foo▶");
    check(lb("foo ▶bar baz").delete_forward_word(), "foo ▶ baz");
    check(lb("▶").delete_forward_word(), "▶");
    check(lb("1▷234▶5").delete_forward_word(), "1▶5");
}

#[test]
fn delete_backward_word() {
    check(lb("▶foo").delete_backward_word(), "▶foo");
    check(lb("fo▶o").delete_backward_word(), "▶o");
    check(lb("foo▶").delete_backward_word(), "▶");
    check(lb("foo bar▶ baz").delete_backward_word(), "foo ▶ baz");
    check(lb("▶").delete_backward_word(), "▶");
    check(lb("1▷234▶5").delete_backward_word(), "1▶5");
}

#[test]
fn backward_word() {
    check(lb("foo bar▶ baz").backward_word(false), "foo ▶bar baz");
    check(lb("foo bar ▶baz").backward_word(false), "foo ▶bar baz");
    check(lb("▶foo bar baz").backward_word(false), "▶foo bar baz");
    check(lb("foo ▶bar▷ baz").backward_word(false), "▶foo bar baz");
    check(lb("foo ▶bar▷ baz").backward_word(true), "▶foo bar▷ baz");
}

#[test]
fn forward_word() {
    check(lb("foo bar ▶baz").forward_word(false), "foo bar baz▶");
    check(lb("foo bar▶ baz").forward_word(false), "foo bar baz▶");
    check(lb("foo bar baz▶").forward_word(false), "foo bar baz▶");
    check(lb("abc\nabcd▶").forward_word(false), "abc\nabcd▶");
    check(lb("abc\nabcd▶ ").forward_word(false), "abc\nabcd ▶");
    check(lb("foo ▷bar ▶baz").forward_word(false), "foo bar baz▶");
    check(lb("foo ▷bar ▶baz").forward_word(true), "foo ▷bar baz▶");
}

#[test]
fn move_to_start_of_line() {
    check(lb("abc▶").move_to_start(false), "▶abc");
    check(lb("abc▶def").move_to_start(false), "▶abcdef");
    check(lb("▶abc").move_to_start(false), "▶abc");
    check(lb("▶").move_to_start(false), "▶");
    check(lb("abc\ndef▶").move_to_start(false), "abc\n▶def");
    check(lb("abc\ndef▶").move_to_start(true), "abc\n▶def▷");
    check(lb("ab▷c▶").move_to_start(false), "▶abc");
}

#[test]
fn move_to_end_of_line() {
    check(lb("abc▶").move_to_end(false), "abc▶");
    check(lb("abc▶def").move_to_end(false), "abcdef▶");
    check(lb("▶abc").move_to_end(false), "abc▶");
    check(lb("▶").move_to_end(false), "▶");
    check(lb("abc\n▶def").move_to_end(false), "abc\ndef▶");
    check(lb("▶ab▷c").move_to_end(false), "abc▶");
    check(lb("abc\n▶def").move_to_end(true), "abc\n▷def▶");
}

#[test]
fn move_to_start_of_buffer_from_start_of_line() {
    check(lb("abc\n▶def").move_to_start(false), "▶abc\ndef");
    check(lb("abc\n▶def").move_to_start(true), "▶abc\n▷def");
}

#[test]
fn move_to_end_of_buffer_from_end_of_line() {
    check(lb("abc▶\ndef").move_to_end(false), "abc\ndef▶");
    check(lb("abc▶\ndef").move_to_end(true), "abc▷\ndef▶");
}

#[test]
fn move_to_end_and_start_of_buffer() {
    check(lb("a▶bc\ndef").move_to_end_of_buffer(false), "abc\ndef▶");
    check(lb("abc\nde▶f").move_to_start_of_buffer(false), "▶abc\ndef");
}

#[test]
fn delete_to_start_of_line() {
    check(lb("▶").delete_to_start_of_line(), "▶");
    check(lb("abc▶").delete_to_start_of_line(), "▶");
    check(lb("▶abc").delete_to_start_of_line(), "▶abc");
    check(lb("foo▶bar").delete_to_start_of_line(), "▶bar");
    check(lb("abc\ndef▶ghi").delete_to_start_of_line(), "abc\n▶ghi");
    check(lb("foo▶bar▷").delete_to_start_of_line(), "▶bar");
}

#[test]
fn delete_to_end_of_line() {
    check(lb("▶").delete_to_end_of_line(), "▶");
    check(lb("abc▶").delete_to_end_of_line(), "abc▶");
    check(lb("▶abc").delete_to_end_of_line(), "▶");
    check(lb("foo▶bar").delete_to_end_of_line(), "foo▶");
    check(lb("abc\ndef▶ghi").delete_to_end_of_line(), "abc\ndef▶");
    check(lb("foo▶bar▷").delete_to_end_of_line(), "foo▶");
}

#[test]
fn replace() {
    check(lb("abc▶").replace(1..2, "X"), "aXc▶");
    check(lb("ab▶c").replace(1..2, "X"), "aX▶c");
    check(lb("a▶bc").replace(1..2, "X"), "aX▶c");
    check(lb("abb▶bbc").replace(1..5, "X"), "aX▶c");

    check(lb("abc▶").replace(1..1, "X"), "aXbc▶");
    check(lb("ab▶c").replace(1..1, "X"), "aXb▶c");
    check(lb("a▶bc").replace(1..1, "X"), "aX▶bc");
    check(lb("▶abc").replace(1..1, "X"), "▶aXbc");

    check(lb("abc▶").replace(3..3, "X"), "abcX▶");

    check(lb("abc▶").replace(1..2, ""), "ac▶");
    check(lb("ab▶c").replace(1..2, ""), "a▶c");
    check(lb("a▶bc").replace(1..2, ""), "a▶c");
    check(lb("▶abc").replace(1..2, ""), "▶ac");
}

// New: graphemes and wide characters

#[test]
fn graphemes() {
    // e + combining acute is one grapheme
    check(lb("ae\u{301}▶b").cursor_left(false), "a▶e\u{301}b");
    check(lb("a▶e\u{301}b").delete(), "a▶b");
    check(lb("日本▶").backspace(), "日▶");
    // Up and down keep the column in graphemes
    check(lb("日本語\nab▶c").cursor_up(false), "日本▶語\nabc");
    // Words of non-ASCII letters
    check(lb("café▶ x").delete_backward_word(), "▶ x");
}
