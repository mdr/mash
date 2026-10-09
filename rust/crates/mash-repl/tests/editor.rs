//! Ports of `editor/QuoteTogglerTest.scala` and `editor/BracketMatcherTest.scala` (`SyntaxSelectionTest` is
//! in `mash-syntax/tests/selection.rs`). `▶` marks the cursor, `◀` the cursor before the preceding character,
//! and `▷` the other end of the selection.

use mash_repl::editor::{matching_bracket, toggle_quotes};
use mash_repl::line_buffer::LineBuffer;

const QUOTE_CASES: &[(&str, &str)] = &[
    ("foo bar▶ baz", r#"foo "bar"▶ baz"#),
    (r#"foo "bar"▶ baz"#, "foo bar▶ baz"),
    ("foo b▶ar baz", r#"foo "b▶ar" baz"#),
    (r#"foo "b▶ar" baz"#, "foo b▶ar baz"),
    ("foo ▶bar baz", r#"foo "▶bar" baz"#),
    (r#"foo "▶bar" baz"#, "foo ▶bar baz"),
    (r#"foo ▶"bar" baz"#, "foo ▶bar baz"),
    ("foo▶", r#""foo"▶"#),
    (r#""foo▶""#, "foo▶"),
    ("'foo▶'", "foo▶"),
    ("▶", "▶"),
    (r#""foo▶"#, r#""foo"▶"#),
    (r#""fo▶o"#, r#""fo▶o""#),
    (r#"foo"▶"#, r#""foo"▶"#),
    ("$▶", r#""`$"▶"#),
    ("fo▶o", r#""fo▶o""#),
    (r#"foo▶""#, r#""foo▶""#),
    (r#""f▶oo"#, r#""f▶oo""#),
    (r#"▶"foo"#, r#"▶"foo""#),
    ("foo ▶ bar", "foo ▶ bar"),
    (r#"foo "▶"#, r#"foo ""▶"#),
    (r#""`$"▶"#, "$▶"),
    ("baz ▷foo bar▶ baz", r#"baz "foo bar"▶ baz"#),
];

#[test]
fn quote_toggler() {
    for (input, expected) in QUOTE_CASES {
        let actual = toggle_quotes(&LineBuffer::parse_marked(input), false);
        assert_eq!(
            actual,
            LineBuffer::parse_marked(expected),
            "toggling quotes in {input:?}"
        );
    }
}

const BRACKET_CASES: &[(&str, &str)] = &[
    ("▶(3 + 5)", "(3 + 5)◀"),
    ("square▶(3 + 5)", "square(3 + 5)◀"),
    ("def square ▶(n) = n * n", "def square (n)◀ = n * n"),
    ("@▶(attribute 10) class X", "@(attribute 10)◀ class X"),
    ("xs▶[0]", "xs[0]◀"),
    ("▶[1, 2]", "[1, 2]◀"),
    ("▶[a, b] = c", "[a, b]◀ = c"),
    ("▶{ x; y }", "{ x; y }◀"),
    ("▶{ x: y }", "{ x: y }◀"),
    ("▶{ x: y } = z", "{ x: y }◀ = z"),
    ("class X ▶{  }", "class X {  }◀"),
    (r#" "$▶{pwd}" "#, r#" "${pwd}◀" "#),
    ("!▶{ ${pwd} }", "!{ ${pwd} }◀"),
    ("!!▶{ ${pwd} }", "!!{ ${pwd} }◀"),
];

#[test]
fn bracket_matcher() {
    for (left, right) in BRACKET_CASES {
        // Both ways round
        for (input, expected) in [(left, right), (right, left)] {
            let input = LineBuffer::parse_marked(input);
            let expected = LineBuffer::parse_marked(expected).cursor();
            assert_eq!(
                matching_bracket(input.text(), input.cursor(), false),
                Some(expected),
                "matching bracket in {input:?}"
            );
        }
    }
}
