//! Port of `evaluator/StringMethodsTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#" 'foo'.matches 'o' "#, r#"true"#),
    Case::eq(r#" 'foo'.matches 'x' "#, r#"false"#),
    Case::eq(r#" 'foo'.matches '^fo?.$' "#, r#"true"#),
    Case::eq(r#" 'foo 666 bar'.matches 666 "#, r#"true"#),
    Case::eq(r#" 'bar'.matches 666 "#, r#"false"#),
    Case::eq(r#" 'foo'.startsWith 'f' "#, r#"true"#),
    Case::eq(r#" 'foo'.startsWith 'o' "#, r#"false"#),
    Case::eq(r#"'foo bar baz'.split"#, r#"['foo', 'bar', 'baz']"#),
    Case::eq(r#"'foo bar baz' | split"#, r#"['foo', 'bar', 'baz']"#),
    Case::eq(r#"'foo  bar     baz'.split"#, r#"['foo', 'bar', 'baz']"#),
    Case::eq(r#"'foo:bar:baz'.split ':'"#, r#"['foo', 'bar', 'baz']"#),
    Case::eq(r#"'foo:bar:baz' | split ':'"#, r#"['foo', 'bar', 'baz']"#),
    Case::eq(r#"'foo:bar:'.split ':'"#, r#"['foo', 'bar', '']"#),
    Case::eq(r#"'foo1bar2baz' | split --regex '[0-9]'"#, r#"['foo', 'bar', 'baz']"#),
    Case::eq(r#"'foo1bar2baz'.split --regex '[0-9]'"#, r#"['foo', 'bar', 'baz']"#),
    Case::eq(r#"'foo`nbar`r`nbaz buzz'.lines"#, r#"['foo', 'bar', 'baz buzz']"#),
    Case::eq(r#"''.lines"#, r#"[]"#),
    Case::eq(r#"'foo'.lines"#, r#"['foo']"#),
    Case::eq(r#"'foo`n'.lines"#, r#"['foo']"#),
    Case::eq(r#"'`nfoo'.lines"#, r#"['', 'foo']"#),
    Case::eq(r#"'foo`n`nbar'.lines"#, r#"['foo', '', 'bar']"#),
    Case::eq(r#"'`n`n'.lines"#, r#"['', '']"#),
    Case::eq(r#"'abc'.toList"#, r#"['a', 'b', 'c']"#),
    Case::eq(r#"'abc'.characters"#, r#"['a', 'b', 'c']"#),
    Case::eq(r#"'abc'.characters.first.tag"#, r#"Character"#),
    Case::eq(r#"''.toList"#, r#"[]"#),
    Case::eq(r#"'foo bar: baz'.deleteUpTo 'bar: '"#, r#"'bar: baz'"#),
    Case::eq(r#"'foo bar: baz'.deleteUpTo --andIncluding 'bar: '"#, r#"'baz'"#),
    Case::eq(r#"'result: { "value": 10 }'.deleteUpTo '{'"#, r#"'{ "value": 10 }'"#),
    Case::eq(r#"'value: 10'.deleteUpTo --andIncluding 'value: '"#, r#"'10'"#),
    Case::eq(r#"'abc'.deleteUpTo 'not found'"#, r#"'abc'"#),
    Case::eq(r#"'{ "value": 10 } # comment'.deleteAfter '}'"#, r#"'{ "value": 10 }'"#),
    Case::eq(r#"'10 # comment'.deleteAfter --andIncluding ' #'"#, r#"'10'"#),
    Case::eq(r#"'abc'.deleteAfter 'not found'"#, r#"'abc'"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
