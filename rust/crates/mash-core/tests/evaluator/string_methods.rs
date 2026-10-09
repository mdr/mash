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
const DEFERRED: &[&str] = &[
    // Cannot find member 'matches' in value of type String
    r#" 'foo'.matches 'o' "#,
    // Cannot find member 'matches' in value of type String
    r#" 'foo'.matches 'x' "#,
    // Cannot find member 'matches' in value of type String
    r#" 'foo'.matches '^fo?.$' "#,
    // Cannot find member 'matches' in value of type String
    r#" 'foo 666 bar'.matches 666 "#,
    // Cannot find member 'matches' in value of type String
    r#" 'bar'.matches 666 "#,
    // Cannot find member 'startsWith' in value of type String
    r#" 'foo'.startsWith 'f' "#,
    // Cannot find member 'startsWith' in value of type String
    r#" 'foo'.startsWith 'o' "#,
    // Cannot find member 'split' in value of type String
    r#"'foo bar baz'.split"#,
    // No binding for 'split'
    r#"'foo bar baz' | split"#,
    // Cannot find member 'split' in value of type String
    r#"'foo  bar     baz'.split"#,
    // Cannot find member 'split' in value of type String
    r#"'foo:bar:baz'.split ':'"#,
    // No binding for 'split'
    r#"'foo:bar:baz' | split ':'"#,
    // Cannot find member 'split' in value of type String
    r#"'foo:bar:'.split ':'"#,
    // No binding for 'split'
    r#"'foo1bar2baz' | split --regex '[0-9]'"#,
    // Cannot find member 'split' in value of type String
    r#"'foo1bar2baz'.split --regex '[0-9]'"#,
    // Cannot find member 'lines' in value of type String
    r#"'foo`nbar`r`nbaz buzz'.lines"#,
    // Cannot find member 'lines' in value of type String
    r#"''.lines"#,
    // Cannot find member 'lines' in value of type String
    r#"'foo'.lines"#,
    // Cannot find member 'lines' in value of type String
    r#"'foo`n'.lines"#,
    // Cannot find member 'lines' in value of type String
    r#"'`nfoo'.lines"#,
    // Cannot find member 'lines' in value of type String
    r#"'foo`n`nbar'.lines"#,
    // Cannot find member 'lines' in value of type String
    r#"'`n`n'.lines"#,
    // Cannot find member 'toList' in value of type String
    r#"'abc'.toList"#,
    // Cannot find member 'characters' in value of type String
    r#"'abc'.characters"#,
    // Cannot find member 'characters' in value of type String
    r#"'abc'.characters.first.tag"#,
    // Cannot find member 'toList' in value of type String
    r#"''.toList"#,
    // Cannot find member 'deleteUpTo' in value of type String
    r#"'foo bar: baz'.deleteUpTo 'bar: '"#,
    // Cannot find member 'deleteUpTo' in value of type String
    r#"'foo bar: baz'.deleteUpTo --andIncluding 'bar: '"#,
    // Cannot find member 'deleteUpTo' in value of type String
    r#"'result: { "value": 10 }'.deleteUpTo '{'"#,
    // Cannot find member 'deleteUpTo' in value of type String
    r#"'value: 10'.deleteUpTo --andIncluding 'value: '"#,
    // Cannot find member 'deleteUpTo' in value of type String
    r#"'abc'.deleteUpTo 'not found'"#,
    // Cannot find member 'deleteAfter' in value of type String
    r#"'{ "value": 10 } # comment'.deleteAfter '}'"#,
    // Cannot find member 'deleteAfter' in value of type String
    r#"'10 # comment'.deleteAfter --andIncluding ' #'"#,
    // Cannot find member 'deleteAfter' in value of type String
    r#"'abc'.deleteAfter 'not found'"#,
];
