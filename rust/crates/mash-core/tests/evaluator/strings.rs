//! String literal behaviour not covered by the Scala tests, including a deliberate difference: a leading `~` in
//! an interpolated string expands to the home directory, as it does in a plain double-quoted string (Scala Mash
//! dropped it, so `"~/$x"` gave `/foo`). The test platform's home is `/home/alice`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    // Plain double-quoted strings (as Scala)
    Case::eq(r#""~/foo""#, r#"'/home/alice/foo'"#),
    Case::eq(r#""~""#, r#"'/home/alice'"#),
    Case::eq(r#"'~/foo'"#, r#"'~' + '/foo'"#),
    Case::eq(r#""`~/foo""#, r#"'~' + '/foo'"#),
    // Interpolated strings: the leading `~` expands (Scala dropped it)
    Case::eq(r#"x = "foo"; "~/$x""#, r#"'/home/alice/foo'"#),
    Case::eq(r#"x = "foo"; "~/${x}/bar""#, r#"'/home/alice/foo/bar'"#),
    Case::eq(r#"x = "foo"; ("~/$x").isA os.Path"#, r#"true"#),
    // Only a leading, unescaped `~` (as Scala)
    Case::eq(r#"x = 1; "a~/$x""#, r#"'a~/1'"#),
    Case::eq(r#"x = 1; "`~/$x""#, r#"'~/1'"#),
    Case::eq(r#"x = 1; '~/$x'"#, r#"'~/' + '$x'"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

const DEFERRED: &[&str] = &[];
