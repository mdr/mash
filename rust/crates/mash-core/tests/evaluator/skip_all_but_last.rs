//! Port of `evaluator/SkipAllButLastTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"[1, 2, 3, 4, 5] | allButLast"#, r#"[1, 2, 3, 4]"#),
    Case::eq(r#"[1, 2, 3, 4, 5].allButLast"#, r#"[1, 2, 3, 4]"#),
    Case::eq(r#"[1, 2, 3, 4, 5] | allButLast 3"#, r#"[1, 2]"#),
    Case::eq(r#"[1, 2, 3] | allButLast 5"#, r#"[]"#),
    Case::eq(r#"[] | allButLast"#, r#"[]"#),
    Case::eq(r#"[] | allButLast 5"#, r#"[]"#),
    Case::eq(r#"'abcde' | allButLast"#, r#"'abcd'"#),
    Case::eq(r#"'abcde'.allButLast"#, r#"'abcd'"#),
    Case::eq(
        r#"{ a: 1, b: 2, c: 3 } | allButLast | select --add --order=(.fields.name.join)"#,
        r#"{ a: 1, b: 2, order: 'ab' }"#,
    ),
    Case::eq(r#"{ a: 1, b: 2, c: 3 }.allButLast"#, r#"{ a: 1, b: 2 }"#),
    Case::eq(r#"[1, 2, 3, 4, 5] | skip 2"#, r#"[3, 4, 5]"#),
    Case::eq(r#"[1, 2, 3, 4, 5].skip 2"#, r#"[3, 4, 5]"#),
    Case::eq(r#"[1, 2] | skip 5"#, r#"[]"#),
    Case::eq(r#"[1, 2] | skip"#, r#"[2]"#),
    Case::eq(r#"'abcde' | skip 3"#, r#"'de'"#),
    Case::eq(r#"'abcde' | skip"#, r#"'bcde'"#),
    Case::eq(r#"'abcde'.skip 3"#, r#"'de'"#),
    Case::eq(r#"'abcde'.skip"#, r#"'bcde'"#),
    Case::eq(
        r#"{ a: 1, b: 2, c: 3 } | skip | select --add --order=(.fields.name.join)"#,
        r#"{ b: 2, c: 3, order: 'bc' }"#,
    ),
    Case::eq(r#"{ a: 1, b: 2, c: 3 }.skip"#, r#"{ b: 2, c: 3 }"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
