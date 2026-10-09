//! Port of `evaluator/PatternsTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"{ foo: 42, bar: 128 } | { foo } => foo + 1"#, r#"43"#),
    Case::eq(r#"{ foo: 42, bar: 128 } | { wibble } => wibble"#, r#"null"#),
    Case::eq(r#"{ foo: 42, bar: 128 } | { foo, bar } => foo + bar"#, r#"170"#),
    Case::eq(r#"{ foo: 42, bar: 128 } | { foo: foofar } => foofar"#, r#"42"#),
    Case::eq(r#"{ foo: 42, bar: 128 } | { foo: _ } => 10"#, r#"10"#),
    Case::eq(r#"(_ => 42) 10"#, r#"42"#),
    Case::eq(r#"def foo { bar } = bar; foo { bar: 42 }"#, r#"42"#),
    Case::throws(r#"{ baz } = 42"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
