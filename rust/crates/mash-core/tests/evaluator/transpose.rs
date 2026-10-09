//! Port of `evaluator/TransposeTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"[] | transpose"#, r#"[]"#),
    Case::eq(r#"[[1, 2, 3], [4, 5, 6]].transpose"#, r#"[[1, 4], [2, 5], [3, 6]]"#),
    Case::eq(r#"[[1, 2, 3], [4, 5, 6]] | transpose"#, r#"[[1, 4], [2, 5], [3, 6]]"#),
    Case::eq(r#"[[1, 2, 3]] | transpose"#, r#"[[1], [2], [3]]"#),
    Case::eq(
        r#"[[1, 2, 3], [4, 5], [6, 7, 8]] | transpose"#,
        r#"[[1, 4, 6], [2, 5, 7], [3, null, 8]]"#,
    ),
    Case::eq(
        r#"[[1, 2, 3], [4, 5], [6, 7, 8]] | transpose --skipGaps"#,
        r#"[[1, 4, 6], [2, 5, 7], [3, 8]]"#,
    ),
    Case::eq(r#"[[]] | transpose"#, r#"[]"#),
    Case::eq(r#"[[1]] | transpose"#, r#"[[1]]"#),
    Case::eq(
        r#"[{ foo: 42, bar: 100 }, { foo: 100, baz: 99 }] | transpose"#,
        r#"{ foo: [42, 100], bar: [100, null], baz: [null, 99] }"#,
    ),
    Case::eq(
        r#"[{ foo: 42, bar: 100 }, { foo: 100, baz: 99 }] | transpose --skipGaps"#,
        r#"{ foo: [42, 100], bar: [100], baz: [99] }"#,
    ),
    Case::eq(r#"[{ foo: 42, bar: 100 }] | transpose"#, r#"{ foo: [42], bar: [100] }"#),
    Case::eq(r#"[{ foo: 42 }] | transpose"#, r#"{ foo: [42] }"#),
    Case::eq(r#"[{}] | transpose"#, r#"{}"#),
    Case::eq(r#"{} | transpose"#, r#"{}"#),
    Case::eq(
        r#"{ foo: [1, 2, 3], bar: [4, 5, 6] } | transpose"#,
        r#"[{ foo: 1, bar: 4 }, { foo: 2, bar: 5 }, { foo: 3, bar: 6 }]"#,
    ),
    Case::eq(
        r#"{ foo: [1, 2, 3], bar: [4, 5, 6] }.transpose"#,
        r#"[{ foo: 1, bar: 4 }, { foo: 2, bar: 5 }, { foo: 3, bar: 6 }]"#,
    ),
    Case::eq(
        r#"{ foo: [1, 2, 3], bar: [4, 5, 6] } | transpose | transpose"#,
        r#"{ foo: [1, 2, 3], bar: [4, 5, 6] }"#,
    ),
    Case::eq(
        r#"{ foo: [1, 2], bar: [4, 5, 6] } | transpose"#,
        r#"[{ foo: 1, bar: 4 }, { foo: 2, bar: 5 }, { bar: 6 }]"#,
    ),
    Case::eq(r#"{ foo: [1, 2] } | transpose"#, r#"[{ foo: 1 }, { foo: 2 }]"#),
    Case::eq(r#"{ foo: [1] } | transpose"#, r#"[{ foo: 1 }]"#),
    Case::eq(r#"{ foo: [] } | transpose"#, r#"[]"#),
    Case::eq(
        r#"{ foo: { a: 1, b: 2 }, bar: { a: 3, b: 4 } } | transpose"#,
        r#"{ a: { foo: 1, bar: 3 }, b: { foo: 2, bar: 4 } }"#,
    ),
    Case::eq(
        r#"{ foo: { a: 1, b: 2 }, bar: { a: 3, b: 4 } } | transpose | transpose"#,
        r#"{ foo: { a: 1, b: 2 }, bar: { a: 3, b: 4 } }"#,
    ),
    Case::eq(
        r#"{ foo: { a: 1, b: 2 }, bar: { a: 3 } } | transpose"#,
        r#"{ a: { foo: 1, bar: 3 }, b: { foo: 2 } }"#,
    ),
    Case::eq(
        r#"{ foo: { a: 1, b: 2 } } | transpose"#,
        r#"{ a: { foo: 1 }, b: { foo: 2 } }"#,
    ),
    Case::eq(r#"{ foo: { a: 1 } } | transpose"#, r#"{ a: { foo: 1 } }"#),
    Case::eq(r#"{ foo: {} } | transpose"#, r#"{}"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
