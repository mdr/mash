//! Port of `evaluator/VariadicParameterTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"def mkList n... = n; mkList 1 2 3"#, r#"[1, 2, 3]"#),
    Case::eq(r#"def mkList n... = n; mkList"#, r#"[]"#),
    Case::eq(
        r#"def mkList a b c n... = n + [a, b, c]; mkList 1 2 3 4 5"#,
        r#"[4, 5, 1, 2, 3]"#,
    ),
    Case::eq(r#"def mkList (n...) = n; mkList 1 2 3"#, r#"[1, 2, 3]"#),
    Case::throws(r#"def mkList (xs...) = xs; mkList --xs=42"#),
    Case::eq(r#"(x... => x.sum) 1 2 3"#, r#"6"#),
    Case::eq(
        r#"def foo bar... (baz=42) = { bar, baz }; foo 1 2 3"#,
        r#"{ bar: [1, 2], baz: 3 }"#,
    ),
    Case::eq(
        r#"def foo bar... baz = { bar, baz }; foo 1 2 3"#,
        r#"{ bar: [1, 2], baz: 3 }"#,
    ),
    Case::eq(r#"def foo bar... baz = { bar, baz }; foo 1"#, r#"{ bar: [], baz: 1 }"#),
    Case::eq(
        r#"def foo bar... baz = { bar, baz }; foo 1 --bar=[2, 3]"#,
        r#"{ bar: [2, 3], baz: 1 }"#,
    ),
    Case::setup(
        r#"def f a (b=2) c... (d=4) e = { a, b, c, d, e }"#,
        &[
            Case::eq(r#"f 10 50"#, r#"{ a: 10, b: 2, c: [], d: 4, e: 50 }"#),
            Case::eq(r#"f 10 20 50"#, r#"{ a: 10, b: 20, c: [], d: 4, e: 50 }"#),
            Case::eq(r#"f 10 20 40 50"#, r#"{ a: 10, b: 20, c: [], d: 40, e: 50 }"#),
            Case::eq(r#"f 10 20 30 40 50"#, r#"{ a: 10, b: 20, c: [30], d: 40, e: 50 }"#),
            Case::eq(r#"f 20 50 --a=10"#, r#"{ a: 10, b: 20, c: [], d: 4, e: 50 }"#),
        ],
    ),
    Case::eq(
        r#"def f (@flatten args...) = { args }; f 1 2 3"#,
        r#"{ args: [1, 2, 3] }"#,
    ),
    Case::eq(
        r#"def f (@flatten args...) = { args }; f [1, 2, 3]"#,
        r#"{ args: [1, 2, 3] }"#,
    ),
    Case::eq(r#"def f (args...) = { args }; f [1, 2, 3]"#, r#"{ args: [[1, 2, 3]] }"#),
    Case::setup(r#"def f (@atLeastOne args...) = { args }"#, &[Case::eq(r#"f"#, r#"f"#)]),
    Case::setup(r#"def f (args...) = { args }"#, &[Case::eq(r#"f"#, r#"{ args: [] }"#)]),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
