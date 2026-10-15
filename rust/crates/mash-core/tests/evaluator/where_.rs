//! Port of `evaluator/WhereTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"[1, 2, 3, 2, 1] | where (_ > 2)"#, r#"[3]"#),
    Case::eq(r#"[1, 2, 3, 2, 1]. where (_ > 2)"#, r#"[3]"#),
    Case::eq(r#"'foobar' | where (_ > 'm')"#, r#"'oor'"#),
    Case::eq(r#"'foobar' | where (_ => false)"#, r#"''"#),
    Case::eq(r#"'foobar'.toPath | where (_ > 'm') | .tag"#, r#"os.Path"#),
    Case::eq(r#"'foobar'.where (_ > 'm')"#, r#"'oor'"#),
    Case::eq(
        r#"{ apple: 1, bob: 2, aardvark: 3 }.where (.startsWith 'a')"#,
        r#"{ apple: 1, aardvark: 3 }"#,
    ),
    Case::eq(
        r#"{ apple: 1, bob: 2, aardvark: 3 }.where (f v => f.startsWith 'b' or v == 3)"#,
        r#"{ bob: 2, aardvark: 3 }"#,
    ),
    Case::eq(
        r#"{ apple: 1, bob: 2, aardvark: 3 } | where (.startsWith 'a')"#,
        r#"{ apple: 1, aardvark: 3 }"#,
    ),
    Case::eq(
        r#"{ apple: 1, bob: 2, aardvark: 3 } | where (f v => f.startsWith 'b' or v == 3)"#,
        r#"{ bob: 2, aardvark: 3 }"#,
    ),
    Case::eq(
        r#"{ apple: 1, bob: 2, aardvark: 3 }.where (f v i => i == 1)"#,
        r#"{ bob: 2 }"#,
    ),
    Case::eq(r#"[1, 2, 3, 2, 1] | whereNot (_ > 2)"#, r#"[1, 2, 2, 1]"#),
    Case::eq(r#"[1, 2, 3, 2, 1].whereNot (_ > 2)"#, r#"[1, 2, 2, 1]"#),
    Case::eq(r#"'foobar' | whereNot (_ > 'm')"#, r#"'fba'"#),
    Case::eq(r#"'foobar'.whereNot (_ > 'm')"#, r#"'fba'"#),
    Case::eq(
        r#"{ apple: 1, bob: 2, aardvark: 3 }.whereNot (.startsWith 'a')"#,
        r#"{ bob: 2 }"#,
    ),
    Case::eq(
        r#"{ apple: 1, bob: 2, aardvark: 3 }.whereNot (f v => f.startsWith 'b' or v == 3)"#,
        r#"{ apple: 1 }"#,
    ),
    Case::eq(
        r#"{ apple: 1, bob: 2, aardvark: 3 } | whereNot (.startsWith 'a')"#,
        r#"{ bob: 2 }"#,
    ),
    Case::eq(
        r#"{ apple: 1, bob: 2, aardvark: 3 } | whereNot (f v => f.startsWith 'b' or v == 3)"#,
        r#"{ apple: 1 }"#,
    ),
    Case::eq(
        r#"{ apple: 1, bob: 2, aardvark: 3 }.whereNot (f v i => i == 1)"#,
        r#"{ apple: 1, aardvark: 3 }"#,
    ),
    // New: whereNotNull leaves out nulls (only nulls), or elements whose field or value is null or missing
    Case::eq(r#"whereNotNull [1, null, 3, 0, false, '']"#, r#"[1, 3, 0, false, '']"#),
    Case::eq(
        r#"[{ e: 'a' }, { e: null }, { x: 1 }] | whereNotNull 'e'"#,
        r#"[{ e: 'a' }]"#,
    ),
    Case::eq(
        r#"[{ c: { a: 1 } }, { c: { a: null } }] | whereNotNull (.c.a)"#,
        r#"[{ c: { a: 1 } }]"#,
    ),
    Case::eq(r#"[1, null].whereNotNull"#, r#"[1]"#),
    Case::eq(r#"[] | whereNotNull 'e'"#, r#"[]"#),
    Case::throws(r#"[{ x: 1 }] | whereNotNull (.e)"#),
    Case::throws(r#"'abc' | whereNotNull"#),
    Case::throws(r#"[1] | whereNotNull 5"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones (or other modules)
const DEFERRED: &[&str] = &[];
