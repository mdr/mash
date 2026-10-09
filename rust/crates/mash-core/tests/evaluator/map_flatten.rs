//! Port of `evaluator/MapFlattenTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"[1, 2, 3].map (_ * 2)"#, r#"[2, 4, 6]"#),
    Case::eq(r#"map --f=(_ * 2) --sequence=[1, 2, 3]"#, r#"[2, 4, 6]"#),
    Case::eq(r#"map (_ * 2) --sequence=[1, 2, 3]"#, r#"[2, 4, 6]"#),
    Case::eq(r#"[1, 2, 3] | map --f=(_ * 2)"#, r#"[2, 4, 6]"#),
    Case::eq(r#"[1, 2, 3] | map (n i => n + i)"#, r#"[1, 3, 5]"#),
    Case::eq(r#"[1, 2, 3] | map (.toString)"#, r#"['1', '2', '3']"#),
    Case::eq(r#"'123' | map (_.toNumber)"#, r#"[1, 2, 3]"#),
    Case::eq(r#"'foo' | map (.toUpper)"#, r#"'FOO'"#),
    Case::eq(r#"'foo'.map (.toUpper)"#, r#"'FOO'"#),
    Case::eq(r#"'abc'.map (c => c + c)"#, r#"'aabbcc'"#),
    Case::eq(r#"'a1b2'.map (.isDigit)"#, r#"[false, true, false, true]"#),
    Case::eq(r#"''.map (.isDigit)"#, r#"''"#),
    Case::eq(
        r#"{ apple: 1, bob: 2, cat: 3 }.map (f v => { (f.toUpper): v * v })"#,
        r#"{ APPLE: 1, BOB: 4, CAT: 9 }"#,
    ),
    Case::eq(
        r#"{ apple: 1, bob: 2, cat: 3 } | map (f v => { (f.toUpper): v * v })"#,
        r#"{ APPLE: 1, BOB: 4, CAT: 9 }"#,
    ),
    Case::eq(
        r#"{ apple: 1 }.map (f v => { (f): v, (f.reverse): v })"#,
        r#"{ apple: 1, elppa: 1 }"#,
    ),
    Case::eq(
        r#"{ apple: 1 } | map (f v => { (f): v, (f.reverse): v })"#,
        r#"{ apple: 1, elppa: 1 }"#,
    ),
    Case::eq(r#"{ a: 1, b: 2 } | map (f => f)"#, r#"['a', 'b']"#),
    Case::eq(r#"{ a: 1, b: 2 } | map (f v => f + v)"#, r#"['a1', 'b2']"#),
    Case::eq(r#"{ a: 1, b: 2 } | map (f v i => f + v + i)"#, r#"['a10', 'b21']"#),
    Case::eq(r#"[[], [1], [2, 3]] | flatten"#, r#"[1, 2, 3]"#),
    Case::eq(r#"[[], [1], [2, 3]].flatten"#, r#"[1, 2, 3]"#),
    Case::eq(r#"flatten []"#, r#"[]"#),
    Case::eq(r#"flatten ['', 'a', 'bc']"#, r#"'abc'"#),
    Case::eq(r#"flatten [{ a: 1, b: 2 }, { c: 3 }]"#, r#"{ a: 1, b: 2, c: 3 }"#),
    Case::eq(r#"flatten [{ a: 1 }, { a: 2 }, { a: 3 }]"#, r#"{ a: 3 }"#),
    Case::eq(r#"[1, 2, 3].flatMap (n => [n * 10, n])"#, r#"[10, 1, 20, 2, 30, 3]"#),
    Case::eq(r#"[1, 2, 3] | flatMap (n => [n * 10, n])"#, r#"[10, 1, 20, 2, 30, 3]"#),
    Case::eq(r#"[1, 22, 333] | flatMap (.toString)"#, r#"'122333'"#),
    Case::eq(r#"[1, 2, 3] | flatMap (n i => [n, i])"#, r#"[1, 0, 2, 1, 3, 2]"#),
    Case::eq(
        r#"['a', 'b', 'c'] | flatMap (s i => { (s): i })"#,
        r#"{ a: 0, b: 1, c: 2 }"#,
    ),
    Case::eq(r#"'abc' | flatMap (_ + '!')"#, r#"'a!b!c!'"#),
    Case::eq(r#"'abc' | flatMap (c => [c])"#, r#"['a', 'b', 'c']"#),
    Case::eq(r#"'abc'.flatMap (c => [c]) "#, r#"['a', 'b', 'c']"#),
    Case::eq(r#"'abc' | flatMap (s i => { (s): i })"#, r#"{ a: 0, b: 1, c: 2 }"#),
    Case::eq(
        r#"{ apple: 1 }.flatMap (f v => { (f): v, (f.reverse): v })"#,
        r#"{ apple: 1, elppa: 1 }"#,
    ),
    Case::eq(
        r#"{ apple: 1 } | flatMap (f v => { (f): v, (f.reverse): v })"#,
        r#"{ apple: 1, elppa: 1 }"#,
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones (or other modules)
const DEFERRED: &[&str] = &[];
