//! Port of `evaluator/SortTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#" ['c', 'a', 'b'].sort "#, r#"['a', 'b', 'c']"#),
    Case::eq(r#"[1, null, 2].sort"#, r#"[null, 1, 2]"#),
    Case::eq(r#"sort [1, 3, 2] --descending"#, r#"[3, 2, 1]"#),
    // New: descending sorts are stable, keeping equal elements in their original order (Scala reversed them)
    Case::eq(
        r#"[{ k: 'a', n: 1 }, { k: 'b', n: 1 }, { k: 'c', n: 2 }] | sortBy (.n) --descending | map (.k)"#,
        r#"['c', 'a', 'b']"#,
    ),
    Case::eq(
        r#"[{ k: 'a', n: 1 }, { k: 'b', n: 2 }, { k: 'c', n: 1 }].sortBy (.n) --descending | map (.k)"#,
        r#"['b', 'a', 'c']"#,
    ),
    Case::eq(
        r#"['a10', 'b2', 'a2', 'c1'] | sortBy first --naturalOrder --descending"#,
        r#"['c1', 'b2', 'a10', 'a2']"#,
    ),
    Case::eq(r#"'abcb' | sortBy (c => c == 'b') --descending"#, r#"'bbac'"#),
    Case::eq(
        r#"{ a: 1, b: 2, c: 1 }.sortBy (f v => v) --descending | .fields.name.join"#,
        r#"'bac'"#,
    ),
    // Two passes sort by a second key, as the first pass's order survives ties in the second
    Case::eq(
        r#"[{ k: 'b', n: 1 }, { k: 'c', n: 2 }, { k: 'a', n: 1 }] | sortBy (.k) | sortBy (.n) --descending | map (.k)"#,
        r#"['c', 'a', 'b']"#,
    ),
    Case::eq(
        r#"sort ['a1.txt', 'a10.txt', 'a2.txt'] --naturalOrder"#,
        r#"['a1.txt', 'a2.txt', 'a10.txt']"#,
    ),
    Case::ok(
        r#"sort [15, 12, 9, 3, 2, 90, 75, 7, 18, 9, 2, 1, 1, 14, 3, 3, 2, 21, 53, 2, 61, 24, 31, 1, 13, 14, 21, 4, 28, 17, 2, 5, 1, 17, 3, 3, 10, 100, 246, 176, 2, 10, 2, 4, 1, 1, 2, 1, 261, 1, 27, 10, 3, 6, 390, 44, 1, 2, 4, 1, 13, 4, 6, 1, 2, 8, 9, 3, 33, 9, 3, 131, 10, 2, 15, 35, 2, 157, 71, 32, 4, 12, 6, 7, 3, 8, 43, 8, 35, 1, 1, 11, 4, 2, 1, 9, 1]"#,
    ),
    Case::eq(r#"'eaebcd' | sort"#, r#"'abcdee'"#),
    Case::eq(r#"'eaebcd'.sort"#, r#"'abcdee'"#),
    Case::eq(
        r#"{ z: 26, m: 13, b: 2, c: 3, a: 1 } | sort | .fields.name.join"#,
        r#"'abcmz'"#,
    ),
    Case::eq(
        r#"{ z: 26, m: 13, b: 2, c: 3, a: 1 }.sort.fields.name.join"#,
        r#"'abcmz'"#,
    ),
    Case::eq(
        r#"sort { a1: 1, a10: 10, a2: 10 } --naturalOrder | .fields.name"#,
        r#"['a1', 'a2', 'a10']"#,
    ),
    Case::eq(r#"sort {}"#, r#"{}"#),
    Case::eq(r#" ['aa', 'b', 'ccc'] | sortBy length "#, r#" ['b', 'aa', 'ccc'] "#),
    Case::eq(
        r#"[{ foo: 1 }, { foo: null }, { foo: 2 }].sortBy 'foo'"#,
        r#"[{ foo: null }, { foo: 1 }, { foo: 2 }]"#,
    ),
    Case::eq(
        r#"[{ foo: 'a1.txt'}, { foo: 'a10.txt' }, { foo: 'a2.txt' }] | sortBy (.foo) --naturalOrder"#,
        r#"[{ foo: 'a1.txt'}, { foo: 'a2.txt' }, { foo: 'a10.txt' }]"#,
    ),
    Case::eq(r#"'123' | sortBy (-_.toNumber)"#, r#"'321'"#),
    Case::eq(r#"'123'.sortBy (-_.toNumber)"#, r#"'321'"#),
    Case::eq(r#"{ a: 3, b: 2, c: 1 }.sortBy (_2) | .fields.name.join"#, r#"'cba'"#),
    Case::eq(r#"{ a: 3, b: 2, c: 1 } | sortBy (_2) | .fields.name.join"#, r#"'cba'"#),
    Case::eq(
        r#"{ a: 3, b: 2, d: 1, c: 1 }.sortBy (f v => [v, f]) | .fields.name.join"#,
        r#"'cdba'"#,
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
