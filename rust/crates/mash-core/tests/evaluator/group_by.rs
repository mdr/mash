//! Port of `evaluator/GroupByTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(
        r#"[{ path: 'file.txt', type: 'file' }, { path: 'script.mash', type: 'file' }, { path: 'src', type: 'dir' }] | groupBy 'type'"#,
        r#"{ file: [{ path: 'file.txt', type: 'file' }, { path: 'script.mash', type: 'file' }], dir: [{ path: 'src', type: 'dir' }] }"#,
    ),
    Case::eq(
        r#"[{ path: 'file.txt', type: 'file' }, { path: '000', type: null }] | groupBy 'type'"#,
        r#"{ file: [{ path: 'file.txt', type: 'file' }] }"#,
    ),
    Case::eq(
        r#"['a', 'bb', 'c', 'ddd'] | groupBy length"#,
        r#"{ (1): ['a', 'c'], (2): ['bb'], (3): ['ddd'] }"#,
    ),
    Case::eq(
        r#"[{ a: 1, b: 1 }, { a: 1, b: 2 }, { a: 2, b: 3 }, { a: 1, b: 4 }] | groupBy (.a) --groups | map (.unbless)"#,
        r#"[{ key: 1, values: [{ a: 1, b: 1 }, { a: 1, b: 2 }, { a: 1, b: 4 }] }, { key: 2, values: [{ a: 2, b: 3 }] }]"#,
    ),
    Case::eq(
        r#"'foo' | groupBy (x => x) --groups | select 'key' 'count' | sortBy 'key'"#,
        r#"[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "#,
    ),
    Case::eq(
        r#"'foo'.groupBy (x => x) --groups | select 'key' 'count' | sortBy 'key'"#,
        r#"[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "#,
    ),
    Case::eq(
        r#"[null] | groupBy --includeNull (x => x) --groups | select 'key' 'count'"#,
        r#"[ { key: null, count: 1 } ]"#,
    ),
    Case::eq(
        r#"[null] | groupBy --includeNull='nope' (x => x) --groups | select 'key'"#,
        r#"[ { key: 'nope' } ]"#,
    ),
    Case::eq(
        r#"[1, 2, 1] | groupBy --all (x => x) --groups | select 'key' 'count' | sortBy 'count'"#,
        r#"[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'All', count: 3 } ]"#,
    ),
    Case::eq(
        r#"[1, 2, 1] | groupBy --all='totalCount' (x => x) --groups | select 'key' 'count' | sortBy 'count'"#,
        r#"[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'totalCount', count: 3 } ]"#,
    ),
    Case::eq(
        r#"['apple', 'bike', 'book'] | groupBy first --groups | sortBy (.key) | map (.count)"#,
        r#"[1, 2]"#,
    ),
    Case::eq(
        r#"[{ path: 'file.txt', type: 'file' }, { path: 'src', type: 'dir' }] | groupBy 'type' --select='path' --groups | select 'key' 'values' | sortBy 'key'"#,
        r#"[{ key: 'dir', values: ['src'] }, { key: 'file', values: ['file.txt'] }]"#,
    ),
    // New: a key for --all or --includeNull that is also a key of the data is an error (Scala replaced that
    // group), and falsy keys are keys (Scala ignored them)
    Case::throws(r#"['All', 'x'] | groupBy (x => x) --all"#),
    Case::throws(r#"[null, 'x'] | groupBy (x => x) --includeNull='x'"#),
    Case::eq(
        r#"[null, 1] | groupBy (x => x) --includeNull=0"#,
        r#"{ 0: [null], 1: [1] }"#,
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
