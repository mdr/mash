//! `countBy`, new in the Rust port: `groupBy`, keeping only the size of each group.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(
        r#"countBy (x => x) ['a', 'b', 'a', 'c', 'a']"#,
        r#"{ a: 3, b: 1, c: 1 }"#,
    ),
    Case::eq(
        r#"['apple', 'bike', 'book', 'cat'] | countBy first"#,
        r#"{ a: 1, b: 2, c: 1 }"#,
    ),
    Case::eq(r#"['a', 'bb', 'c', 'ddd'] | countBy length"#, r#"{ 1: 2, 2: 1, 3: 1 }"#),
    Case::eq(
        r#"[{ type: 'file' }, { type: 'dir' }, { type: 'file' }] | countBy 'type'"#,
        r#"{ file: 2, dir: 1 }"#,
    ),
    // Keys keep the order they are first seen in
    Case::eq(r#"[3, 1, 3, 2] | countBy (x => x) | .fields.name"#, r#"[3, 1, 2]"#),
    Case::eq(
        r#"[{ type: 'file' }, { type: 'dir' }, { type: 'file' }] | countBy 'type' --groups"#,
        r#"[{ key: 'file', count: 2 }, { key: 'dir', count: 1 }]"#,
    ),
    Case::eq(r#"[1, 2, 1] | countBy (x => x) -g | map (.count)"#, r#"[2, 1]"#),
    Case::eq(
        r#"[1, 2, 1] | countBy --groups (x => x) | sortBy (.count) --descending | first | .key"#,
        r#"1"#,
    ),
    // Null keys are left out unless asked for
    Case::eq(r#"[1, null, null] | countBy (x => x)"#, r#"{ 1: 1 }"#),
    Case::eq(
        r#"[1, null, null] | countBy --includeNull (x => x) --groups"#,
        r#"[{ key: 1, count: 1 }, { key: null, count: 2 }]"#,
    ),
    Case::eq(
        r#"[1, null, null] | countBy --includeNull='none' (x => x)"#,
        r#"{ 1: 1, none: 2 }"#,
    ),
    Case::eq(r#"[1, 2, 1] | countBy --all (x => x)"#, r#"{ 1: 2, 2: 1, All: 3 }"#),
    Case::eq(
        r#"[1, 2, 1] | countBy --all='total' (x => x)"#,
        r#"{ 1: 2, 2: 1, total: 3 }"#,
    ),
    // The total counts null keys, as groupBy's all group holds them
    Case::eq(r#"[1, null] | countBy --all (x => x)"#, r#"{ 1: 1, All: 2 }"#),
    Case::eq(r#"[] | countBy (x => x)"#, r#"{}"#),
    Case::eq(r#"[] | countBy (x => x) --groups"#, r#"[]"#),
    // Keys can be any value, such as objects for counting by two things at once
    Case::eq(
        r#"[{ a: 1, b: 1 }, { a: 1, b: 2 }, { a: 1, b: 1 }] | countBy (x => x) --groups | map (r => r.key + { count: r.count })"#,
        r#"[{ a: 1, b: 1, count: 2 }, { a: 1, b: 2, count: 1 }]"#,
    ),
    Case::eq(r#"[true, false, true] | countBy (x => x)"#, r#"{ true: 2, false: 1 }"#),
    // A method on lists and strings (counting characters)
    Case::eq(r#"[1, 2, 1].countBy (x => x)"#, r#"{ 1: 2, 2: 1 }"#),
    Case::eq(r#"'mississippi'.countBy (x => x)"#, r#"{ m: 1, i: 4, s: 4, p: 2 }"#),
    Case::eq(r#"'mississippi' | countBy (x => x)"#, r#"{ m: 1, i: 4, s: 4, p: 2 }"#),
    Case::throws(r#"countBy (x => x) 42"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
