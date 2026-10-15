//! `parse`, new in the Rust port: lines of text into objects, by a pattern (as Nushell's `parse`) or a regex.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(
        r#"['x=1', 'y=2'] | parse '{name}={value}'"#,
        r#"[{ name: 'x', value: '1' }, { name: 'y', value: '2' }]"#,
    ),
    // Lines that don't match are left out; the pattern matches the whole line
    Case::eq(r#"['x=1', 'bad', 'y=2'] | parse '{k}={v}' | map (.k)"#, r#"['x', 'y']"#),
    Case::eq(r#"'zz a=1' | parse 'a={v}'"#, r#"[]"#),
    // Each field takes as little as it can, and the last the rest
    Case::eq(r#"'k: v: w' | parse '{k}: {v}'"#, r#"[{ k: 'k', v: 'v: w' }]"#),
    // A space matches any run of whitespace (not as Nushell, where it's one space)
    Case::eq(r#"'a    1' | parse '{name} {n}'"#, r#"[{ name: 'a', n: '1' }]"#),
    Case::eq(
        r#"'root     1  0.0 /sbin/init splash' | parse '{user} {pid} {_} {command}'"#,
        r#"[{ user: 'root', pid: '1', command: '/sbin/init splash' }]"#,
    ),
    // Other text matches itself, regex characters included
    Case::eq(r#"'[x] (1)' | parse '[{a}] ({b})'"#, r#"[{ a: 'x', b: '1' }]"#),
    Case::eq(
        r#"'{"k": 1}' | parse '{"{key}": {value}}'"#,
        r#"[{ key: 'k', value: '1' }]"#,
    ),
    Case::eq(r#"'a.b' | parse '{x}.{y}'"#, r#"[{ x: 'a', y: 'b' }]"#),
    // A string is parsed line by line
    Case::eq(
        r#"'a 1`nnope`nb 2' | parse '{name} {n}' | map (.name)"#,
        r#"['a', 'b']"#,
    ),
    Case::eq(r#"'a 1`nb 2'.parse '{name} {n}' | map (.n)"#, r#"['1', '2']"#),
    Case::eq(r#"['a 1'].parse '{name} {n}' | map (.n)"#, r#"['1']"#),
    // Items are parsed as strings
    Case::eq(r#"[1, 22] | parse '{d}'"#, r#"[{ d: '1' }, { d: '22' }]"#),
    Case::throws(r#"'a b' | parse '{x} {x}'"#),
    Case::throws(r#"{ a: 1 } | parse '{x}'"#),
    // --regex: named groups, every match in a line, null for a group that took no part
    Case::eq(
        r#"'took 35ms, then 2s' | parse --regex '(?<n>\d+)(?<unit>ms|s)'"#,
        r#"[{ n: '35', unit: 'ms' }, { n: '2', unit: 's' }]"#,
    ),
    Case::eq(
        r#"'a-1 b' | parse -r '(?<l>[a-z])(-(?<d>\d))?'"#,
        r#"[{ l: 'a', d: '1' }, { l: 'b', d: null }]"#,
    ),
    Case::throws(r#"'x' | parse --regex '\w'"#),
    Case::throws(r#"'x' | parse --regex '(?<x'"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
