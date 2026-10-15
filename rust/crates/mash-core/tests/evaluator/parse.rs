//! `parse`, `parseTable` and `scan`, new in the Rust port: lines of text into objects, by a pattern (as Nushell's
//! `parse`) or a regex; column-aligned text into objects; and every match of a regex (as Python's `re.findall`).

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
    // parseTable: headers say where columns are; words in a gap go left; the last column takes the rest
    Case::eq(
        r#"'CONTAINER ID   IMAGE    SIZE  STATUS          NAMES`nabc123         nginx    12MB  Up 2 hours      web`ndef456         redis   140MB  Exited (0) 3d   cache' | parseTable"#,
        r#"[{ 'CONTAINER ID': 'abc123', IMAGE: 'nginx', SIZE: '12MB', STATUS: 'Up 2 hours', NAMES: 'web' },
            { 'CONTAINER ID': 'def456', IMAGE: 'redis', SIZE: '140MB', STATUS: 'Exited (0) 3d', NAMES: 'cache' }]"#,
    ),
    Case::eq(
        r#"'Filesystem      Size  Used Avail Use% Mounted on`n/dev/disk3s1s1  927G  757G  171G  82% /`n/dev/disk3s6    927G  757G  171G  82% /System/Volumes/VM' | parseTable | map (_['Mounted on'])"#,
        r#"['/', '/System/Volumes/VM']"#,
    ),
    // Right-aligned numbers wider than their headers, and a last column with spaces
    Case::eq(
        r#"'USER   PID  %CPU      VSZ COMMAND`nroot     1 129.8 435463808 /sbin/launchd -s`nmatt 67975  38.3   1002 rustc --edition=2024' | parseTable | map (.VSZ)"#,
        r#"['435463808', '1002']"#,
    ),
    Case::eq(
        r#"'USER   PID  %CPU      VSZ COMMAND`nroot     1 129.8 435463808 /sbin/launchd -s' | parseTable | first | .COMMAND"#,
        r#"'/sbin/launchd -s'"#,
    ),
    // A missing value is null; blank lines are skipped
    Case::eq(
        r#"'Destination  Gateway      Netif Expire`n`ndefault      192.168.1.1  en0`n127.0.0.1    127.0.0.1    lo0   30' | parseTable | map (.Expire)"#,
        r#"[null, '30']"#,
    ),
    Case::eq(
        r#"['NAME  SIZE', 'a     1', 'b     2'] | parseTable | map (.SIZE)"#,
        r#"['1', '2']"#,
    ),
    Case::eq(r#"'NAME  SIZE`na     1'.parseTable"#, r#"[{ NAME: 'a', SIZE: '1' }]"#),
    Case::eq(r#"'NAME  SIZE' | parseTable"#, r#"[]"#),
    Case::eq(r#"'' | parseTable"#, r#"[]"#),
    Case::eq(
        r#"'matt  console  Oct 14 23:30`nmatt  ttys000  Oct 15 09:19' | parseTable --noHeaders | first"#,
        r#"{ column1: 'matt', column2: 'console', column3: 'Oct', column4: '14', column5: '23:30' }"#,
    ),
    Case::throws(r#"{ a: 1 } | parseTable"#),
    // scan: the text matched, one group's value, or several groups' values
    Case::eq(r#"'disk 81%, swap 12%'.scan '\d+'"#, r#"['81', '12']"#),
    Case::eq(r#"'id=1 id=22'.scan 'id=(\d+)'"#, r#"['1', '22']"#),
    Case::eq(r#"'id=1 id=22'.scan 'id=(?<n>\d+)'"#, r#"['1', '22']"#),
    Case::eq(r#"'a=1 b=2'.scan '(\w)=(\d)'"#, r#"[['a', '1'], ['b', '2']]"#),
    Case::eq(r#"'a=1 b'.scan '(\w)(?:=(\d))?'"#, r#"[['a', '1'], ['b', null]]"#),
    Case::eq(r#"'none'.scan '\d'"#, r#"[]"#),
    Case::eq(r#"'AbC'.scan '(?i)[a-c]'"#, r#"['A', 'b', 'C']"#),
    // As a function; a list item by item
    Case::eq(r#"scan '\d+' 'a1 b22'"#, r#"['1', '22']"#),
    Case::eq(r#"['a1', 'b22 c3'] | scan '\d+'"#, r#"['1', '22', '3']"#),
    Case::eq(r#"'a1`nb2' | scan '^\w'"#, r#"['a']"#),
    Case::throws(r#"'x'.scan '('"#),
    Case::throws(r#"{ a: 1 } | scan 'a'"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
