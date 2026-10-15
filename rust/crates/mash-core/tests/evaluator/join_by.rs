//! `joinBy`, new in the Rust port: joining two tables on keys (as Nushell's `join`).

use crate::harness::*;

macro_rules! with_data {
    ($code:literal) => {
        concat!(
            "people = [{ name: 'Ada', team: 'core', managerId: 10 }, { name: 'Bob', team: 'web', managerId: 20 }, { name: 'Cy', team: 'ops', managerId: null }]; managers = [{ id: 10, name: 'Grace', team: 'core' }, { id: 30, name: 'Linus', team: 'kernel' }]; ",
            $code
        )
    };
}

pub(crate) const CASES: &[Case] = &[
    // Inner by default; null keys match nothing; clashing names get a _ suffix; differently named keys both stay
    Case::eq(
        with_data!("people | joinBy managers 'managerId' 'id'"),
        r#"[{ name: 'Ada', team: 'core', managerId: 10, id: 10, name_: 'Grace', team_: 'core' }]"#,
    ),
    // --left: the other side's fields null
    Case::eq(
        with_data!("people | joinBy --left managers 'managerId' 'id' | map (r => [r.name, r.name_])"),
        r#"[['Ada', 'Grace'], ['Bob', null], ['Cy', null]]"#,
    ),
    // --right and --outer: the other table's unmatched rows at the end
    Case::eq(
        with_data!("people | joinBy --right managers 'managerId' 'id' | map (r => [r.name, r.name_])"),
        r#"[['Ada', 'Grace'], [null, 'Linus']]"#,
    ),
    Case::eq(
        with_data!("people | joinBy --outer managers 'managerId' 'id' | map (.id)"),
        r#"[10, null, null, 30]"#,
    ),
    // --prefix: on all the other table's fields
    Case::eq(
        with_data!("people | joinBy managers 'managerId' 'id' --prefix='manager_' | first | .fields.name"),
        r#"['name', 'team', 'managerId', 'manager_id', 'manager_name', 'manager_team']"#,
    ),
    // A key field with the same name in both appears once, even in the other table's unmatched rows
    Case::eq(
        r#"[{ id: 1, n: 'a' }, { id: 2 }] | joinBy --outer [{ id: 1, x: 10 }, { id: 3, x: 30 }] 'id'"#,
        r#"[{ id: 1, n: 'a', x: 10 }, { id: 2, x: null }, { id: 3, n: null, x: 30 }]"#,
    ),
    // Every pairing, in this table's order
    Case::eq(
        r#"[{ c: 7, id: 1 }, { c: 7, id: 2 }] | joinBy [{ c: 7, t: 'x' }, { c: 7, t: 'y' }] 'c' | map (r => [r.id, r.t])"#,
        r#"[[1, 'x'], [1, 'y'], [2, 'x'], [2, 'y']]"#,
    ),
    // Keys as functions; a row without the key field has a null key
    Case::eq(with_data!("people.joinBy managers (.managerId) (.id) | length"), r#"1"#),
    Case::eq(
        r#"[{ a: 1 }, { b: 2 }] | joinBy --left [{ a: 1, x: 'y' }] 'a' | map (.x)"#,
        r#"['y', null]"#,
    ),
    Case::throws(with_data!("people | joinBy -l -r managers 'managerId' 'id'")),
    Case::throws(r#"[1] | joinBy [{ a: 1 }] 'a'"#),
    Case::throws(r#"[{ a: 1 }] | joinBy 'x' 'a'"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
