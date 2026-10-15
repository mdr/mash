//! `rename`, new in the Rust port: renaming fields of an object or a list of objects (as Nushell's `rename`).

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    // By an object of new names; fields keep their places
    Case::eq(
        r#"{ a: 1, b: 2, c: 3 }.rename { b: 'x' } | .fields.name"#,
        r#"['a', 'x', 'c']"#,
    ),
    Case::eq(r#"rename { a: 'x', b: 'y' } { a: 1, b: 2 }"#, r#"{ x: 1, y: 2 }"#),
    Case::eq(
        r#"[{ a: 1, b: 2 }, { a: 3 }] | rename { a: 'x' }"#,
        r#"[{ x: 1, b: 2 }, { x: 3 }]"#,
    ),
    Case::eq(
        r#"[{ 'Mounted on': '/' }].rename { 'Mounted on': 'mount' }"#,
        r#"[{ mount: '/' }]"#,
    ),
    // Objects without the field are left alone; but some object must have it
    Case::eq(r#"[{ a: 1 }, { b: 2 }] | rename { a: 'x' }"#, r#"[{ x: 1 }, { b: 2 }]"#),
    Case::throws(r#"[{ a: 1 }] | rename { zz: 'x' }"#),
    Case::eq(r#"[] | rename { zz: 'x' }"#, r#"[]"#),
    // By a function of the name
    Case::eq(r#"[{ Ab: 1, CD: 2 }].rename (.toLower)"#, r#"[{ ab: 1, cd: 2 }]"#),
    Case::eq(r#"{ a: 1 }.rename (n => n + '_x')"#, r#"{ a_x: 1 }"#),
    // Two fields of the same name would be an error
    Case::throws(r#"{ a: 1, b: 2 }.rename { a: 'b' }"#),
    Case::throws(r#"{ a: 1, b: 2 }.rename (n => 'k')"#),
    // Swapping names is fine
    Case::eq(r#"{ a: 1, b: 2 }.rename { a: 'b', b: 'a' }"#, r#"{ b: 1, a: 2 }"#),
    Case::throws(r#"rename 'x' { a: 1 }"#),
    Case::throws(r#"[1] | rename { a: 'b' }"#),
    Case::throws(r#"rename { a: 'b' } 42"#),
    // A field called rename wins over the method
    Case::eq(r#"{ rename: 5 }.rename"#, r#"5"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
