//! `rename`, `update` and `default`, new in the Rust port: renaming fields of an object or a list of objects,
//! changing their values, and filling in missing ones (as Nushell's commands of those names).

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
    // update: by a function of the field's value, or a new value; fields keep their places
    Case::eq(
        r#"[{ pid: '1' }, { pid: '22' }] | update 'pid' (.toNumber)"#,
        r#"[{ pid: 1 }, { pid: 22 }]"#,
    ),
    Case::eq(
        r#"{ a: 1, b: 2, c: 3 }.update 'b' (_ + 10) | .fields.name"#,
        r#"['a', 'b', 'c']"#,
    ),
    Case::eq(r#"update 'b' (_ + 10) { a: 1, b: 2 }"#, r#"{ a: 1, b: 12 }"#),
    Case::eq(
        r#"[{ status: 'todo' }, { status: null }] | update 'status' 'done'"#,
        r#"[{ status: 'done' }, { status: 'done' }]"#,
    ),
    // Several fields at once
    Case::eq(
        r#"[{ pid: '1', cpu: '0.5', user: 'root' }].update { pid: (.toNumber), cpu: (.toNumber) }"#,
        r#"[{ pid: 1, cpu: 0.5, user: 'root' }]"#,
    ),
    // A function isn't called on null; objects without the field are left alone, but one must have it
    Case::eq(
        r#"[{ a: 1 }, { a: null }, { b: 2 }] | update 'a' (_ * 10)"#,
        r#"[{ a: 10 }, { a: null }, { b: 2 }]"#,
    ),
    Case::throws(r#"[{ a: 1 }] | update 'zz' (_ + 1)"#),
    Case::eq(r#"[] | update 'zz' (_ + 1)"#, r#"[]"#),
    // The results are plain objects, as their fields may no longer suit their class
    Case::eq(
        r#"class Point x y; (Point 1 2).update 'x' (_ + 1) | .getClass.name"#,
        r#"'Object'"#,
    ),
    Case::throws(r#"[{ a: 1 }] | update 'a'"#),
    Case::throws(r#"[{ a: 1 }] | update { a: (_ + 1) } 5 [{ a: 1 }]"#),
    Case::throws(r#"update 1 2 [{ a: 1 }]"#),
    Case::throws(r#"[1] | update 'a' 2"#),
    Case::eq(r#"{ update: 3 }.update"#, r#"3"#),
    // default: null and missing fields are filled in (missing ones added at the end); 0 and false are values
    Case::eq(
        r#"[{ a: 1 }, { a: null }, { b: 2 }] | default 'a' 0"#,
        r#"[{ a: 1 }, { a: 0 }, { b: 2, a: 0 }]"#,
    ),
    Case::eq(
        r#"{ a: 0, b: false }.default { a: 9, b: true }"#,
        r#"{ a: 0, b: false }"#,
    ),
    Case::eq(
        r#"[{ n: 'x' }, { n: 'y', role: 'admin' }] | default { role: 'user', active: true }"#,
        r#"[{ n: 'x', role: 'user', active: true }, { n: 'y', role: 'admin', active: true }]"#,
    ),
    Case::eq(r#"default 'a' 0 { b: 1 }"#, r#"{ b: 1, a: 0 }"#),
    // Empty strings, lists and objects only with --empty
    Case::eq(r#"[{ a: '' }] | default 'a' '-'"#, r#"[{ a: '' }]"#),
    Case::eq(
        r#"[{ a: '' }, { a: [] }, { a: {} }, { a: 'x' }] | default --empty 'a' '-' | map (.a)"#,
        r#"['-', '-', '-', 'x']"#,
    ),
    Case::eq(r#"[{ a: '' }].default -e 'a' '-'"#, r#"[{ a: '-' }]"#),
    Case::throws(r#"[{ a: 1 }] | default 'a'"#),
    Case::throws(r#"[{ a: 1 }] | default { a: 1 } 2 [{ a: 1 }]"#),
    Case::eq(r#"{ default: 1 }.default"#, r#"1"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
