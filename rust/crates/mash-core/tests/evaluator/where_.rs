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
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones (or other modules)
const DEFERRED: &[&str] = &[
    // Cannot find member 'toPath' in value of type String
    r#"'foobar'.toPath | where (_ > 'm') | .tag"#,
    // Cannot find member 'where' in value of type String
    r#"'foobar'.where (_ > 'm')"#,
    // Cannot find member 'where' in value of type Object
    r#"{ apple: 1, bob: 2, aardvark: 3 }.where (.startsWith 'a')"#,
    // Cannot find member 'where' in value of type Object
    r#"{ apple: 1, bob: 2, aardvark: 3 }.where (f v => f.startsWith 'b' or v == 3)"#,
    // Cannot find member 'startsWith' in value of type String
    r#"{ apple: 1, bob: 2, aardvark: 3 } | where (.startsWith 'a')"#,
    // Cannot find member 'startsWith' in value of type String
    r#"{ apple: 1, bob: 2, aardvark: 3 } | where (f v => f.startsWith 'b' or v == 3)"#,
    // Cannot find member 'where' in value of type Object
    r#"{ apple: 1, bob: 2, aardvark: 3 }.where (f v i => i == 1)"#,
    // Cannot find member 'whereNot' in value of type String
    r#"'foobar'.whereNot (_ > 'm')"#,
    // Cannot find member 'whereNot' in value of type Object
    r#"{ apple: 1, bob: 2, aardvark: 3 }.whereNot (.startsWith 'a')"#,
    // Cannot find member 'whereNot' in value of type Object
    r#"{ apple: 1, bob: 2, aardvark: 3 }.whereNot (f v => f.startsWith 'b' or v == 3)"#,
    // Cannot find member 'startsWith' in value of type String
    r#"{ apple: 1, bob: 2, aardvark: 3 } | whereNot (.startsWith 'a')"#,
    // Cannot find member 'startsWith' in value of type String
    r#"{ apple: 1, bob: 2, aardvark: 3 } | whereNot (f v => f.startsWith 'b' or v == 3)"#,
    // Cannot find member 'whereNot' in value of type Object
    r#"{ apple: 1, bob: 2, aardvark: 3 }.whereNot (f v i => i == 1)"#,
];
