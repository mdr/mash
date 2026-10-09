//! Port of `evaluator/DefaultArgumentsTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"def foo (x = 42) = x + 1; foo"#, r#"43"#),
    Case::eq(r#"def foo (x = 42) = x + 1; foo 100"#, r#"101"#),
    Case::eq(r#"def fun ({ foo } = { foo: 42 }) = foo + 1; fun"#, r#"43"#),
    Case::eq(
        r#"def fun ({ foo } = { foo: 42 }) = foo + 1; fun { foo: 100 }"#,
        r#"101"#,
    ),
    Case::setup(
        r#"def mkList (xs... = [1, 2, 3]) = xs"#,
        &[
            Case::eq(r#"mkList"#, r#"[1, 2, 3]"#),
            Case::eq(r#"mkList --xs=[]"#, r#"[]"#),
            Case::eq(r#"mkList --xs=[4, 5, 6]"#, r#"[4, 5, 6]"#),
        ],
    ),
    Case::eq(r#"def mkList (xs... = null) = xs; mkList"#, r#"null"#),
    Case::eq(
        r#"class Foo wibble { def get (n = wibble) = n }; Foo 100 | .get"#,
        r#"100"#,
    ),
    Case::eq(
        r#"class Foo wibble { def get (n = this) = n }; Foo 100 | .get.wibble"#,
        r#"100"#,
    ),
    Case::eq(
        r#"class Foo { def foo (n = bar) = n; def bar = 100 }; Foo.new.foo"#,
        r#"100"#,
    ),
    Case::eq(r#"init = 42; a = ((v = init) => init = v); a"#, r#"42"#),
    Case::eq(r#"init = 42; a = (def mkFun (v = init) = init = v); a"#, r#"42"#),
    Case::eq(r#"def foo (x = [1]) = { y = x[0]; x[0] = 100; y }; foo; foo"#, r#"1"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
