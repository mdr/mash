//! Port of `evaluator/StringInterpolationTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"(name => "Hello $name!") "Matt" "#, r#"'Hello Matt!'"#),
    Case::eq(r#"(name => "Hello ${name}!") "Matt" "#, r#"'Hello Matt!'"#),
    Case::eq(r#"(name => "Hello $name.reverse!") "Matt" "#, r#"'Hello ttaM!'"#),
    Case::eq(r#"(name => "Hello ${name.reverse}!") "Matt" "#, r#"'Hello ttaM!'"#),
    Case::eq(r#"(name => "Hello ${name | reverse}!") "Matt" "#, r#"'Hello ttaM!'"#),
    Case::eq(r#" 42 | "$_" "#, r#"'42'"#),
    Case::eq(r#" 'foo' | "$_.reverse" "#, r#"'oof'"#),
    Case::eq(r#" "`"${42}`"" "#, r#" '"42"' "#),
    Case::eq(r#" "'${42}'" "#, r#" "'42'" "#),
    Case::eq(r#" "${4}'bar'${2}" "#, r#" "4'bar'2" "#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
