//! Port of `evaluator/HelpTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"listFiles?"#, r#"safe listFiles"#),
    Case::eq(r#"help groupBy"#, r#"groupBy?"#),
    Case::eq(r#"man groupBy"#, r#"groupBy?"#),
    Case::eq(r#"[1, 2, 3].reverse? .name"#, r#"'reverse'"#),
    Case::eq(r#"[1, 2, 3].sortBy? .name"#, r#"'sortBy'"#),
    Case::eq(r#"help [1,2, 3].sortBy"#, r#"[1, 2, 3].sortBy?"#),
    Case::eq(r#"[1, 2, 3].sortBy.help"#, r#"[1, 2, 3].sortBy?"#),
    Case::eq(r#"pwd.info.permissions? .name"#, r#"'permissions'"#),
    Case::eq(r#"[pwd].info.permissions? .name"#, r#"'permissions'"#),
    Case::eq(r#"git.log?"#, r#"safe git.log"#),
    Case::eq(r#"help (42.getClass) | _.name"#, r#"'Number'"#),
    Case::eq(r#"help pwd"#, r#"pwd?"#),
    Case::eq(r#"help [].reverse"#, r#"[].reverse?"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[
    // No binding for 'listFiles'
    r#"listFiles?"#,
    // No binding for 'pwd'
    r#"pwd.info.permissions? .name"#,
    // No binding for 'pwd'
    r#"[pwd].info.permissions? .name"#,
    // No binding for 'git'
    r#"git.log?"#,
    // No binding for 'pwd'
    r#"help pwd"#,
];
