//! Forgiving parses of incomplete input, as completion/assistance would see them.
//! Run with `--nocapture` to see the trees.

use mash_syntax::ast::{AstNode, Expr, MemberExpr};
use mash_syntax::{Mode, SyntaxKind, TextSize, parse};

fn tree(s: &str) -> String {
    let p = parse(s, Mode::Mash);
    println!(
        "=== {s:?}\n{}errors: {:?}\n",
        p.debug_tree(),
        p.errors().iter().map(|e| &e.message).collect::<Vec<_>>()
    );
    p.debug_tree()
}

/// What member completion would do: find the MemberExpr whose dot/name is at the cursor, and its target
fn member_completion_target(s: &str, cursor: usize) -> Option<(String, String)> {
    let root = parse(s, Mode::Mash).syntax();
    let token = root.token_at_offset(TextSize::from(cursor as u32)).left_biased()?;
    let member = token.parent_ancestors().find_map(MemberExpr::cast)?;
    let target = member.target()?.syntax().text().to_string();
    let prefix = member.name_token().map(|t| t.text().to_string()).unwrap_or_default();
    Some((target, prefix))
}

#[test]
fn member_completion_after_dot() {
    let t = tree("foo.");
    assert!(t.contains("MEMBER_EXPR@0..4"), "{t}");
    assert_eq!(member_completion_target("foo.", 4), Some(("foo".into(), "".into())));
}

#[test]
fn member_completion_with_prefix() {
    tree("foo.ba");
    assert_eq!(member_completion_target("foo.ba", 6), Some(("foo".into(), "ba".into())));
}

#[test]
fn member_completion_inside_lambda_in_pipe() {
    let s = "ls | where (_.own";
    tree(s);
    assert_eq!(member_completion_target(s, s.len()), Some(("_".into(), "own".into())));
}

#[test]
fn member_completion_on_complex_target() {
    let s = "(ls | first).";
    tree(s);
    assert_eq!(
        member_completion_target(s, s.len()),
        Some(("(ls | first)".into(), "".into()))
    );
}

#[test]
fn member_completion_in_middle_of_line() {
    let s = "foo. | bar";
    tree(s);
    assert_eq!(member_completion_target(s, 4), Some(("foo".into(), "".into())));
}

#[test]
fn dot_then_newline_takes_next_line_ident() {
    // As in Scala: no semicolon is inferred after a DOT, so the next line's identifier becomes the member name
    let s = "foo.\nbar";
    tree(s);
    assert_eq!(member_completion_target(s, 4), Some(("foo".into(), "bar".into())));
}

#[test]
fn incomplete_paren() {
    let t = tree("f (x");
    assert!(t.contains("INVOCATION_EXPR") && t.contains("PAREN_EXPR"), "{t}");
}

#[test]
fn incomplete_object() {
    let t = tree("{ a: ");
    assert!(t.contains("OBJECT_EXPR") && t.contains("FULL_OBJECT_ENTRY"), "{t}");
}

#[test]
fn incomplete_interpolation() {
    let t = tree("\"${");
    assert!(
        t.contains("INTERPOLATED_STRING") && t.contains("COMPLEX_INTERPOLATION"),
        "{t}"
    );
    let t = tree("\"${foo.");
    assert!(t.contains("MEMBER_EXPR"), "{t}");
    assert_eq!(member_completion_target("\"${foo.", 7), Some(("foo".into(), "".into())));
}

#[test]
fn incomplete_list() {
    let t = tree("[1, 2");
    assert!(t.contains("LIST_EXPR"), "{t}");
}

#[test]
fn incomplete_def() {
    let p = parse("def f x =", Mode::Mash);
    tree("def f x =");
    let Expr::FunctionDecl(f) = &p.source_file().exprs()[0] else {
        panic!()
    };
    assert_eq!(f.name_token().unwrap().text(), "f");
    assert!(f.body().is_none());
}

#[test]
fn incomplete_pipe() {
    let p = parse("x | ", Mode::Mash);
    tree("x | ");
    let Expr::Pipe(pipe) = &p.source_file().exprs()[0] else {
        panic!()
    };
    assert!(pipe.lhs().is_some() && pipe.rhs().is_none());
}

#[test]
fn incomplete_flag_for_flag_completion() {
    let s = "ls --";
    tree(s);
    let root = parse(s, Mode::Mash).syntax();
    let token = root.token_at_offset(TextSize::from(5)).left_biased().unwrap();
    assert_eq!(token.kind(), SyntaxKind::LONG_FLAG);
    // The flag's enclosing invocation gives the function to complete flags for
    let inv = token
        .parent_ancestors()
        .find(|n| n.kind() == SyntaxKind::INVOCATION_EXPR)
        .unwrap();
    assert!(inv.text().to_string().starts_with("ls"));
}

#[test]
fn garbage_is_contained_in_error_nodes() {
    let t = tree(") ] } foo");
    assert!(t.contains("ERROR"), "{t}");
    assert!(t.contains("IDENTIFIER_EXPR"), "{t}");
}
