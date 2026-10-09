//! Doc comments attached to declarations (cases from DocCommentsTest.scala, at the syntax level).

use mash_syntax::ast::{AstNode, ClassDecl, FunctionDecl};
use mash_syntax::doc_comment::DocComment;
use mash_syntax::{Mode, parse};

fn function_docs(s: &str) -> Vec<(String, Option<DocComment>)> {
    let p = parse(s, Mode::Mash);
    assert!(p.ok(), "{:?}", p.errors());
    p.syntax()
        .descendants()
        .filter_map(FunctionDecl::cast)
        .map(|f| (f.name_token().unwrap().text().to_string(), f.doc_comment()))
        .collect()
}

fn summary(s: &str, function: &str) -> Option<String> {
    function_docs(s)
        .into_iter()
        .find(|(name, _)| name == function)
        .and_then(|(_, doc)| doc)
        .map(|d| d.summary)
}

#[test]
fn first_def_in_a_statement_sequence() {
    let s = "# Square a number\ndef square n = n * n\nsquare.summary\n";
    assert_eq!(summary(s, "square").as_deref(), Some("Square a number"));
}

#[test]
fn description_lines() {
    let s = "# Summary\n# Description 1\n# Description 2\n# Description 3\ndef fun x = x\nfun.description\n";
    let docs = function_docs(s);
    let doc = docs[0].1.as_ref().unwrap();
    assert_eq!(doc.summary, "Summary");
    assert_eq!(
        doc.description.as_deref(),
        Some("Description 1\nDescription 2\nDescription 3")
    );
}

#[test]
fn before_attributes() {
    let s = "# Square a number\n@private def square n = n * n\nsquare.summary\n";
    assert_eq!(summary(s, "square").as_deref(), Some("Square a number"));
}

#[test]
fn methods_with_attributes() {
    for attribute in ["@attribute", "@(attribute \"with argument\")"] {
        let s = format!(
            "class A {{\n  # Do something\n  {attribute}\n  def method = 42\n}}\n(safe A.new.method).summary\n"
        );
        assert_eq!(summary(&s, "method").as_deref(), Some("Do something"), "{s}");
    }
}

#[test]
fn later_defs_in_a_sequence() {
    let s = "def a = 1\n# Second one\ndef b = 2\n";
    assert_eq!(summary(s, "a"), None);
    assert_eq!(summary(s, "b").as_deref(), Some("Second one"));
}

#[test]
fn blank_line_separates_comment_from_declaration() {
    assert_eq!(summary("# File header\n\ndef f = 1", "f"), None);
    assert_eq!(
        summary("# File header\n\n# Doc\ndef f = 1", "f").as_deref(),
        Some("Doc")
    );
}

#[test]
fn indented_and_crlf() {
    assert_eq!(summary("  # Doc\r\n  def f = 1", "f").as_deref(), Some("Doc"));
}

#[test]
fn class_doc_comment() {
    let p = parse("# A point\nclass Point x y", Mode::Mash);
    let class = p.syntax().descendants().find_map(ClassDecl::cast).unwrap();
    assert_eq!(class.doc_comment().map(|d| d.summary).as_deref(), Some("A point"));
}
