//! Lowering and the compiler passes. Ports of AbstractifierTest, DesugarHolesTest, DesugarPipesTest,
//! ParenRemoverTest and BareStringifyTest, plus lowering of each kind of syntax.
//!
//! Trees are compared via `ast::pretty`, which ignores node ids and source ranges.

use std::collections::HashSet;
use std::rc::Rc;

use mash_core::ast::{E, ExprKind, NodeIdGen, SourceUnit, pretty};
use mash_core::compile::{self, Settings};

/// Lowered but not desugared (forgiving: incomplete input is fine)
fn lowered(s: &str) -> E {
    lowered_with(s, &mut NodeIdGen::default())
}

fn lowered_with(s: &str, ids: &mut NodeIdGen) -> E {
    let parse = mash_syntax::parse(s, mash_syntax::Mode::Mash);
    compile::lower(&parse, SourceUnit::new("test", s), ids).0.body
}

fn unit() -> Rc<SourceUnit> {
    SourceUnit::new("test", "")
}

// ------------------------------------------------------------------------------------------------ lowering

#[track_caller]
fn lowers_to(s: &str, expected: &str) {
    assert_eq!(pretty(&lowered(s)), expected, "lowering {s:?}");
}

#[test]
fn string_literals_do_not_include_quotes() {
    lowers_to("\"foo\"", "\"foo\"");
    lowers_to("'foo'", "'foo'");
    // Unterminated
    lowers_to("\"foo", "\"foo\"");
    lowers_to("\"~/foo\"", "\"~/foo\"");
    // Only double-quoted strings have a home-directory prefix
    let ExprKind::String { value, tilde, .. } = &lowered("'~/foo'").kind else {
        panic!()
    };
    assert_eq!((&**value, *tilde), ("~/foo", false));
    let ExprKind::String { value, tilde, .. } = &lowered("\"~/foo\"").kind else {
        panic!()
    };
    assert_eq!((&**value, *tilde), ("/foo", true));
}

#[test]
fn invocations_collect_their_arguments() {
    lowers_to("ls pwd", "(call ls pwd)");
    lowers_to("foo --bar=baz", "(call foo --bar=baz)");
    lowers_to("foo --bar -abc 1", "(call foo --bar -abc 1)");
    lowers_to("foo(1, 2)", "(call( foo 1 2)");
}

#[test]
fn lowering_each_kind_of_expression() {
    let cases = [
        ("42", "42"),
        ("true", "true"),
        ("null", "null"),
        ("\"a$b c${d + 1}\"", "(interpolated \"a\" b \" c\" (+ d 1))"),
        ("_", "_1"),
        ("_2", "_2"),
        ("this", "this"),
        ("!ls", "!ls"),
        ("(a)", "(paren a)"),
        ("{ a }", "(object a)"),
        ("{ a; b }", "(block (seq a b))"),
        ("a; b", "(seq a b)"),
        ("[1, 2]", "(list 1 2)"),
        ("{ a: 1, b, (c): 2 }", "(object (a: 1) b ((paren c): 2))"),
        ("a.b", "(a.b)"),
        ("a?.b", "(a?.b)"),
        (".b", "(.b)"),
        ("a[1]", "(lookup a 1)"),
        ("- a", "(- a)"),
        ("a + b * c", "(+ a (* b c))"),
        ("a and b or c", "(or (and a b) c)"),
        ("a < b <= c", "(chain a < b <= c)"),
        ("if a then b else c", "(if a b c)"),
        ("if a then b", "(if a b)"),
        ("a = 1", "(= a 1)"),
        ("a.b += 1", "(+= (a.b) 1)"),
        ("{ x, y: [z, _] } = foo", "(= {x, y: [z, _]} foo)"),
        ("x y => x", "(lambda _ [x y] x)"),
        ("a | b", "(| a b)"),
        ("def f x (y = 2) z... = x", "(def f [x y=2 z...] x)"),
        ("@private def f = 1", "(def f @private [] 1)"),
        (
            "def f (@flag @(shortFlag 'd') x = false) = x",
            "(def f [@flag @shortFlag( 'd' ) x=false] x)",
        ),
        (
            "class Point x y { def sum = x + y }",
            "(class Point [x y] (def sum [] (+ x y)))",
        ),
        ("a?", "(help a)"),
        ("import foo.bar.baz", "(import (foo.bar) baz)"),
        ("import foo._", "(import foo _)"),
        ("!{ls -l $x > out}", "(mish! \"ls\" \"-l\" ($ x) >\"out\")"),
        ("!!{vi \"file\"}", "(mish \"vi\" \"file\")"),
    ];
    for (input, expected) in cases {
        lowers_to(input, expected);
    }
}

#[test]
fn incomplete_input_lowers_with_missing_parts() {
    lowers_to("a +", "(+ a <missing>)");
    lowers_to("foo.", "(foo.)");
    lowers_to("if", "(if <missing> <missing>)");
}

#[test]
fn doc_comments_and_namespaces_are_attached_to_definitions() {
    let s = "namespace my.ns\n# Square a number\n# @param n The number\ndef square n = n * n";
    let parse = mash_syntax::parse(s, mash_syntax::Mode::Mash);
    let (program, _) = compile::lower(&parse, SourceUnit::new("test", s), &mut NodeIdGen::default());
    assert_eq!(
        program.namespace.as_deref().map(|ns| ns.join(".")),
        Some("my.ns".to_string())
    );
    // After the namespace declaration, the body is a sequence of statements
    let ExprKind::StatementSeq(statements) = &program.body.kind else {
        panic!("{}", pretty(&program.body))
    };
    let ExprKind::FunctionDecl(def) = &statements[0].kind else {
        panic!("{}", pretty(&program.body))
    };
    let doc = def.doc.as_ref().unwrap();
    assert_eq!(doc.summary, "Square a number");
    assert_eq!(doc.param("n").unwrap().summary, "The number");
    assert_eq!(
        def.namespace.as_deref().map(|ns| ns.join(".")),
        Some("my.ns".to_string())
    );
    assert_eq!(def.source(), Some("def square n = n * n"));
}

#[test]
fn mish_mode() {
    let unit = SourceUnit::new("test", "ls -la $dir");
    let settings = Settings {
        mish: true,
        ..Settings::default()
    };
    let compiled = compile::compile(unit, settings, &HashSet::new(), &mut NodeIdGen::default()).unwrap();
    assert_eq!(pretty(&compiled.program.body), "(mish \"ls\" \"-la\" ($ dir))");
}

#[test]
fn node_ids_are_unique_and_the_source_map_finds_nodes() {
    let s = "foo.bar + 1";
    let parse = mash_syntax::parse(s, mash_syntax::Mode::Mash);
    let (program, source_map) = compile::lower(&parse, SourceUnit::new("test", s), &mut NodeIdGen::default());
    let mut ids = vec![];
    collect_ids(&program.body, &mut ids);
    let unique: HashSet<_> = ids.iter().collect();
    assert_eq!(unique.len(), ids.len());
    let foo_range = mash_syntax::TextRange::new(0.into(), 3.into());
    let ExprKind::BinOp { lhs, .. } = &program.body.kind else {
        panic!()
    };
    let ExprKind::Member { target, .. } = &lhs.kind else {
        panic!()
    };
    assert_eq!(source_map.by_range.get(&foo_range), Some(&target.id));
}

fn collect_ids(e: &E, ids: &mut Vec<mash_core::ast::NodeId>) {
    ids.push(e.id);
    e.for_each_child(&mut |child| collect_ids(child, ids));
}

// ------------------------------------------------------------------------------------------------ DesugarHoles

#[track_caller]
fn holes_desugar_to(s: &str, expected: &str) {
    let mut ids = NodeIdGen::default();
    let desugared = compile::desugar_holes(&lowered_with(s, &mut ids), &mut ids, &unit());
    let actual = pretty(&desugared).replace(compile::HOLE_VARIABLE_PREFIX, "x");
    assert_eq!(actual, pretty(&lowered(expected)), "desugaring holes in {s:?}");
}

#[test]
fn desugar_holes() {
    holes_desugar_to("_", "x1 => x1");
    holes_desugar_to("foo _", "x1 => foo x1");
    holes_desugar_to("_ 42", "x1 => x1 42");
    holes_desugar_to("a | map (_.b)", "a | map (x1 => x1.b)");
    holes_desugar_to(
        "ls | keepIf (_.isFile and _.size < 1000)",
        "ls | keepIf (x1 => x1.isFile and x1.size < 1000)",
    );
    holes_desugar_to("y => _.size", "y => x1 => x1.size");
    holes_desugar_to("_.foo | _.bar", "x1 => x1.foo | x1 => x1.bar");
    holes_desugar_to("foo --bar=_", "x1 => foo --bar=x1");
    holes_desugar_to("_.foo (_.bar)", "x1 => x1.foo (x1 => x1.bar)");
    holes_desugar_to("_1 * _2", "x1 x2 => x1 * x2");
    holes_desugar_to("_2", "x1 x2 => x2");
}

#[test]
fn desugar_holes_at_other_boundaries() {
    holes_desugar_to("f(_ + 1, 2)", "f(x1 => x1 + 1, 2)");
    holes_desugar_to("{ _ }", "{ x1 => x1 }");
    holes_desugar_to("def f = _ + 1", "def f = x1 => x1 + 1");
    holes_desugar_to("def f (x = _) = x", "def f (x = x1 => x1) = x");
}

// ------------------------------------------------------------------------------------------------ DesugarPipes

#[track_caller]
fn pipes_desugar_to(s: &str, expected: &str) {
    let actual = compile::desugar_pipes(&compile::remove_parens(&lowered(s)));
    assert_eq!(
        pretty(&actual),
        pretty(&compile::remove_parens(&lowered(expected))),
        "desugaring pipes in {s:?}"
    );
}

#[test]
fn desugar_pipes() {
    pipes_desugar_to("a | b", "b a");
    pipes_desugar_to("a | b c", "b c a");
    pipes_desugar_to("a | b | c", "c (b a)");
    pipes_desugar_to("a | b c | d e", "d e (b c a)");
    pipes_desugar_to("foo --bar=(a | b)", "foo --bar=(b a)");
}

// ------------------------------------------------------------------------------------------------ ParenRemover

#[test]
fn remove_parens() {
    assert_eq!(pretty(&compile::remove_parens(&lowered("(foo)"))), "foo");
    assert_eq!(pretty(&compile::remove_parens(&lowered("((foo))"))), "foo");
    assert_eq!(
        pretty(&compile::remove_parens(&lowered("foo --bar=(baz)"))),
        pretty(&lowered("foo --bar=baz"))
    );
}

// ------------------------------------------------------------------------------------------------ BareStringify

#[track_caller]
fn bare_stringifies_to(s: &str, bindings: &[&str], expected: &str) {
    let bindings: HashSet<Rc<str>> = bindings.iter().map(|&b| b.into()).collect();
    let (actual, _) = compile::bare_stringify(&lowered(s), &bindings);
    assert_eq!(pretty(&actual), pretty(&lowered(expected)), "bare-stringifying {s:?}");
}

#[test]
fn bare_stringify() {
    bare_stringifies_to("unbound", &[], "\"unbound\"");
    bare_stringifies_to("bound", &["bound"], "bound");
    bare_stringifies_to("class A { def foo = fields }", &[], "class A { def foo = \"fields\" }");
    bare_stringifies_to(
        "class A { def foo = toString }",
        &[],
        "class A { def foo = \"toString\" }",
    );
    bare_stringifies_to(
        "class A { def foo = bar; def bar = 42 }",
        &[],
        "class A { def foo = bar; def bar = 42 }",
    );
    bare_stringifies_to(
        "def doSomething (@flag @(shortFlag d) dryRun = false) = 42",
        &[],
        "def doSomething (@flag @(shortFlag \"d\") dryRun = false) = 42",
    );
}

#[test]
fn bare_stringify_respects_earlier_statements_and_parameters() {
    bare_stringifies_to("a = 1; a; b", &[], "a = 1; a; \"b\"");
    bare_stringifies_to("x => x + y", &[], "x => x + \"y\"");
    bare_stringifies_to("def f x = f x y", &[], "def f x = f x \"y\"");
    bare_stringifies_to("{ a: b }", &[], "{ a: \"b\" }");
    bare_stringifies_to("cd foo", &["cd"], "cd \"foo\"");
}

#[test]
fn bare_words_are_reported() {
    let (_, ranges) = compile::bare_stringify(&lowered("cd foo"), &HashSet::from(["cd".into()]));
    assert_eq!(ranges, [mash_syntax::TextRange::new(3.into(), 6.into())]);
}

// ------------------------------------------------------------------------------------------------ the pipeline

#[test]
fn compile_runs_all_passes() {
    let unit = SourceUnit::new("test", "ls | where (.isFile)");
    let compiled = compile::compile(unit, Settings::default(), &HashSet::new(), &mut NodeIdGen::default()).unwrap();
    assert_eq!(
        pretty(&compiled.program.body),
        "(call where (paren (lambda _ [$hole1] ($hole1.isFile))) ls)"
    );
}

#[test]
fn compile_reports_syntax_errors() {
    let unit = SourceUnit::new("test", "a +");
    let errors = compile::compile(unit, Settings::default(), &HashSet::new(), &mut NodeIdGen::default()).unwrap_err();
    assert!(!errors.is_empty());
}
