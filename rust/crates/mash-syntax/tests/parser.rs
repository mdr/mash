//! Port of MashParserTest.scala plus tests for the tricky bits (semicolons, block vs object, lambdas,
//! interpolation, mish) and the typed AST layer.

mod common;

use mash_syntax::ast::{self, AstNode, Expr, LiteralValue};
use mash_syntax::{Mode, SyntaxKind, SyntaxNode, parse};

/// Compact rendering: `KIND(child child ...)`, tokens as their text, trivia omitted
fn shape_of(node: &SyntaxNode) -> String {
    let parts: Vec<String> = node
        .children_with_tokens()
        .filter_map(|e| match e {
            rowan::NodeOrToken::Node(n) => Some(shape_of(&n)),
            rowan::NodeOrToken::Token(t) if !t.kind().is_trivia() => Some(t.text().to_string()),
            _ => None,
        })
        .collect();
    format!("{:?}({})", node.kind(), parts.join(" "))
}

fn shape(s: &str) -> String {
    let p = parse(s, Mode::Mash);
    let root = p.syntax();
    let inner: Vec<String> = root.children().map(|n| shape_of(&n)).collect();
    inner.join(" ")
}

fn ok_shape(s: &str) -> String {
    let p = parse(s, Mode::Mash);
    assert!(p.ok(), "errors parsing {s:?}: {:?}\n{}", p.errors(), p.debug_tree());
    shape(s)
}

fn single_expr(s: &str) -> Expr {
    let p = parse(s, Mode::Mash);
    let exprs = p.source_file().exprs();
    assert_eq!(exprs.len(), 1, "{}", p.debug_tree());
    exprs.into_iter().next().unwrap()
}

// ---------------------------------------------------------------- ported from MashParserTest

#[test]
fn inequality() {
    assert_eq!(ok_shape("10 != 20"), "BIN_OP_EXPR(LITERAL(10) != LITERAL(20))");
}

#[test]
fn pipes_are_left_associative() {
    assert_eq!(
        ok_shape("a | b | c"),
        "PIPE_EXPR(PIPE_EXPR(IDENTIFIER_EXPR(a) | IDENTIFIER_EXPR(b)) | IDENTIFIER_EXPR(c))"
    );
}

#[test]
fn lambda_on_left_of_pipe_binds_looser() {
    assert_eq!(
        ok_shape("x => x | a"),
        "LAMBDA_EXPR(PARAM_LIST(SIMPLE_PARAM(IDENT_PATTERN(x))) => PIPE_EXPR(IDENTIFIER_EXPR(x) | IDENTIFIER_EXPR(a)))"
    );
}

#[test]
fn nested_lambda_on_left_of_pipe() {
    assert_eq!(
        ok_shape("x => y => y | length"),
        "LAMBDA_EXPR(PARAM_LIST(SIMPLE_PARAM(IDENT_PATTERN(x))) => LAMBDA_EXPR(PARAM_LIST(SIMPLE_PARAM(IDENT_PATTERN(y))) => PIPE_EXPR(IDENTIFIER_EXPR(y) | IDENTIFIER_EXPR(length))))"
    );
}

#[test]
fn lambda_on_right_of_pipe_binds_tighter() {
    assert_eq!(
        ok_shape("a | x => x | b"),
        "PIPE_EXPR(PIPE_EXPR(IDENTIFIER_EXPR(a) | LAMBDA_EXPR(PARAM_LIST(SIMPLE_PARAM(IDENT_PATTERN(x))) => IDENTIFIER_EXPR(x))) | IDENTIFIER_EXPR(b))"
    );
}

#[test]
fn nested_lambda_on_right_of_pipe() {
    assert_eq!(
        ok_shape("a | x => y => y | b"),
        "PIPE_EXPR(PIPE_EXPR(IDENTIFIER_EXPR(a) | LAMBDA_EXPR(PARAM_LIST(SIMPLE_PARAM(IDENT_PATTERN(x))) => LAMBDA_EXPR(PARAM_LIST(SIMPLE_PARAM(IDENT_PATTERN(y))) => IDENTIFIER_EXPR(y)))) | IDENTIFIER_EXPR(b))"
    );
}

#[test]
fn chained_comparisons() {
    assert_eq!(
        ok_shape("0 <= x < y <= 100"),
        "CHAINED_OP_EXPR(LITERAL(0) <= IDENTIFIER_EXPR(x) < IDENTIFIER_EXPR(y) <= LITERAL(100))"
    );
    assert_eq!(
        ok_shape("100 >= x > y >= 0"),
        "CHAINED_OP_EXPR(LITERAL(100) >= IDENTIFIER_EXPR(x) > IDENTIFIER_EXPR(y) >= LITERAL(0))"
    );
}

#[test]
fn and_binds_tighter_than_or() {
    assert_eq!(
        ok_shape("a and b or c"),
        "BIN_OP_EXPR(BIN_OP_EXPR(IDENTIFIER_EXPR(a) and IDENTIFIER_EXPR(b)) or IDENTIFIER_EXPR(c))"
    );
}

#[test]
fn invocation_binds_tighter_than_comparison() {
    assert_eq!(
        ok_shape("length xs > 5"),
        "BIN_OP_EXPR(INVOCATION_EXPR(IDENTIFIER_EXPR(length) IDENTIFIER_EXPR(xs)) > LITERAL(5))"
    );
}

#[test]
fn long_flag_arguments() {
    assert_eq!(
        ok_shape("a --b=c.d"),
        "INVOCATION_EXPR(IDENTIFIER_EXPR(a) LONG_ARG(--b = MEMBER_EXPR(IDENTIFIER_EXPR(c) . d)))"
    );
}

#[test]
fn lookup() {
    assert_eq!(ok_shape("a[0]"), "LOOKUP_EXPR(IDENTIFIER_EXPR(a) [ LITERAL(0) ])");
}

#[test]
fn keyword_in_interpolation_is_an_error() {
    assert!(!parse(r#" "$if" "#, Mode::Mash).ok());
}

#[test]
fn interpolated_strings() {
    assert_eq!(
        ok_shape(r#""Hello $name!""#),
        r#"INTERPOLATED_STRING("Hello  SIMPLE_INTERPOLATION($ IDENTIFIER_EXPR(name)) !")"#
    );
    assert_eq!(
        ok_shape(r#""Hello $user.name!""#),
        r#"INTERPOLATED_STRING("Hello  SIMPLE_INTERPOLATION($ MEMBER_EXPR(IDENTIFIER_EXPR(user) . name)) !")"#
    );
    assert_eq!(
        ok_shape(r#""Hello ${name}!""#),
        r#"INTERPOLATED_STRING("Hello  COMPLEX_INTERPOLATION(${ IDENTIFIER_EXPR(name) }) !")"#
    );
    assert_eq!(
        ok_shape(r#""Hello $name! You are in $pwd!""#),
        r#"INTERPOLATED_STRING("Hello  SIMPLE_INTERPOLATION($ IDENTIFIER_EXPR(name)) ! You are in  SIMPLE_INTERPOLATION($ IDENTIFIER_EXPR(pwd)) !")"#
    );
}

#[test]
fn assignment_is_a_pattern_assignment() {
    assert_eq!(
        ok_shape("a = 42"),
        "PATTERN_ASSIGNMENT_EXPR(IDENT_PATTERN(a) = LITERAL(42))"
    );
    assert_eq!(
        ok_shape("a.b = 42"),
        "ASSIGNMENT_EXPR(MEMBER_EXPR(IDENTIFIER_EXPR(a) . b) = LITERAL(42))"
    );
    assert_eq!(ok_shape("a += 1"), "ASSIGNMENT_EXPR(IDENTIFIER_EXPR(a) += LITERAL(1))");
}

#[test]
fn region_of_partial_lambda_covers_input() {
    let p = parse("(=> 1 +", Mode::Mash);
    assert_eq!(p.syntax().text_range().len(), 7.into());
}

// ---------------------------------------------------------------- block vs object

#[test]
fn block_vs_object() {
    assert!(ok_shape("{ a: 1 }").starts_with("OBJECT_EXPR"));
    assert!(ok_shape("{}").starts_with("OBJECT_EXPR"));
    assert!(ok_shape("{ a }").starts_with("OBJECT_EXPR({ SHORTHAND_OBJECT_ENTRY"));
    assert!(ok_shape("{ a, b: 2 }").starts_with("OBJECT_EXPR"));
    assert!(ok_shape("{ (x + 1): 2 }").starts_with("OBJECT_EXPR"));
    assert!(ok_shape("{ 'quoted': 2 }").starts_with("OBJECT_EXPR"));
    assert!(ok_shape("{ a; b }").starts_with("BLOCK_EXPR"));
    assert!(ok_shape("{ foo bar }").starts_with("BLOCK_EXPR"));
    assert!(ok_shape("{ x => x }").starts_with("BLOCK_EXPR"));
    assert!(ok_shape("{ f { a: 1 } }").starts_with("BLOCK_EXPR"));
    // A colon after an inferred semicolon: decided as a block (then an error), as Scala would
    assert!(!parse("{\n  a\n  b: 1\n}", Mode::Mash).ok());
    // Nested: the inner braces are decided independently
    assert_eq!(
        ok_shape("{ a: { b; c } }"),
        "OBJECT_EXPR({ FULL_OBJECT_ENTRY(IDENTIFIER_EXPR(a) : BLOCK_EXPR({ STATEMENT_SEQ(IDENTIFIER_EXPR(b) ; IDENTIFIER_EXPR(c)) })) })"
    );
}

#[test]
fn deeply_nested_blocks_are_linear() {
    // With naive speculation (no memoisation) this would be exponential
    let s = "{ ".repeat(200) + &"} ".repeat(200);
    let start = std::time::Instant::now();
    let p = parse(&s, Mode::Mash);
    assert!(start.elapsed().as_millis() < 500, "took {:?}", start.elapsed());
    assert_eq!(p.syntax().text().to_string(), s);
}

// ---------------------------------------------------------------- semicolon inference

#[test]
fn newlines_separate_statements() {
    assert_eq!(
        ok_shape("a = 1\nb = 2\na + b"),
        "STATEMENT_SEQ(PATTERN_ASSIGNMENT_EXPR(IDENT_PATTERN(a) = LITERAL(1)) PATTERN_ASSIGNMENT_EXPR(IDENT_PATTERN(b) = LITERAL(2)) BIN_OP_EXPR(IDENTIFIER_EXPR(a) + IDENTIFIER_EXPR(b)))"
    );
}

#[test]
fn leading_dot_continues_previous_line() {
    assert_eq!(
        ok_shape("foo\n  .bar\n  ?.baz"),
        "MEMBER_EXPR(MEMBER_EXPR(IDENTIFIER_EXPR(foo) . bar) ?. baz)"
    );
}

#[test]
fn newline_after_operator_does_not_end_statement() {
    assert_eq!(
        ok_shape("a +\n b"),
        "BIN_OP_EXPR(IDENTIFIER_EXPR(a) + IDENTIFIER_EXPR(b))"
    );
    assert_eq!(
        ok_shape("a |\n b"),
        "PIPE_EXPR(IDENTIFIER_EXPR(a) | IDENTIFIER_EXPR(b))"
    );
}

#[test]
fn comments_end_lines_too() {
    assert_eq!(
        ok_shape("ls # comment\npwd"),
        "STATEMENT_SEQ(IDENTIFIER_EXPR(ls) IDENTIFIER_EXPR(pwd))"
    );
}

#[test]
fn function_header_does_not_infer_semis_but_body_does() {
    let s = "def f = {\n  x = 1\n  x\n}\nf";
    let sh = ok_shape(s);
    assert!(sh.starts_with("STATEMENT_SEQ(FUNCTION_DECL("), "{sh}");
    assert!(sh.contains("BLOCK_EXPR({ STATEMENT_SEQ("), "{sh}");
}

#[test]
fn class_body_methods_separated_by_newlines() {
    let file = parse("class Box n\n{\n  def increment = n += 1\n  def get = n\n}", Mode::Mash).source_file();
    let Expr::ClassDecl(class) = &file.exprs()[0] else {
        panic!()
    };
    assert_eq!(class.name_token().unwrap().text(), "Box");
    let names: Vec<String> = class
        .methods()
        .iter()
        .map(|m| m.name_token().unwrap().text().to_string())
        .collect();
    assert_eq!(names, ["increment", "get"]);
}

// ---------------------------------------------------------------- invocation, flags, parens

#[test]
fn paren_invocation_vs_paren_arg() {
    assert_eq!(
        ok_shape("f(a, b)"),
        "PAREN_INVOCATION_EXPR(IDENTIFIER_EXPR(f) ( IDENTIFIER_EXPR(a) , IDENTIFIER_EXPR(b) ))"
    );
    assert_eq!(
        ok_shape("f (a)"),
        "INVOCATION_EXPR(IDENTIFIER_EXPR(f) PAREN_EXPR(( IDENTIFIER_EXPR(a) )))"
    );
}

#[test]
fn flags() {
    let Expr::Invocation(inv) = single_expr("f a --flag --other=value.x -abc") else {
        panic!()
    };
    let args = inv.args();
    assert_eq!(args.len(), 4);
    assert!(matches!(&args[0], ast::Arg::Positional(Expr::Identifier(_))));
    assert!(matches!(&args[1], ast::Arg::Long(l) if l.name() == "flag" && l.value().is_none()));
    assert!(matches!(&args[2], ast::Arg::Long(l) if l.name() == "other" && l.value().is_some()));
    assert!(matches!(&args[3], ast::Arg::Short(s) if s.flags() == "abc"));
}

#[test]
fn negative_number_argument_quirk() {
    // Same as Scala: `f -1` lexes -1 as a number literal so it's an argument, not subtraction
    assert_eq!(ok_shape("f -1"), "INVOCATION_EXPR(IDENTIFIER_EXPR(f) LITERAL(-1))");
    assert_eq!(ok_shape("f - 1"), "BIN_OP_EXPR(IDENTIFIER_EXPR(f) - LITERAL(1))");
}

#[test]
fn help() {
    assert_eq!(ok_shape("ls?"), "HELP_EXPR(IDENTIFIER_EXPR(ls) ?)");
}

#[test]
fn holes_and_headless_members() {
    assert_eq!(ok_shape("_ + _2"), "BIN_OP_EXPR(HOLE_EXPR(_) + HOLE_EXPR(_2))");
    assert_eq!(ok_shape(".foo.bar"), "MEMBER_EXPR(HEADLESS_MEMBER_EXPR(. foo) . bar)");
}

// ---------------------------------------------------------------- lambdas & patterns & defs

#[test]
fn lambda_params() {
    let Expr::Lambda(l) = single_expr("(a = 1) b... { c } [d, _] => a") else {
        panic!()
    };
    let params = l.params().unwrap().params();
    assert_eq!(params.len(), 4);
    let ast::Param::Paren(p0) = &params[0] else { panic!() };
    assert!(p0.default().is_some());
    let ast::Param::Simple(p1) = &params[1] else { panic!() };
    assert!(p1.is_variadic());
    assert!(matches!(params[2], ast::Param::Simple(ref p) if matches!(p.pattern(), Some(ast::Pattern::Object(_)))));
    assert!(matches!(params[3], ast::Param::Simple(ref p) if matches!(p.pattern(), Some(ast::Pattern::List(_)))));
}

#[test]
fn zero_param_lambda() {
    assert_eq!(ok_shape("=> 42"), "LAMBDA_EXPR(=> LITERAL(42))");
}

#[test]
fn not_a_lambda_without_arrow() {
    assert!(ok_shape("(a = 1) b").starts_with("INVOCATION_EXPR"));
}

#[test]
fn pattern_assignment_with_destructuring() {
    assert!(ok_shape("{ a, b } = obj").starts_with("PATTERN_ASSIGNMENT_EXPR(OBJECT_PATTERN"));
    assert!(ok_shape("[x, y] = [1, 2]").starts_with("PATTERN_ASSIGNMENT_EXPR(LIST_PATTERN"));
}

#[test]
fn function_declaration() {
    let Expr::FunctionDecl(f) = single_expr("# Square a number\ndef square (@lazy n) = n * n") else {
        panic!()
    };
    assert_eq!(f.name_token().unwrap().text(), "square");
    assert_eq!(f.doc_comment().map(|d| d.summary).as_deref(), Some("Square a number"));
    let ast::Param::Paren(p) = &f.params().unwrap().params()[0] else {
        panic!()
    };
    assert_eq!(p.attributes().unwrap().attributes()[0].name(), "lazy");
    assert!(matches!(f.body(), Some(Expr::BinOp(_))));
}

#[test]
fn attributes_before_def() {
    let Expr::FunctionDecl(f) = single_expr("@private @(alias foo) def bar = 1") else {
        panic!()
    };
    let names: Vec<String> = f.attributes().unwrap().attributes().iter().map(|a| a.name()).collect();
    assert_eq!(names, ["private", "alias"]);
}

#[test]
fn import_and_namespace() {
    let p = parse("namespace a.b\nimport hash._", Mode::Mash);
    assert!(p.ok(), "{:?}", p.errors());
    let file = p.source_file();
    assert_eq!(file.namespace().unwrap().segments(), ["a", "b"]);
    // The newline after the namespace is an inferred semicolon, so (as in Scala) the body is a StatementSeq
    let Expr::StatementSeq(seq) = &file.exprs()[0] else {
        panic!("{}", p.debug_tree())
    };
    let Expr::Import(i) = &seq.statements()[0] else {
        panic!("{}", p.debug_tree())
    };
    assert_eq!(i.segments(), ["hash", "_"]);
}

// ---------------------------------------------------------------- literals & strings

#[test]
fn literal_values() {
    let lit = |s: &str| match single_expr(s) {
        Expr::Literal(l) => l.value().unwrap(),
        e => panic!("{e:?}"),
    };
    assert_eq!(lit("42"), LiteralValue::Number(42.0));
    assert_eq!(lit("-1.5"), LiteralValue::Number(-1.5));
    assert_eq!(lit("true"), LiteralValue::Bool(true));
    assert_eq!(lit("null"), LiteralValue::Null);
    assert_eq!(
        lit("\"foo`nbar\""),
        LiteralValue::String {
            value: "foo\nbar".into(),
            double_quoted: true,
            tilde: false
        }
    );
    assert_eq!(
        lit("\"~/Documents\""),
        LiteralValue::String {
            value: "/Documents".into(),
            double_quoted: true,
            tilde: true
        }
    );
    assert_eq!(
        lit("'a`'b'"),
        LiteralValue::String {
            value: "a'b".into(),
            double_quoted: false,
            tilde: false
        }
    );
}

#[test]
fn interpolation_parts() {
    let Expr::InterpolatedString(s) = single_expr(r#""~/a $x.y b ${ 1 + 2 } c`$""#) else {
        panic!()
    };
    assert!(s.tilde());
    let parts = s.parts();
    assert!(matches!(&parts[0], ast::InterpolationPart::Text(t) if t == "/a "));
    assert!(matches!(&parts[1], ast::InterpolationPart::Simple(si) if matches!(si.expr(), Some(Expr::Member(_)))));
    assert!(matches!(&parts[2], ast::InterpolationPart::Text(t) if t == " b "));
    assert!(matches!(&parts[3], ast::InterpolationPart::Complex(c) if matches!(c.expr(), Some(Expr::BinOp(_)))));
    assert!(matches!(&parts[4], ast::InterpolationPart::Text(t) if t == " c$"));
}

#[test]
fn deeply_nested_modes() {
    let sh = ok_shape(r#""${ !{ls $x} }""#);
    assert_eq!(
        sh,
        r#"INTERPOLATED_STRING(" COMPLEX_INTERPOLATION(${ MISH_INTERPOLATION_EXPR(!{ MISH_EXPR(ls SIMPLE_INTERPOLATION($ IDENTIFIER_EXPR(x))) }) }) ")"#
    );
}

// ---------------------------------------------------------------- mish

#[test]
fn mish_interpolation() {
    let Expr::MishInterpolation(m) = single_expr("!{ls -l $dir ${pwd} 'q' > out.txt}") else {
        panic!()
    };
    assert!(m.captures());
    let items = m.mish().unwrap().items();
    assert_eq!(items.len(), 6);
    assert!(matches!(&items[0], ast::MishItem::Word(w) if w.text() == "ls"));
    assert!(matches!(&items[2], ast::MishItem::Simple(_)));
    assert!(matches!(&items[3], ast::MishItem::Complex(_)));
    assert!(matches!(&items[4], ast::MishItem::Literal(_)));
    assert!(matches!(&items[5], ast::MishItem::Redirect(r) if r.op() == Some(SyntaxKind::GREATER_THAN)));
    let Expr::MishInterpolation(m) = single_expr("!!{vim foo}") else {
        panic!()
    };
    assert!(!m.captures());
}

#[test]
fn mish_mode_line() {
    let p = parse("git log --oneline $branch > \"out $x.txt\"", Mode::Mish);
    assert!(p.ok(), "{:?}", p.errors());
    let items = p.source_file().mish().unwrap().items();
    assert_eq!(items.len(), 5);
}

#[test]
fn mish_function() {
    let Expr::Invocation(i) = single_expr("!mplayer video") else {
        panic!()
    };
    let Some(Expr::MishFunction(f)) = i.function() else {
        panic!()
    };
    assert_eq!(f.command(), "mplayer");
}

// ---------------------------------------------------------------- whole corpus via typed layer

#[test]
fn every_expr_node_casts() {
    for s in common::CORPUS {
        let root = parse(s, Mode::Mash).syntax();
        for node in root.descendants() {
            if node.kind().is_expr() {
                assert!(Expr::cast(node.clone()).is_some(), "{:?} in {s:?}", node.kind());
            }
        }
    }
}
