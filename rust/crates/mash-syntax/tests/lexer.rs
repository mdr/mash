//! Port of a large part of MashLexerTest.scala

use mash_syntax::SyntaxKind::{self, *};
use mash_syntax::lexer::tokenise;

fn kinds(s: &str, mish: bool) -> Vec<SyntaxKind> {
    tokenise(s, mish)
        .tokens
        .iter()
        .map(|t| t.kind)
        .filter(|k| !k.is_trivia())
        .collect()
}

fn raw_kinds(s: &str) -> Vec<SyntaxKind> {
    tokenise(s, false).tokens.iter().map(|t| t.kind).collect()
}

macro_rules! lex_tests {
    ($($name:ident: $input:expr => [$($kind:ident),*];)*) => {$(
        #[test]
        fn $name() {
            assert_eq!(kinds($input, false), vec![$($kind),*], "lexing {:?}", $input);
        }
    )*};
}

macro_rules! mish_lex_tests {
    ($($name:ident: $input:expr => [$($kind:ident),*];)*) => {$(
        #[test]
        fn $name() {
            assert_eq!(kinds($input, true), vec![$($kind),*], "lexing (mish) {:?}", $input);
        }
    )*};
}

lex_tests! {
    empty: "" => [];
    ident: "foo" => [IDENTIFIER];
    dq_string: r#" "foo" "# => [STRING_LITERAL];
    empty_string: "\"\"" => [STRING_LITERAL];
    sq_string: "'foo'" => [STRING_LITERAL];
    multiline_string: "\"foo\nbar\"" => [STRING_LITERAL];
    escaped_quote: r#" "foo`"" "# => [STRING_LITERAL];
    escaped_quote_then_number: r#" "foo`"" 3 "# => [STRING_LITERAL, NUMBER_LITERAL];
    num1: "1" => [NUMBER_LITERAL];
    num2: "1.0" => [NUMBER_LITERAL];
    num3: "0.0" => [NUMBER_LITERAL];
    num4: "0" => [NUMBER_LITERAL];
    num5: "10" => [NUMBER_LITERAL];
    num6: "1234567890" => [NUMBER_LITERAL];
    num_member: "1.day" => [NUMBER_LITERAL, DOT, IDENTIFIER];
    num_dot: "1." => [NUMBER_LITERAL, DOT];
    negative: "-1" => [NUMBER_LITERAL];
    negative_fraction: "-1.23" => [NUMBER_LITERAL];
    booleans: "true false" => [TRUE_KW, FALSE_KW];
    this: "this" => [THIS_KW];
    short_flag: "ls -a" => [IDENTIFIER, SHORT_FLAG];
    long_flag: "ls --recursive" => [IDENTIFIER, LONG_FLAG];
    long_flag_value: "foo --bar=baz" => [IDENTIFIER, LONG_FLAG, SHORT_EQUALS, IDENTIFIER];
    eq: "a==b" => [IDENTIFIER, LONG_EQUALS, IDENTIFIER];
    neq: "a!=b" => [IDENTIFIER, NOT_EQUALS, IDENTIFIER];
    lt: "a<b<=c" => [IDENTIFIER, LESS_THAN, IDENTIFIER, LESS_THAN_EQUALS, IDENTIFIER];
    gt: "a>b>=c" => [IDENTIFIER, GREATER_THAN, IDENTIFIER, GREATER_THAN_EQUALS, IDENTIFIER];
    at: "@" => [AT];
    hole: "_" => [HOLE];
    numbered_hole: "_3" => [HOLE];
    underscore_ident: "_a" => [IDENTIFIER];
    alphabet: "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" => [IDENTIFIER];
    alnum: "a0b1c2_3" => [IDENTIFIER];
    alnum_2: "a123457890" => [IDENTIFIER];
    punctuation: "| => ( )" => [PIPE, RIGHT_ARROW, LPAREN, RPAREN];
    and_or: "and or" => [AND_KW, OR_KW];
    arith: "+ - * /" => [PLUS, MINUS, TIMES, DIVIDE];
    if_then_else: "if then else" => [IF_KW, THEN_KW, ELSE_KW];
    def: "def" => [DEF_KW];
    class: "class" => [CLASS_KW];
    squares: "[ ] ," => [LSQUARE, RSQUARE, COMMA];
    braces: "{ } :" => [LBRACE, RBRACE, COLON];
    list_arg: "first [1]" => [IDENTIFIER, LSQUARE, NUMBER_LITERAL, RSQUARE];
    lookup: "first[1]" => [IDENTIFIER, LSQUARE_LOOKUP, NUMBER_LITERAL, RSQUARE];
    hole_lookup: "_[1]" => [HOLE, LSQUARE_LOOKUP, NUMBER_LITERAL, RSQUARE];
    this_lookup: "this[1]" => [THIS_KW, LSQUARE_LOOKUP, NUMBER_LITERAL, RSQUARE];
    number_lookup: "3[" => [NUMBER_LITERAL, LSQUARE_LOOKUP];
    string_lookup: "\"foo\"[" => [STRING_LITERAL, LSQUARE_LOOKUP];
    null_lookup: "null[" => [NULL_KW, LSQUARE_LOOKUP];
    paren_lookup: ")[" => [RPAREN, LSQUARE_LOOKUP];
    square_lookup: "][" => [RSQUARE, LSQUARE_LOOKUP];
    brace_lookup: "}[" => [RBRACE, LSQUARE_LOOKUP];
    paren_invoke: "f(x)" => [IDENTIFIER, LPAREN_INVOKE, IDENTIFIER, RPAREN];
    paren_not_invoke: "f (x)" => [IDENTIFIER, LPAREN, IDENTIFIER, RPAREN];
    null_safe: "?." => [DOT_NULL_SAFE];
    ellipsis: "..." => [ELLIPSIS];
    null_safe_member: "foo?.bar" => [IDENTIFIER, DOT_NULL_SAFE, IDENTIFIER];
    interp1: r#" "Hello $name, how are you?" "# => [STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END];
    interp2: r#" "Hello $name, how are you $name?" "# => [STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_MIDDLE, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END];
    interp_member: r#" "Hello $user.name, how are you?" "# => [STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, DOT, IDENTIFIER, STRING_END];
    interp_complex: r#" "Hello ${name}, how are you?" "# => [STRING_START, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, STRING_END];
    interp_complex_expr: r#" "Hello ${3 + 2}, how are you?" "# => [STRING_START, STRING_INTERPOLATION_START_COMPLEX, NUMBER_LITERAL, PLUS, NUMBER_LITERAL, RBRACE, STRING_END];
    interp_only: r#" "$name" "# => [STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END];
    interp_adjacent: r#" "$name$bar" "# => [STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END];
    interp_complex_only: r#" "${name}" "# => [STRING_START, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, STRING_END];
    interp_complex_adjacent: r#" "${name}${bar}" "# => [STRING_START, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, STRING_END];
    interp_nested_braces: r#" "${{}}" "# => [STRING_START, STRING_INTERPOLATION_START_COMPLEX, LBRACE, RBRACE, RBRACE, STRING_END];
    interp_member_bang: r#" "Hello $user.fullName!" "# => [STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, DOT, IDENTIFIER, STRING_END];
    interp_hole: r#" "$_" "# => [STRING_START, STRING_INTERPOLATION_START_SIMPLE, HOLE, STRING_END];
    interp_escaped_quotes: r#" "`"$foo`"" "# => [STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END];
    interp_newline: " \" $foo\n     \" " => [STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END];
    mish_in_mash: "!{ls}" => [MISH_INTERPOLATION_START, MISH_WORD, RBRACE];
    mish_in_mash_2: "foo !{ls} bar" => [IDENTIFIER, MISH_INTERPOLATION_START, MISH_WORD, RBRACE, IDENTIFIER];
    mish_interp: "!{ls ${pwd} }" => [MISH_INTERPOLATION_START, MISH_WORD, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, RBRACE];
    mish_no_capture: "!!{ls}" => [MISH_INTERPOLATION_START_NO_CAPTURE, MISH_WORD, RBRACE];
    mish_word: "!mplayer" => [MISH_WORD];
    mish_word_arg: "!mplayer video" => [MISH_WORD, IDENTIFIER];
    bang: "!" => [ERROR_TOKEN];
    // Deep nesting: string -> ${ mash -> !{ mish -> $x
    nested_modes: r#""${ !{ls $x} }""# => [STRING_START, STRING_INTERPOLATION_START_COMPLEX, MISH_INTERPOLATION_START, MISH_WORD, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, RBRACE, RBRACE, STRING_END];
    unterminated: "\"unterminated string" => [STRING_LITERAL];
    leading_zero: "03" => [NUMBER_LITERAL];
    dollar: "$" => [ERROR_TOKEN];
    multiline: "foo\n     bar" => [IDENTIFIER, IDENTIFIER];
    multiline_comment: "foo #bar\n     baz" => [IDENTIFIER, IDENTIFIER];
}

mish_lex_tests! {
    mish_foo: "foo" => [MISH_WORD];
    mish_file: "foo.txt" => [MISH_WORD];
    mish_path: "/bin/bash" => [MISH_WORD];
    mish_two: "git status" => [MISH_WORD, MISH_WORD];
    mish_string: r#"cd "My Documents" "# => [MISH_WORD, STRING_LITERAL];
    mish_simple: " ls $pwd -l" => [MISH_WORD, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, MISH_WORD];
    mish_complex: " ls ${pwd} -l" => [MISH_WORD, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, MISH_WORD];
    mish_interp_string: r#" "Hello $name, how are you?" "# => [STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END];
    mish_sq: " 'foo' " => [STRING_LITERAL];
    mish_sq_dollar: " '$foo' " => [STRING_LITERAL];
    mish_dollar: "foo $" => [MISH_WORD, STRING_INTERPOLATION_START_SIMPLE];
    mish_redirect: "ls > out.txt" => [MISH_WORD, GREATER_THAN, MISH_WORD];
}

#[test]
fn comments_and_whitespace() {
    assert_eq!(raw_kinds("# comment"), vec![COMMENT]);
    assert_eq!(raw_kinds("  # comment"), vec![WHITESPACE, COMMENT]);
    assert_eq!(raw_kinds("  #"), vec![WHITESPACE, COMMENT]);
    assert_eq!(raw_kinds("\t\r\n "), vec![WHITESPACE]);
}

#[test]
fn strict_mode_errors_are_reported() {
    // Scala's strict lexer throws on these; we report errors instead
    for s in ["\"unterminated string", "03", "$", "--"] {
        assert!(
            !tokenise(s, false).errors.is_empty(),
            "expected a lexer error for {s:?}"
        );
    }
    assert!(tokenise("foo bar", false).errors.is_empty());
}

#[test]
fn tokens_cover_input_exactly() {
    for s in [
        "foo \"a $b.c ${ !{ls $x > y} } d\" # c\n",
        "!{ls 'x' \"y\"}",
        "é ∑ 😀 --é",
    ] {
        let joined: String = tokenise(s, false).tokens.iter().map(|t| &s[t.range]).collect();
        assert_eq!(joined, s);
    }
}

#[test]
fn legal_identifiers() {
    // From IsLegalIdentifierTest.scala
    use mash_syntax::lexer::is_legal_identifier;
    for s in ["foo", "x", "x_", "a1b2"] {
        assert!(is_legal_identifier(s), "{s}");
    }
    for s in [".", "class", "true", "null", "123", "foo-bar", "_", "_1", ""] {
        assert!(!is_legal_identifier(s), "{s}");
    }
}
