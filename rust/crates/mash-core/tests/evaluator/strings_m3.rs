//! String and regex behaviour not covered by the Scala tests. Expected results were checked against Scala Mash,
//! except where marked DIFFERENT: those follow the agreed decisions (chars as the unit of length and indexing;
//! Rust regex replacement and split semantics; Unicode-aware case-insensitivity).

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    // Regex replacement
    Case::eq(r#"'a1b2'.replace --regex '([a-z])([0-9])' '$2$1'"#, r#"'1a2b'"#),
    Case::eq(r#"'a1'.replace --regex '([a-z])' '${1}x'"#, r#"'ax1'"#),
    // DIFFERENT: in Rust syntax `$1x` names a group `1x` (Java: group 1 then `x`, giving 'ax1'); a group the
    // regex doesn't have is an error (as in Java), rather than quietly replaced by nothing
    Case::throws(r#"'a1'.replace --regex '([a-z])' '$1x'"#),
    Case::throws(r#"'ab'.replace --regex '(a)' '$9'"#),
    Case::throws(r#"'ab'.replace --regex '(a)' '${foo}'"#),
    Case::eq(r#"'ab'.replace --regex '(?<n>a)' '[$n][${n}]'"#, r#"'[a][a]b'"#),
    Case::eq(r#"'ab'.replace --regex 'a' '$$$0'"#, r#"'$ab'"#),
    Case::eq(r#"'ab'.replace --regex 'a' 'cost $'"#, r#"'cost $b'"#),
    Case::eq(r#"'a.c'.replace '.' 'x'"#, r#"'axc'"#),
    Case::eq(r#"'a.c.'.replace '.' ''"#, r#"'ac'"#),
    // Lookaround and backreferences
    Case::eq(r#"'foobar'.matches 'foo(?=bar)'"#, r#"true"#),
    Case::eq(r#"'abab'.matches '(ab)\1'"#, r#"true"#),
    // DIFFERENT: Unicode-aware case-insensitivity (Java's is ASCII-only)
    Case::eq(r#"'É'.matches -i 'é'"#, r#"true"#),
    Case::throws(r#"'abc'.matches '('"#),
    // Match objects
    Case::eq(r#"('(.*)bar'.r.match 'wibblebar').groups"#, r#"['wibble']"#),
    Case::eq(r#"('(.*)bar'.r.match 'wibblebar').matched"#, r#"'wibblebar'"#),
    Case::eq(r#"'x'.r.match 'abc'"#, r#"null"#),
    Case::eq(r#"('(a)?b'.r.match 'b').groups"#, r#"[null]"#),
    Case::eq(r#"('a'.r.match 'a').getClass"#, r#"core.Match"#),
    Case::eq(r#"'a'.regex.tag"#, r#"core.Regex"#),
    // grep with regexes
    Case::eq(r#"['apple', 'ball'] | grep -r '^a'"#, r#"['apple']"#),
    Case::eq(r#"['Apple', 'ball'] | grep -r -i '^a'"#, r#"['Apple']"#),
    Case::eq(r#"'x`ny`nxy'.grep --first 'y'"#, r#"'y'"#),
    Case::eq(r#"'x`ny`nxy'.grep -r 'y$'"#, r#"['y', 'xy']"#),
    // Splitting
    Case::eq(r#"'a,b,,c,'.split ','"#, r#"['a', 'b', '', 'c', '']"#),
    Case::eq(r#"' a b'.split"#, r#"['', 'a', 'b']"#),
    Case::eq(r#"'xay'.split --regex '(?=a)'"#, r#"['x', 'ay']"#),
    // DIFFERENT: Rust's split on an empty separator (Java gives ['a', 'b', 'c', ''])
    Case::eq(r#"'abc'.split ''"#, r#"['', 'a', 'b', 'c', '']"#),
    Case::eq(r#"split ':' 'a:b'"#, r#"['a', 'b']"#),
    Case::eq(r#"('a:b'.toPath.split ':').first.tag"#, r#"os.Path"#),
    Case::throws(r#"'a:b'.split 42"#),
    // DIFFERENT: lengths and indexes count chars, not UTF-16 units (Java gives 2 for both)
    Case::eq(r#"'😀x'.indexOf 'x'"#, r#"1"#),
    Case::eq(r#"'😀'.length"#, r#"1"#),
    Case::eq(r#"'abc'.indexOf 'z'"#, r#"null"#),
    // Conversions and tags
    Case::eq(r#"' 42 '.toNumber"#, r#"42"#),
    Case::throws(r#"'abc'.toNumber"#),
    Case::eq(r#"'straße'.toUpper"#, r#"'STRASSE'"#),
    Case::eq(r#"'foo'.toPath.untagged.tag"#, r#"null"#),
    Case::eq(r#"'foo'.toPath.tag"#, r#"os.Path"#),
    Case::eq(r#"'a'.c.tag"#, r#"core.Character"#),
    Case::throws(r#"'ab'.toCharacter"#),
    Case::eq(r#"'  x '.trim"#, r#"'x'"#),
    // Collection functions as String methods
    Case::eq(r#"'abc'.reverse"#, r#"'cba'"#),
    Case::eq(r#"'abc'.map (_.toUpper)"#, r#"'ABC'"#),
    Case::eq(r#"'abc'.count"#, r#"3"#),
    Case::eq(r#"'abc'.contains 'b'"#, r#"true"#),
    Case::eq(r#"'abc'.endsWith 'bc'"#, r#"true"#),
    // New: a Character tag is kept only for a single character (Scala kept it, making 'Characters' of any length)
    Case::eq(r#"('ß'.c.toUpper).tag"#, r#"null"#),
    Case::eq(r#"('a'.c.replace 'a' 'xyz').tag"#, r#"null"#),
    Case::eq(r#"('a'.c.toUpper).tag"#, r#"core.Character"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

const DEFERRED: &[&str] = &[];
