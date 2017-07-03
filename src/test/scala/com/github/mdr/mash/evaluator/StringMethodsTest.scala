package com.github.mdr.mash.evaluator

class StringMethodsTest extends AbstractEvaluatorTest {

  // String.matches
  " 'foo'.matches 'o' " ==> true
  " 'foo'.matches 'x' " ==> false
  " 'foo'.matches '^fo?.$' " ==> true
  " 'foo 666 bar'.matches 666 " ==> true
  " 'bar'.matches 666 " ==> false

  // String.startsWith
  " 'foo'.startsWith 'f' " ==> true
  " 'foo'.startsWith 'o' " ==> false

  // String.split
  "'foo bar baz'.split" ==> "['foo', 'bar', 'baz']"
  "'foo bar baz' | split" ==> "['foo', 'bar', 'baz']"
  "'foo  bar     baz'.split" ==> "['foo', 'bar', 'baz']"
  "'foo:bar:baz'.split ':'" ==> "['foo', 'bar', 'baz']"
  "'foo:bar:baz' | split ':'" ==> "['foo', 'bar', 'baz']"
  "'foo:bar:'.split ':'" ==> "['foo', 'bar', '']"
  "'foo1bar2baz' | split --regex '[0-9]'" ==> "['foo', 'bar', 'baz']"
  "'foo1bar2baz'.split --regex '[0-9]'" ==> "['foo', 'bar', 'baz']"

  // String.lines
  "'foo`nbar`r`nbaz buzz'.lines" ==> "['foo', 'bar', 'baz buzz']"
  "''.lines" ==> "[]"
  "'foo'.lines" ==> "['foo']"
  "'foo`n'.lines" ==> "['foo']"
  "'`nfoo'.lines" ==> "['', 'foo']"
  "'foo`n`nbar'.lines" ==> "['foo', '', 'bar']"
  "'`n`n'.lines" ==> "['', '']"

  // String.toList
  "'abc'.toList" ==> "['a', 'b', 'c']"
  "'abc'.characters" ==> "['a', 'b', 'c']"
  "'abc'.characters.first.tag" ==> "Character"
  "''.toList" ==> "[]"

  // .deleteUpTo
  "'foo bar: baz'.deleteUpTo 'bar: '" ==> "'bar: baz'"
  "'foo bar: baz'.deleteUpTo --andIncluding 'bar: '" ==> "'baz'"
  """'result: { "value": 10 }'.deleteUpTo '{'""" ==> """'{ "value": 10 }'"""
  "'value: 10'.deleteUpTo --andIncluding 'value: '" ==> "'10'"
  "'abc'.deleteUpTo 'not found'" ==> "'abc'"

  // .deleteAfter
  """'{ "value": 10 } # comment'.deleteAfter '}'""" ==> """'{ "value": 10 }'"""
  "'10 # comment'.deleteAfter --andIncluding ' #'" ==> "'10'"
  "'abc'.deleteAfter 'not found'" ==> "'abc'"
}
