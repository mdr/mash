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
  "'foo  bar     baz'.split" ==> "['foo', 'bar', 'baz']"
  "'foo:bar:baz'.split ':'" ==> "['foo', 'bar', 'baz']"
  "'foo:bar:baz' | split ':'" ==> "['foo', 'bar', 'baz']"
  "'foo:bar:'.split ':'" ==> "['foo', 'bar', '']"

  // String.lines
  "'foo`nbar`r`nbaz buzz'.lines" ==> "['foo', 'bar', 'baz buzz']"
  "''.lines" ==> "[]"
  "'foo'.lines" ==> "['foo']"
  "'foo`n'.lines" ==> "['foo']"
  "'`nfoo'.lines" ==> "['', 'foo']"
  "'foo`n`nbar'.lines" ==> "['foo', '', 'bar']"
  "'`n`n'.lines" ==> "['', '']"


}
