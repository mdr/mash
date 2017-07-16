package com.github.mdr.mash.evaluator

class EvaluatorTest extends AbstractEvaluatorTest {

  "42 | _ < 100" ==> true

  "(x => x) | (f => f 42)" ==> "42"

  "if 1 < 2 then 100 else 200" ==> 100
  "if 1 > 2 then 100 else 200" ==> 200
  "if 1 == 1 then 100" ==> 100

  val Y = "(h => (x => x x) (g => h (args => (g g) args)))" // fixed point combinator
  s"($Y (f => n => if n == 1 then 1 else n * f (n - 1))) 5" ==> 120

  "[1, 2, 3, 4, 5] | filter (_ >= 3)" ==> "[3, 4, 5]"

  "{ foo: 42, bar: 24 } | [_.foo, _.bar]" ==> "[42, 24]"

  // deselect
  "{ foo: 42, bar: 1 } | deselect 'foo'" ==> "{ bar: 1 }"
  "[{ foo: 42, bar: 1 }, { foo: 12, bar: 34 }] | deselect 'foo'" ==> "[{ bar: 1 }, { bar: 34 }]"

  // identity
  "identity 1" ==> 1

  // isNull
  "isNull null" ==> true
  "isNull 0" ==> false

  // not
  "not true" ==> false
  "not false" ==> true
  "not 0" ==> true
  "not []" ==> true
  "not {}" ==> true
  "not 1" ==> false

  // Member vectorisation
  "['foo', 'bar', 'baz'].startsWith 'ba'" ==> "[false, true, true]"
  "['foo'].noSuchMember".shouldThrowAnException
  "['foo bar', 'baz'].split" ==> "[['foo', 'bar'], ['baz']]"
  "['foo:bar', 'baz'].split ':'" ==> "[['foo', 'bar'], ['baz']]"
  "[].noSuchMember" ==> "[]"
  "['foo', 'bar', 'baz'].startsWith | map (_ 'b')" ==> "[false, true, true]"

  // Strings as functions
  " 'foo' { foo: 42 } " ==> 42
  " 'toString' 42 " ==> " '42' "
  " 'foo' [{ foo: 42 }] " ==> "[42]"
  "'reverse' [1, 2, 3]" ==> "[3, 2, 1]"
  "'startsWith' ['foo', 'bar', 'baz'] | map (_ 'b')" ==> "[false, true, true]"

  // Booleans as functions
  "true 1 2" ==> 1
  "false 1 2" ==> 2
  "a = 0; true (a += 1); a" ==> 1
  "a = 0; false (a += 1); a" ==> 0
  "a = 0; true (a += 1) (a -= 1); a" ==> 1
  "a = 0; false (a += 1) (a -= 1); a" ==> -1

  // String escapes
  """ "`$" """ ==> " '$' "
  """ "`"" """ ==> """ '"' """
  """ '`'' """ ==> """ "'" """

  // String literals
  """ "foo`nbar".length """ ==> 7

  "true and true and true" ==> true
  "true or true or true" ==> true
  "1; 2; 3" ==> 3
  "a = 1; a = 2; a" ==> 2
  "if true then if false then 24 else 42 else 1024" ==> 42

  "(if true then a = 42 else a = 24); a" ==> 42

  // Parsing checks: assignment versus lambdas versus pipes
  "a = 0; (100 | (a = _); a)" ==> 100
  "(a = true | not); a" ==> false
  "square = x => x * x; square 3" ==> 9
  "a = 0; (x => a = x) 42; a" ==> 42
  "a = 0; ((100 | a = _); a)" ==> 100
  "(a = x => true | not); a 42" ==> false
  "(a = 42 | x => true); a" ==> true

  // mapping things treats them as functions
  "['foo', 'bar', 'baz'] | map reverse" ==> "['oof', 'rab', 'zab']"
  "['foo', 'bar', 'baz'] | map 'reverse'" ==> "['oof', 'rab', 'zab']"
  "[{foo: 42}] | map 'foo'" ==> "[42]"
  "['f', 'x'] | map 'foo'.startsWith" ==> "[true, false]"

  "def square n = n * n; square 8" ==> 64
  "def square n = n * n; square --n=8" ==> 64
  "def add a b = a + b; add 1 2" ==> 3
  "def pipeFunc s = s.toUpper | reverse; pipeFunc 'foo'" ==> "'OOF'"

  "a = ['aa', 'bbb', 'c'].sortBy; a length" ==> "['c', 'aa', 'bbb']"

  "[].sumBy.target" ==> "[]"

  "now.date.toString".shouldNotThrowAnException

  "[].toString" ==> "'[]'"
  "[1].toString" ==> "'[1]'"
  "[1, 2].toString" ==> "'[1, 2]'"
  "[pwd].toString" ==> "'[' + pwd + ']'"

  "3.14.toInt" ==> 3
  "(-3.14).toInt" ==> -3
  "1485187640884.6155.toInt" ==> "1485187640884"

  """ "foo" { foo: 42 } { foo: 42 }  """.shouldThrowAnException

  // headless members
  "{ foo: 42 } | .foo" ==> 42
  "[1, 2, 3] | .last 2" ==> "[2, 3]"
  "[{ foo: 42 }] | .foo" ==> "[42]"
  "null | ?.foo" ==> null

  // regex
  "'(.*)bar'.r.match 'wibblebar' | .groups.first" ==> "'wibble'"

  // Block expressions
  "{ a = 0; a = a + 1; a }" ==> 1
  "{ a = 42 }; a" ==> 42

  // list subtraction
  "[1, 2, 3, 4] - [2, 4]" ==> "[1, 3]"
  "[1, 2, 3, 4] - []" ==> "[1, 2, 3, 4]"
  "[] - [1, 2, 3]" ==> "[]"
  "[1, 2, 3, 2, 1] - [2, 1]" ==> "[3, 2, 1]"

  {
    // bare words
    implicit val config = Config(bareWords = true)

    "foo" ==> "'foo'"

    // Was a bug here, where foo was incorrectly identified as a bare word
    "def foo x = if x == 0 then 1 else foo (x - 1); foo 5" ==> 1

    "a = 10; a" ==> 10
  }

  // Lambdas with multiple parameters
  "(x y => x * y) 2 3" ==> 6
  "f = => 42; f" ==> 42

  // holes
  "def baz = 128 + _; (baz) 72" ==> 200
  "def foo (x = _) = 42; foo" ==> 42
  "((x = _) => 42) | x => x" ==> 42
  "1.to 5 | reduce (_1 * _2)" ==> 120
  "[{foo: 42}].map(_.foo)" ==> "[42]"
  "'foo' | { _: 42 }" ==> "{ foo: 42 }"

  // Lambdas inside parens and blocks
  "a = [0]; { x => a[0] += x; a[0] += x } 21; a[0]" ==> 42
  "a = 0; (x => a += x; a += x) 21; a" ==> 42

  // Lazy arguments
  "a = 0; def twice (@lazy block) = { block; block }; twice (a += 1); a" ==> 2
  "a = 0; def twice (@lazy block) = { block; block }; twice --block=(a += 1); a" ==> 2
  "a = 0; ((@lazy block) => { block; block }) (a += 1); a" ==> 2

  "def foo n = n += _; f = foo 1; f 2; f 3" ==> 6

  // maths.stats
  "[1, 2, 3] | maths.stats | .mean" ==> 2

  // maths.log
  "maths.log 1000 --base=10" ==> 3

  // tap
  "x = 0; 42 | tap (x = _) | [x, _]" ==> "[42, 42]"

  "def foo (n = 0) = n * 2; foo -2" ==> -4 // ←- negative number literal confusion bug

  // scope
  "a = 0; while (a < 10) { a = a + 1 }; a" ==> 10
  "42 | a = _; a" ==> 42

  // type.hint
  "type.hint [String] 42" ==> 42

  "(def foo = 42) | x => x" ==> 42

  "(_ => b = 100) 42; b" ==> 100

  // attributes
  """@attribute
    |def foo = 42
    |foo
  """.stripMargin ==> "42"

  // "class Foo ({ wibble }); Foo { wibble: 42 } | .wibble" ==> 42

  // Recursion
  "def makeFact = { def fact n = if (n <= 1) then n else n * fact (n - 1) }; (makeFact) 5" ==> 120
  """def mutu m = {
    |  def foo n = if n == 0 then 0 else bar (n - 1)
    |  def bar n = if n == 0 then 1 else foo (n - 1)
    |  foo m
    |}
    |mutu 3""" ==> 1

  // .bless
  "class Point x y { def diff = x - y }; { y: 4, x: 10 }.bless Point | .diff" ==> 6
  "class Point x y { def diff = x - y }; Point.bless { y: 4, x: 10 } | .diff" ==> 6

  // @alias
  "class A { @(alias 'a') def aardvark = 42 }; A.new.a" ==> 42

  // defs, classes as expressions
  "[def foo = 42].first.invoke" ==> 42
  "{ foo: def foo = 42 }.foo" ==> 42
  "[@attribute def foo = 42].first.invoke" ==> 42
  "[class Bob].first.new.getClass.name" ==> "'Bob'"

  // xml.fromString
  "xml.fromString '<foo>bar</foo>'" ==> "{ foo: 'bar' }"

  // json.prettyPrint
  "123456789 | json.prettyPrint" ==> "'123456789'"

  // List.new
  "List 1 2 3" ==> "[1, 2, 3]"
  "List.new" ==> "[]"

  // net.urlEncode
  "net.urlEncode 'user@example.com'" ==> "'user%40example.com'"

  // Check against exponential complexity parser problem
  "{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}" ==> "{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}"

  "class A { def foo = [1, 2, 3] | where (_ > 1) }; A.new.foo" ==> "[2, 3]"

  // Url.withQueryParams
  "net.url 'http://example.com' | .withQueryParams { param: 42 }" ==> "'http://example.com?param=42'"
  "net.url 'http://example.com' | .withQueryParams --param=42" ==> "'http://example.com?param=42'"

  // Url.host
  "net.url 'http://example.com' | .host" ==> "'example.com'"

  // try
  "try (error 'bang')" ==> "()"
  "try (error 'bang') --catch='recovered'" ==> "'recovered'"
  "a = 0; try 'no probs' --catch=(a += 1); a" ==> 0
}