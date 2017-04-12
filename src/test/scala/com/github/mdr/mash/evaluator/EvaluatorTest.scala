package com.github.mdr.mash.evaluator

import scala.language.postfixOps

class EvaluatorTest extends AbstractEvaluatorTest {

  "{ foo: 42, bar: 100 } == { bar: 100, foo: 42 }" ==> true
  "class Box n; Box 42 == { n: 42 }" ==> false
  "class Box n; b1 = Box 42; class Box n; b2 = Box 42; b1 == b2" ==> false

  "(class Box n) == (class Box n)" ==> false
  "class Box n; Box == Box" ==> true

  "42 | _ < 100" ==> true

  "(x => x) | (f => f 42)" ==> "42"

  "if 1 < 2 then 100 else 200" ==> 100
  "if 1 > 2 then 100 else 200" ==> 200
  "if 1 == 1 then 100" ==> 100

  val Y = "(h => (x => x x) (g => h (args => (g g) args)))" // fixed point combinator
  s"($Y (f => n => if n == 1 then 1 else n * f (n - 1))) 5" ==> 120

  "[1, 2, 3, 4, 5] | filter (_ >= 3)" ==> "[3, 4, 5]"

  "{ foo: 42, bar: 24 } | [_.foo, _.bar]" ==> "[42, 24]"

  // Indexing
  "[1, 2, 3][0]" ==> 1
  "[1, 2, 3][-1]" ==> 3
  "[1, 2, 3] | _[0]" ==> 1
  " 'bar'[0] " ==> " 'b' "
  "{ foo: 42 }['foo'] " ==> 42

  // all
  "[2, 4, 6] | all (_ < 10)" ==> true
  "[2, 4, 6] | all (_ < 5)" ==> false
  "[] | all (_ < 10)" ==> true
  "[1] | all (_)" ==> true
  "[0] | all (_)" ==> false
  "'aaa' | all (_ == 'a')" ==> true
  "'aaa'.all (_ == 'a')" ==> true

  // any
  "[2, 4, 6] | any (_ > 10)" ==> false
  "[2, 4, 6] | any (_ > 5)" ==> true
  "[] | any (_ > 10)" ==> false
  "[1] | any (_)" ==> true
  "[0] | any (_)" ==> false
  "'abc' | any (_ == 'a')" ==> true
  "'abc'.any (_ == 'a')" ==> true

  // contains
  "[1, 2, 3].contains 2" ==> true
  "[1, 2, 3].contains 7" ==> false
  "[] | contains 42" ==> false
  "'abc' | contains 'b'" ==> true
  "'abc'.untagged | contains 'b'" ==> true
  "'abc' | contains 'b'.untagged" ==> true

  // count
  "[1, 2, 3, 4, 5].count" ==> 5
  "'abc'.count" ==> 3

  // countMatches
  "[1, 2, 300, 2, 400].countMatches (_ < 100)" ==> 3
  "'foo' | countMatches (_ == 'o')" ==> 2
  "'foo'.countMatches (_ == 'o')" ==> 2

  // deselect
  "{ foo: 42, bar: 1 } | deselect 'foo'" ==> "{ bar: 1 }"
  "[{ foo: 42, bar: 1 }, { foo: 12, bar: 34 }] | deselect 'foo'" ==> "[{ bar: 1 }, { bar: 34 }]"

  // each
  "a = 0; [1, 2, 3] | each (a = a + _); a" ==> 6
  "a = ''; 'foo' | each (a = _ + a); a" ==> "'oof'"
  "a = ''; 'foo'.each (a = _ + a); a" ==> "'oof'"

  // find
  "[1, 2, 300, 4, 5] | find (_ > 100)" ==> 300
  "[1, 2, 3, 4, 5] | find (_ > 100)" ==> null
  "'xxxaxxx' | find (_ != 'x')" ==> "'a'"
  "'xxxaxxx'.find (_ != 'x')" ==> "'a'"

  // first
  "first [0]" ==> 0
  "first [1, 2, 3]" ==> 1
  "first 2 [1, 2, 3]" ==> "[1, 2]"
  " first 'abc' " ==> " 'a' "
  " first 2 'abc' " ==> " 'ab' "
  " 'abc'.first " ==> " 'a' "
  " 'abc'.first 2" ==> " 'ab' "
  "first []" ==> null
  "first ''" ==> null

  // grep
  "['foo', 'bar', 'baz'].grep 'b'" ==> "['bar', 'baz']"
  "['foo', 'bar', 'baz'].grep -n 'b'" ==> "['foo']"
  "['apple', { obj: 'ball' }] | grep 'b'" ==> "[{ obj: 'ball' }]"
  "['foo', 'bar', 'BAZ'] | grep -i 'b'" ==> "['bar', 'BAZ']"
  "['apple', 'ball', 'cup'] | grep -r '(a|b)'" ==> "['apple', 'ball']"
  "['APPLE', 'ball', 'cup'] | grep -r -i '(a|B)'" ==> "['APPLE', 'ball']"
  "'foo`nbar`nbaz' | grep 'b'" ==> "['bar', 'baz']"
  "'foo`nbar`nbaz'.grep 'b'" ==> "['bar', 'baz']"

  // identity
  "identity 1" ==> 1

  // isEmpty
  "isEmpty []" ==> true
  "isEmpty [1, 2, 3]" ==> false
  "''.isEmpty" ==> true
  "'abc'.isEmpty" ==> false

  // isNull
  "isNull null" ==> true
  "isNull 0" ==> false

  // last
  "last [1, 2, 3]" ==> 3
  "last 2 [1, 2, 3]" ==> "[2, 3]"
  "last 'xyz'" ==> " 'z' "
  "last 2 'xyz'" ==> " 'yz' "
  "'xyz'.last" ==> " 'z' "
  "'xyz'.last 2" ==> " 'yz' "
  "last []" ==> null
  "last ''" ==> null

  // length
  "length []" ==> 0
  "length [1, 2, 3]" ==> 3
  " length '' " ==> 0
  " length 'abc' " ==> 3
  " ''.length " ==> 0
  " 'abc'.length " ==> 3
  " 'abc' | length " ==> 3

  // flatMap
  "[1, 2, 3].flatMap (n => [n * 10, n])" ==> "[10, 1, 20, 2, 30, 3]"
  "flatMap (n => [n * 10, n]) [1, 2, 3]" ==> "[10, 1, 20, 2, 30, 3]"
  "flatMap (_ + '!') 'abc'" ==> "'a!b!c!'"
  "flatMap (.toString) [1, 22, 333]" ==> "'122333'"
  "'abc' | flatMap (c => [c])" ==> "['a', 'b', 'c']"
  "'abc'.flatMap (c => [c]) " ==> "['a', 'b', 'c']"
  "flatMap (n i => [n, i]) [1, 2, 3]" ==> "[1, 0, 2, 1, 3, 2]"

  // flatten
  "flatten [[], [1], [2, 3]]" ==> "[1, 2, 3]"
  "flatten []" ==> "[]"
  "flatten ['', 'a', 'bc']" ==> "'abc'"

  // map
  "[1, 2, 3].map (_ * 2)" ==> "[2, 4, 6]"
  "map --f=(_ * 2) --sequence=[1, 2, 3]" ==> "[2, 4, 6]"
  "map (_ * 2) --sequence=[1, 2, 3]" ==> "[2, 4, 6]"
  "map --f=(_ * 2) [1, 2, 3]" ==> "[2, 4, 6]"
  "[1, 2, 3] | map --f=(_ * 2)" ==> "[2, 4, 6]"
  "'123' | map (_.toNumber)" ==> "[1, 2, 3]"
  "map (n i => n + i) [1, 2, 3]" ==> "[1, 3, 5]"
  "'foo' | map (.toUpper)" ==> "'FOO'"
  "'foo'.map (.toUpper)" ==> "'FOO'"

  // nonEmpty
  "nonEmpty []" ==> false
  "nonEmpty [1, 2, 3]" ==> true
  "''.nonEmpty" ==> false
  "'abc'.nonEmpty" ==> true

  // not
  "not true" ==> false
  "not false" ==> true
  "not 0" ==> true
  "not []" ==> true
  "not {}" ==> true
  "not 1" ==> false

  // reverse
  "reverse 'trebor'" ==> " 'robert' "
  "'trebor'.reverse" ==> " 'robert' "
  "[1, 2, 3].reverse" ==> "[3, 2, 1]"

  // skip
  "[1, 2, 3, 4, 5] | skip 2" ==> "[3, 4, 5]"
  "[1, 2] | skip 5" ==> "[]"
  "[1, 2] | skip" ==> "[2]"
  "'abcde' | skip 3" ==> "'de'"
  "'abcde' | skip" ==> "'bcde'"
  "'abcde'.skip 3" ==> "'de'"
  "'abcde'.skip" ==> "'bcde'"

  // skipWhile
  "[1, 2, 3, 4, 5, 1] | skipWhile (_ < 3)" ==> "[3, 4, 5, 1]"
  "'abcdef' | skipWhile (_ < 'd')" ==> "'def'"
  "'abcdef'.skipWhile (_ < 'd')" ==> "'def'"

  // skipUntil
  "[1, 2, 3, 4, 5, 1] | skipUntil (_ > 3)" ==> "[4, 5, 1]"
  "'abcdef' | skipUntil (_ > 'd')" ==> "'ef'"
  "'abcdef'.skipUntil (_ > 'd')" ==> "'ef'"

  // sliding
  "[1, 2, 3] | sliding 2" ==> "[[1, 2], [2, 3]]"
  "'abc' | sliding 2" ==> "['ab', 'bc']"
  "'abc'.sliding 2" ==> "['ab', 'bc']"

  // sum
  " [] | sum " ==> 0
  " [1, 2, 3] | sum " ==> 6
  " [1.bytes].sum.tag " ==> " 1.bytes.tag "
  "['foo', 'bar'] | sum" ==> "'foobar'"
  "[[1, 2], [3]] | sum" ==> "[1, 2, 3]"
  "sum '' []" ==> "''"

  // sumBy
  " ['a', 'bb', 'ccc'] | sumBy length " ==> 6
  " [1.bytes].sumBy (_) | _.tag " ==> " 1.bytes.tag "
  "sumBy (_.toNumber) '123'" ==> 6
  "sumBy (_.toNumber) '' []" ==> "''"

  // takeWhile
  "[1, 2, 3, 4, 3].takeWhile (_ <= 3)" ==> "[1, 2, 3]"
  "[1, 2, 3, 4, 3] | takeWhile (_ <= 3)" ==> "[1, 2, 3]"
  "'abcded'.takeWhile (_ <= 'c')" ==> "'abc'"

  // unique
  "unique [1, 2, 3, 2, 1]" ==> "[1, 2, 3]"
  "unique 'abcba'" ==> "'abc'"
  "'abcba'.unique" ==> "'abc'"

  // where
  "[1, 2, 3] | where (_ > 2)" ==> "[3]"
  "'foobar' | where (_ > 'm')" ==> "'oor'"
  "'foobar'.where (_ > 'm')" ==> "'oor'"

  // whereNot
  "[1, 2, 3] | whereNot (_ > 2)" ==> "[1, 2]"
  "'foobar' | whereNot (_ > 'm')" ==> "'fba'"
  "'foobar'.whereNot (_ > 'm')" ==> "'fba'"

  // zip
  "zip [1, 2, 3] [4, 5]" ==> "[[1, 4], [2, 5]]"

  // String.matches
  " 'foo'.matches 'o' " ==> true
  " 'foo'.matches 'x' " ==> false
  " 'foo'.matches '^fo?.$' " ==> true
  " 'foo 666 bar'.matches 666 " ==> true
  " 'bar'.matches 666 " ==> false

  // startsWith
  " 'foo'.startsWith 'f' " ==> true
  " 'foo'.startsWith 'o' " ==> false

  // .split
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

  // Member vectorisation
  "['foo', 'bar', 'baz'].startsWith 'ba'" ==> "[false, true, true]"
  "['foo'].noSuchMember".shouldThrowAnException
  "['foo bar', 'baz'].split" ==> "[['foo', 'bar'], ['baz']]"
  "['foo:bar', 'baz'].split ':'" ==> "[['foo', 'bar'], ['baz']]"
  "[].noSuchMember" ==> "[]"

  // Strings as functions
  " 'foo' { foo: 42 } " ==> 42
  " 'toString' 42 " ==> " '42' "
  " 'foo' [{ foo: 42 }] " ==> "[42]"

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

  // varargs
  "def mkList n... = n; mkList 1 2 3" ==> "[1, 2, 3]"
  "def mkList n... = n; mkList" ==> "[]"
  "def mkList a b c n... = n + [a, b, c]; mkList 1 2 3 4 5" ==> "[4, 5, 1, 2, 3]"
  "def mkList (n...) = n; mkList 1 2 3" ==> "[1, 2, 3]"

  "a = ['aa', 'bbb', 'c'].sortBy; a length" ==> "['c', 'aa', 'bbb']"

  "[].sumBy.target" ==> "[]"

  "now.date.toString" shouldNotThrowAnException

  "[].toString" ==> "'[]'"
  "[1].toString" ==> "'[1]'"
  "[1, 2].toString" ==> "'[1, 2]'"
  "[pwd].toString" ==> "'[' + pwd + ']'"

  "3.14.toInt" ==> 3
  "(-3.14).toInt" ==> -3
  "1485187640884.6155.toInt" ==> "1485187640884"

  """ "foo" { foo: 42 } { foo: 42 }  """ shouldThrowAnException

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

  // Holes in paren invocation args
  "[{foo: 42}].map(_.foo)" ==> "[42]"

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

  // lambdas with varargs
  "(x... => x.sum) 1 2 3" ==> 6

  // reduce
  "[1, 2, 3, 4, 5] | reduce (x y => x + y)" ==> 15
  "[1] | reduce (x y => x + y)" ==> 1
  "[1, 2, 3, 4, 5] | reduce (x y => x + y) 10" ==> 25
  "[] | reduce (x y => x + y) 10" ==> 10

  // holes
  "def baz = 128 + _; (baz) 72" ==> 200
  "def foo (x = _) = 42; foo" ==> 42
  "((x = _) => 42) | x => x" ==> 42

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

  "def foo (n = 0) = n * 2; foo -2" ==> -4 // <-- negative number literal confusion bug

  // scope
  "a = 0; while (a < 10) { a = a + 1 }; a" ==> 10
  "42 | a = _; a" ==> 42

  // type.hint
  "type.hint [String] 42" ==> 42

  "(def foo = 42) | x => x" ==> 42

  "(_ => b = 100) 42; b" ==> 100

  """{ a: 1, b: 2 } | select --a
  'b'""" ==> "'b'"

  """[] | sort -d
  100""" ==> 100

  """{ foo: { bar: 100 } }
      .foo
      .bar
  """ ==> 100

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

  "def mkList (xs...) = xs; mkList --xs=42" shouldThrowAnException

  "class A { def foo = [1, 2, 3] | where (_ > 1) }; A.new.foo" ==> "[2, 3]"

  // chunked
  "chunked 2 [1, 2, 3, 4, 5]" ==> "[[1, 2], [3, 4], [5]]"
  "chunked 1 [1, 2, 3]" ==> "[[1], [2], [3]]"
  "chunked 3 'foobar'" ==> "['foo', 'bar']"

  // Url.withQueryParams
  "net.url 'http://example.com' | .withQueryParams { param: 42 }" ==> "'http://example.com?param=42'"
  "net.url 'http://example.com' | .withQueryParams --param=42" ==> "'http://example.com?param=42'"

  // Url.host
  "net.url 'http://example.com' | .host" ==> "'example.com'"

}