package com.github.mdr.mash.evaluator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class EvaluatorTest extends AbstractEvaluatorTest {

  // Comparisons

  "1 < 2" shouldEvaluateTo true
  "1 < 1" shouldEvaluateTo false
  "1 < 0" shouldEvaluateTo false

  "1 <= 2" shouldEvaluateTo true
  "1 <= 1" shouldEvaluateTo true
  "1 <= 0" shouldEvaluateTo false

  "1 > 2" shouldEvaluateTo false
  "1 > 1" shouldEvaluateTo false
  "1 > 0" shouldEvaluateTo true

  "1 >= 2" shouldEvaluateTo false
  "1 >= 1" shouldEvaluateTo true
  "1 >= 0" shouldEvaluateTo true

  "[] < [1]" shouldEvaluateTo true
  "[1] < [1, 2]" shouldEvaluateTo true
  
  "1 == 1" shouldEvaluateTo true
  "1 == 2" shouldEvaluateTo false

  "1 != 1" shouldEvaluateTo false
  "1 != 2" shouldEvaluateTo true
  
  // Chained comparisons
  "1 < 2 < 3" shouldEvaluateTo true
  "1 <= 2 < 3" shouldEvaluateTo true
  "1 < 100 < 3" shouldEvaluateTo false
  "1 < 2 < -100" shouldEvaluateTo false
  "1 < -10 < 100" shouldEvaluateTo false
  "a = 0; -100 < (a = a + 1; a) < 100; a" shouldEvaluateTo 1
  "a = 0; (a = a + 1; 100) < (a = a + 1; -100) < (a = a + 1; -1000); a" shouldEvaluateTo 2 // short circuits the last

  // date/time comparisons
  "now > 3.days.ago" shouldEvaluateTo true
  "now < 3.days.ago" shouldEvaluateTo false
  "now.date > 3.days.ago.date" shouldEvaluateTo true
  "now.date < 3.days.ago.date" shouldEvaluateTo false

  // comparisons with null
  "null < 1" shouldEvaluateTo false
  "null <= 1" shouldEvaluateTo false
  "null > 1" shouldEvaluateTo false
  "null >= 1" shouldEvaluateTo false
  "1 < null" shouldEvaluateTo false
  "1 <= null" shouldEvaluateTo false
  "1 > null" shouldEvaluateTo false
  "1 >= null" shouldEvaluateTo false
  "null < null" shouldEvaluateTo false
  "null < null < null" shouldEvaluateTo false

  "true and true" shouldEvaluateTo true
  "true and false" shouldEvaluateTo false
  "false and true" shouldEvaluateTo false
  "false and false" shouldEvaluateTo false

  "true or true" shouldEvaluateTo true
  "true or false" shouldEvaluateTo true
  "false or true" shouldEvaluateTo true
  "false or false" shouldEvaluateTo false

  "true or null.bang" shouldEvaluateTo true
  "false and null.bang" shouldEvaluateTo false

  "{} or 42" shouldEvaluateTo "42"
  "{ a: 1 } or 42" shouldEvaluateTo "{ a: 1 }"

  "[] or 42" shouldEvaluateTo "42"
  "[1] or 42" shouldEvaluateTo "[1]"

  "0 or 42" shouldEvaluateTo "42"
  "1 or 42" shouldEvaluateTo "1"

  "null or 42" shouldEvaluateTo "42"

  "42 | _ < 100" shouldEvaluateTo true

  "(x => x) | (f => f 42)" shouldEvaluateTo "42"

  // Multiplication
  "'xy' * 3" shouldEvaluateTo "'xyxyxy'"
  "3 * 'xy'" shouldEvaluateTo "'xyxyxy'"
  "'x' * 1" shouldEvaluateTo "'x'"
  "'x' * 0" shouldEvaluateTo "''"

  "[1, 2] * 3" shouldEvaluateTo "[1, 2, 1, 2, 1, 2]"
  "3 * [1, 2]" shouldEvaluateTo "[1, 2, 1, 2, 1, 2]"
  "[1] * 1" shouldEvaluateTo "[1]"
  "[1] * 0" shouldEvaluateTo "[]"

  "3 * 1.second | .tag" shouldEvaluateTo "time.Seconds"
  "1.second * 3 | .tag" shouldEvaluateTo "time.Seconds"

  "1 * 2 + 3 * 4" shouldEvaluateTo 14
  "3 * 100 / 3" shouldEvaluateTo 100
  "10 / 10 / 10" shouldEvaluateTo 0.1
  "1 - 2 - 3" shouldEvaluateTo -4

  "if 1 < 2 then 100 else 200" shouldEvaluateTo 100
  "if 1 > 2 then 100 else 200" shouldEvaluateTo 200
  "if 1 == 1 then 100" shouldEvaluateTo 100

  val Y = "(h => (x => x x) (g => h (args => (g g) args)))" // fixed point combinator
  s"($Y (f => n => if n == 1 then 1 else n * f (n - 1))) 5" shouldEvaluateTo 120

  "[1, 2, 3, 4, 5] | filter (_ >= 3)" shouldEvaluateTo "[3, 4, 5]"

  "{ foo: 42, bar: 24 } | [_.foo, _.bar]" shouldEvaluateTo "[42, 24]"

  "now > 1.day.ago" shouldEvaluateTo true

  // null
  "null.in [1, 2, 3]" shouldEvaluateTo false
  "null.in [null]" shouldEvaluateTo true

  // null-safe dereferencing
  "null?.toString" shouldEvaluateTo null
  "[null, { foo: 42 }] | map (_?.foo)" shouldEvaluateTo "[null, 42]"
  "[ { foo: { bar: 42 } }, { foo: null } ].foo?.bar" shouldEvaluateTo "[42, null]"

  // Indexing
  "[1, 2, 3][0]" shouldEvaluateTo 1
  "[1, 2, 3][-1]" shouldEvaluateTo 3
  "[1, 2, 3] | _[0]" shouldEvaluateTo 1
  " 'bar'[0] " shouldEvaluateTo " 'b' "
  "{ foo: 42 }['foo'] " shouldEvaluateTo 42

  // all
  "[2, 4, 6] | all (_ < 10)" shouldEvaluateTo true
  "[2, 4, 6] | all (_ < 5)" shouldEvaluateTo false
  "[] | all (_ < 10)" shouldEvaluateTo true
  "[1] | all (_)" shouldEvaluateTo true
  "[0] | all (_)" shouldEvaluateTo false
  "'aaa' | all (_ == 'a')" shouldEvaluateTo true

  // any
  "[2, 4, 6] | any (_ > 10)" shouldEvaluateTo false
  "[2, 4, 6] | any (_ > 5)" shouldEvaluateTo true
  "[] | any (_ > 10)" shouldEvaluateTo false
  "[1] | any (_)" shouldEvaluateTo true
  "[0] | any (_)" shouldEvaluateTo false
  "'abc' | any (_ == 'a')" shouldEvaluateTo true

  // contains
  "[1, 2, 3].contains 2" shouldEvaluateTo true
  "[1, 2, 3].contains 7" shouldEvaluateTo false
  "[] | contains 42" shouldEvaluateTo false
  "'abc' | contains 'b'" shouldEvaluateTo true
  "'abc'.untagged | contains 'b'" shouldEvaluateTo true
  "'abc' | contains 'b'.untagged" shouldEvaluateTo true

  // count 
  "[1, 2, 3, 4, 5].count" shouldEvaluateTo 5

  // countMatches 
  "[1, 2, 300, 2, 400].countMatches (_ < 100)" shouldEvaluateTo 3
  "'foo' | countMatches (_ == 'o')" shouldEvaluateTo 2

  // deselect
  "{ foo: 42, bar: 1 } | deselect 'foo'" shouldEvaluateTo "{ bar: 1 }"
  "[{ foo: 42, bar: 1 }, { foo: 12, bar: 34 }] | deselect 'foo'" shouldEvaluateTo "[{ bar: 1 }, { bar: 34 }]"

  // each
  "a = 0; [1, 2, 3] | each (a = a + _); a" shouldEvaluateTo 6
  "a = ''; 'foo' | each (a = _ + a); a" shouldEvaluateTo "'oof'"

  // find
  "[1, 2, 300, 4, 5] | find (_ > 100)" shouldEvaluateTo 300
  "[1, 2, 3, 4, 5] | find (_ > 100)" shouldEvaluateTo null
  "'xxxaxxx' | find (_ != 'x')" shouldEvaluateTo "'a'"

  // first
  "first [0]" shouldEvaluateTo 0
  "first [1, 2, 3]" shouldEvaluateTo 1
  "first 2 [1, 2, 3]" shouldEvaluateTo "[1, 2]"
  " first 'abc' " shouldEvaluateTo " 'a' "
  " first 2 'abc' " shouldEvaluateTo " 'ab' "
  " 'abc'.first " shouldEvaluateTo " 'a' "
  " 'abc'.first 2" shouldEvaluateTo " 'ab' "
  "first []" shouldEvaluateTo null
  "first ''" shouldEvaluateTo null

  // grep
  "['foo', 'bar', 'baz'].grep 'b'" shouldEvaluateTo "['bar', 'baz']"
  "['apple', { obj: 'ball' }] | grep 'b'" shouldEvaluateTo "[{ obj: 'ball' }]"
  "['foo', 'bar', 'BAZ'] | grep -i 'b'" shouldEvaluateTo "['bar', 'BAZ']"

  // groupBy 
  "[1, 2, 3, 1] | groupBy (x => x) | select 'key' 'count' | sortBy 'key'" shouldEvaluateTo
    "[ { key: 1, count: 2 }, { key: 2, count: 1 }, { key: 3, count: 1 } ] "
  "'foo' | groupBy (x => x) | select 'key' 'count' | sortBy 'key'" shouldEvaluateTo
    "[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "

  "[null] | groupBy --includeNull (x => x) | select 'key' 'count'" shouldEvaluateTo
    "[ { key: null, count: 1 } ]"
  "[null] | groupBy --includeNull='nope' (x => x) | select 'key'" shouldEvaluateTo
    "[ { key: 'nope' } ]"

  "[1, 2, 1] | groupBy --total (x => x) | select 'key' 'count' | sortBy 'count'" shouldEvaluateTo
    "[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'Total', count: 3 } ]"
  "[1, 2, 1] | groupBy --total='totalCount' (x => x) | select 'key' 'count' | sortBy 'count'" shouldEvaluateTo
    "[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'totalCount', count: 3 } ]"

  // identity
  "identity 1" shouldEvaluateTo 1

  // isEmpty
  "isEmpty []" shouldEvaluateTo true
  "isEmpty [1, 2, 3]" shouldEvaluateTo false
  "''.isEmpty" shouldEvaluateTo true
  "'abc'.isEmpty" shouldEvaluateTo false

  // isNull
  "isNull null" shouldEvaluateTo true
  "isNull 0" shouldEvaluateTo false

  // join
  " [1, 2, 3].join(', ') " shouldEvaluateTo " '1, 2, 3' "
  " [1, 2, 3].join " shouldEvaluateTo " '123' "
  " join ', ' [1, 2, 3] " shouldEvaluateTo " '1, 2, 3' "
  " join [1, 2, 3] " shouldEvaluateTo " '123' "
  " join --sequence=[1, 2, 3] " shouldEvaluateTo " '123' "
  " join ', ' --sequence=[1, 2, 3] " shouldEvaluateTo " '1, 2, 3' "
  " join --separator=', ' --sequence=[1, 2, 3] " shouldEvaluateTo " '1, 2, 3' "
  " join --separator=', ' [1, 2, 3] " shouldEvaluateTo " '1, 2, 3' "
  " join [] " shouldEvaluateTo " '' "
  " join 'abc' " shouldEvaluateTo " 'abc' "
  " join ':' 'abc' " shouldEvaluateTo " 'a:b:c' "

  // last
  "last [1, 2, 3]" shouldEvaluateTo 3
  "last 2 [1, 2, 3]" shouldEvaluateTo "[2, 3]"
  " last 'xyz' " shouldEvaluateTo " 'z' "
  " last 2 'xyz' " shouldEvaluateTo " 'yz' "
  " 'xyz'.last " shouldEvaluateTo " 'z' "
  " 'xyz'.last 2 " shouldEvaluateTo " 'yz' "
  "last []" shouldEvaluateTo null
  "last ''" shouldEvaluateTo null

  // length
  "length []" shouldEvaluateTo 0
  "length [1, 2, 3]" shouldEvaluateTo 3
  " length '' " shouldEvaluateTo 0
  " length 'abc' " shouldEvaluateTo 3
  " ''.length " shouldEvaluateTo 0
  " 'abc'.length " shouldEvaluateTo 3
  " 'abc' | length " shouldEvaluateTo 3

  // flatMap
  "[1, 2, 3].flatMap (n => [n * 10, n])" shouldEvaluateTo "[10, 1, 20, 2, 30, 3]"
  "flatMap (n => [n * 10, n]) [1, 2, 3]" shouldEvaluateTo "[10, 1, 20, 2, 30, 3]"
  "flatMap --withIndex (n i => [n, i]) [1, 2, 3]" shouldEvaluateTo "[1, 0, 2, 1, 3, 2]"

  // map
  "[1, 2, 3].map (_ * 2)" shouldEvaluateTo "[2, 4, 6]"
  "map --f=(_ * 2) --sequence=[1, 2, 3]" shouldEvaluateTo "[2, 4, 6]"
  "map (_ * 2) --sequence=[1, 2, 3]" shouldEvaluateTo "[2, 4, 6]"
  "map --f=(_ * 2) [1, 2, 3]" shouldEvaluateTo "[2, 4, 6]"
  "[1, 2, 3] | map --f=(_ * 2)" shouldEvaluateTo "[2, 4, 6]"
  "map (_.toUpper) 'foo'" shouldEvaluateTo "'FOO'"
  "'123' | map (_.toNumber)" shouldEvaluateTo "[1, 2, 3]"
  "map --withIndex (n i => n + i) [1, 2, 3]" shouldEvaluateTo "[1, 3, 5]"

  // max
  "max [1, 200, 3]" shouldEvaluateTo 200
  "max 'abc'" shouldEvaluateTo "'c'"
  "max 1 2 3" shouldEvaluateTo 3
  "max [2, null, 1]" shouldEvaluateTo 2

  // maxBy
  "maxBy length [ 'a', 'bbb', 'cc'] " shouldEvaluateTo " 'bbb' "
  "maxBy (_) 'abcde'" shouldEvaluateTo "'e'"
  "maxBy 'foo' [{ foo: null }, { foo: 42 }]" shouldEvaluateTo "{ foo: 42 }"

  // min
  "min [100, 2, 300]" shouldEvaluateTo 2
  "min 'abc'" shouldEvaluateTo "'a'"
  "min 1 2 3" shouldEvaluateTo 1
  "min [2, null, 1]" shouldEvaluateTo 1

  // minBy
  "minBy length [ 'a', 'bbb', 'cc'] " shouldEvaluateTo " 'a' "
  "minBy 'foo' [{ foo: null }, { foo: 42 }]" shouldEvaluateTo "{ foo: 42 }"

  // nonEmpty
  "nonEmpty []" shouldEvaluateTo false
  "nonEmpty [1, 2, 3]" shouldEvaluateTo true
  "''.nonEmpty" shouldEvaluateTo false
  "'abc'.nonEmpty" shouldEvaluateTo true

  // not
  "not true" shouldEvaluateTo false
  "not false" shouldEvaluateTo true
  "not 0" shouldEvaluateTo true
  "not []" shouldEvaluateTo true
  "not {}" shouldEvaluateTo true
  "not 1" shouldEvaluateTo false

  // reverse
  " reverse 'trebor' " shouldEvaluateTo " 'robert' "
  " 'trebor'.reverse " shouldEvaluateTo " 'robert' "
  "[1, 2, 3].reverse" shouldEvaluateTo "[3, 2, 1]"

  // select
  " { foo: 42, bar: 24 } | select 'foo' " shouldEvaluateTo " { foo: 42 } "
  " { foo: 42, bar: 24 } | select --foo " shouldEvaluateTo " { foo: 42 } "
  " { foo: 42, bar: 24 } | select --foo=(_.foo) " shouldEvaluateTo " { foo: 42 } "
  " { foo: 42, bar: 24 } | select 'foo' --baz=(_.foo) 'bar' " shouldEvaluateTo " { foo: 42, baz: 42, bar: 24 } "
  " [{ foo: 42, bar: 24 }] | select 'foo' " shouldEvaluateTo " [{ foo: 42 }] "
  " { foo: 42 } | select --add --bar=(_.foo) " shouldEvaluateTo " { foo: 42, bar: 42 } "
  " { foo: 42, bar: 24 } | select 'foo' --bar " shouldEvaluateTo " { foo: 42, bar: 24  } "
  " select 'foo' --target=[{ foo: 42 }]" shouldEvaluateTo " [{ foo: 42 }] "
  " select 'foo' --target={ foo: 42 }" shouldEvaluateTo " { foo: 42 } "
  " select --foo --target={ foo: 42 }" shouldEvaluateTo " { foo: 42 } "
  " select --add --bar=(_.foo) --target={ foo: 42 }" shouldEvaluateTo " { foo: 42, bar: 42 } "
  " select -a --bar=(_.foo) --target={ foo: 42 }" shouldEvaluateTo " { foo: 42, bar: 42 } "

  // skip
  "[1, 2, 3, 4, 5] | skip 2" shouldEvaluateTo "[3, 4, 5]"
  "[1, 2] | skip 5" shouldEvaluateTo "[]"
  "[1, 2] | skip" shouldEvaluateTo "[2]"
  "'abcde' | skip 3" shouldEvaluateTo "'de'"
  "'abcde' | skip" shouldEvaluateTo "'bcde'"

  // skipWhile
  "[1, 2, 3, 4, 5, 1] | skipWhile (_ < 3)" shouldEvaluateTo "[3, 4, 5, 1]"
  "'abcdef' | skipWhile (_ < 'd')" shouldEvaluateTo "'def'"

  // skipUntil
  "[1, 2, 3, 4, 5, 1] | skipUntil (_ > 3)" shouldEvaluateTo "[4, 5, 1]"
  "'abcdef' | skipUntil (_ > 'd')" shouldEvaluateTo "'ef'"

  // sliding
  "[1, 2, 3] | sliding 2" shouldEvaluateTo "[[1, 2], [2, 3]]"
  "'abc' | sliding 2" shouldEvaluateTo "['ab', 'bc']"

  // sort
  " ['c', 'a', 'b'].sort " shouldEvaluateTo " ['a', 'b', 'c'] "
  "'eaebcd' | sort" shouldEvaluateTo "'abcdee'"
  "[1, null, 2].sort" shouldEvaluateTo "[null, 1, 2]  "
  // Bug where we used lte rather than lt to sort with:
  "sort [15, 12, 9, 3, 2, 90, 75, 7, 18, 9, 2, 1, 1, 14, 3, 3, 2, 21, 53, 2, 61, 24, 31, 1, 13, 14, 21, 4, 28, 17, 2, 5, 1, 17, 3, 3, 10, 100, 246, 176, 2, 10, 2, 4, 1, 1, 2, 1, 261, 1, 27, 10, 3, 6, 390, 44, 1, 2, 4, 1, 13, 4, 6, 1, 2, 8, 9, 3, 33, 9, 3, 131, 10, 2, 15, 35, 2, 157, 71, 32, 4, 12, 6, 7, 3, 8, 43, 8, 35, 1, 1, 11, 4, 2, 1, 9, 1]" shouldNotThrowAnException

  // sortBy
  " ['aa', 'b', 'ccc'] | sortBy length " shouldEvaluateTo " ['b', 'aa', 'ccc'] "
  "'123' | sortBy (-_.toNumber)" shouldEvaluateTo "'321'"
  "[{ foo: 1 }, { foo: null }, { foo: 2 }].sortBy 'foo'" shouldEvaluateTo "[{ foo: null }, { foo: 1 }, { foo: 2 }]"

  // sum
  " [] | sum " shouldEvaluateTo 0
  " [1, 2, 3] | sum " shouldEvaluateTo 6
  " [1.bytes].sum.tag " shouldEvaluateTo " 1.bytes.tag "
  "['foo', 'bar'] | sum" shouldEvaluateTo "'foobar'"
  "[[1, 2], [3]] | sum" shouldEvaluateTo "[1, 2, 3]"
  "sum '' []" shouldEvaluateTo "''"

  // sumBy
  " ['a', 'bb', 'ccc'] | sumBy length " shouldEvaluateTo 6
  " [1.bytes].sumBy (_) | _.tag " shouldEvaluateTo " 1.bytes.tag "
  "sumBy (_.toNumber) '123'" shouldEvaluateTo 6
  "sumBy (_.toNumber) '' []" shouldEvaluateTo "''"

  // unique
  "unique [1, 2, 3, 2, 1]" shouldEvaluateTo "[1, 2, 3]"
  "unique 'abcba'" shouldEvaluateTo "'abc'"

  // where
  "[1, 2, 3] | where (_ > 2)" shouldEvaluateTo "[3]"
  "'foobar' | where (_ > 'm')" shouldEvaluateTo "'oor'"

  // whereNot
  "[1, 2, 3] | whereNot (_ > 2)" shouldEvaluateTo "[1, 2]"
  "'foobar' | whereNot (_ > 'm')" shouldEvaluateTo "'fba'"

  // matches
  " 'foo'.matches 'o' " shouldEvaluateTo true
  " 'foo'.matches 'x' " shouldEvaluateTo false
  " 'foo'.matches '^fo?.$' " shouldEvaluateTo true
  " 'foo 666 bar'.matches 666 " shouldEvaluateTo true
  " 'bar'.matches 666 " shouldEvaluateTo false

  // startsWith
  " 'foo'.startsWith 'f' " shouldEvaluateTo true
  " 'foo'.startsWith 'o' " shouldEvaluateTo false

  // in
  "42.in [1, 2, 3]" shouldEvaluateTo false
  "2.in [1, 2, 3]" shouldEvaluateTo true

  // toString
  "null.toString" shouldEvaluateTo " 'null' "
  "2.toString" shouldEvaluateTo " '2' "
  "().toString" shouldEvaluateTo "''"
  "true.toString" shouldEvaluateTo " 'true' "
  "[1, 2, 3].toString" shouldEvaluateTo "'[1, 2, 3]'"
  """ "foo".toString.tag """ shouldEvaluateTo """ "foo".tag """
  "{ foo: 42 }" shouldEvaluateTo "{ foo: 42 }"

  // split
  "'foo bar baz'.split" shouldEvaluateTo "['foo', 'bar', 'baz']"
  "'foo  bar     baz'.split" shouldEvaluateTo "['foo', 'bar', 'baz']"
  "'foo:bar:baz'.split ':'" shouldEvaluateTo "['foo', 'bar', 'baz']"
  "'foo:bar:'.split ':'" shouldEvaluateTo "['foo', 'bar', '']"

  // String concat
  " 'foo' + 42 " shouldEvaluateTo " 'foo42' "
  " 42 + 'foo' " shouldEvaluateTo " '42foo' "

  // List concat
  "[1, 2, 3] + [4, 5]" shouldEvaluateTo "[1, 2, 3, 4, 5]"

  // Member vectorisation
  "['foo', 'bar', 'baz'].startsWith 'ba'" shouldEvaluateTo "[false, true, true]"
  "['foo'].noSuchMember".shouldThrowAnException
  "['foo bar', 'baz'].split" shouldEvaluateTo "[['foo', 'bar'], ['baz']]"
  "['foo:bar', 'baz'].split ':'" shouldEvaluateTo "[['foo', 'bar'], ['baz']]"
  "[].noSuchMember" shouldEvaluateTo "[]"

  // Strings as functions
  " 'foo' { foo: 42 } " shouldEvaluateTo 42
  " 'toString' 42 " shouldEvaluateTo " '42' "
  " 'foo' [{ foo: 42 }] " shouldEvaluateTo "[42]"

  // String interpolation
  """(name => "Hello $name!") "Matt" """ shouldEvaluateTo "'Hello Matt!'"
  """(name => "Hello ${name}!") "Matt" """ shouldEvaluateTo "'Hello Matt!'"
  """(name => "Hello $name.reverse!") "Matt" """ shouldEvaluateTo "'Hello ttaM!'"
  """(name => "Hello ${name.reverse}!") "Matt" """ shouldEvaluateTo "'Hello ttaM!'"
  """(name => "Hello ${name | reverse}!") "Matt" """ shouldEvaluateTo "'Hello ttaM!'"
  """ 42 | "$_" """ shouldEvaluateTo "'42'"
  """ "\"${42}\"" """ shouldEvaluateTo """ '"42"' """
  """ "'${42}'" """ shouldEvaluateTo """ "'42'" """
  """ "${4}'bar'${2}" """ shouldEvaluateTo """ "4'bar'2" """

  // String escapes
  """ "\$" """ shouldEvaluateTo " '$' "
  """ "\"" """ shouldEvaluateTo """ '"' """
  """ '\'' """ shouldEvaluateTo """ "'" """

  // String literals
  "\"foo\nbar\".length" shouldEvaluateTo 7

  "true and true and true" shouldEvaluateTo true
  "true or true or true" shouldEvaluateTo true
  "1; 2; 3" shouldEvaluateTo 3
  "a = 1; a = 2; a" shouldEvaluateTo 2
  "if true then if false then 24 else 42 else 1024" shouldEvaluateTo 42

  "(if true then a = 42 else a = 24); a" shouldEvaluateTo 42

  // Parsing checks: assignment versus lambdas versus pipes
  "a = 0; (100 | (a = _); a)" shouldEvaluateTo 100
  "(a = true | not); a" shouldEvaluateTo false
  "square = x => x * x; square 3" shouldEvaluateTo 9
  "a = 0; (x => a = x) 42; a" shouldEvaluateTo 42
  "a = 0; ((100 | a = _); a)" shouldEvaluateTo 100
  "(a = x => true | not); a 42" shouldEvaluateTo false
  "(a = 42 | x => true); a" shouldEvaluateTo true

  // mapping things treats them as functions
  "['foo', 'bar', 'baz'] | map reverse" shouldEvaluateTo "['oof', 'rab', 'zab']"
  "['foo', 'bar', 'baz'] | map 'reverse'" shouldEvaluateTo "['oof', 'rab', 'zab']"
  "[{foo: 42}] | map 'foo'" shouldEvaluateTo "[42]"
  "['f', 'x'] | map 'foo'.startsWith" shouldEvaluateTo "[true, false]"

  // help

  "listFiles? .name" shouldEvaluateTo "'listFiles'"
  "help groupBy" shouldEvaluateTo "groupBy?"
  "man groupBy" shouldEvaluateTo "groupBy?"
  "groupBy.help" shouldEvaluateTo "groupBy?"

  "[1, 2, 3].reverse? .name" shouldEvaluateTo "'reverse'"
  "[1, 2, 3].sortBy? .name" shouldEvaluateTo "'sortBy'"
  "help [1,2, 3].sortBy" shouldEvaluateTo "[1, 2, 3].sortBy?"
  "[1, 2, 3].sortBy.help" shouldEvaluateTo "[1, 2, 3].sortBy?"

  "pwd.info.permissions? .name" shouldEvaluateTo "'permissions'"
  "[pwd].info.permissions? .name" shouldEvaluateTo "'permissions'"

  "git.log? .name" shouldEvaluateTo "'log'"
  "help 42.class | _.name" shouldEvaluateTo "'Number'"

  "def square n = n * n; square 8" shouldEvaluateTo 64
  "def square n = n * n; square --n=8" shouldEvaluateTo 64
  "def add a b = a + b; add 1 2" shouldEvaluateTo 3
  "def pipeFunc s = s.toUpper | reverse; pipeFunc 'foo'" shouldEvaluateTo "'OOF'"

  // varargs
  "def mkList n... = n; mkList 1 2 3" shouldEvaluateTo "[1, 2, 3]"
  "def mkList n... = n; mkList" shouldEvaluateTo "[]"
  "def mkList a b c n... = n + [a, b, c]; mkList 1 2 3 4 5" shouldEvaluateTo "[4, 5, 1, 2, 3]"

  "a = alias ['aa', 'bbb', 'c'].sortBy; a length" shouldEvaluateTo "['c', 'aa', 'bbb']"

  "[].sumBy.target" shouldEvaluateTo "[]"

  "now.date.toString" shouldNotThrowAnException

  "[].toString" shouldEvaluateTo "'[]'"
  "[1].toString" shouldEvaluateTo "'[1]'"
  "[1, 2].toString" shouldEvaluateTo "'[1, 2]'"
  "[pwd].toString" shouldEvaluateTo "'[' + pwd + ']'"

  "3.14.toInt" shouldEvaluateTo 3
  "(-3.14).toInt" shouldEvaluateTo -3

  """ "foo" { foo: 42 } { foo: 42 }  """ shouldThrowAnException

  // headless members
  "{ foo: 42 } | .foo" shouldEvaluateTo 42
  "[1, 2, 3] | .last 2" shouldEvaluateTo "[2, 3]"
  "[{ foo: 42 }] | .foo" shouldEvaluateTo "[42]"
  "null | ?.foo" shouldEvaluateTo null

  // regex
  "'(.*)bar'.r.match 'wibblebar' | .groups.first" shouldEvaluateTo "'wibble'"

  "a = now; b = 3.days.ago; min a b" shouldEvaluateTo "b"

  // Block expressions
  "{ a = 0; a = a + 1; a }" shouldEvaluateTo 1
  "{ a = 42 }; a" shouldThrowAnException

  // Holes in paren invocation args
  "[{foo: 42}].map(_.foo)" shouldEvaluateTo "[42]"

  // .fields
  "{ foo: 42 }.fields.name" shouldEvaluateTo "['foo']"
  "42.fields" shouldThrowAnException

  // object addition
  "{ foo: 42 } + { bar: 100 }" shouldEvaluateTo "{ foo: 42, bar: 100 }"
  "{ foo: 42 } + { }" shouldEvaluateTo "{ foo: 42 }"
  "{ foo: 42 } + { foo: 100 }" shouldEvaluateTo "{ foo: 100 }"

  // object field subtraction
  "{ foo: 42, bar: 100 } - 'foo'" shouldEvaluateTo "{ bar: 100 }"
  "{ foo: 42 } - 'bar'" shouldEvaluateTo "{ foo: 42 }"

  // assignment 
  "a = 42; a" shouldEvaluateTo 42
  "a = {}; a['foo'] = 42; a.foo" shouldEvaluateTo 42
  "a = [1, 2, 3]; a[1] = 42; a" shouldEvaluateTo "[1, 42, 3]"

  "a = 0; a += 42; a" shouldEvaluateTo 42
  "a = 42; a -= 42; a" shouldEvaluateTo 0
  "a = 3; a *= 4; a" shouldEvaluateTo 12
  "a = 15; a /= 5; a" shouldEvaluateTo 3
  "a = { foo: 0 }; a.foo += 42; a" shouldEvaluateTo "{ foo: 42 }"
  "a = [1, 0, 3]; a[1] += 42; a" shouldEvaluateTo "[1, 42, 3]"

  "a = 42" shouldEvaluateTo 42
  "a = 5; a += 10" shouldEvaluateTo 15
  
  { // bare words
    implicit val config = Config(bareWords = true)

    "foo" shouldEvaluateTo "'foo'"

    // Was a bug here, where foo was incorrectly identified as a bare word
    "def foo x = if x == 0 then 1 else foo (x - 1); foo 5" shouldEvaluateTo 1

    "a = 10; a" shouldEvaluateTo 10
  }

  // Object.hasField
  "{ foo: 42 }.hasField 'foo'" shouldEvaluateTo true
  "{ foo: 42 }.hasField 'bar'" shouldEvaluateTo false

  // Any.isA
  "42.isA Number" shouldEvaluateTo true
  "42.isA String" shouldEvaluateTo false
  "3.days.isA 3.days.tag" shouldEvaluateTo true

  // Lambdas with multiple parameters
  "(x y => x * y) 2 3" shouldEvaluateTo 6
  "f = => 42; f" shouldEvaluateTo 42

  // lambdas with varargs
  "(x... => x.sum) 1 2 3" shouldEvaluateTo 6

  // reduce
  "[1, 2, 3, 4, 5] | reduce (x y => x + y)" shouldEvaluateTo 15
  "[1] | reduce (x y => x + y)" shouldEvaluateTo 1
  "[1, 2, 3, 4, 5] | reduce (x y => x + y) 10" shouldEvaluateTo 25
  "[] | reduce (x y => x + y) 10" shouldEvaluateTo 10

  // Object.withField
  "{}.withField 'foo' 42" shouldEvaluateTo "{ foo: 42 }"
  "{ foo: 42 }.withField 'bar' 256" shouldEvaluateTo "{ foo: 42, bar: 256 }"
  "{ foo: 42 }.withField 'foo' 256" shouldEvaluateTo "{ foo: 256 }"

  // Object.get
  "{ foo: 42 }.get 'foo'" shouldEvaluateTo 42
  "{}.get 'nope'" shouldEvaluateTo null
  "{}.get 'nope' --default=42" shouldEvaluateTo 42

  // Object literals
  "{ 'foo': 42 }.foo" shouldEvaluateTo 42
  
  // Semicolon inference
  """|a = 1
     |b = a + 1
     |a + b""" shouldEvaluateTo 3

  """|a = 1
     |b = a +
     |  1
     |a + b""" shouldEvaluateTo 3

  // Default arguments
  "def foo (x = 42) = x + 1; foo" shouldEvaluateTo 43
  "def foo (x = 42) = x + 1; foo 100" shouldEvaluateTo 101

  // holes
  "def baz = 128 + _; (baz) 72" shouldEvaluateTo 200
  "def foo (x = _) = 42; foo" shouldEvaluateTo 42
  "((x = _) => 42) | x => x" shouldEvaluateTo 42

  // Number.to
  "1.to 5" shouldEvaluateTo "[1, 2, 3, 4, 5]"
  "1.to 10 --step=2" shouldEvaluateTo "[1, 3, 5, 7, 9]"
  "5.to 1 --step=-1" shouldEvaluateTo "[5, 4, 3, 2, 1]"

  // Number.until
  "1.until 5" shouldEvaluateTo "[1, 2, 3, 4]"
  "1.until 10 --step=2" shouldEvaluateTo "[1, 3, 5, 7, 9]"
  "5.until 1 --step=-1" shouldEvaluateTo "[5, 4, 3, 2]"
  
  // Lambdas inside parens and blocks
  "a = [0]; { x => a[0] += x; a[0] += x } 21; a[0]" shouldEvaluateTo 42
  "a = 0; (x => a += x; a += x) 21; a" shouldEvaluateTo 42

  // Number.times
  "a = 0; 5.times (a += 1); a" shouldEvaluateTo 5
  "a = 0; (a += 1) | 5.times" shouldEvaluateTo "[1, 2, 3, 4, 5]"
  
  // Lazy arguments
  "a = 0; def twice (lazy block) = { block; block }; twice (a += 1); a" shouldEvaluateTo 2
  "a = 0; def twice (lazy block) = { block; block }; twice --block=(a += 1); a" shouldEvaluateTo 2
  "a = 0; ((lazy block) => { block; block }) (a += 1); a" shouldEvaluateTo 2

  "def foo n = n += _; f = foo 1; f 2; f 3" shouldEvaluateTo 6
 
  // Object.hoist
  "{ foo: 42, bar: { baz1: 100, baz2: 200 } }.hoist 'bar'" shouldEvaluateTo "{ foo: 42, baz1: 100, baz2: 200 }"
  "{ foo: 42, bar: [{ baz1: 100, baz2: 200 }, { baz1: 300, baz2: 400 }] }.hoist 'bar'" shouldEvaluateTo
    "[{ foo: 42, baz1: 100, baz2: 200 }, { foo: 42, baz1: 300, baz2: 400 }]"
  "{ foo: 42, bar: { baz1: 100, baz2: 200 } }.hoist 'bar' --prefix='bar_'" shouldEvaluateTo "{ foo: 42, bar_baz1: 100, bar_baz2: 200 }"

  // Patterns in parameters
  "(_ => 42) 10" shouldEvaluateTo 42
 
  // maths.stats
  "[1, 2, 3] | maths.stats | .mean" shouldEvaluateTo 2
  
  // maths.log
  "maths.log 1000 --base=10" shouldEvaluateTo 3
  
  // tap
  "x = 0; 42 | tap (x = _) | [x, _]" shouldEvaluateTo "[42, 42]"

  "def foo (n = 0) = n * 2; foo -2" shouldEvaluateTo -4 // <-- negative number literal confusion bug

  // scope
  "a = 0; while (a < 10) { a = a + 1 }; a" shouldEvaluateTo 10

  // Patterns
  "{ foo: 42, bar: 128 } | { foo } => foo + 1" shouldEvaluateTo 43
  "{ foo: 42, bar: 128 } | { wibble } => wibble" shouldEvaluateTo null
  "{ foo: 42, bar: 128 } | { foo, bar } => foo + bar" shouldEvaluateTo 170
  "{ foo, bar } = { bar: 1, baz: 2, foo: 3 }; foo + bar" shouldEvaluateTo 4
  "{ baz } = { foo: 42 }; baz" shouldEvaluateTo null
  "{ baz } = 42" shouldThrowAnException

  // Object.merge
  "Object.merge []" shouldEvaluateTo "{}"
  "Object.merge { foo: 42 }" shouldEvaluateTo "{ foo: 42 }"
  "Object.merge { foo: 42 } { bar: 128 }" shouldEvaluateTo "{ foo: 42, bar: 128 }"
  "Object.merge { foo: 42 } { foo: 128 }" shouldEvaluateTo "{ foo: 128 }"
}