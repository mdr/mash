package com.github.mdr.mash.evaluator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class EvaluatorTest extends AbstractEvaluatorTest {

  // Comparisons

  "1 < 2" ==> true
  "1 < 1" ==> false
  "1 < 0" ==> false

  "1 <= 2" ==> true
  "1 <= 1" ==> true
  "1 <= 0" ==> false

  "1 > 2" ==> false
  "1 > 1" ==> false
  "1 > 0" ==> true

  "1 >= 2" ==> false
  "1 >= 1" ==> true
  "1 >= 0" ==> true

  "[] < [1]" ==> true
  "[1] < [1, 2]" ==> true

  "1 == 1" ==> true
  "1 == 2" ==> false

  "1 != 1" ==> false
  "1 != 2" ==> true

  // Chained comparisons
  "1 < 2 < 3" ==> true
  "1 <= 2 < 3" ==> true
  "1 < 100 < 3" ==> false
  "1 < 2 < -100" ==> false
  "1 < -10 < 100" ==> false
  "a = 0; -100 < (a = a + 1; a) < 100; a" ==> 1
  "a = 0; (a = a + 1; 100) < (a = a + 1; -100) < (a = a + 1; -1000); a" ==> 2 // short circuits the last

  // date/time comparisons
  "now > 3.days.ago" ==> true
  "now < 3.days.ago" ==> false
  "now.date > 3.days.ago.date" ==> true
  "now.date < 3.days.ago.date" ==> false

  // comparisons with null
  "null < 1" ==> false
  "null <= 1" ==> false
  "null > 1" ==> false
  "null >= 1" ==> false
  "1 < null" ==> false
  "1 <= null" ==> false
  "1 > null" ==> false
  "1 >= null" ==> false
  "null < null" ==> false
  "null < null < null" ==> false

  "true and true" ==> true
  "true and false" ==> false
  "false and true" ==> false
  "false and false" ==> false

  "true or true" ==> true
  "true or false" ==> true
  "false or true" ==> true
  "false or false" ==> false

  "true or null.bang" ==> true
  "false and null.bang" ==> false

  "{ foo: 42, bar: 100 } == { bar: 100, foo: 42 }" ==> true
  "class Box n; Box 42 == { n: 42 }" ==> false

  "(class Box n) == (class Box n)" ==> false
  "class Box n; Box == Box" ==> true

  "{} or 42" ==> "42"
  "{ a: 1 } or 42" ==> "{ a: 1 }"

  "[] or 42" ==> "42"
  "[1] or 42" ==> "[1]"

  "0 or 42" ==> "42"
  "1 or 42" ==> "1"

  "null or 42" ==> "42"

  "42 | _ < 100" ==> true

  "(x => x) | (f => f 42)" ==> "42"

  // Multiplication
  "'xy' * 3" ==> "'xyxyxy'"
  "3 * 'xy'" ==> "'xyxyxy'"
  "'x' * 1" ==> "'x'"
  "'x' * 0" ==> "''"

  "[1, 2] * 3" ==> "[1, 2, 1, 2, 1, 2]"
  "3 * [1, 2]" ==> "[1, 2, 1, 2, 1, 2]"
  "[1] * 1" ==> "[1]"
  "[1] * 0" ==> "[]"

  "3 * 1.second | .tag" ==> "time.Seconds"
  "1.second * 3 | .tag" ==> "time.Seconds"

  "1 * 2 + 3 * 4" ==> 14
  "3 * 100 / 3" ==> 100
  "10 / 10 / 10" ==> 0.1
  "1 - 2 - 3" ==> -4

  "if 1 < 2 then 100 else 200" ==> 100
  "if 1 > 2 then 100 else 200" ==> 200
  "if 1 == 1 then 100" ==> 100

  val Y = "(h => (x => x x) (g => h (args => (g g) args)))" // fixed point combinator
  s"($Y (f => n => if n == 1 then 1 else n * f (n - 1))) 5" ==> 120

  "[1, 2, 3, 4, 5] | filter (_ >= 3)" ==> "[3, 4, 5]"

  "{ foo: 42, bar: 24 } | [_.foo, _.bar]" ==> "[42, 24]"

  "now > 1.day.ago" ==> true

  // null
  "null.in [1, 2, 3]" ==> false
  "null.in [null]" ==> true

  // null-safe dereferencing
  "null?.toString" ==> null
  "[null, { foo: 42 }] | map (_?.foo)" ==> "[null, 42]"
  "[ { foo: { bar: 42 } }, { foo: null } ].foo?.bar" ==> "[42, null]"

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

  // groupBy
  "[1, 2, 3, 1] | groupBy (x => x) | select 'key' 'count' | sortBy 'key'" ==>
    "[ { key: 1, count: 2 }, { key: 2, count: 1 }, { key: 3, count: 1 } ] "
  "'foo' | groupBy (x => x) | select 'key' 'count' | sortBy 'key'" ==>
    "[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "
  "'foo'.groupBy (x => x) | select 'key' 'count' | sortBy 'key'" ==>
    "[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "

  "[null] | groupBy --includeNull (x => x) | select 'key' 'count'" ==>
    "[ { key: null, count: 1 } ]"
  "[null] | groupBy --includeNull='nope' (x => x) | select 'key'" ==>
    "[ { key: 'nope' } ]"

  "[1, 2, 1] | groupBy --total (x => x) | select 'key' 'count' | sortBy 'count'" ==>
    "[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'Total', count: 3 } ]"
  "[1, 2, 1] | groupBy --total='totalCount' (x => x) | select 'key' 'count' | sortBy 'count'" ==>
    "[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'totalCount', count: 3 } ]"

  // Group.count
  "['apple', 'bike', 'book'] | groupBy first | sortBy (.key) | map (.count)" ==> "[1, 2]"

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

  // join
  " [1, 2, 3].join(', ') " ==> " '1, 2, 3' "
  " [1, 2, 3].join " ==> " '123' "
  " join ', ' [1, 2, 3] " ==> " '1, 2, 3' "
  " join [1, 2, 3] " ==> " '123' "
  " join --sequence=[1, 2, 3] " ==> " '123' "
  " join ', ' --sequence=[1, 2, 3] " ==> " '1, 2, 3' "
  " join --separator=', ' --sequence=[1, 2, 3] " ==> " '1, 2, 3' "
  " join --separator=', ' [1, 2, 3] " ==> " '1, 2, 3' "
  " join [] " ==> " '' "
  " join 'abc' " ==> " 'abc' "
  " join ':' 'abc' " ==> " 'a:b:c' "
  " 'abc'.join ':'" ==> " 'a:b:c' "

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

  "flatMap --withIndex (n i => [n, i]) [1, 2, 3]" ==> "[1, 0, 2, 1, 3, 2]"

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
  "map --withIndex (n i => n + i) [1, 2, 3]" ==> "[1, 3, 5]"
  "'foo' | map (.toUpper)" ==> "'FOO'"
  "'foo'.map (.toUpper)" ==> "'FOO'"

  // max
  "max [1, 200, 3]" ==> 200
  "[1, 200, 3].max" ==> 200
  "max 'abc'" ==> "'c'"
  "'abc'.max" ==> "'c'"
  "max 1 2 3" ==> 3
  "max [2, null, 1]" ==> 2
  "max [] --default=0" ==> "0"
  "[].max --default=0" ==> "0"

  // maxBy
  "maxBy length [ 'a', 'bbb', 'cc'] " ==> " 'bbb' "
  "maxBy (_) 'abcde'" ==> "'e'"
  "maxBy 'foo' [{ foo: null }, { foo: 42 }]" ==> "{ foo: 42 }"
  "'abcde'.maxBy (_)" ==> "'e'"
  "maxBy length [] --default=0" ==> "0"
  "[].maxBy length --default=0" ==> "0"

  // min
  "min [100, 2, 300]" ==> 2
  "[100, 2, 300].min" ==> 2
  "min 'abc'" ==> "'a'"
  "'abc'.min" ==> "'a'"
  "min 1 2 3" ==> 1
  "min [2, null, 1]" ==> 1
  "min [] --default=0" ==> "0"
  "[].min --default=0" ==> "0"
  "[].min [1, 2, 3]" shouldThrowAnException // used to be a bug where we leaked an argument

  // minBy
  "minBy length [ 'a', 'bbb', 'cc'] " ==> " 'a' "
  "minBy 'foo' [{ foo: null }, { foo: 42 }]" ==> "{ foo: 42 }"
  "minBy length [] --default=0" ==> "0"
  "[].minBy length --default=0" ==> "0"

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

  // select
  "{ foo: 42, bar: 24 } | select 'foo' " ==> "{ foo: 42 }"
  "{ foo: 42, bar: 24 } | select --foo " ==> "{ foo: 42 }"
  "{ foo: 42, bar: 24 } | select --foo=(_.foo) " ==> "{ foo: 42 }"
  "{ foo: 42, bar: 24 } | select 'foo' --baz=(_.foo) 'bar' " ==> "{ foo: 42, baz: 42, bar: 24 }"
  " [{ foo: 42, bar: 24 }] | select 'foo' " ==> " [{ foo: 42 }] "
  "{ foo: 42 } | select --add --bar=(_.foo) " ==> "{ foo: 42, bar: 42 }"
  "{ foo: 42, bar: 24 } | select 'foo' --bar " ==> "{ foo: 42, bar: 24  }"
  " select 'foo' --target=[{ foo: 42 }]" ==> " [{ foo: 42 }] "
  " select 'foo' --target={ foo: 42 }" ==> "{ foo: 42 }"
  " select --foo --target={ foo: 42 }" ==> "{ foo: 42 }"
  " select --add --bar=(_.foo) --target={ foo: 42 }" ==> "{ foo: 42, bar: 42 }"
  " select -a --bar=(_.foo) --target={ foo: 42 }" ==> "{ foo: 42, bar: 42 }"

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

  // sort
  " ['c', 'a', 'b'].sort " ==> "['a', 'b', 'c']"
  "'eaebcd' | sort" ==> "'abcdee'"
  "'eaebcd'.sort" ==> "'abcdee'"
  "[1, null, 2].sort" ==> "[null, 1, 2]"
  "sort [1, 3, 2] --descending" ==> "[3, 2, 1]"
  "sort ['a1.txt', 'a10.txt', 'a2.txt'] --naturalOrder" ==> "['a1.txt', 'a2.txt', 'a10.txt']"
  // Bug where we used lte rather than lt to sort with:
  "sort [15, 12, 9, 3, 2, 90, 75, 7, 18, 9, 2, 1, 1, 14, 3, 3, 2, 21, 53, 2, 61, 24, 31, 1, 13, 14, 21, 4, 28, 17, 2, 5, 1, 17, 3, 3, 10, 100, 246, 176, 2, 10, 2, 4, 1, 1, 2, 1, 261, 1, 27, 10, 3, 6, 390, 44, 1, 2, 4, 1, 13, 4, 6, 1, 2, 8, 9, 3, 33, 9, 3, 131, 10, 2, 15, 35, 2, 157, 71, 32, 4, 12, 6, 7, 3, 8, 43, 8, 35, 1, 1, 11, 4, 2, 1, 9, 1]" shouldNotThrowAnException

  // sortBy
  " ['aa', 'b', 'ccc'] | sortBy length " ==> " ['b', 'aa', 'ccc'] "
  "'123' | sortBy (-_.toNumber)" ==> "'321'"
  "'123'.sortBy (-_.toNumber)" ==> "'321'"
  "[{ foo: 1 }, { foo: null }, { foo: 2 }].sortBy 'foo'" ==> "[{ foo: null }, { foo: 1 }, { foo: 2 }]"

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

  // matches
  " 'foo'.matches 'o' " ==> true
  " 'foo'.matches 'x' " ==> false
  " 'foo'.matches '^fo?.$' " ==> true
  " 'foo 666 bar'.matches 666 " ==> true
  " 'bar'.matches 666 " ==> false

  // startsWith
  " 'foo'.startsWith 'f' " ==> true
  " 'foo'.startsWith 'o' " ==> false

  // in
  "42.in [1, 2, 3]" ==> false
  "2.in [1, 2, 3]" ==> true

  // .toString
  "null.toString" ==> " 'null' "
  "2.toString" ==> " '2' "
  "().toString" ==> "''"
  "true.toString" ==> " 'true' "
  "[1, 2, 3].toString" ==> "'[1, 2, 3]'"
  """ "foo".toString.tag """ ==> """ "foo".tag """
  "{ foo: 42 }" ==> "{ foo: 42 }"

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

  // String concat
  " 'foo' + 42 " ==> " 'foo42' "
  " 42 + 'foo' " ==> " '42foo' "

  // List concat
  "[1, 2, 3] + [4, 5]" ==> "[1, 2, 3, 4, 5]"

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

  // String interpolation
  """(name => "Hello $name!") "Matt" """ ==> "'Hello Matt!'"
  """(name => "Hello ${name}!") "Matt" """ ==> "'Hello Matt!'"
  """(name => "Hello $name.reverse!") "Matt" """ ==> "'Hello ttaM!'"
  """(name => "Hello ${name.reverse}!") "Matt" """ ==> "'Hello ttaM!'"
  """(name => "Hello ${name | reverse}!") "Matt" """ ==> "'Hello ttaM!'"
  """ 42 | "$_" """ ==> "'42'"
  """ 'foo' | "$_.reverse" """ ==> "'oof'"
  """ "`"${42}`"" """ ==> """ '"42"' """
  """ "'${42}'" """ ==> """ "'42'" """
  """ "${4}'bar'${2}" """ ==> """ "4'bar'2" """

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

  // help

  "listFiles? .name" ==> "'listFiles'"
  "help groupBy" ==> "groupBy?"
  "man groupBy" ==> "groupBy?"
  "groupBy.help" ==> "groupBy?"

  "[1, 2, 3].reverse? .name" ==> "'reverse'"
  "[1, 2, 3].sortBy? .name" ==> "'sortBy'"
  "help [1,2, 3].sortBy" ==> "[1, 2, 3].sortBy?"
  "[1, 2, 3].sortBy.help" ==> "[1, 2, 3].sortBy?"

  "pwd.info.permissions? .name" ==> "'permissions'"
  "[pwd].info.permissions? .name" ==> "'permissions'"

  "git.log? .name" ==> "'log'"
  "help 42.getClass | _.name" ==> "'Number'"

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

  "a = now; b = 3.days.ago; min a b" ==> "b"

  // Block expressions
  "{ a = 0; a = a + 1; a }" ==> 1
  "{ a = 42 }; a" ==> 42

  // Holes in paren invocation args
  "[{foo: 42}].map(_.foo)" ==> "[42]"

  // .fields
  "{ foo: 42 }.fields.name" ==> "['foo']"
  "42.fields" shouldThrowAnException

  // object addition
  "{ foo: 42 } + { bar: 100 }" ==> "{ foo: 42, bar: 100 }"
  "{ foo: 42 } + { }" ==> "{ foo: 42 }"
  "{ foo: 42 } + { foo: 100 }" ==> "{ foo: 100 }"
  "class Box n; Box 5 + { foo: 42 } | .getClass.name" ==> "'Box'"
  "class Box n; { foo: 42 } + Box 5 | .getClass.name" ==> "'Box'"
  "class A a; class B b; A 'a' + B 'b' | .getClass.name" ==> "'B'"

  // object field subtraction
  "{ foo: 42, bar: 100 } - 'foo'" ==> "{ bar: 100 }"
  "{ foo: 42 } - 'bar'" ==> "{ foo: 42 }"

  "{ foo: 42 } - []" ==> "{ foo: 42 }"
  "{ foo: 1, bar: 2, baz: 3 } - ['baz', 'foo']" ==> "{ bar: 2 }"
  "{ foo: 1, bar: 2, baz: 3 } - ['baz', 'foo', 'notInObject']" ==> "{ bar: 2 }"

  // list subtraction
  "[1, 2, 3, 4] - [2, 4]" ==> "[1, 3]"
  "[1, 2, 3, 4] - []" ==> "[1, 2, 3, 4]"
  "[] - [1, 2, 3]" ==> "[]"
  "[1, 2, 3, 2, 1] - [2, 1]" ==> "[3, 2, 1]"

  // assignment
  "a = 42; a" ==> 42
  "a = {}; a['foo'] = 42; a.foo" ==> 42
  "a = [1, 2, 3]; a[1] = 42; a" ==> "[1, 42, 3]"

  "a = 0; a += 42; a" ==> 42
  "a = 42; a -= 42; a" ==> 0
  "a = 3; a *= 4; a" ==> 12
  "a = 15; a /= 5; a" ==> 3
  "a = { foo: 0 }; a.foo += 42; a" ==> "{ foo: 42 }"
  "a = [1, 0, 3]; a[1] += 42; a" ==> "[1, 42, 3]"

  "a = 42" ==> 42
  "a = 5; a += 10" ==> 15
  "_ = 10" ==> 10
  "{ foo } = { foo: 42 }" ==> "{ foo: 42 }"

  {
    // bare words
    implicit val config = Config(bareWords = true)

    "foo" ==> "'foo'"

    // Was a bug here, where foo was incorrectly identified as a bare word
    "def foo x = if x == 0 then 1 else foo (x - 1); foo 5" ==> 1

    "a = 10; a" ==> 10
  }

  // Object.hasField
  "{ foo: 42 }.hasField 'foo'" ==> true
  "{ foo: 42 }.hasField 'bar'" ==> false

  // Any.isA
  "42.isA Number" ==> true
  "42.isA String" ==> false
  "3.days.isA 3.days.tag" ==> true

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

  // Object.withField
  "{}.withField 'foo' 42" ==> "{ foo: 42 }"
  "{ foo: 42 }.withField 'bar' 256" ==> "{ foo: 42, bar: 256 }"
  "{ foo: 42 }.withField 'foo' 256" ==> "{ foo: 256 }"
  "{ a: 1, b: 2, c: 3, d: 4, e: 5, f: 6 }.withField 'c' 100 | .fields[2].name" ==> "'c'"

  // Object.get
  "{ foo: 42 }.get 'foo'" ==> 42
  "{}.get 'nope'" ==> null
  "{}.get 'nope' --default=42" ==> 42

  // Object literals
  "{ 'foo': 42 }.foo" ==> 42
  "{ ('foo' + 'bar'): 42 }" ==> "{ foobar: 42 }"
  "foo = 42; bar = 128; { foo, bar }" ==> "{ foo: 42, bar: 128 }"
  "def pi = 3.14; { pi }" ==> "{ pi: 3.14 }"

  // Semicolon inference
  """a = 1
    |b = a + 1
    |a + b""" ==> 3

  """a = 1
    |b = a +
    |  1
    |a + b""" ==> 3

  """def foo (arg1 = 10)
    |        (arg2 = 20) = 42
    |foo
  """ ==> 42

  """def foo (arg1 = {
    |           a = 10
    |           b = 20
    |           a + b
    |        }) = arg1
    |foo
  """ ==> 30

  """class A (arg1 = 10)
    |A.new.arg1
  """ ==> 10

  """class A (arg1 = 10) (arg2 = 20) {
    |  def a = arg1 + arg2
    |  def b = a
    |}
    |A.new.b
  """ ==> 30

  """class A
    |A.new.getClass.name
  """ ==> "'A'"

  """@attribute
    |class A {
    |  @attribute
    |  def a = 42
    |}
    |A.new.a
  """ ==> 42

  // Default arguments
  "def foo (x = 42) = x + 1; foo" ==> 43
  "def foo (x = 42) = x + 1; foo 100" ==> 101
  "def fun ({ foo } = { foo: 42 }) = foo + 1; fun" ==> 43
  "def fun ({ foo } = { foo: 42 }) = foo + 1; fun { foo: 100 }" ==> 101
  "def mkList (xs... = [1, 2, 3]) = xs; mkList" ==> "[1, 2, 3]"
  "def mkList (xs... = [1, 2, 3]) = xs; mkList --xs=[]" ==> "[]"
  "def mkList (xs... = [1, 2, 3]) = xs; mkList --xs=[4, 5, 6]" ==> "[4, 5, 6]"
  "def mkList (xs... = null) = xs; mkList" ==> "null"

  // holes
  "def baz = 128 + _; (baz) 72" ==> 200
  "def foo (x = _) = 42; foo" ==> 42
  "((x = _) => 42) | x => x" ==> 42

  // Number.to
  "1.to 5" ==> "[1, 2, 3, 4, 5]"
  "1.to 10 --step=2" ==> "[1, 3, 5, 7, 9]"
  "5.to 1 --step=-1" ==> "[5, 4, 3, 2, 1]"

  // Number.until
  "1.until 5" ==> "[1, 2, 3, 4]"
  "1.until 10 --step=2" ==> "[1, 3, 5, 7, 9]"
  "5.until 1 --step=-1" ==> "[5, 4, 3, 2]"

  // Lambdas inside parens and blocks
  "a = [0]; { x => a[0] += x; a[0] += x } 21; a[0]" ==> 42
  "a = 0; (x => a += x; a += x) 21; a" ==> 42

  // Number.times
  "a = 0; 5.times (a += 1); a" ==> 5
  "a = 0; (a += 1) | 5.times" ==> "[1, 2, 3, 4, 5]"

  // Lazy arguments
  "a = 0; def twice (@lazy block) = { block; block }; twice (a += 1); a" ==> 2
  "a = 0; def twice (@lazy block) = { block; block }; twice --block=(a += 1); a" ==> 2
  "a = 0; ((@lazy block) => { block; block }) (a += 1); a" ==> 2

  "def foo n = n += _; f = foo 1; f 2; f 3" ==> 6

  // Object.hoist
  "{ foo: 42, bar: { baz1: 100, baz2: 200 } }.hoist 'bar'" ==> "{ foo: 42, baz1: 100, baz2: 200 }"
  "{ foo: 42, bar: [{ baz1: 100, baz2: 200 }, { baz1: 300, baz2: 400 }] }.hoist 'bar'" ==>
    "[{ foo: 42, baz1: 100, baz2: 200 }, { foo: 42, baz1: 300, baz2: 400 }]"
  "{ foo: 42, bar: { baz1: 100, baz2: 200 } }.hoist 'bar' --prefix='bar_'" ==> "{ foo: 42, bar_baz1: 100, bar_baz2: 200 }"

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

  // Patterns
  "{ foo: 42, bar: 128 } | { foo } => foo + 1" ==> 43
  "{ foo: 42, bar: 128 } | { wibble } => wibble" ==> null
  "{ foo: 42, bar: 128 } | { foo, bar } => foo + bar" ==> 170
  "{ foo, bar } = { bar: 1, baz: 2, foo: 3 }; foo + bar" ==> 4
  "{ baz } = { foo: 42 }; baz" ==> null
  "{ foo: { bar } } = { foo: { bar: 42 } }; bar" ==> 42
  "{ foo: 42, bar: 128 } | { foo: foofar } => foofar" ==> 42
  "{ foo: 42, bar: 128 } | { foo: _ } => 10" ==> 10
  "(_ => 42) 10" ==> 42
  "def foo { bar } = bar; foo { bar: 42 }" ==> 42
  "[a, b, c] = [1, 2, 3]; a + b + c" ==> 6
  "[a, b] = [1, 2, 3]; a + b" ==> 3
  "[a, b] = [1]; b" ==> null
  "{ baz } = 42" shouldThrowAnException

  // Object.merge
  "Object.merge []" ==> "{}"
  "Object.merge { foo: 42 }" ==> "{ foo: 42 }"
  "Object.merge { foo: 42 } { bar: 128 }" ==> "{ foo: 42, bar: 128 }"
  "Object.merge { foo: 42 } { foo: 128 }" ==> "{ foo: 128 }"
  "class A a; class B b; class C c; Object.merge [A 1, B 2, C 3] | .getClass.name" ==> "'C'"

  // type.hint
  "type.hint [String] 42" ==> 42

  // classes
  "class Point x y; Point.new 3 4 | [.x, .y]" ==> "[3, 4]"
  "class Point x y; Point 3 4 | [.x, .y]" ==> "[3, 4]"
  "class Point x y { def sum = x + y }; Point 3 4 | .sum" ==> 7
  "class Point x y { def sum = this.x + this.y }; Point 3 4 | .sum" ==> 7
  "class Point x y { def sum = x + y; def sumSquared = sum * sum }; Point 3 4 | .sumSquared" ==> 49

  "class Box n { def update n = this.n = n }; b = Box 0; b.update 10; b.n" ==> 10
  "class Box n { def increment = n += 1 }; box = Box 10; box.increment; box.n" ==> 11
  "class Box n { def increment = this['n'] += 1 }; box = Box 10; box.increment; box.n" ==> 11

  """class Outer o1 {
       def outer o2 = {
         class Inner i1 {
           def inner i2 = o1 + o2 + i1 + i2
         }
         Inner 1 | .inner 2
       }
     }
     Outer 3 | .outer 4
  """ ==> "10"

  "class Thing x { def x = 100 }; Thing 42 | .x" ==> 42
  "class Thing { def x = 100; def y = { x = 42; x } }; Thing.new.y" ==> 42
  "class Thing; Thing.new.getClass.name" ==> "'Thing'"
  "class Thing { }; Thing.new.getClass.name" ==> "'Thing'"
  "class Point x y { def add = x + y }; [Point 1 2, Point 3 4].add" ==> "[3, 7]"

  "[Object, Object].merge { foo: 42 }" ==> "[{ foo: 42 }, { foo: 42 }]"

  // Default arguments for methods
  "class Foo wibble { def get (n = wibble) = n }; Foo 100 | .get" ==> 100
  "class Foo wibble { def get (n = this) = n }; Foo 100 | .get.wibble" ==> 100
  "class Foo { def foo (n = bar) = n; def bar = 100 }; Foo.new.foo" ==> 100

  "class Foo { def getFields = fields }; Foo.new.getFields" ==> "[]"
  "class Foo { def getFields = fields }; Foo.new.fields" ==> "[]"
  "class Foo { def getFields = toString }; Foo.new.getFields" ==> "'{ Foo | }'"

  "(class A { def foo = 42 }) | .new.foo" ==> 42
  "(def foo = 42) | x => x" ==> 42

  "false < true" ==> true
  "false < false" ==> false
  "true < false" ==> false
  "true < true" ==> false
  "false <= false" ==> true
  "true <= true" ==> true

  "(_ => b = 100) 42; b" ==> 100

  """{ a: 1, b: 2 } | select --a
  'b'""" ==> "'b'"

  """[] | sort -d
  100""" ==> 100

  """{ foo: { bar: 100 } }
      .foo
      .bar
  """ ==> 100

  // private visibility
  "class A { @private def a = 42 }; A.new.a" shouldThrowAnException

  "class A { @private def a = 42 }; A.new['a']" shouldThrowAnException

  "class A { @private def a = 42 }; 'a' A.new" shouldThrowAnException

  "class A { @private def a = 42 }; [A.new].a" shouldThrowAnException

  "class A { @private def a = 42; def b = a }; A.new.b" ==> 42
  "class A { @private def a = 42; def b = this.a }; A.new.b" ==> 42
  "class A { @private def a = 42; def b = this['a'].invoke }; A.new.b" ==> 42

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

  // @last
  "def foo n... (@last m) = m; foo 1 2 3" ==> 3
  "def foo n... (@last m) = m; foo 3" ==> 3
  "def foo (n = 10) (@last m) = m; foo 3" ==> 3

  "def fun first (@last second = 10) = { first, second }; fun 1 2" ==> "{ first: 1, second: 2 }"
  "def fun first (@last second = 10) = { first, second }; fun --first=1" ==> "{ first: 1, second: 10 }"
  "def fun first (@last second = 10) = { first, second }; fun 1" shouldThrowAnException

  "def fun args... (@last arg = 10) = { args, arg }; fun" ==> "{ args: [], arg: 10 }"
  "def fun args... (@last arg = 10) = { args, arg }; fun 1" ==> "{ args: [], arg: 1 }"
  "def fun args... (@last arg = 10) = { args, arg }; fun 1 2" ==> "{ args: [1], arg: 2 }"
  "def fun args... (@last arg = 10) = { args, arg }; fun --arg=20" ==> "{ args: [], arg: 20 }"
  "def fun args... (@last arg = 10) = { args, arg }; fun 1 --arg=20" ==> "{ args: [1], arg: 20 }"

  // @flag
  "def foo (@flag m = 10) n = m + n; foo 3" ==> 13
  "def foo (@flag m) n = m + n; foo 3 --m=20" ==> 23
  "def twice (@lazy @flag body) = (body; body); a = 0; twice --body=(a += 1); a" ==> 2
  "def twice (@lazy @flag body) = (body; body); a = 0; twice (a += 1); a" shouldThrowAnException

  // @shortFlag
  """def doSomething (@flag @(shortFlag 'd') dryRun = false) =
    |  if dryRun then 'Dry run' else 'For reals'
    |doSomething -d
    |""".stripMargin ==> "'Dry run'"

  // @namedArgs
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --foo=1" ==> "{ foo: 1 }"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --foo" ==> "{ foo: true }"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --namedArgs=1" ==> "{ namedArgs: 1}"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject -a" ==> "{ a: true }"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject -abc" ==> "{ a: true, b: true, c: true }"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject" ==> "{}"
  "def makeObject (@namedArgs namedArgs) (@flag otherArg) = { namedArgs, otherArg }; makeObject --otherArg=10" ==>
    "{ namedArgs: { otherArg: 10 }, otherArg: 10 }"
  "def makeObject otherParam (@namedArgs namedArgs) = [otherParam, namedArgs]; makeObject 1 --foo=2" ==> "[1, { foo: 2 }]"
  "def makeObject otherParam (@namedArgs namedArgs) = [otherParam, namedArgs]; makeObject --foo=2 --otherParam=1" ==>
    "[1, { foo: 2, otherParam: 1 }]"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject 1" shouldThrowAnException

  // doc comments
  """# Square a number
    |def square n = n * n
    |square.help.summary
  """.stripMargin ==> "'Square a number'"

  """# Summary
    |# Description 1
    |# Description 2
    |# Description 3
    |def fun x = x
    |fun.help.description
  """.stripMargin ==> "'Description 1`nDescription 2`nDescription 3'"

  """# Square a number
    |@private def square n = n * n
    |square.help.summary
  """.stripMargin ==> "'Square a number'"

  """class A {
    |  # Do something
    |  @attribute
    |  def method = 42
    |}
    |A.new.method? | .summary
  """.stripMargin ==> "'Do something'"

  """class A {
    |  # Do something
    |  @(attribute "with argument")
    |  def method = 42
    |}
    |A.new.method? | .summary
  """.stripMargin ==> "'Do something'"

  // .bless
  "class Point x y { def diff = x - y }; { y: 4, x: 10 }.bless Point | .diff" ==> 6
  "class Point x y { def diff = x - y }; Point.bless { y: 4, x: 10 } | .diff" ==> 6

  // .unbless
  "class Thing; Thing.new.unbless.getClass" ==> "Object"

  // @alias
//  "class A { @(alias 'a') def aardvark = 42 }; A.new.a" ==> 42

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

  // Object.whereField
  "{ apple: 1, bob: 2, aardvark: 3 }.whereField (.startsWith 'a')" ==> "{ apple: 1, aardvark: 3 }"
}