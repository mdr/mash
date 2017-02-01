package com.github.mdr.mash.inference

import com.github.mdr.mash.compiler._
import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.ns.collections
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.core.help._
import com.github.mdr.mash.ns.git._
import com.github.mdr.mash.ns.os._
import com.github.mdr.mash.ns.time._
import org.apache.commons.lang3.SystemUtils
import org.junit.runner.RunWith
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class TypeInferencerTest extends FlatSpec with Matchers {

  import Type._

  private val TaggedStringType = Tagged(StringClass, PathClass)

  "42" shouldBeInferredAsHavingType NumberClass

  "{ foo: 42 }.foo" shouldBeInferredAsHavingType NumberClass
  "{ foo: 42 }" shouldBeInferredAsHavingType obj("foo" -> NumberClass)
  "foo = 42; { foo }" shouldBeInferredAsHavingType obj("foo" -> NumberClass)
  "{ bar: '{ foo : 42 }' | json.fromString | .foo }" shouldBeInferredAsHavingType obj("bar" -> AnyClass)

  "[{ foo: 42 }] | map (_.foo) | first" shouldBeInferredAsHavingType NumberClass

  "map [1, 2, 3].map [(_ * 2), (_ * _)]" shouldBeInferredAsHavingType Seq(Seq(NumberClass))

  "(boundMethod => [(_ * 2), (_ * _)].map boundMethod) [1, 2, 3].map" shouldBeInferredAsHavingType Seq(Seq(NumberClass))

  "[1, 2, 3][0]" shouldBeInferredAsHavingType NumberClass

  "[1, 2, 3] 0" shouldBeInferredAsHavingType NumberClass

  " 'foo'[0] " shouldBeInferredAsHavingType StringClass

  // Addition
  " 'foo' + 'bar' " shouldBeInferredAsHavingType StringClass
  """ "foo" + "bar" """ shouldBeInferredAsHavingType TaggedStringType
  " 'foo' + 42 " shouldBeInferredAsHavingType StringClass
  " 42 + 'bar' " shouldBeInferredAsHavingType StringClass
  " 'foo'.untagged + 'bar'.untagged" shouldBeInferredAsHavingType StringClass
  "42 + 24" shouldBeInferredAsHavingType NumberClass
  "[1] + [2]" shouldBeInferredAsHavingType Seq(NumberClass)
  "{ foo: 42 } + { bar: 100 }" shouldBeInferredAsHavingType obj("foo" -> NumberClass, "bar" -> NumberClass)

  // subtraction
  "2 - 1" shouldBeInferredAsHavingType NumberClass
  "{ foo: 42, bar: 100 } - 'foo'" shouldBeInferredAsHavingType obj("bar" -> NumberClass)
  "{ foo: 42, bar: 100, baz: 128 } - ['foo', 'baz']" shouldBeInferredAsHavingType obj("bar" -> NumberClass)
  // "field = 'foo'; { foo: 42, bar: 100 } - field" shouldBeInferredAsHavingType obj("bar" -> NumberClass)

  "[1, 2, 3] - [2]" shouldBeInferredAsHavingType Seq(NumberClass)

  // Multiplication
  "2 * 2" shouldBeInferredAsHavingType NumberClass
  "'foo' * 2" shouldBeInferredAsHavingType StringClass
  "2 * 'foo'" shouldBeInferredAsHavingType StringClass
  """ "foo" * 2 """ shouldBeInferredAsHavingType TaggedStringType
  "[1, 2] * 3" shouldBeInferredAsHavingType Seq(NumberClass)
  "3 * [1, 2]" shouldBeInferredAsHavingType Seq(NumberClass)
  "5 * 1.seconds" shouldBeInferredAsHavingType (NumberClass taggedWith SecondsClass)
  "1.seconds * 5" shouldBeInferredAsHavingType (NumberClass taggedWith SecondsClass)

  // Map
  "[true].map 'not' " shouldBeInferredAsHavingType Seq(BooleanClass)
  "['f', 'g'].map ('foo'.startsWith)" shouldBeInferredAsHavingType Seq(BooleanClass)
  "['f', 'g'].map 'foo'.startsWith" shouldBeInferredAsHavingType Seq(BooleanClass)
  "[ (_ * 2), (_ + 1) ] | map (_ 10)" shouldBeInferredAsHavingType Seq(NumberClass)
  "map --f=(_ * 2) --sequence=[1, 2, 3]" shouldBeInferredAsHavingType Seq(NumberClass)
  "map --f=(_ * 2) [1, 2, 3]" shouldBeInferredAsHavingType Seq(NumberClass)
  "map (_.toUpper) 'foo'" shouldBeInferredAsHavingType StringClass
  "map (x => x x) [1, 2, 3]" shouldBeInferredAsHavingType Seq(Any)
  "map (_.toNumber) '123'" shouldBeInferredAsHavingType Seq(NumberClass)
  "'[1, 2, 3]' | json.fromString | map (x => 2)" shouldBeInferredAsHavingType Seq(NumberClass)

  // flatMap
  "[1].flatMap (n => [n.toString])" shouldBeInferredAsHavingType Seq(StringClass)
  "unknown | flatMap (a => [42])" shouldBeInferredAsHavingType Seq(NumberClass)

  "42.toString" shouldBeInferredAsHavingType StringClass

  "ls.first.children" shouldBeInferredAsHavingType Seq(Instance(PathSummaryClass))

  "[]" shouldBeInferredAsHavingType Seq(Any)

  "[].join" shouldBeInferredAsHavingType StringClass

  if (!SystemUtils.IS_OS_MAC_OSX) {
    "user.groups" shouldBeInferredAsHavingType Seq(Tagged(StringClass, GroupClass))
    "user.primaryGroup" shouldBeInferredAsHavingType Tagged(StringClass, GroupClass)
    "user.primaryGroup.gid" shouldBeInferredAsHavingType Tagged(NumberClass, GidClass)
  }

  // grep
  "[1, 2, 3] | grep 2" shouldBeInferredAsHavingType Seq(NumberClass)

  // last
  "last [1, 2, 3]" shouldBeInferredAsHavingType NumberClass
  "last 2 [1, 2, 3]" shouldBeInferredAsHavingType Seq(NumberClass)
  "last --n=2 --sequence=[1, 2, 3]" shouldBeInferredAsHavingType Seq(NumberClass)
  "'foo' | last" shouldBeInferredAsHavingType StringClass
  "'foo' | last 2" shouldBeInferredAsHavingType StringClass
  "'foo'.last" shouldBeInferredAsHavingType StringClass
  "'foo'.last 2" shouldBeInferredAsHavingType StringClass

  // first
  "first [1, 2, 3]" shouldBeInferredAsHavingType NumberClass
  "first 2 [1, 2, 3]" shouldBeInferredAsHavingType Seq(NumberClass)
  " 'foo' | first " shouldBeInferredAsHavingType StringClass
  " 'foo' | first 2" shouldBeInferredAsHavingType StringClass
  " 'foo'.first " shouldBeInferredAsHavingType StringClass
  " 'foo'.first 2" shouldBeInferredAsHavingType StringClass

  // select
  "{ foo: 42 } | select 'foo' " shouldBeInferredAsHavingType Object(ListMap("foo" -> NumberClass))
  "[{ foo: 42 }] | select 'foo' " shouldBeInferredAsHavingType Seq(Object(ListMap("foo" -> NumberClass)))
  "{ foo: 42 } | select --add --bar=(_.foo * 2) " shouldBeInferredAsHavingType Object(ListMap("foo" -> NumberClass, "bar" -> NumberClass))
  "{ foo: 42, bar: 24 } | select --foo 'bar' " shouldBeInferredAsHavingType Object(ListMap("foo" -> NumberClass, "bar" -> NumberClass))
  " { foo: 42 } | select --bar='foo' " shouldBeInferredAsHavingType Object(ListMap("bar" -> NumberClass))
  " { foo: 42 } | select --bar=(_.foo * 2) " shouldBeInferredAsHavingType Object(ListMap("bar" -> NumberClass))

  // reverse
  "reverse [42]" shouldBeInferredAsHavingType Seq(NumberClass)
  "reverse 'abc'" shouldBeInferredAsHavingType StringClass

  // where
  "[1, 2, 3] | where (_ > 2)" shouldBeInferredAsHavingType Seq(NumberClass)
  "'foo' | where (_ > 'm')" shouldBeInferredAsHavingType StringClass
  "'[1, 2, 3]' | json.fromString | where (_ > 2)"  shouldBeInferredAsHavingType Seq(AnyClass)

  "null" shouldBeInferredAsHavingType Instance(NullClass)

  // sum
  " [1.bytes] | sum " shouldBeInferredAsHavingType Tagged(NumberClass, BytesClass)
  " [] | sum " shouldBeInferredAsHavingType NumberClass
  " ['foo', 'bar'] | sum" shouldBeInferredAsHavingType StringClass
  " [[1, 2], [3]] | sum" shouldBeInferredAsHavingType Seq(NumberClass)

  // sumBy
  " [1.bytes] | sumBy (_) " shouldBeInferredAsHavingType Tagged(NumberClass, BytesClass)
  " [] | sumBy (_) " shouldBeInferredAsHavingType NumberClass
  "sumBy (_.toNumber) '123'" shouldBeInferredAsHavingType NumberClass

  // max
  "[1, 2, 3] | max" shouldBeInferredAsHavingType NumberClass
  "'abc' | max" shouldBeInferredAsHavingType StringClass
  "max 1 2 3" shouldBeInferredAsHavingType NumberClass
  "max 'a' 'b' 'c'" shouldBeInferredAsHavingType StringClass

  // min
  "[1, 2, 3] | min" shouldBeInferredAsHavingType NumberClass
  "'abc' | min" shouldBeInferredAsHavingType StringClass
  "min 1 2 3" shouldBeInferredAsHavingType NumberClass
  "min 'a' 'b' 'c'" shouldBeInferredAsHavingType StringClass

  // maxBy
  "['a', 'bb', 'ccc'] | maxBy length" shouldBeInferredAsHavingType StringClass
  "'abc' | maxBy (_)" shouldBeInferredAsHavingType StringClass

  // minBy
  "['a', 'bb', 'ccc'] | minBy length" shouldBeInferredAsHavingType StringClass
  "'abc' | minBy (_)" shouldBeInferredAsHavingType StringClass

  // find
  "['a', 'bb', 'ccc'] | find (_.length == 2)" shouldBeInferredAsHavingType StringClass
  "'foo' | find (_ != 'f')" shouldBeInferredAsHavingType StringClass

  // isEmpty
  "isEmpty []" shouldBeInferredAsHavingType BooleanClass

  "'size' pwd" shouldBeInferredAsHavingType Tagged(NumberClass, BytesClass)
  "'size' ls" shouldBeInferredAsHavingType Seq(Tagged(NumberClass, BytesClass))

  "pwd?.parent" shouldBeInferredAsHavingType TaggedStringType

  "['foo', 'bar'].startsWith 'f'" shouldBeInferredAsHavingType Seq(BooleanClass)

  // Vectorisation / .split
  "['foo bar', 'baz'].split" shouldBeInferredAsHavingType Seq(Seq(StringClass))
  "['foo:bar', 'baz'].split ':'" shouldBeInferredAsHavingType Seq(Seq(StringClass))

  "'foo:bar:baz'.split ':'" shouldBeInferredAsHavingType Seq(StringClass)
  "'foo:bar:baz' | split ':'" shouldBeInferredAsHavingType Seq(StringClass)

  // .lines
  "'string'.lines" shouldBeInferredAsHavingType Seq(StringClass)

  // groupBy and Group
  " [ {foo: 42} ] | groupBy 'foo' " shouldBeInferredAsHavingType Seq(Generic(collections.GroupClass, NumberClass, Object(ListMap("foo" -> NumberClass))))
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.key) " shouldBeInferredAsHavingType NumberClass
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.values) " shouldBeInferredAsHavingType Seq(Object(ListMap("foo" -> NumberClass)))
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.count) " shouldBeInferredAsHavingType NumberClass
  " 'foo' | groupBy (_) " shouldBeInferredAsHavingType Seq(Generic(collections.GroupClass, StringClass, StringClass))

  // sliding
  "[1, 2, 3] | sliding 2" shouldBeInferredAsHavingType Seq(Seq(NumberClass))

  // Strings as functions
  " 'foo' { foo: 42 } " shouldBeInferredAsHavingType NumberClass
  " 'toString' 42 " shouldBeInferredAsHavingType StringClass
  " 'foo' [{ foo: 42 }] " shouldBeInferredAsHavingType Seq(NumberClass)

  "ls.first.name.absolute" shouldBeInferredAsHavingType Tagged(StringClass, PathClass)

  "{}.toString" shouldBeInferredAsHavingType StringClass

  // help
  "ls?" shouldBeInferredAsHavingType Instance(FunctionHelpClass)
  "help readLines" shouldBeInferredAsHavingType Instance(FunctionHelpClass)
  "help [].maxBy" shouldBeInferredAsHavingType Instance(FunctionHelpClass)
  "help 42.getClass" shouldBeInferredAsHavingType Instance(ClassHelpClass)
  "ls? .parameters" shouldBeInferredAsHavingType Seq(ParameterHelpClass)

  // target
  "[1].sumBy.target" shouldBeInferredAsHavingType Seq(NumberClass)

  "!!{nano}" shouldBeInferredAsHavingType Instance(UnitClass)
  "!{which ls}" shouldBeInferredAsHavingType Instance(ProcessResultClass)

  "git.status" shouldBeInferredAsHavingType Instance(StatusClass)
  "git['status']" shouldBeInferredAsHavingType BuiltinFunction(StatusFunction)
  "[1, 2, 3]['reverse']" shouldBeInferredAsHavingType BoundBuiltinMethod(Seq(NumberClass), ListClass.methods.find(_.name == "reverse").get)
  "[1, 2, 3].reverse" shouldBeInferredAsHavingType Seq(NumberClass)

  "{ foo: 42 } | .foo" shouldBeInferredAsHavingType NumberClass

  "now" shouldBeInferredAsHavingType DateTimeClass
  "now.date" shouldBeInferredAsHavingType DateClass

  "()" shouldBeInferredAsHavingType UnitClass

  "[{foo: 42}].map(_.foo)" shouldBeInferredAsHavingType Seq(NumberClass)

  // lambdas
  "(x y => x * y) 42 24" shouldBeInferredAsHavingType NumberClass
  "(x => x + 1) 10" shouldBeInferredAsHavingType NumberClass
  "(=> 42) | x => x" shouldBeInferredAsHavingType NumberClass
  "{ a: 42 } | { a } => a" shouldBeInferredAsHavingType NumberClass

  "[1, 2, 3] | reduce (x y => x + [y]) []" shouldBeInferredAsHavingType Seq(NumberClass)

  // Object.withField
  "{ foo: 42 }.withField 'bar' 256" shouldBeInferredAsHavingType obj("foo" -> NumberClass, "bar" -> NumberClass)
  "ls.first.withField 'bar' 256" shouldBeInferredAsHavingType PathSummaryClass

  // Object.get
  "{ foo: 42 }.get 'foo'" shouldBeInferredAsHavingType NumberClass

  // @lazy parameters
  "((@lazy block) => block) 42" shouldBeInferredAsHavingType NumberClass

  // @last parameters
  "def foo a... (@last b) = b; foo 42" shouldBeInferredAsHavingType NumberClass

  // @flag parameters
  "def foo (@flag a) b = a; foo [] --a=10" shouldBeInferredAsHavingType NumberClass

  // Number.times
  "5.times { print 'Hi' }" shouldBeInferredAsHavingType Seq(Unit)

  // timeTaken
  "timeTaken ls" shouldBeInferredAsHavingType Generic(TimedResultClass, Seq(PathSummaryClass))
  "timeTaken ls | .result" shouldBeInferredAsHavingType Seq(PathSummaryClass)

  // .hoist
  "{ foo: 42, bar: { a: 1, b: 2 } }.hoist 'bar'" shouldBeInferredAsHavingType
    obj("foo" -> NumberClass, "a" -> NumberClass, "b" -> NumberClass)

  // statements
  "a = 42; a" shouldBeInferredAsHavingType NumberClass
  "a = 42; b = 20; c = a + b; c" shouldBeInferredAsHavingType NumberClass
  "{ a } = { a: 42 }; a" shouldBeInferredAsHavingType NumberClass

  // User-defined functions
  "def square n = n * n; square 42" shouldBeInferredAsHavingType NumberClass

  "def foo n = if n > 0 then bar (n - 1) else 42; def bar n = foo (n - 1); bar 10" shouldBeInferredAsHavingType NumberClass

  "{ foo: => 42 }.foo" shouldBeInferredAsHavingType NumberClass

  "'{ foo: 42 }' | json.fromString | .foo" shouldBeInferredAsHavingType AnyClass

  // hint
  "json.fromFile 'file.json' | type.hint { name: String, addresses: [{ houseNumber: String, postcode: String }] }" shouldBeInferredAsHavingType
    Object(Map(
        "name" -> StringClass,
        "addresses" ->
          Seq(
            obj(
              "houseNumber" -> StringClass,
              "postcode" -> StringClass))))

  "42.isNull" shouldBeInferredAsHavingType BooleanClass
  "42.isTruthy" shouldBeInferredAsHavingType BooleanClass
  "42.isA Number" shouldBeInferredAsHavingType BooleanClass

  "def rev x = x.reverse; rev.isNull" shouldBeInferredAsHavingType BooleanClass
  "(x => x).isNull" shouldBeInferredAsHavingType BooleanClass

  // static
  "Object.merge" shouldBeInferredAsHavingType Type.BuiltinFunction(ObjectClass.MergeFunction)

  // classes
  "class Foo { def bar = 10 }; Foo.new.bar" shouldBeInferredAsHavingType NumberClass
  "class Foo n { def bar = 10 }; Foo.new 5 | .bar" shouldBeInferredAsHavingType NumberClass
  "class Foo { def bar = 10 }; Foo 5 | .bar" shouldBeInferredAsHavingType NumberClass
  "class Foo { def bar m = 10 }; Foo 5 | .bar 10" shouldBeInferredAsHavingType NumberClass
  "class Foo { def bar m = m }; Foo 5 | .bar 10" shouldBeInferredAsHavingType NumberClass
  "class Foo { def bar m = m }; [Foo.new, Foo.new].bar 42" shouldBeInferredAsHavingType Seq(NumberClass)
  // "class Foo { def a = 10; def b = a }; Foo.new.b" shouldBeInferredAsHavingType NumberClass

  // this
  "class A { def method1 = this; def method2 = 10 }; A.new.method1.method2" shouldBeInferredAsHavingType NumberClass
  "class A { def method1 n = this; def method2 = 10 }; A.new.method1 3 | .method2" shouldBeInferredAsHavingType NumberClass

  "[2, 3, 1].sortBy | method => method identity" shouldBeInferredAsHavingType Seq(NumberClass)

  "'foo'['reverse'] | x => x" shouldBeInferredAsHavingType StringClass
  "class A { def bar = 100 }; A.new['bar'] | x => x" shouldBeInferredAsHavingType NumberClass
  "class A { def bar = this; def baz = 100 }; A.new['bar'] | x => x.baz" shouldBeInferredAsHavingType NumberClass

  // Vectorised static member
  // "[Object].merge {}" shouldBeInferredAsHavingType Seq(Object(Map()))

  "class A { def square n = n * n }; A.new.square.help" shouldBeInferredAsHavingType FunctionHelpClass

  "def foo (n = ls) = 42; foo" shouldBeInferredAsHavingType NumberClass

  "(class A { def foo = 42 }) | .new.foo" shouldBeInferredAsHavingType NumberClass
  "(def foo = 42) | x => x" shouldBeInferredAsHavingType NumberClass

  "class Bob { def n = 42 }; 1 | type.hint Bob | .n" shouldBeInferredAsHavingType NumberClass
  "class Bob { def n = 42 }; 1 | type.hint [Bob] | .n" shouldBeInferredAsHavingType Seq(NumberClass)

  "[1, 2, 3].intersect [3, 4]" shouldBeInferredAsHavingType Seq(NumberClass)

  private implicit class RichString(s: String) {

    def shouldBeInferredAsHavingType(expectedType: Type) {
      "TypeInferencer" should s"infer '$s' as having type '$expectedType'" in {
        val settings = CompilationSettings(inferTypes = true)
        val expr = Compiler.compileForgiving(CompilationUnit(s), bindings = StandardEnvironment.create.bindings, settings).body
        val Some(actualType) = expr.typeOpt
        actualType should equal(expectedType)
      }
    }

  }

}