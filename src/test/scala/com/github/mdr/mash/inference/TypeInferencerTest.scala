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

  private val NumberType = Instance(NumberClass)
  private val BooleanType = Instance(BooleanClass)
  private val StringType = Instance(StringClass)
  private val TaggedStringType = Tagged(StringClass, PathClass)

  "42" shouldBeInferredAsHavingType NumberType

  "{ foo: 42 }.foo" shouldBeInferredAsHavingType NumberType

  "[{ foo: 42 }] | map (_.foo) | first" shouldBeInferredAsHavingType NumberType

  "map [1, 2, 3].map [(_ * 2), (_ * _)]" shouldBeInferredAsHavingType Seq(Seq(NumberType))

  "(boundMethod => [(_ * 2), (_ * _)].map boundMethod) [1, 2, 3].map" shouldBeInferredAsHavingType Seq(Seq(NumberType))

  "[1, 2, 3][0]" shouldBeInferredAsHavingType NumberType

  "[1, 2, 3] 0" shouldBeInferredAsHavingType NumberType

  " 'foo'[0] " shouldBeInferredAsHavingType StringType

  // Addition
  " 'foo' + 'bar' " shouldBeInferredAsHavingType StringType
  """ "foo" + "bar" """ shouldBeInferredAsHavingType TaggedStringType
  " 'foo' + 42 " shouldBeInferredAsHavingType StringType
  " 42 + 'bar' " shouldBeInferredAsHavingType StringType
  " 'foo'.untagged + 'bar'.untagged" shouldBeInferredAsHavingType StringType
  "42 + 24" shouldBeInferredAsHavingType NumberType
  "[1] + [2]" shouldBeInferredAsHavingType Seq(NumberType)
  "{ foo: 42 } + { bar: 100 }" shouldBeInferredAsHavingType Object(Map("foo" -> NumberType, "bar" -> NumberType))

  // subtraction
  "2 - 1" shouldBeInferredAsHavingType NumberType
  "{ foo: 42, bar: 100 } - 'foo'" shouldBeInferredAsHavingType Object(Map("bar" -> NumberType))

  // Multiplication
  "2 * 2" shouldBeInferredAsHavingType NumberType
  "'foo' * 2" shouldBeInferredAsHavingType StringType
  "2 * 'foo'" shouldBeInferredAsHavingType StringType
  """ "foo" * 2 """ shouldBeInferredAsHavingType TaggedStringType
  "[1, 2] * 3" shouldBeInferredAsHavingType Seq(NumberType)
  "3 * [1, 2]" shouldBeInferredAsHavingType Seq(NumberType)
  "5 * 1.seconds" shouldBeInferredAsHavingType (NumberClass taggedWith SecondsClass)
  "1.seconds * 5" shouldBeInferredAsHavingType (NumberClass taggedWith SecondsClass)

  // Map
  "[true].map 'not' " shouldBeInferredAsHavingType Seq(BooleanType)
  "['f', 'g'].map ('foo'.startsWith)" shouldBeInferredAsHavingType Seq(BooleanType)
  "['f', 'g'].map 'foo'.startsWith" shouldBeInferredAsHavingType Seq(BooleanType)
  "[ (_ * 2), (_ + 1) ] | map (_ 10)" shouldBeInferredAsHavingType Seq(NumberType)
  "map --f=(_ * 2) --sequence=[1, 2, 3]" shouldBeInferredAsHavingType Seq(NumberType)
  "map --f=(_ * 2) [1, 2, 3]" shouldBeInferredAsHavingType Seq(NumberType)
  "map (_.toUpper) 'foo'" shouldBeInferredAsHavingType StringType
  "map (x => x x) [1, 2, 3]" shouldBeInferredAsHavingType Seq(Any)
  "map (_.toNumber) '123'" shouldBeInferredAsHavingType Seq(NumberType)

  // flatMap
  "[1].flatMap (n => [n.toString])" shouldBeInferredAsHavingType Seq(StringType)

  "42.toString" shouldBeInferredAsHavingType StringType

  "ls.first.children" shouldBeInferredAsHavingType Seq(Instance(PathSummaryClass))

  "[]" shouldBeInferredAsHavingType Seq(Any)

  "[].join" shouldBeInferredAsHavingType StringType

  if (!SystemUtils.IS_OS_MAC_OSX) {
    "user.groups" shouldBeInferredAsHavingType Seq(Tagged(StringClass, GroupClass))
    "user.primaryGroup" shouldBeInferredAsHavingType Tagged(StringClass, GroupClass)
    "user.primaryGroup.gid" shouldBeInferredAsHavingType Tagged(NumberClass, GidClass)
  }

  // grep
  "[1, 2, 3] | grep 2" shouldBeInferredAsHavingType Seq(NumberType)

  // last
  "last [1, 2, 3]" shouldBeInferredAsHavingType NumberType
  "last 2 [1, 2, 3]" shouldBeInferredAsHavingType Seq(NumberType)
  "last --n=2 --sequence=[1, 2, 3]" shouldBeInferredAsHavingType Seq(NumberType)
  "'foo' | last" shouldBeInferredAsHavingType StringType
  "'foo' | last 2" shouldBeInferredAsHavingType StringType
  "'foo'.last" shouldBeInferredAsHavingType StringType
  "'foo'.last 2" shouldBeInferredAsHavingType StringType

  // first
  "first [1, 2, 3]" shouldBeInferredAsHavingType NumberType
  "first 2 [1, 2, 3]" shouldBeInferredAsHavingType Seq(NumberType)
  " 'foo' | first " shouldBeInferredAsHavingType StringType
  " 'foo' | first 2" shouldBeInferredAsHavingType StringType
  " 'foo'.first " shouldBeInferredAsHavingType StringType
  " 'foo'.first 2" shouldBeInferredAsHavingType StringType

  // select
  "{ foo: 42 } | select 'foo' " shouldBeInferredAsHavingType Object(ListMap("foo" -> NumberType))
  "[{ foo: 42 }] | select 'foo' " shouldBeInferredAsHavingType Seq(Object(ListMap("foo" -> NumberType)))
  "{ foo: 42 } | select --add --bar=(_.foo * 2) " shouldBeInferredAsHavingType Object(ListMap("foo" -> NumberType, "bar" -> NumberType))
  "{ foo: 42, bar: 24 } | select --foo 'bar' " shouldBeInferredAsHavingType Object(ListMap("foo" -> NumberType, "bar" -> NumberType))
  " { foo: 42 } | select --bar='foo' " shouldBeInferredAsHavingType Object(ListMap("bar" -> NumberType))
  " { foo: 42 } | select --bar=(_.foo * 2) " shouldBeInferredAsHavingType Object(ListMap("bar" -> NumberType))

  // reverse
  "reverse [42]" shouldBeInferredAsHavingType Seq(NumberType)
  "reverse 'abc'" shouldBeInferredAsHavingType StringType

  // where
  "[1, 2, 3] | where (_ > 2)" shouldBeInferredAsHavingType Seq(NumberType)
  "'foo' | where (_ > 'm')" shouldBeInferredAsHavingType StringType

  "null" shouldBeInferredAsHavingType Instance(NullClass)

  // sum
  " [1.bytes] | sum " shouldBeInferredAsHavingType Tagged(NumberClass, BytesClass)
  " [] | sum " shouldBeInferredAsHavingType NumberType
  " ['foo', 'bar'] | sum" shouldBeInferredAsHavingType StringType
  " [[1, 2], [3]] | sum" shouldBeInferredAsHavingType Seq(NumberType)

  // sumBy
  " [1.bytes] | sumBy (_) " shouldBeInferredAsHavingType Tagged(NumberClass, BytesClass)
  " [] | sumBy (_) " shouldBeInferredAsHavingType NumberType
  "sumBy (_.toNumber) '123'" shouldBeInferredAsHavingType NumberType

  // max
  "[1, 2, 3] | max" shouldBeInferredAsHavingType NumberType
  "'abc' | max" shouldBeInferredAsHavingType StringType
  "max 1 2 3" shouldBeInferredAsHavingType NumberType
  "max 'a' 'b' 'c'" shouldBeInferredAsHavingType StringType

  // min
  "[1, 2, 3] | min" shouldBeInferredAsHavingType NumberType
  "'abc' | min" shouldBeInferredAsHavingType StringType
  "min 1 2 3" shouldBeInferredAsHavingType NumberType
  "min 'a' 'b' 'c'" shouldBeInferredAsHavingType StringType

  // maxBy
  "['a', 'bb', 'ccc'] | maxBy length" shouldBeInferredAsHavingType StringType
  "'abc' | maxBy (_)" shouldBeInferredAsHavingType StringType

  // minBy
  "['a', 'bb', 'ccc'] | minBy length" shouldBeInferredAsHavingType StringType
  "'abc' | minBy (_)" shouldBeInferredAsHavingType StringType

  // find
  "['a', 'bb', 'ccc'] | find (_.length == 2)" shouldBeInferredAsHavingType StringType
  "'foo' | find (_ != 'f')" shouldBeInferredAsHavingType StringType

  // isEmpty
  "isEmpty []" shouldBeInferredAsHavingType BooleanType

  "'size' pwd" shouldBeInferredAsHavingType Tagged(NumberClass, BytesClass)
  "'size' ls" shouldBeInferredAsHavingType Seq(Tagged(NumberClass, BytesClass))

  "pwd?.parent" shouldBeInferredAsHavingType TaggedStringType

  "['foo', 'bar'].startsWith 'f'" shouldBeInferredAsHavingType Seq(BooleanType)
  "['foo bar', 'baz'].split" shouldBeInferredAsHavingType Seq(Seq(StringType))
  "['foo:bar', 'baz'].split ':'" shouldBeInferredAsHavingType Seq(Seq(StringType))

  // groupBy and Group
  " [ {foo: 42} ] | groupBy 'foo' " shouldBeInferredAsHavingType Seq(Generic(collections.GroupClass, NumberType, Object(ListMap("foo" -> NumberType))))
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.key) " shouldBeInferredAsHavingType NumberType
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.values) " shouldBeInferredAsHavingType Seq(Object(ListMap("foo" -> NumberType)))
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.count) " shouldBeInferredAsHavingType NumberType
  " 'foo' | groupBy (_) " shouldBeInferredAsHavingType Seq(Generic(collections.GroupClass, StringType, StringType))

  // sliding
  "[1, 2, 3] | sliding 2" shouldBeInferredAsHavingType Seq(Seq(NumberType))

  // Strings as functions
  " 'foo' { foo: 42 } " shouldBeInferredAsHavingType NumberType
  " 'toString' 42 " shouldBeInferredAsHavingType StringType
  " 'foo' [{ foo: 42 }] " shouldBeInferredAsHavingType Seq(NumberType)

  "ls.first.name.absolute" shouldBeInferredAsHavingType Tagged(StringClass, PathClass)

  "{}.toString" shouldBeInferredAsHavingType StringType

  // help
  "ls?" shouldBeInferredAsHavingType Instance(FunctionHelpClass)
  "help readLines" shouldBeInferredAsHavingType Instance(FunctionHelpClass)
  "help [].maxBy" shouldBeInferredAsHavingType Instance(FunctionHelpClass)
  "help 42.class" shouldBeInferredAsHavingType Instance(ClassHelpClass)
  "ls? .parameters" shouldBeInferredAsHavingType Seq(ParameterHelpClass)

  // target
  "[1].sumBy.target" shouldBeInferredAsHavingType Seq(NumberType)

  "!!{nano}" shouldBeInferredAsHavingType Instance(UnitClass)
  "!{which ls}" shouldBeInferredAsHavingType Instance(ProcessResultClass)

  "git.status" shouldBeInferredAsHavingType Instance(StatusClass)
  "git['status']" shouldBeInferredAsHavingType DefinedFunction(StatusFunction)
  "[1, 2, 3]['reverse']" shouldBeInferredAsHavingType BoundMethod(Seq(NumberClass), ListClass.methods.find(_.name == "reverse").get)
  "[1, 2, 3].reverse" shouldBeInferredAsHavingType Seq(NumberClass)

  "{ foo: 42 } | .foo" shouldBeInferredAsHavingType NumberClass

  "now" shouldBeInferredAsHavingType DateTimeClass
  "now.date" shouldBeInferredAsHavingType DateClass

  "()" shouldBeInferredAsHavingType UnitClass

  "[{foo: 42}].map(_.foo)" shouldBeInferredAsHavingType Seq(NumberClass)

  "(x y => x * y) 42 24" shouldBeInferredAsHavingType NumberClass

  "[1, 2, 3] | reduce (x y => x + [y]) []" shouldBeInferredAsHavingType Seq(NumberClass)

  // Object.withField
  "{ foo: 42 }.withField 'bar' 256" shouldBeInferredAsHavingType Object(Map("foo" -> NumberType, "bar" -> NumberType))
  "ls.first.withField 'bar' 256" shouldBeInferredAsHavingType PathSummaryClass

  // Object.get
  "{ foo: 42 }.get 'foo'" shouldBeInferredAsHavingType NumberClass

  // lazy parameters
  "((lazy block) => block) 42" shouldBeInferredAsHavingType NumberClass

  // Number.times
  "5.times { print 'Hi' }" shouldBeInferredAsHavingType Seq(Unit)

  // timeTaken
  "timeTaken ls" shouldBeInferredAsHavingType Generic(TimedResultClass, Seq(PathSummaryClass))
  "timeTaken ls | .result" shouldBeInferredAsHavingType Seq(PathSummaryClass)

  // .hoist
  "{ foo: 42, bar: { a: 1, b: 2 } }.hoist 'bar'" shouldBeInferredAsHavingType
    Object(Map("foo" -> NumberType, "a" -> NumberType, "b" -> NumberType))

  private implicit class RichString(s: String) {

    def shouldBeInferredAsHavingType(expectedType: Type) {
      "TypeInferencer" should s"infer '$s' as having type '$expectedType'" in {
        val settings = CompilationSettings(inferTypes = true)
        val expr = Compiler.compileForgiving(CompilationUnit(s), bindings = StandardEnvironment.create.bindings, settings)
        val Some(actualType) = expr.typeOpt
        actualType should equal(expectedType)
      }
    }

  }

}