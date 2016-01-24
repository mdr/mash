package com.github.mdr.mash.inference

import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.compiler.Compiler
import org.scalatest.junit.JUnitRunner
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.ns.os._
import scala.collection.immutable.ListMap
import com.github.mdr.mash.ns.core.help._

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
  " 'foo' + 42 " shouldBeInferredAsHavingType StringType
  " 42 + 'bar' " shouldBeInferredAsHavingType StringType
  " 'foo'.untagged + 'bar'.untagged" shouldBeInferredAsHavingType StringType
  "42 + 24" shouldBeInferredAsHavingType NumberType
  "[1] + [2]" shouldBeInferredAsHavingType Seq(NumberType)

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

  "42.toString" shouldBeInferredAsHavingType StringType

  "ls.first.children" shouldBeInferredAsHavingType Seq(Instance(PathSummaryClass))

  "[]" shouldBeInferredAsHavingType Seq(Any)

  "[].join" shouldBeInferredAsHavingType StringType

  "user.groups" shouldBeInferredAsHavingType Seq(Tagged(StringClass, GroupClass))
  "user.primaryGroup" shouldBeInferredAsHavingType Tagged(StringClass, GroupClass)
  "user.primaryGroup.gid" shouldBeInferredAsHavingType Tagged(NumberClass, GidClass)

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
  " [ {foo: 42} ] | groupBy 'foo' " shouldBeInferredAsHavingType Seq(Group(NumberType, Object(ListMap("foo" -> NumberType))))
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.key) " shouldBeInferredAsHavingType NumberType
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.values) " shouldBeInferredAsHavingType Seq(Object(ListMap("foo" -> NumberType)))
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.count) " shouldBeInferredAsHavingType NumberType
  " 'foo' | groupBy (_) " shouldBeInferredAsHavingType Seq(Group(StringType, StringType))

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
  "ls? .parameters" shouldBeInferredAsHavingType Seq(Instance(ParameterHelpClass))

  // target
  "[1].sumBy.target" shouldBeInferredAsHavingType Seq(NumberType)

  private implicit class RichString(s: String) {

    def shouldBeInferredAsHavingType(expectedType: Type) {
      "TypeInferencer" should s"infer '$s' as having type '$expectedType'" in {
        val Some(actualType) = Compiler.compile(s, inferTypes = true, environment = Environment.create).flatMap(_.typeOpt)
        actualType should equal(expectedType)
      }
    }

  }

}