package com.github.mdr.mash.inference

import com.github.mdr.mash.compiler._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.ns.collections
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.core.help.{ FunctionHelpClass, _ }
import com.github.mdr.mash.ns.core.objectClass.MergeStaticMethod
import com.github.mdr.mash.ns.git._
import com.github.mdr.mash.ns.os._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.runtime.MashValue
import org.apache.commons.lang3.SystemUtils
import org.junit.runner.RunWith
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TypeInferencerTest extends FlatSpec with Matchers {

  import Type._

  "42" ==> NumberClass

  // Object literals
  "{ foo: 42 }.foo" ==> NumberClass
  "{ foo: 42 }" ==> obj("foo" -> NumberClass)
  "foo = 42; { foo }" ==> obj("foo" -> NumberClass)
  "def foo = 42; { foo }" ==> obj("foo" -> NumberClass)
  "{ bar: '{ foo : 42 }' | json.fromString | .foo }" ==> obj("bar" -> AnyClass)

  "[{ foo: 42 }] | map (_.foo) | first" ==> NumberClass

  "map [1, 2, 3].map [(_ * 2), (_ * _)]" ==> Seq(Seq(NumberClass))

  "(boundMethod => [(_ * 2), (_ * _)].map boundMethod) [1, 2, 3].map" ==> Seq(Seq(NumberClass))

  "[1, 2, 3][0]" ==> NumberClass

  "[1, 2, 3] 0" ==> NumberClass

  " 'foo'[0] " ==> StringClass

  // Addition
  " 'foo' + 'bar' " ==> StringClass
  """ "foo" + "bar" """ ==> TaggedStringType
  " 'foo' + 42 " ==> StringClass
  " 42 + 'bar' " ==> StringClass
  " 'foo'.untagged + 'bar'.untagged" ==> StringClass
  "42 + 24" ==> NumberClass
  "[1] + [2]" ==> Seq(NumberClass)
  "{ foo: 42 } + { bar: 100 }" ==> obj("foo" -> NumberClass, "bar" -> NumberClass)
  "class Bob { def foo = 42 }; Bob.new + Bob.new | .foo" ==> NumberClass
  "ls.first + ls.first" ==> PathSummaryClass

  // subtraction
  "2 - 1" ==> NumberClass
  "{ foo: 42, bar: 100 } - 'foo'" ==> obj("bar" -> NumberClass)
  "{ foo: 42, bar: 100, baz: 128 } - ['foo', 'baz']" ==> obj("bar" -> NumberClass)
  // "field = 'foo'; { foo: 42, bar: 100 } - field" ==> obj("bar" -> NumberClass)

  "[1, 2, 3] - [2]" ==> Seq(NumberClass)

  // Multiplication
  "2 * 2" ==> NumberClass
  "'foo' * 2" ==> StringClass
  "2 * 'foo'" ==> StringClass
  """ "foo" * 2 """ ==> TaggedStringType
  "[1, 2] * 3" ==> Seq(NumberClass)
  "3 * [1, 2]" ==> Seq(NumberClass)
  "5 * 1.seconds" ==> (NumberClass taggedWith SecondsClass)
  "1.seconds * 5" ==> (NumberClass taggedWith SecondsClass)

  // map
  "[true].map 'not' " ==> Seq(BooleanClass)
  "['f', 'g'].map ('foo'.startsWith)" ==> Seq(BooleanClass)
  "['f', 'g'].map 'foo'.startsWith" ==> Seq(BooleanClass)
  "[ (_ * 2), (_ + 1) ] | map (_ 10)" ==> Seq(NumberClass)
  "map --f=(_ * 2) --sequence=[1, 2, 3]" ==> Seq(NumberClass)
  "map --f=(_ * 2) [1, 2, 3]" ==> Seq(NumberClass)
  "map (_.toUpper) 'foo'" ==> StringClass
  "map (x => x x) [1, 2, 3]" ==> Seq(Any)
  "map (_.toNumber) '123'" ==> Seq(NumberClass)
  "'[1, 2, 3]' | json.fromString | map (x => 2)" ==> Seq(NumberClass)

  // parallelMap
  "[1, 2, 3] | parallelMap (x => x)" ==> Seq(NumberClass)

  // flatMap
  "[1].flatMap (n => [n.toString])" ==> Seq(StringClass)
  "unknown | flatMap (a => [42])" ==> Seq(NumberClass)

  "42.toString" ==> StringClass

  "ls.first.children" ==> Seq(PathSummaryClass)

  "[]" ==> Seq(Any)

  "[].join" ==> StringClass

  if (!SystemUtils.IS_OS_MAC_OSX) {
    "user.groups" ==> Seq(Tagged(StringClass, GroupClass))
    "user.primaryGroup" ==> Tagged(StringClass, GroupClass)
    "user.primaryGroup.gid" ==> Tagged(NumberClass, GidClass)
  }

  // grep
  "[1, 2, 3] | grep 2" ==> Seq(NumberClass)
  "'foo`nbar' | grep 'b'" ==> Seq(StringClass)

  // last
  "last [1, 2, 3]" ==> NumberClass
  "last 2 [1, 2, 3]" ==> Seq(NumberClass)
  "last --n=2 --sequence=[1, 2, 3]" ==> Seq(NumberClass)
  "'foo' | last" ==> StringClass
  "'foo' | last 2" ==> StringClass
  "'foo'.last" ==> StringClass
  "'foo'.last 2" ==> StringClass

  // first
  "first [1, 2, 3]" ==> NumberClass
  "first 2 [1, 2, 3]" ==> Seq(NumberClass)
  " 'foo' | first " ==> StringClass
  " 'foo' | first 2" ==> StringClass
  " 'foo'.first " ==> StringClass
  " 'foo'.first 2" ==> StringClass

  // select
  "{ foo: 42 } | select 'foo' " ==> obj("foo" -> NumberClass)
  "[{ foo: 42 }] | select 'foo' " ==> Seq(obj("foo" -> NumberClass))
  "{ foo: 42 } | select --add --bar=(_.foo * 2) " ==> obj("foo" -> NumberClass, "bar" -> NumberClass)
  "{ foo: 42, bar: 24 } | select --foo 'bar' " ==> obj("foo" -> NumberClass, "bar" -> NumberClass)
  " { foo: 42 } | select --bar='foo' " ==> obj("bar" -> NumberClass)
  " { foo: 42 } | select --bar=(_.foo * 2) " ==> obj("bar" -> NumberClass)

  // reverse
  "reverse [42]" ==> Seq(NumberClass)
  "reverse 'abc'" ==> StringClass

  // where
  "[1, 2, 3] | where (_ > 2)" ==> Seq(NumberClass)
  "'foo' | where (_ > 'm')" ==> StringClass
  "'[1, 2, 3]' | json.fromString | where (_ > 2)" ==> Seq(AnyClass)

  "null" ==> NullClass

  // sum
  "[1.bytes] | sum" ==> Tagged(NumberClass, BytesClass)
  "['foo', 'bar'] | sum" ==> StringClass
  "[[1, 2], [3]] | sum" ==> Seq(NumberClass)

  // sumBy
  "[1.bytes] | sumBy (_)" ==> (NumberClass taggedWith BytesClass)
  "sumBy (_.toNumber) '123'" ==> NumberClass
  "[{ foo: 42 }, {foo: 100 }] | sumBy identity" ==> obj("foo" -> NumberClass)
  "[{ foo: 42 }, {foo: 100 }].sumBy (.foo)" ==> NumberClass
  "[ls.first, ls.first] | sumBy identity" ==> PathSummaryClass
  "class Bob { def foo = 42 }; [Bob.new, Bob.new] | sumBy identity | .foo" ==> NumberClass
  "['a', 'b'] | sumBy identity"  ==> StringClass
  "['a'.r, 'b'.r] | sumBy identity"  ==> (StringClass taggedWith RegexClass)

  // max
  "[1, 2, 3] | max" ==> NumberClass
  "'abc' | max" ==> StringClass
  "max 1 2 3" ==> NumberClass
  "max 'a' 'b' 'c'" ==> StringClass

  // min
  "[1, 2, 3] | min" ==> NumberClass
  "'abc' | min" ==> StringClass
  "min 1 2 3" ==> NumberClass
  "min 'a' 'b' 'c'" ==> StringClass

  // maxBy
  "['a', 'bb', 'ccc'] | maxBy length" ==> StringClass
  "'abc' | maxBy (_)" ==> StringClass

  // minBy
  "['a', 'bb', 'ccc'] | minBy length" ==> StringClass
  "'abc' | minBy (_)" ==> StringClass

  // find
  "['a', 'bb', 'ccc'] | find (_.length == 2)" ==> StringClass
  "'foo' | find (_ != 'f')" ==> StringClass

  // isEmpty
  "isEmpty []" ==> BooleanClass

  "'size' pwd" ==> (NumberClass taggedWith BytesClass)
  "'size' ls" ==> Seq(NumberClass taggedWith BytesClass)

  "pwd?.parent" ==> TaggedStringType

  "['foo', 'bar'].startsWith 'f'" ==> Seq(BooleanClass)

  // Vectorisation / .split
  "['foo bar', 'baz'].split" ==> Seq(Seq(StringClass))
  "['foo:bar', 'baz'].split ':'" ==> Seq(Seq(StringClass))

  "'foo:bar:baz'.split ':'" ==> Seq(StringClass)
  "'foo:bar:baz' | split ':'" ==> Seq(StringClass)

  // .lines
  "'string'.lines" ==> Seq(StringClass)

  // groupBy and Group
  " [ {foo: 42} ] | groupBy 'foo' " ==> Seq(Generic(collections.GroupClass, NumberClass, obj("foo" -> NumberClass)))
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.key) " ==> NumberClass
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.values) " ==> Seq(obj("foo" -> NumberClass))
  " [ {foo: 42} ] | groupBy 'foo' | (_.first.count) " ==> NumberClass
  " 'foo' | groupBy (_) " ==> Seq(Generic(collections.GroupClass, StringClass, StringClass))

  // sliding
  "[1, 2, 3] | sliding 2" ==> Seq(Seq(NumberClass))

  // chunked
  "[1, 2, 3] | chunked 2" ==> Seq(Seq(NumberClass))

  // Strings as functions
  " 'foo' { foo: 42 } " ==> NumberClass
  " 'toString' 42 " ==> StringClass
  " 'foo' [{ foo: 42 }] " ==> Seq(NumberClass)

  "ls.first.name.absolute" ==> Tagged(StringClass, PathClass)

  "{}.toString" ==> StringClass

  // help
  "ls?" ==> FunctionHelpClass
  "help readLines" ==> FunctionHelpClass
  "help [].maxBy" ==> FunctionHelpClass
  "help 42.getClass" ==> ClassHelpClass
  "ls? .parameters" ==> Seq(ParameterHelpClass)
  "'foo'.regex?" ==> FunctionHelpClass
  "def foo = 42; foo?" ==> FunctionHelpClass
  "class A { def a = 42 }; A.new.a?" ==> FunctionHelpClass
  "class A { def a b = 42 }; help A.new.a" ==> FunctionHelpClass
  "class A; help A" ==> ClassHelpClass

  // target
  "[1].sumBy.target" ==> Seq(NumberClass)

  "!!{nano}" ==> UnitClass
  "!{which ls}" ==> ProcessResultClass

  "git.status" ==> StatusClass
  "git['status']" ==> BuiltinFunction(StatusFunction)
  "[1, 2, 3]['reverse']" ==> BoundBuiltinMethod(Seq(NumberClass), ListClass.methods.find(_.name == "reverse").get)
  "[1, 2, 3].reverse" ==> Seq(NumberClass)

  "{ foo: 42 } | .foo" ==> NumberClass

  "now" ==> DateTimeClass
  "now.date" ==> DateClass

  "()" ==> UnitClass

  "[{foo: 42}].map(_.foo)" ==> Seq(NumberClass)

  // lambdas
  "(x y => x * y) 42 24" ==> NumberClass
  "(x => x + 1) 10" ==> NumberClass
  "(=> 42) | x => x" ==> NumberClass
  "{ a: 42 } | { a } => a" ==> NumberClass

  "[1, 2, 3] | reduce (x y => x + [y]) []" ==> Seq(NumberClass)

  // Object.withField
  "{ foo: 42 }.withField 'bar' 256" ==> obj("foo" -> NumberClass, "bar" -> NumberClass)
  "ls.first.withField 'bar' 256" ==> PathSummaryClass

  // Object.get
  "{ foo: 42 }.get 'foo'" ==> NumberClass

  // @lazy parameters
  "((@lazy block) => block) 42" ==> NumberClass

  // @last parameters
  "def foo a... (@last b) = b; foo 42" ==> NumberClass

  // @flag parameters
  "def foo (@flag a) b = a; foo [] --a=10" ==> NumberClass

  // Number.times
  "5.times { print 'Hi' }" ==> Seq(Unit)

  // timeTaken
  "timeTaken ls" ==> Generic(TimedResultClass, Seq(PathSummaryClass))
  "timeTaken ls | .result" ==> Seq(PathSummaryClass)

  // zip
  "zip [1] [2]" ==> Seq(Seq(NumberClass))

  // .hoist
  "{ foo: 42, bar: { a: 1, b: 2 } }.hoist 'bar'" ==>
    obj("foo" -> NumberClass, "a" -> NumberClass, "b" -> NumberClass)

  // statements
  "a = 42; a" ==> NumberClass
  "a = 42; b = 20; c = a + b; c" ==> NumberClass
  "{ a } = { a: 42 }; a" ==> NumberClass

  // User-defined functions
  "def square n = n * n; square 42" ==> NumberClass

  "def foo n = if n > 0 then bar (n - 1) else 42; def bar n = foo (n - 1); bar 10" ==> NumberClass

  "{ foo: => 42 }.foo" ==> NumberClass

  "'{ foo: 42 }' | json.fromString | .foo" ==> AnyClass

  // hint
  "json.fromFile 'file.json' | type.hint { name: String, addresses: [{ houseNumber: String, postcode: String }] }" ==>
    obj(
      "name" -> StringClass,
      "addresses" ->
        Seq(
          obj(
            "houseNumber" -> StringClass,
            "postcode" -> StringClass)))

  "42.isNull" ==> BooleanClass
  "42.isTruthy" ==> BooleanClass
  "42.isA Number" ==> BooleanClass

  "def rev x = x.reverse; rev.isNull" ==> BooleanClass
  "(x => x).isNull" ==> BooleanClass

  // static
  "Object.merge" ==> Type.BuiltinFunction(MergeStaticMethod)

  // classes
  "class Foo { def bar = 10 }; Foo.new.bar" ==> NumberClass
  "class Foo n { def bar = 10 }; Foo.new 5 | .bar" ==> NumberClass
  "class Foo { def bar = 10 }; Foo 5 | .bar" ==> NumberClass
  "class Foo { def bar m = 10 }; Foo 5 | .bar 10" ==> NumberClass
  "class Foo { def bar m = m }; Foo 5 | .bar 10" ==> NumberClass
  "class Foo { def bar m = m }; [Foo.new, Foo.new].bar 42" ==> Seq(NumberClass)
  "class Foo { def a = 10; def b = a }; Foo.new.b" ==> NumberClass
  "class Foo { @(alias 'aaa') def aardvark = 10; def b = aaa }; Foo.new.b" ==> NumberClass

  havingFirstRun("class Alias { @(alias 'aaa') def aardvark = 10; def b = aaa }") { implicit environment ⇒
    "Alias.new.b" ==> NumberClass
  }

  // this
  "class A { def method1 = this; def method2 = 10 }; A.new.method1.method2" ==> NumberClass
  "class A { def method1 n = this; def method2 = 10 }; A.new.method1 3 | .method2" ==> NumberClass

  "[2, 3, 1].sortBy | method => method identity" ==> Seq(NumberClass)

  "'foo'['reverse'] | x => x" ==> StringClass
  "class A { def bar = 100 }; A.new['bar'] | x => x" ==> NumberClass
  "class A { def bar = this; def baz = 100 }; A.new['bar'] | x => x.baz" ==> NumberClass

  // Vectorised static member
  // "[Object].merge {}" ==> Seq(Object(Map()))

  "class A { def square n = n * n }; A.new.square.help" ==> FunctionHelpClass

  "def foo (n = ls) = 42; foo" ==> NumberClass

  "(class A { def foo = 42 }) | .new.foo" ==> NumberClass
  "(def foo = 42) | x => x" ==> NumberClass

  "class Bob { def n = 42 }; 1 | type.hint Bob | .n" ==> NumberClass
  "class Bob { def n = 42 }; 1 | type.hint [Bob] | .n" ==> Seq(NumberClass)

  "[1, 2, 3].intersect [3, 4]" ==> Seq(NumberClass)

  // .bless
  "class Point x y { def method = 42 }; {x: 10, y: 20}.bless Point | .method" ==> NumberClass
  "class Point x y { def method = 42 }; Point.bless {x: 10, y: 20}| .method" ==> NumberClass
  "{ name: 'name', value: 42 }.bless core.FieldAndValue" ==> FieldAndValueClass

  // .unbless
  "class Point x y; Point 3 4 | .unbless" ==> obj("x" -> Any, "y" -> Any)
  "{ foo: 42 }.fields.first.unbless" ==> obj("name" -> StringClass, "value" -> Any)

  // @alias
  "class A { @(alias 'a') def aardvark = 42 }; A.new.a" ==> NumberClass

  // List.new
  "List 1 2 3" ==> Seq(NumberClass)
  "List.new" ==> Seq(Any)

  havingFirstRun("class Foo { def a = 10; def b = a }") { implicit environment ⇒
    "Foo.new.a" ==> NumberClass
    "Foo.new.b" ==> NumberClass
  }

  // skip
  "[1, 2, 3] | skip 2" ==> Seq(NumberClass)
  "[1, 2, 3] | skip" ==> Seq(NumberClass)

  // allButLast
  "[1, 2, 3] | allButLast 2" ==> Seq(NumberClass)
  "[1, 2, 3] | allButLast" ==> Seq(NumberClass)

  private lazy val TaggedStringType = StringClass taggedWith PathClass

  private def compile(s: String, bindings: Map[String, MashValue]): Expr =
    Compiler.compileForgiving(CompilationUnit(s), bindings = bindings).body

  private def havingFirstRun(s: String)(f: Environment ⇒ Any) = {
    implicit val environment = StandardEnvironment.create
    val expr = compile(s, environment.valuesMap)
    implicit val context = EvaluationContext(ScopeStack(environment.globalVariables))
    Evaluator.evaluate(expr)
    f(environment)
  }

  private implicit class RichString(s: String)(implicit val environment: Environment = StandardEnvironment.create) {

    def ==>(expectedType: Type) {
      "TypeInferencer" should s"infer '$s' as having type '$expectedType'" in {
        val settings = CompilationSettings(inferTypes = true)
        val expr = Compiler.compileForgiving(CompilationUnit(s), bindings = environment.bindings, settings).body
        val Some(actualType) = expr.typeOpt
        actualType should equal(expectedType)
      }
    }

  }

}