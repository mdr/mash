package com.github.mdr.mash.evaluator

class ClassesTest extends AbstractEvaluatorTest {

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

  "class Foo { def getFields = this.fields }; Foo.new.getFields" ==> "[]"
  "class Foo { def getFields = this.toString }; Foo.new.getFields" ==> "'{}'"

  "(class A { def foo = 42 }) | .new.foo" ==> 42
}
