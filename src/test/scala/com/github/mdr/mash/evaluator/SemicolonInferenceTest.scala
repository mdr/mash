package com.github.mdr.mash.evaluator

object SemicolonInferenceTest extends AbstractEvaluatorTest {

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

  """{ a: 1, b: 2 } | select --a
  'b'""" ==> "'b'"

  """[] | sort -d
  100""" ==> 100

  """{ foo: { bar: 100 } }
      .foo
      .bar
  """ ==> 100

}
