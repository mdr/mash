package com.github.mdr.mash.evaluator

class DocCommentsTest extends AbstractEvaluatorTest {

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
    |(safe A.new.method).summary
  """.stripMargin ==> "'Do something'"

  """class A {
    |  # Do something
    |  @(attribute "with argument")
    |  def method = 42
    |}
    |(safe A.new.method).summary
  """.stripMargin ==> "'Do something'"

}
