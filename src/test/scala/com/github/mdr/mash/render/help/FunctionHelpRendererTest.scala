package com.github.mdr.mash.render.help

import com.github.mdr.mash.functions._
import com.github.mdr.mash.runtime.MashValue

class FunctionHelpRendererTest extends AbstractHelpRendererTest {

  "Rendering function help" should "work when all information is provided" in {

    object TestFunction extends MashFunction("maths.squareRoot") {

      override def aliases = Seq(FullyQualifiedName("sqrt"))

      object Params {
        val N = Parameter(
          nameOpt = Some("n"),
          summaryOpt = Some("Number to take square root of"),
          descriptionOpt = Some("Must not be negative"))
        val GoFaster = Parameter(
          nameOpt = Some("goFaster"),
          summaryOpt = Some("Compute it faster"),
          isFlag = true,
          isBooleanFlag = true)
      }

      import Params._

      def params: ParameterModel = ParameterModel(N, GoFaster)

      def call(boundParams: BoundParams): MashValue = ???

      def summaryOpt: Option[String] = Some("Take the square root of a number")

      override def descriptionOpt = Some("The number must not be negative")

      override def sourceOpt = Some("def squareRoot n = findSquareRoot n")
    }

    val actualLines = getText(FunctionHelpRenderer.render(TestFunction))

    actualLines should equal(
      """FUNCTION
        |    maths.squareRoot, sqrt - Take the square root of a number
        |
        |CALLING SYNTAX
        |    squareRoot --goFaster <n>
        |
        |PARAMETERS
        |    n - Number to take square root of
        |        Must not be negative
        |
        |    --goFaster - Compute it faster
        |
        |DESCRIPTION
        |    The number must not be negative
        |
        |SOURCE
        |    def squareRoot n = findSquareRoot n""".stripMargin)
  }

  it should "omit calling syntax if it is the same as the function name" in {

    object TestFunction extends MashFunction("os.currentDirectory") {
      override def call(boundParams: BoundParams): MashValue = ???

      override def summaryOpt: Option[String] = None

      override def params: ParameterModel = ParameterModel.Empty
    }

    val actualLines = getText(FunctionHelpRenderer.render(TestFunction))

    actualLines should equal(
      """FUNCTION
        |    os.currentDirectory""".stripMargin)

  }

}
