package com.github.mdr.mash.render.help

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.help.{ FunctionHelpClass, ParameterHelpClass }
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.Line
import org.scalatest.{ FlatSpec, Matchers }

class FunctionHelpRendererTest extends FlatSpec with Matchers {

  "Rendering function help" should "work when all information is provided" in {

    object TestFunction extends MashFunction("maths.squareRoot") {

      object Params {
        val N = Parameter(
          nameOpt = Some("n"),
          summaryOpt = Some("Number to take square root of"))
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
    }

    val help =
      FunctionHelpClass.create(
        name = TestFunction.name,
        fullyQualifiedName = TestFunction.fullyQualifiedName.toString,
        aliases = Seq("sqrt"),
        summaryOpt = TestFunction.summaryOpt,
        descriptionOpt = TestFunction.descriptionOpt,
        parameters = Seq(
          ParameterHelpClass.create(
            nameOpt = Some("n"),
            summaryOpt = Some("Number to take square root of"),
            descriptionOpt = Some("Must not be negative")),
          ParameterHelpClass.create(
            nameOpt = Some("goFaster"),
            summaryOpt = Some("Compute it faster"),
            isFlag = true)),
        classOpt = None,
        sourceOpt = Some("def squareRoot n = findSquareRoot n"),
        functionOpt = Some(TestFunction))

    val actualLines = join(FunctionHelpRenderer.render(help))

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
    val help =
      FunctionHelpClass.create(
        name = "currentDirectory",
        fullyQualifiedName = "os.currentDirectory")

    val actualLines = join(FunctionHelpRenderer.render(help))

    actualLines should equal(
      """FUNCTION
        |    os.currentDirectory""".stripMargin)

  }

  private def join(lines: Seq[Line]): String = lines.map(_.string.forgetStyling).mkString("\n")

}