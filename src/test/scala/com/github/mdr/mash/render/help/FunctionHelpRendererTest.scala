package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.{ FunctionHelpClass, ParameterHelpClass }
import com.github.mdr.mash.screen.Line
import org.scalatest.{ FlatSpec, Matchers }

class FunctionHelpRendererTest extends FlatSpec with Matchers {

  "Rendering function help" should "work when all information is provided" in {

    val help =
      FunctionHelpClass.create(
        name = "squareRoot",
        fullyQualifiedName = "maths.squareRoot",
        aliases = Seq("sqrt"),
        summaryOpt = Some("Take the square root of a number"),
        callingSyntax = "sqrt <n>",
        descriptionOpt = Some("The number must not be negative"),
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
        sourceOpt = Some("def squareRoot n = findSquareRoot n"))

    val actualLines = join(FunctionHelpRenderer.render(help))

    actualLines should equal(
      """FUNCTION
        |    maths.squareRoot, sqrt - Take the square root of a number
        |
        |CALLING SYNTAX
        |    sqrt <n>
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

  private def join(lines: Seq[Line]): String = lines.map(_.string.forgetStyling).mkString("\n")

}
