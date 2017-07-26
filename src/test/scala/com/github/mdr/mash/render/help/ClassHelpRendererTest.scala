package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, FunctionHelpClass, ParameterHelpClass }
import com.github.mdr.mash.screen.Line
import org.scalatest.{ FlatSpec, Matchers }

class ClassHelpRendererTest extends FlatSpec with Matchers {

  "Rendering class help" should "work when all information is provided" in {
    val help =
      ClassHelpClass.create(
        name = "Point",
        fullyQualifiedName = "geometry.Point",
        summaryOpt = Some("A point in 2D space"),
        descriptionOpt = Some("Has an x and y coordinate"),
        parentOpt = None,
        fields = Seq(),
        methods = Seq(),
        staticMethods = Seq())

    val actualLines = join(ClassHelpRenderer.render(help))

    actualLines should equal(
      """CLASS
        |    geometry.Point - A point in 2D space
        |
        |DESCRIPTION
        |    Has an x and y coordinate""".stripMargin)
  }

  private def join(lines: Seq[Line]): String = lines.map(_.string.forgetStyling).mkString("\n")

}
