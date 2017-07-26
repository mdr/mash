package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, FieldHelpClass, FunctionHelpClass }
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
        fields = Seq(FieldHelpClass.create(
          name = "x",
          klass = "geometry.Point",
          summaryOpt = Some("The x coordinate"),
          descriptionOpt = None)),
        methods = Seq(FunctionHelpClass.create(
          name = "norm",
          fullyQualifiedName = "norm",
          summaryOpt = Some("Calculate the norm of the point"),
          callingSyntax = "norm",
          parameters = Seq(),
          classOpt = Some("Point"))),
        staticMethods = Seq(FunctionHelpClass.create(
          name = "random",
          fullyQualifiedName = "random",
          summaryOpt = Some("Generate a random point"),
          callingSyntax = "random",
          parameters = Seq(),
          classOpt = Some("Point"))))

    val actualLines = join(ClassHelpRenderer.render(help))

    actualLines should equal(
      """CLASS
        |    geometry.Point - A point in 2D space
        |
        |DESCRIPTION
        |    Has an x and y coordinate
        |
        |FIELDS
        |    x - The x coordinate
        |
        |STATIC METHODS
        |    random - Generate a random point

        |METHODS
        |    norm - Calculate the norm of the point""".stripMargin)
  }

  private def join(lines: Seq[Line]): String = lines.map(_.string.forgetStyling).mkString("\n")

}
