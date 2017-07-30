package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, FieldHelpClass, FunctionHelpClass, MethodHelpClass }
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.Line
import org.scalatest.{ FlatSpec, Matchers }

class ClassHelpRendererTest extends FlatSpec with Matchers {

  "Rendering class help" should "work when all information is provided" in {
    object TestClass extends MashClass("geometry.Point") {
      override def summaryOpt: Option[String] = Some("A point in 2D space")

      override def descriptionOpt = Some("Has an x and y coordinate")

      object TestMethod extends MashMethod("norm") {
        override def call(target: MashValue, boundParams: BoundParams): MashValue = ???

        override def summaryOpt: Option[String] = Some("Calculate the norm of the point")

        override def params: ParameterModel = ParameterModel.Empty
      }

      override def methods = Seq(TestMethod)
    }
    val help =
      ClassHelpClass.create(
        name = "Point",
        fullyQualifiedName = "geometry.Point",
        summaryOpt = TestClass.summaryOpt,
        descriptionOpt = TestClass.descriptionOpt,
        parentOpt = None,
        fields = Seq(FieldHelpClass.create(
          name = "x",
          klass = "geometry.Point",
          summaryOpt = Some("The x coordinate"),
          descriptionOpt = None)),
        methods = Seq(MethodHelpClass.create(
          name = "norm",
          klass = TestClass)),
        staticMethods = Seq(FunctionHelpClass.create(
          name = "random",
          fullyQualifiedName = "random",
          summaryOpt = Some("Generate a random point"),
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
