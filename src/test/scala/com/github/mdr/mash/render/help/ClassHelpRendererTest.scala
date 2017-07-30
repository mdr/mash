package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.Line
import org.scalatest.{ FlatSpec, Matchers }

class ClassHelpRendererTest extends FlatSpec with Matchers {

  "Rendering class help" should "work when all information is provided" in {
    object TestClass extends MashClass("geometry.Point") {
      val Name = Field("x", Some("The x coordinate"), StringClass)

      override def fields = Seq(Name)

      override def summaryOpt: Option[String] = Some("A point in 2D space")

      override def descriptionOpt = Some("Has an x and y coordinate")
      object TestStaticMethod extends MashFunction("random") {
        override def call(boundParams: BoundParams): MashValue = ???

        override def summaryOpt: Option[String] = Some("Generate a random point")

        override def params: ParameterModel = ParameterModel.Empty
      }

      object TestMethod extends MashMethod("norm") {
        override def call(target: MashValue, boundParams: BoundParams): MashValue = ???

        override def summaryOpt: Option[String] = Some("Calculate the norm of the point")

        override def params: ParameterModel = ParameterModel.Empty
      }

      override def methods = Seq(TestMethod)

      override def staticMethods = Seq(TestStaticMethod)
    }

    val actualLines = join(ClassHelpRenderer.render(TestClass))

    actualLines should equal(
      """CLASS
        |    geometry.Point - A point in 2D space
        |
        |DESCRIPTION
        |    Has an x and y coordinate
        |
        |PARENT
        |    core.Object
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
