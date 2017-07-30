package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.screen.Line
import org.scalatest.{ FlatSpec, Matchers }

class FieldHelpRendererTest extends FlatSpec with Matchers {

  "Rendering field help" should "work when all information is provided" in {
    val help = FieldHelpClass.create(name = "x", klass = TestPointClass)

    val actualLines = join(FieldHelpRenderer.render(help))

    actualLines should equal(
      """FIELD
        |    x - The x coordinate
        |
        |CLASS
        |    geometry.Point
        |
        |DESCRIPTION
        |    The horizontal coordinate. Examples:
        |      point.x""".stripMargin)

  }

  it should "work when information is omitted" in {

    object TestPointClass extends MashClass("geometry.Point") {

      val Name = Field("x", fieldType = StringClass)

      override def fields = Seq(Name)

      override def summaryOpt: Option[String] = None

    }

    val help = FieldHelpClass.create(name = "x", klass = TestPointClass)

    val actualLines = join(FieldHelpRenderer.render(help))

    actualLines should equal(
      """FIELD
        |    x
        |
        |CLASS
        |    geometry.Point""".stripMargin)
  }

  private def join(lines: Seq[Line]): String = lines.map(_.string.forgetStyling).mkString("\n")

}
