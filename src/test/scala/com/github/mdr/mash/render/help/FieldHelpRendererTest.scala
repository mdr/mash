package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.core.help.FieldHelpClass

class FieldHelpRendererTest extends AbstractHelpRendererTest {

  "Rendering field help" should "work when all information is provided" in {
    val help = FieldHelpClass.create(name = "x", owningClass = TestPointClass)

    val actualLines = getText(FieldHelpRenderer.render(help))

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

    val help = FieldHelpClass.create(name = "x", owningClass = TestPointClass)

    val actualLines = getText(FieldHelpRenderer.render(help))

    actualLines should equal(
      """FIELD
        |    x
        |
        |CLASS
        |    geometry.Point""".stripMargin)
  }

}
