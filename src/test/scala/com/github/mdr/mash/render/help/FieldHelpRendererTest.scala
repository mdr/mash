package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.screen.Line
import org.scalatest.{ FlatSpec, Matchers }

class FieldHelpRendererTest extends FlatSpec with Matchers {

  "Rendering field help" should "work when all information is provided" in {
    val help = FieldHelpClass.create(name = "x", klass = "Point", summaryOpt = Some("Horizontal coordinate"),
      descriptionOpt = Some(
        """The horizontal coordinate. Examples:
          |<mash>
          |  point.x
          |</mash>""".stripMargin))

    val actualLines = join(FieldHelpRenderer.render(help))

    actualLines should equal(
      """FIELD
        |    x - Horizontal coordinate
        |
        |CLASS
        |    Point
        |
        |DESCRIPTION
        |    The horizontal coordinate. Examples:
        |      point.x""".stripMargin)

  }

  it should "work when information is omitted" in {
    val help = FieldHelpClass.create(name = "x", klass = "Point", summaryOpt = None, descriptionOpt = None)

    val actualLines = join(FieldHelpRenderer.render(help))

    actualLines should equal(
      """FIELD
        |    x
        |
        |CLASS
        |    Point""".stripMargin)
  }

  private def join(lines: Seq[Line]): String = lines.map(_.string.forgetStyling).mkString("\n")

}
