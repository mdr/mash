package com.github.mdr.mash.render.help

import com.github.mdr.mash.screen.Line
import org.scalatest.{ FlatSpec, Matchers }

class ClassHelpRendererTest extends AbstractHelpRendererTest {

  "Rendering class help" should "work when all information is provided" in {

    val actualLines = join(ClassHelpRenderer.render(TestPointClass))

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

}
