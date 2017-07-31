package com.github.mdr.mash.render.help

class ClassHelpRendererTest extends AbstractHelpRendererTest {

  "Rendering class help" should "work when all information is provided" in {

    val actualLines = getText(ClassHelpRenderer.render(TestPointClass).lines)

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
