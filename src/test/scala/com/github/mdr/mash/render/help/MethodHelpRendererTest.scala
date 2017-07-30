package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.MethodHelpClass

class MethodHelpRendererTest extends AbstractHelpRendererTest {

  "Rendering method help" should "work" in {
    val actualLines = join(MethodHelpRenderer.render(MethodHelpClass.create("norm", TestPointClass)))

    actualLines should equal(
      """METHOD
        |    norm - Calculate the norm of the point
        |
        |CLASS
        |    geometry.Point
        |
        |CALLING SYNTAX
        |    point.norm""".stripMargin)
  }

}
