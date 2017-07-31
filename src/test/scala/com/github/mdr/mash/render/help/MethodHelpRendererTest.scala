package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.MethodHelpClass

class MethodHelpRendererTest extends AbstractHelpRendererTest {

  "Rendering method help" should "work" in {
    val actualLines = getText(MethodHelpRenderer.render(MethodHelpClass.create("norm", TestPointClass)).lines)

    actualLines should equal(
      """METHOD
        |    norm - Calculate the norm of the point
        |
        |CLASS
        |    geometry.Point""".stripMargin)
  }

}
