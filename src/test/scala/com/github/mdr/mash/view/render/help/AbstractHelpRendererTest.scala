package com.github.mdr.mash.view.render.help

import org.scalatest.{ FlatSpec, Matchers }

class AbstractHelpRendererTest extends FlatSpec with Matchers {

  protected def getText(linesAndLinks: LinesAndLinks): String =
    linesAndLinks.lines.map(_.string.forgetStyling).mkString("\n")

}
