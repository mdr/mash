package com.github.mdr.mash.render.help

import com.github.mdr.mash.screen.Line
import org.scalatest.{ FlatSpec, Matchers }

class AbstractHelpRendererTest extends FlatSpec with Matchers {

  protected def join(lines: Seq[Line]): String = lines.map(_.string.forgetStyling).mkString("\n")

}
