package com.github.mdr.mash.render

import com.github.mdr.mash.screen.Line
import org.scalatest.{ FlatSpec, Matchers }

trait RendererTest extends FlatSpec with Matchers {

  protected def getText(line: Line): String = line.string.forgetStyling

}
