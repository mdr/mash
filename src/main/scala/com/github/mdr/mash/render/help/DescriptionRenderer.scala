package com.github.mdr.mash.render.help

import com.github.mdr.mash.screen.{ Line, StyledString }
import com.github.mdr.mash.utils.LineInfo

object DescriptionRenderer {

  def render(description: String, indentLevel: Int = 1): Seq[Line] = {
    val renderedDescription: StyledString = MashMarkupRenderer.render(description)
    new LineInfo(renderedDescription.forgetStyling)
      .lineRegions
      .map(region ⇒ Line(HelpRenderer.IndentSpace * indentLevel + region.of(renderedDescription.chars)))
  }

}
