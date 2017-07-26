package com.github.mdr.mash.render.help

import com.github.mdr.mash.screen.{ BasicColour, Style }

/**
  * Render function/method/field help objects in a similar style to man pages
  */
object HelpRenderer {

  val ParamNameStyle = Style(foregroundColour = BasicColour.Blue, bold = true)

  val FieldMethodStyle = Style(foregroundColour = BasicColour.Blue, bold = true)

  val SectionTitleStyle = Style(bold = true, foregroundColour = BasicColour.Yellow)

  val NameStyle = Style(bold = true)

  val IndentSpace = " " * 4

}
