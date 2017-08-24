package com.github.mdr.mash.screen

import com.github.mdr.mash.terminal.ansi.StyleToEscapeSequence

object StyledStringDrawer {

  def drawStyledChars(string: StyledString): String = {
    val sb = new StringBuilder
    var previousStyleOpt: Option[Style] = None
    for (StyledCharacter(c, style) ← string.chars) {
      if (previousStyleOpt != Some(style)) {
        sb.append(StyleToEscapeSequence.Reset)
        sb.append(StyleToEscapeSequence(style))
      }
      sb.append(c)
      previousStyleOpt = Some(style)
    }
    sb.append(StyleToEscapeSequence.Reset)
    sb.toString
  }

}
