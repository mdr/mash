package com.github.mdr.mash.screen

import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.terminal.ansi.{ EscapeSequence, StyleToEscapeSequence }
import com.github.mdr.mash.utils.Point

object Line {

  val Empty = Line(StyledString.Empty)

}

case class Line(string: StyledString, endsInNewline: Boolean = true) {

  def truncate(n: Int): Line =
    if (string.size > n)
      if (n > 0)
        copy(string.take(n - 1) + "…".style(string(n - 1).style))
      else
        copy(StyledString.Empty)
    else
      this

}

case class Screen(lines: Seq[Line],
                  cursorPosOpt: Option[Point],
                  title: String) {

  private def cursorPos = cursorPosOpt getOrElse Point(0, 0)

  /**
    * Advance past the entire screen, leaving it untouched.
    */
  def acceptScreen: String = {
    val sb = new StringBuilder
    var currentRow = cursorPos.row
    while (currentRow < lines.length) {
      sb.append("\r\n")
      currentRow += 1
    }
    sb.append(EscapeSequence.EraseLineFromCursor)
    sb.append(StyleToEscapeSequence.Reset)
    sb.toString
  }

}