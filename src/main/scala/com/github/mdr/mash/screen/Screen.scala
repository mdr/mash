package com.github.mdr.mash.screen

import com.github.mdr.mash.utils.Utils
import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi.Color._
import org.fusesource.jansi.Ansi._

case class Point(row: Int, column: Int) {

  def up(rows: Int): Point = copy(row = row - rows)

  def down(rows: Int = 1): Point = copy(row = row + rows)
}

case class Line(chars: Seq[StyledCharacter], endsInNewline: Boolean = true)

case class Screen(lines: Seq[Line], cursorPos: Point, cursorVisible: Boolean = true, title: String) {

  private def drawStyledChars(chars: Seq[StyledCharacter]): String = {
    val sb = new StringBuilder()
    var previousStyleOpt: Option[Style] = None
    for (StyledCharacter(c, style) ← chars) {
      var ansi = Ansi.ansi()
      if (previousStyleOpt != Some(style)) {
        ansi.reset()
        if (style.bold)
          ansi.bold()
        if (style.inverse)
          ansi.a(Attribute.NEGATIVE_ON)
        ansi.fg(ansiColour(style.foregroundColour))
      }
      ansi.a(c)
      sb.append(ansi.toString)
      previousStyleOpt = Some(style)
    }
    sb.append(Ansi.ansi().reset())
    sb.toString
  }

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
    sb.append(Ansi.ansi().eraseLine())
    sb.append(Ansi.ansi().reset())
    sb.toString
  }

  def draw(previousScreenOpt: Option[Screen], columns: Int): String = {
    val currentPos = previousScreenOpt.map(_.cursorPos).getOrElse(Point(0, 0))
    val previousLines = previousScreenOpt.map(_.lines).getOrElse(Seq())
    val drawState = new DrawState(currentPos.row, currentPos.column)
    val newAndPreviousLines: Seq[(Option[Line], Option[Line])] =
      Utils.zipPad(lines.map(Some(_)), previousLines.map(Some(_)), None)
    for ((linePair, row) ← newAndPreviousLines.zipWithIndex) {
      linePair match {
        case (newLineOpt, previousLineOpt) if newLineOpt == previousLineOpt ⇒ //noop
        case (Some(newLine), previousLineOpt) ⇒
          drawState.navigateUpToRowOrDownToJustAbove(row)
          if (drawState.getCurrentRow == row - 1) { // We ended up on the line above
            val aboveLine = lines(row - 1)
            if (aboveLine.endsInNewline)
              drawState.crlf()
            else { // We rewrite the last character to force a wrap
              val lastChars = aboveLine.chars.drop(aboveLine.chars.size - 1)
              drawState.navigateToColumn(aboveLine.chars.size)
              val drawnChars = drawStyledChars(lastChars)
              drawState.addChars(drawnChars, lastChars.length)
              drawState.funkyWrap()
            }
          }
          val commonPrefixLength = previousLineOpt match {
            case Some(previousLine) ⇒ Utils.commonPrefix(newLine.chars, previousLine.chars).length
            case _                  ⇒ 0
          }
          if (previousLineOpt.map(_.chars) != Some(newLine.chars)) { // Only redraw if the actual characters have changed
            drawState.navigateToColumn(commonPrefixLength)
            drawState.eraseLine()
            val remainder = newLine.chars.drop(commonPrefixLength)
            val drawnChars = drawStyledChars(remainder)
            drawState.addChars(drawnChars, remainder.length)
          }
        case (None, Some(previousLine)) ⇒
          drawState.navigateUpOrDownToRow(row)
          drawState.cr()
          drawState.eraseLine()
      }

    }
    drawState.navigateUpOrDownToRow(cursorPos.row)
    drawState.navigateToColumn(cursorPos.column)
    if (previousScreenOpt.map(_.title) != Some(title)) {
      drawState.title(title)
    }
    drawState.complete(showCursor = cursorVisible)
  }

  private def ansiColour(colour: Colour) = colour match {
    case Colour.Cyan    ⇒ CYAN
    case Colour.Blue    ⇒ BLUE
    case Colour.Green   ⇒ GREEN
    case Colour.Default ⇒ DEFAULT
    case Colour.Red     ⇒ RED
    case Colour.Yellow  ⇒ YELLOW
    case Colour.Magenta ⇒ MAGENTA
  }

}