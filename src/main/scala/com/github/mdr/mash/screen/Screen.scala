package com.github.mdr.mash.screen

import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.terminal.ansi.{ Ansi, StyleToAnsi }
import com.github.mdr.mash.utils.Utils

case class Point(row: Int, column: Int) {

  def up(rows: Int = 1): Point = copy(row = row - rows)

  def down(rows: Int = 1): Point = copy(row = row + rows)

}

case class Line(string: StyledString, endsInNewline: Boolean = true) {

  def truncate(n: Int) =
    if (string.size > n)
      if (n > 0)
        copy(string.take(n - 1) + "…".style(string(n - 1).style))
      else
        copy(StyledString.empty)
    else
      this

}

object Screen {

  def drawStyledChars(string: StyledString): String = {
    val sb = new StringBuilder
    var previousStyleOpt: Option[Style] = None
    for (StyledCharacter(c, style) ← string.chars) {
      if (previousStyleOpt != Some(style)) {
        sb.append(StyleToAnsi.Reset)
        sb.append(StyleToAnsi(style))
      }
      sb.append(c)
      previousStyleOpt = Some(style)
    }
    sb.append(StyleToAnsi.Reset)
    sb.toString
  }

}

case class Screen(lines: Seq[Line], cursorPos: Point, cursorVisible: Boolean = true, title: String) {


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
    sb.append(Ansi.EraseLine)
    sb.append(StyleToAnsi.Reset)
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
        case (Some(newLine), previousLineOpt)                               ⇒
          drawState.navigateUpToRowOrDownToJustAbove(row)
          if (drawState.getCurrentRow == row - 1) {
            // We ended up on the line above
            val aboveLine = lines(row - 1)
            if (aboveLine.endsInNewline)
              drawState.crlf()
            else {
              // We rewrite the last character to force a wrap
              val lastChars = aboveLine.string.drop(aboveLine.string.size - 1)
              drawState.navigateToColumn(aboveLine.string.size)
              val drawnChars = Screen.drawStyledChars(lastChars)
              drawState.addChars(drawnChars, lastChars.length)
              drawState.funkyWrap()
            }
          }
          val commonPrefixLength = previousLineOpt match {
            case Some(previousLine) ⇒ Utils.commonPrefix(newLine.string.chars, previousLine.string.chars).length
            case _                  ⇒ 0
          }
          if (previousLineOpt.map(_.string) != Some(newLine.string)) {
            // Only redraw if the actual characters have changed
            drawState.navigateToColumn(commonPrefixLength)
            drawState.eraseLine()
            val remainder = newLine.string.drop(commonPrefixLength)
            val drawnChars = Screen.drawStyledChars(remainder)
            drawState.addChars(drawnChars, remainder.length)
          }
        case (None, Some(previousLine))                                     ⇒
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

}