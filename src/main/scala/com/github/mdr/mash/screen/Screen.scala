package com.github.mdr.mash.screen

import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.Utils
import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi.Color._
import org.fusesource.jansi.Ansi._

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
      val ansi = Ansi.ansi()
      if (previousStyleOpt != Some(style)) {
        ansi.reset()
        if (style.bold)
          ansi.bold()
        if (style.inverse)
          ansi.a(Attribute.NEGATIVE_ON)
        fg(ansi, style.foregroundColour)
        bg(ansi, style.backgroundColour)
      }
      ansi.a(c)
      sb.append(ansi.toString)
      previousStyleOpt = Some(style)
    }
    sb.append(Ansi.ansi().reset())
    sb.toString
  }

  private def fg(ansi: Ansi, colour: Colour): Unit = colour match {
    case DefaultColour                     ⇒ ansi.fg(DEFAULT)
    case BasicColour.Black                 ⇒ ansi.fg(BLACK)
    case BasicColour.Red                   ⇒ ansi.fg(RED)
    case BasicColour.Green                 ⇒ ansi.fg(GREEN)
    case BasicColour.Yellow                ⇒ ansi.fg(YELLOW)
    case BasicColour.Blue                  ⇒ ansi.fg(BLUE)
    case BasicColour.Cyan                  ⇒ ansi.fg(CYAN)
    case BasicColour.Magenta               ⇒ ansi.fg(MAGENTA)
    case BasicColour.Grey                  ⇒ ansi.fg(WHITE)
    case BrightColour(BasicColour.Black)   ⇒ ansi.fgBright(BLACK)
    case BrightColour(BasicColour.Red)     ⇒ ansi.fgBright(RED)
    case BrightColour(BasicColour.Green)   ⇒ ansi.fgBright(GREEN)
    case BrightColour(BasicColour.Yellow)  ⇒ ansi.fgBright(YELLOW)
    case BrightColour(BasicColour.Blue)    ⇒ ansi.fgBright(BLUE)
    case BrightColour(BasicColour.Cyan)    ⇒ ansi.fgBright(CYAN)
    case BrightColour(BasicColour.Magenta) ⇒ ansi.fgBright(MAGENTA)
    case BrightColour(BasicColour.Grey)    ⇒ ansi.fgBright(WHITE)
  }

  private def bg(ansi: Ansi, colour: Colour): Unit = colour match {
    case DefaultColour                     ⇒ ansi.bg(DEFAULT)
    case BasicColour.Black                 ⇒ ansi.bg(BLACK)
    case BasicColour.Red                   ⇒ ansi.bg(RED)
    case BasicColour.Green                 ⇒ ansi.bg(GREEN)
    case BasicColour.Yellow                ⇒ ansi.bg(YELLOW)
    case BasicColour.Blue                  ⇒ ansi.bg(BLUE)
    case BasicColour.Cyan                  ⇒ ansi.bg(CYAN)
    case BasicColour.Magenta               ⇒ ansi.bg(MAGENTA)
    case BasicColour.Grey                  ⇒ ansi.bg(WHITE)
    case BrightColour(BasicColour.Black)   ⇒ ansi.bgBright(BLACK)
    case BrightColour(BasicColour.Red)     ⇒ ansi.bgBright(RED)
    case BrightColour(BasicColour.Green)   ⇒ ansi.bgBright(GREEN)
    case BrightColour(BasicColour.Yellow)  ⇒ ansi.bgBright(YELLOW)
    case BrightColour(BasicColour.Blue)    ⇒ ansi.bgBright(BLUE)
    case BrightColour(BasicColour.Cyan)    ⇒ ansi.bgBright(CYAN)
    case BrightColour(BasicColour.Magenta) ⇒ ansi.bgBright(MAGENTA)
    case BrightColour(BasicColour.Grey)    ⇒ ansi.bgBright(WHITE)
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