package com.github.mdr.mash.screen

import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.{ Dimensions, Point, Utils }
import com.github.mdr.mash.utils.Utils._

case class ScreenDraw(drawString: String, swappedOutScreenOpt: Option[Screen])

class ScreenDrawer(terminalSize: Dimensions) {

  def draw(newScreen: Screen,
           previousScreenOpt: Option[Screen] = None,
           swappedOutScreenOpt: Option[Screen] = None): ScreenDraw = {
    val Screen(lines, cursorPosOpt, title, alternateScreen) = newScreen
    val cursorPos = cursorPosOpt.getOrElse(Point(0, 0))

    val oldAlternateScreen = previousScreenOpt.exists(_.isAlternateScreen)
    val swappingOut = alternateScreen && !oldAlternateScreen
    val swappingBackIn = !alternateScreen && oldAlternateScreen

    val actualPreviousScreenOpt = if (swappingBackIn) swappedOutScreenOpt else previousScreenOpt

    val currentPos = actualPreviousScreenOpt.flatMap(_.cursorPosOpt).getOrElse(Point(0, 0))
    val drawState = new DrawState(terminalSize, currentPos.row, currentPos.column, Style.Default)

    if (swappingOut) {
      /**
        * Os X's Terminal.app has a bug that, if there is a character in the very bottom right of the
        * window when coming back from the alternate screen, a newline is inserted.
        *
        * As a workaround, we delete any bottom right character of a Screen before going to the alternate screen
        * (even if this might not be the bottom right of the Terminal window, we can't tell because have no idea
        * about scrollback). The character is restored when we return from the alternate screen.
        */
      for ((point, _) ← getBottomRightChar(actualPreviousScreenOpt)) {
        drawState.moveCursor(point)
        drawState.eraseLineFromCursor()
      }
      drawState.switchToAlternateScreen()
    } else if (swappingBackIn) {
      drawState.returnFromAlternateScreen(currentPos)
      for ((point, c) ← getBottomRightChar(actualPreviousScreenOpt)) {
        drawState.moveCursor(point)
        drawState.addChar(c)
      }
    }

    drawLines(drawState, lines, actualPreviousScreenOpt)

    drawState.moveCursor(cursorPos)

    if (previousScreenOpt.map(_.title) != Some(title))
      drawState.setTitle(title)

    val output = drawState.complete(showCursor = cursorPosOpt.isDefined)

    val newSwappedOutScreenOpt =
      if (swappingOut)
        previousScreenOpt
      else if (swappingBackIn)
        None
      else
        swappedOutScreenOpt

    ScreenDraw(output, swappedOutScreenOpt = newSwappedOutScreenOpt)
  }

  private def getBottomRightChar(actualPreviousScreenOpt: Option[Screen]) = {
    for {
      screen ← actualPreviousScreenOpt
      lastLine ← screen.lines.lastOption
      if lastLine.length == terminalSize.columns
      c ← lastLine.string.chars.lastOption
      size = screen.size
      bottomRight = Point(size.rows - 1, size.columns - 1)
    } yield (bottomRight, c)
  }

  private def drawLines(drawState: DrawState, lines: Seq[Line], actualPreviousScreenOpt: Option[Screen]) {
    val previousLines = actualPreviousScreenOpt.map(_.lines).getOrElse(Seq())
    val newAndPreviousLines: Seq[(Option[Line], Option[Line])] =
      Utils.zipPad(lines.map(Some(_)), previousLines.map(Some(_)), None)
    for (((newLineOpt, previousLineOpt), row) ← newAndPreviousLines.zipWithIndex)
      drawLine(drawState, lines, newLineOpt, previousLineOpt, row)
  }

  private def drawLine(drawState: DrawState,
                       lines: Seq[Line],
                       newLineOpt: Option[Line],
                       previousLineOpt: Option[Line],
                       row: Int): Unit =
    (newLineOpt, previousLineOpt) match {
      case (_, _) if newLineOpt == previousLineOpt ⇒ // nothing needed
      case (Some(newLine), _)                      ⇒ drawLine(drawState, lines, newLine, previousLineOpt, row)
      case (None, Some(_))                         ⇒ eraseLine(drawState, row)
    }

  private def drawLine(drawState: DrawState, lines: Seq[Line], newLine: Line, previousLineOpt: Option[Line], row: Int) {
    drawState.navigateUpToRowOrDownToJustAbove(row)
    var firstCharDrawn = false
    if (drawState.getCurrentRow == row - 1) {
      // We ended up on the line above
      val aboveLine = lines(row - 1)
      if (aboveLine.endsInNewline)
        drawState.crlf()
      else {
        // Rewrite the last character of the previous line and the first character of the new line to force a wrap
        if (drawState.getCurrentColumn < terminalSize.columns) {
          val lastCharOfPreviousLine = aboveLine.string.lastOption getOrElse ' '.style
          drawState.moveCursorToColumn(aboveLine.string.size - 1)
          drawState.addChar(lastCharOfPreviousLine)
        }
        val firstCharOfNewLine = newLine.string.headOption getOrElse ' '.style
        drawState.addChar(firstCharOfNewLine)
        firstCharDrawn = true
      }
    }
    previousLineOpt match {
      case Some(previousLine) ⇒
        val previousChars = previousLine.string.chars
        val newChars = newLine.string.chars
        val previousAndNewChars = previousChars.map(Some(_)).padTo(newChars.length, None).zip(newChars)
        for (((previousCharOpt, newChar), col) ← previousAndNewChars.zipWithIndex.when(firstCharDrawn, _ drop 1))
          if (previousCharOpt != Some(newChar)) {
            drawState.moveCursorToColumn(col)
            drawState.addChar(newChar)
          }
        if (previousLine.length > newLine.length) {
          drawState.moveCursorToColumn(newLine.length)
          drawState.eraseLineFromCursor()
        }
      case None               ⇒
        if (firstCharDrawn) {
          drawState.moveCursorToColumn(1)
          drawState.addChars(newLine.string drop 1)
        } else {
          drawState.moveCursorToColumn(0)
          drawState.addChars(newLine.string)
        }
    }
  }

  private def eraseLine(drawState: DrawState, row: Int) {
    drawState.moveCursorToRow(row)
    drawState.cr()
    drawState.eraseLineFromCursor()
  }

}
