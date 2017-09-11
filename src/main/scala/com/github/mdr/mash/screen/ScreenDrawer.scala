package com.github.mdr.mash.screen

import com.github.mdr.mash.utils.{ Point, Utils }

case class ScreenDraw(drawString: String, swappedOutScreenOpt: Option[Screen])

object ScreenDrawer {

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
    val drawState = new DrawState(currentPos.row, currentPos.column, Style.Default)

    if (swappingOut)
      drawState.switchToAlternateScreen()
    else if (swappingBackIn)
      drawState.returnFromAlternateScreen(currentPos)

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
    if (drawState.getCurrentRow == row - 1) {
      // We ended up on the line above
      val aboveLine = lines(row - 1)
      if (aboveLine.endsInNewline)
        drawState.crlf()
      else {
        // We rewrite the last character to force a wrap
        val lastChars = aboveLine.string.takeRight(1)
        drawState.moveCursorToColumn(aboveLine.string.size)
        drawState.addChars(lastChars)
        drawState.funkyWrap()
      }
    }
    val commonPrefixLength = previousLineOpt match {
      case Some(previousLine) ⇒ Utils.commonPrefix(newLine.string.chars, previousLine.string.chars).length
      case _                  ⇒ 0
    }
    if (previousLineOpt.map(_.string) != Some(newLine.string)) {
      // Only redraw if the actual characters have changed
      drawState.moveCursorToColumn(commonPrefixLength)
      drawState.eraseLine()
      val remainder = newLine.string.drop(commonPrefixLength)
      drawState.addChars(remainder)
    }
  }

  private def eraseLine(drawState: DrawState, row: Int) {
    drawState.moveCursorToRow(row)
    drawState.cr()
    drawState.eraseLine()
  }

}
