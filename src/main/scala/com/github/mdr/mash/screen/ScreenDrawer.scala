package com.github.mdr.mash.screen

import com.github.mdr.mash.utils.{ Point, Utils }

object ScreenDrawer {

  def draw(newScreen: Screen, previousScreenOpt: Option[Screen]): String = {
    val Screen(lines, cursorPosOpt, title, alternateScreen) = newScreen

    val cursorPos = cursorPosOpt.getOrElse(Point(0, 0))
    val currentPos = previousScreenOpt.flatMap(_.cursorPosOpt).getOrElse(Point(0, 0))

    val oldAlternateScreen = previousScreenOpt.exists(_.alternateScreen)

    val previousLines = if (oldAlternateScreen != alternateScreen) Seq() else previousScreenOpt.map(_.lines).getOrElse(Seq())
    val drawState = new DrawState(currentPos.row, currentPos.column)

    if (alternateScreen && !oldAlternateScreen)
      drawState.switchToAlternateScreen()
    else if (!alternateScreen && oldAlternateScreen)
      drawState.returnFromAlternateScreen()

    val newAndPreviousLines: Seq[(Option[Line], Option[Line])] =
      Utils.zipPad(lines.map(Some(_)), previousLines.map(Some(_)), None)
    for (((newLineOpt, previousLineOpt), row) ← newAndPreviousLines.zipWithIndex)
      drawLine(drawState, lines, newLineOpt, previousLineOpt, row)

    drawState.navigateUpOrDownToRow(cursorPos.row)
    drawState.navigateToColumn(cursorPos.column)

    if (previousScreenOpt.map(_.title) != Some(title))
      drawState.title(title)

    drawState.complete(showCursor = cursorPosOpt.isDefined)
  }

  private def drawLine(drawState: DrawState,
                       lines: Seq[Line],
                       newLineOpt: Option[Line],
                       previousLineOpt: Option[Line],
                       row: Int) =
    (newLineOpt, previousLineOpt) match {
      case (newLineOpt, previousLineOpt) if newLineOpt == previousLineOpt ⇒ //  noop
      case (Some(newLine), previousLineOpt)                               ⇒
        drawState.navigateUpToRowOrDownToJustAbove(row)
        if (drawState.getCurrentRow == row - 1) {
          // We ended up on the line above
          val aboveLine = lines(row - 1)
          if (aboveLine.endsInNewline)
            drawState.crlf()
          else {
            // We rewrite the last character to force a wrap
            val lastChars = aboveLine.string.takeRight(1)
            drawState.navigateToColumn(aboveLine.string.size)
            val drawnChars = StyledStringDrawer.drawStyledChars(lastChars)
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
          val drawnChars = StyledStringDrawer.drawStyledChars(remainder)
          drawState.addChars(drawnChars, remainder.length)
        }
      case (None, Some(_))                                                ⇒
        drawState.navigateUpOrDownToRow(row)
        drawState.cr()
        drawState.eraseLine()
    }
}
