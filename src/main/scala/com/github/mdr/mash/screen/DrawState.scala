package com.github.mdr.mash.screen

import com.github.mdr.mash.terminal.ansi.{ EscapeSequence, StyleToEscapeSequence }
import com.github.mdr.mash.utils.{ Dimensions, Point }

/**
  * Helper class to manage current characters written and state of the terminal during drawing
  */
class DrawState(terminalSize: Dimensions,
                var currentRow: Int,
                var currentColumn: Int,
                var currentStyle: Style) {

  import EscapeSequence._

  private val sb = new StringBuilder(HideCursor + StyleToEscapeSequence.Reset)

  def addChars(s: StyledString) = s.chars.foreach(addChar)

  def addChar(char: StyledCharacter) {
    if (char.style != currentStyle) {
      val differsOnlyInForegroundColour = currentStyle.withForegroundColour(char.style.foregroundColour) == char.style
      if (differsOnlyInForegroundColour)
        sb.append(StyleToEscapeSequence.setForegroundColour(char.style.foregroundColour))
      else {
        sb.append(StyleToEscapeSequence.Reset)
        sb.append(StyleToEscapeSequence(char.style))
      }
      currentStyle = char.style
    }
    sb.append(char.c)
    if (currentColumn == terminalSize.columns) {
      currentColumn = 0
      currentRow += 1
    }
    currentColumn += 1
  }

  def getCurrentRow: Int = currentRow

  def getCurrentColumn: Int = currentColumn

  /**
    * Move up to the correct row, or down to just before the correct row, as required.
    */
  def navigateUpToRowOrDownToJustAbove(row: Int) {
    if (currentRow > row) {
      sb.append(EscapeSequence.cursorUp(currentRow - row))
      currentRow = row
    }
    if (currentRow < row - 1) {
      sb.append(EscapeSequence.cursorDown(row - 1 - currentRow))
      currentRow = row - 1
      cr()
    }
  }

  def moveCursorToRow(row: Int) {
    if (currentRow > row) {
      sb.append(EscapeSequence.cursorUp(currentRow - row))
      currentRow = row
    }
    if (currentRow < row) {
      sb.append(EscapeSequence.cursorDown(row - currentRow))
      currentRow = row
      cr()
    }
  }
  
  def moveCursorToColumn(col: Int) = {
    if (currentColumn == terminalSize.columns)
      cr() // Most reliable way of getting back to a safe position ... cursor backward does different things on xterm, rxvt, etc
    if (currentColumn > col)
      cursorBackward(currentColumn - col)
    else
      cursorForward(col - currentColumn)
  }

  def moveCursor(pos: Point): Unit = {
    moveCursorToRow(pos.row)
    moveCursorToColumn(pos.column)
  }

  def crlf() {
    sb.append("\r\n")
    currentRow += 1
    currentColumn = 0
  }

  def cr() {
    sb.append("\r")
    currentColumn = 0
  }

  def cursorForward(n: Int) =
    if (n > 0) {
      sb.append(EscapeSequence.cursorForward(n))
      currentColumn += n
    }

  def cursorBackward(n: Int) =
    if (n > 0) {
      sb.append(EscapeSequence.cursorBackward(n))
      currentColumn -= n
    }

  def eraseLineFromCursor() =
    sb.append(EscapeSequence.EraseLineFromCursor)

  def setTitle(s: String) =
    sb.append(EscapeSequence.setTitle(s))

  def complete(showCursor: Boolean): String = {
    if (showCursor)
      sb.append(ShowCursor)
    sb.toString
  }

  def switchToAlternateScreen() {
    sb.append(EscapeSequence.SwitchToAlternateScreen)
    sb.append(EscapeSequence.MoveCursorToTopLeft + EscapeSequence.ClearScreen)
    currentRow = 0
    currentColumn = 0
  }

  def returnFromAlternateScreen(cursorPos: Point) {
    sb.append(EscapeSequence.ReturnFromAlternateScreen)
    currentRow = cursorPos.row
    currentColumn = cursorPos.column
  }

}
