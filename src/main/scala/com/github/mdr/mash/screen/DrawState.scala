package com.github.mdr.mash.screen

import com.github.mdr.mash.terminal.ansi.EscapeSequence
import com.github.mdr.mash.utils.Point

/**
  * Helper class to manage current characters written and state of the terminal during drawing
  */
class DrawState(private var currentRow: Int, private var currentColumn: Int) {

  import EscapeSequence._

  private val sb = new StringBuilder(HideCursor)

  def getCurrentRow = currentRow

  def getCurrentColumn = currentColumn

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

  def moveCursorToColumn(col: Int) {
    if (currentColumn > col)
        cr()
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

  def addChars(drawnChars: String, length: Int) {
    sb.append(drawnChars)
    currentColumn += length
  }

  def funkyWrap() {
    sb.append(" \r") // this one weird trick Readline doesn't want you to know
    currentRow += 1
    currentColumn = 0
  }

  def eraseLine() {
    sb.append(EscapeSequence.EraseLineFromCursor)
  }

  def setTitle(s: String) {
    sb.append(EscapeSequence.title(s))
  }

  def complete(showCursor: Boolean): String = {
    if (showCursor)
      sb.append(ShowCursor)
    sb.toString
  }

  def switchToAlternateScreen(): Unit = {
    sb.append(EscapeSequence.SwitchToAlternateScreen)
    sb.append(EscapeSequence.MoveCursorToTopLeft + EscapeSequence.ClearScreen)
    currentRow = 0
    currentColumn = 0
  }

  def returnFromAlternateScreen(cursorPos: Point): Unit = {
    sb.append(EscapeSequence.ReturnFromAlternateScreen)
    currentRow = cursorPos.row
    currentColumn = cursorPos.column
  }

}
