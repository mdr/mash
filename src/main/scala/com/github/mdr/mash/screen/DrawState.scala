package com.github.mdr.mash.screen

import com.github.mdr.mash.terminal.ansi.EscapeSequence
import com.github.mdr.mash.utils.CharUtils._

/**
 * Helper class to manage current characters written and location of cursor during drawing
 */
class DrawState(private var currentRow: Int, private var currentColumn: Int) {

  import EscapeSequence._

  private val sb = new StringBuilder(HideCursor)

  def getCurrentRow = currentRow

  def getCurrentColumn = currentColumn

  // Move up to the correct row, or down to just before the correct row, as required.
  def navigateUpToRowOrDownToJustAbove(row: Int) {
    while (currentRow > row) {
      sb.append(CursorUp)
      currentRow -= 1
    }
    while (currentRow < row - 1)
      crlf()
  }

  def navigateUpOrDownToRow(row: Int) {
    while (currentRow > row) {
      sb.append(CursorUp)
      currentRow -= 1
    }
    while (currentRow < row) {
      sb.append("\r\n")
      currentRow += 1
      currentColumn = 0
    }
  }

  def navigateToColumn(col: Int) {
    if (currentColumn > col) {
      val delta = currentColumn - col
      if (delta > col)
        cr()
      else
        cursorBackward(delta)
    }
    cursorForward(col - currentColumn)
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

  def cursorForward(n: Int) {
    sb.append(CursorForward * n)
    currentColumn += n
  }

  def cursorBackward(n: Int) {
    sb.append(CursorBackward * n)
    currentColumn += n
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
    sb.append(EscapeSequence.EraseLine)
  }

  def title(s: String) {
    sb.append(EscapeSequence.title(s))
  }

  def complete(showCursor: Boolean): String = {
    if (showCursor)
      sb.append(ShowCursor)
    sb.toString
  }

}
