package com.github.mdr.mash.screen

import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi._
import org.fusesource.jansi.Ansi.Color._
import com.github.mdr.mash.utils.Utils

object DrawState {

  private val CursorForward = "\u001b[C"
  private val CursorBackward = "\b"
  private val CursorUp = "\u001b[A"
  private val CursorDown = "\u001b[B"
  private val HideCursor = "\u001b[?25l"
  private val ShowCursor = "\u001b[?25h"

}

/**
 * Helper class to manage current characters written and location of cursor during drawing
 */
class DrawState(private var currentRow: Int, private var currentColumn: Int) {

  import DrawState._

  private var sb: StringBuilder = new StringBuilder(HideCursor)

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
      //      currentColumn = 0
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
    sb.append(Ansi.ansi().eraseLine())
  }

  def title(s: String) {
    sb.append(s"\u001b]0;${s}\u0007")
  }

  def complete(showCursor: Boolean): String = {
    if (showCursor)
      sb.append(ShowCursor)
    sb.toString
  }

}
