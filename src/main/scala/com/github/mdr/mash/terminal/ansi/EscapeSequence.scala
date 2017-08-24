package com.github.mdr.mash.terminal.ansi

import com.github.mdr.mash.utils.CharUtils._

object EscapeSequence {

  val EraseLineFromCursor = s"$Esc[K"
  val EraseEntireLine = s"$Esc[2K"

  val ClearScreen = s"$Esc[H$Esc[2J"

  val CursorForward = s"$Esc[C"

  def cursorForward(n: Int) = {
    require(n >= 1)
    s"$Esc[${n}C"
  }

  val CursorBackward = "\b"

  def cursorBackward(n: Int) = {
    require(n >= 1)
    s"$Esc[${n}D"
  }

  val CursorUp = s"$Esc[A"

  def cursorUp(n: Int) = {
    require(n >= 1)
    s"$Esc[${n}A"
  }

  val CursorDown = s"$Esc[B"

  def cursorDown(n: Int) = {
    require(n >= 1)
    s"$Esc[${n}B"
  }

  val HideCursor = s"$Esc[?25l"
  val ShowCursor = s"$Esc[?25h"

  def title(s: String) = s"$Esc]0;${s}\u0007"

}
