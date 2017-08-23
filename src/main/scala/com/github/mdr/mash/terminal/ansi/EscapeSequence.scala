package com.github.mdr.mash.terminal.ansi
import com.github.mdr.mash.utils.CharUtils._

object EscapeSequence {

  val EraseLine = s"$Esc[K"
  val ClearScreen = s"$Esc[H\u001b[2J"

  val CursorForward = s"$Esc[C"
  val CursorBackward = "\b"
  val CursorUp = s"$Esc[A"
  val CursorDown = s"$Esc[B"

  val HideCursor = s"$Esc[?25l"
  val ShowCursor = s"$Esc[?25h"

  def title(s: String) = s"$Esc]0;${s}\u0007"

}
