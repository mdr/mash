package com.github.mdr.mash.render

import java.io.OutputStream

import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Utils._

object DiscoBorders {

  val Colours =
    Array(
      RgbColour.parse("#ff0000"),
      RgbColour.parse("#ff7000"),
      RgbColour.parse("#ffbe00"),
      RgbColour.parse("#ffd000"),
      RgbColour.parse("#ffef00"),
      RgbColour.parse("#97f000"),
      RgbColour.parse("#05ff00"),
      RgbColour.parse("#02d580"),
      RgbColour.parse("#00b0ff"),
      RgbColour.parse("#0060ff"),
      RgbColour.parse("#000bff"),
      RgbColour.parse("#4500ff"),
      RgbColour.parse("#9200ff"))

  private var shift = 0

  def addDiscoBorders(screen: Screen): Screen = {
    val newLines = screen.lines.zipWithIndex.map { case (line, row) ⇒ addDiscoBorders(line, row) }
    shift = shift - 1 + Colours.length
    screen.copy(lines = newLines)
  }

  private def addDiscoBorders(line: Line, row: Int): Line = {
    val newString = StyledString(line.string.chars.zipWithIndex.map { case (char, column) ⇒ addDiscoBorders(char, row, column) })
    line.copy(string = newString)
  }

  private def addDiscoBorders(character: StyledCharacter, row: Int, column: Int): StyledCharacter = {
    val restyle = "╔╗╝╚╤║│═╟─┼┬╢╧├└┌" contains character.c
    val colour = Colours((row + column + shift + Colours.length) % Colours.length)
    character.when(restyle, _.updateStyle(_.withForegroundColour(colour)))
  }

}
