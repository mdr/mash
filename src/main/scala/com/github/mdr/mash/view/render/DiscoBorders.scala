package com.github.mdr.mash.view.render

import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Utils._

sealed trait DiscoMode

object DiscoMode {

  case object Static extends DiscoMode

  case object Animated extends DiscoMode

}

object DiscoBorders {

  private val BorderCharacters = "╔╗╝╚╤║│═╟─┼┬╢╧├└┌┐┘".toSet

  private val HalfColours =
    Array(
      "#ff0000",
      "#ff7000",
      "#ffbe00",
      "#ffd000",
      "#ffef00",
      "#97f000",
      "#05ff00",
      "#02d580",
      "#00b0ff",
      "#0060ff",
      "#000bff",
      "#4500ff",
      "#9200ff").map(RgbColour.parse)

  private val Colours = HalfColours ++ HalfColours.reverse.drop(1)

  private var shift = 0

  def addDiscoBorders(screen: Screen, discoMode: DiscoMode): Screen =
    screen.copy(lines = addDiscoBorders(screen.lines, discoMode))

  def addDiscoBorders(lines: Seq[Line], discoMode: DiscoMode): Seq[Line] = {
    val newLines = lines.zipWithIndex.map { case (line, row) ⇒ addDiscoBorders(line, row) }
    if (discoMode == DiscoMode.Animated)
      shift = shift - 1 + Colours.length
    newLines
  }

  private def addDiscoBorders(line: Line, row: Int): Line = {
    val newString = StyledString(line.string.chars.zipWithIndex.map { case (char, column) ⇒
      addDiscoBorders(char, row, column)
    })
    line.copy(string = newString)
  }

  private def addDiscoBorders(character: StyledCharacter, row: Int, column: Int): StyledCharacter = {
    val shouldRestyle = BorderCharacters contains character.c
    val colour = getColour(row, column)
    character.when(shouldRestyle, _.updateStyle(_.withForegroundColour(colour)))
  }

  private def getColour(row: Int, column: Int) =
    Colours((row + column + shift + Colours.length) % Colours.length)

}
