package com.github.mdr.mash.screen

sealed trait Colour

object Colour {
  case object Default extends Colour
  case object Cyan extends Colour
  case object Blue extends Colour
  case object Yellow extends Colour
  case object Red extends Colour
  case object Green extends Colour
  case object Magenta extends Colour
}

object Style {

  implicit class StylableString(s: String) {

    def style: Seq[StyledCharacter] = s.map(StyledCharacter(_))

    def style(st: Style): Seq[StyledCharacter] = s.map(StyledCharacter(_, st))

  }

}

case class Style(
  foregroundColour: Colour = Colour.Default,
  backgroundColour: Colour = Colour.Default,
  bold: Boolean = false,
  inverse: Boolean = false)

case class StyledCharacter(c: Char, style: Style = Style())
