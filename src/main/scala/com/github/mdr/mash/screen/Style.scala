package com.github.mdr.mash.screen

import com.github.mdr.mash.utils.Utils

sealed trait Colour

object Colour {
  case object Default extends Colour
  case object Black extends Colour
  case object Red extends Colour
  case object Green extends Colour
  case object Yellow extends Colour
  case object Blue extends Colour
  case object Magenta extends Colour
  case object Cyan extends Colour
}

object Style {

  implicit class StylableString(s: String) {

    def style: StyledString = StyledString(s.map(StyledCharacter(_)))

    def style(st: Style): StyledString = StyledString(s.map(StyledCharacter(_, st)))

  }

}

case class Style(
  foregroundColour: Colour = Colour.Default,
  backgroundColour: Colour = Colour.Default,
  bold: Boolean = false,
  inverse: Boolean = false)

case class StyledCharacter(c: Char, style: Style = Style()) {

  def withStyle(style: Style) = copy(style = style)

  def updateStyle(f: Style ⇒ Style) = withStyle(f(style))

}

object StyledString {

  val empty = StyledString(Seq())

  def mkString(strings: Seq[StyledString], separator: StyledString) =
    StyledString(Utils.intercalate(strings.map(_.chars), separator.chars))
}

case class StyledString(chars: Seq[StyledCharacter]) {

  def size: Int = chars.size

  def length: Int = chars.size

  def take(n: Int) = copy(chars.take(n))

  def drop(n: Int) = copy(chars.drop(n))

  def apply(i: Int) = chars.apply(i)

  def +(that: StyledString): StyledString = StyledString(this.chars ++ that.chars)

  def +(that: Seq[StyledCharacter]): StyledString = StyledString(this.chars ++ that)

  def updated(i: Int, c: StyledCharacter) = copy(chars.updated(i, c))

  def grouped(n: Int) = chars.grouped(n).map(StyledString(_))

  def isEmpty = chars.isEmpty


}
