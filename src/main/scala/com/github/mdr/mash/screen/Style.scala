package com.github.mdr.mash.screen

import com.github.mdr.mash.utils.Utils

sealed trait Colour

sealed trait BasicColour extends Colour {

  def bright = BrightColour(this)

}

case object DefaultColour extends Colour

object BasicColour {

  case object Black extends BasicColour

  case object Red extends BasicColour

  case object Green extends BasicColour

  case object Yellow extends BasicColour

  case object Blue extends BasicColour

  case object Magenta extends BasicColour

  case object Cyan extends BasicColour

  case object Grey extends BasicColour

}

case class BrightColour(colour: BasicColour) extends Colour

sealed trait Colour256 extends Colour

case class RgbColour256(red: Int, green: Int, blue: Int) extends Colour256 {
  require(0 <= red && red < 6)
  require(0 <= green && green < 6)
  require(0 <= blue && blue < 6)
}

case class GreyscaleColour256(brightness: Int) extends Colour256 {
  require(0 <= brightness && brightness < 24)
}

object Style {

  implicit class StylableString(s: String) {

    def style: StyledString = StyledString(s.map(StyledCharacter(_)))

    def style(st: Style): StyledString = StyledString(s.map(StyledCharacter(_, st)))

    def style(foregroundColour: Colour = DefaultColour,
              backgroundColour: Colour = DefaultColour,
              bold: Boolean = false,
              inverse: Boolean = false): StyledString =
      style(Style(foregroundColour = foregroundColour,
        backgroundColour = backgroundColour,
        bold = bold,
        inverse = inverse))

  }

}

case class Style(foregroundColour: Colour = DefaultColour,
                 backgroundColour: Colour = DefaultColour,
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
