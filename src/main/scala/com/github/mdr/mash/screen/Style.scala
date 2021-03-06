package com.github.mdr.mash.screen

import scala.collection.mutable.ArrayBuffer

object Style {

  val Default = Style()
  
  implicit class StyledStringInterpolation(val sc: StringContext) extends AnyVal {

    def style(args: Any*): StyledString = {
      def styleIfRequired(x: Any): StyledString = x match {
        case s: StyledString ⇒ s
        case _               ⇒ x.toString.style
      }
      val chunks = new ArrayBuffer[StyledString]
      val strings = sc.parts.iterator
      val expressions = args.iterator
      chunks += styleIfRequired(strings.next)
      while (strings.hasNext) {
        chunks += styleIfRequired(expressions.next)
        chunks += styleIfRequired(strings.next)
      }
      "".style.join(chunks)
    }
  }

  implicit class StylableChar(c: Char) {

    def style: StyledCharacter = StyledCharacter(c)

    def style(aStyle: Style): StyledCharacter = aStyle(c)

  }

  implicit class StylableString(s: String) {

    def style: StyledString = StyledString(s.map(StyledCharacter(_)))

    def style(aStyle: Style): StyledString = aStyle(s)

    def style(foregroundColour: Colour = DefaultColour,
              backgroundColour: Colour = DefaultColour,
              bold: Boolean = false,
              inverse: Boolean = false,
              underline: Boolean = false): StyledString =
      style(Style(
        foregroundColour = foregroundColour,
        backgroundColour = backgroundColour,
        bold = bold,
        inverse = inverse,
        underline = underline))

  }

}

case class Style(foregroundColour: Colour = DefaultColour,
                 backgroundColour: Colour = DefaultColour,
                 bold: Boolean = false,
                 inverse: Boolean = false,
                 underline: Boolean = false) {
  def withForegroundColour(colour: Colour): Style = copy(foregroundColour = colour)

  def withUnderline: Style = copy(underline = true)

  def withInverse: Style = copy(inverse = true)

  def withBold : Style = copy(bold = true)

  def apply(s: String): StyledString = StyledString(s.map(StyledCharacter(_, this)))

  def apply(c: Character): StyledCharacter = StyledCharacter(c, this)

  def invert: Style = copy(inverse = !inverse)
}