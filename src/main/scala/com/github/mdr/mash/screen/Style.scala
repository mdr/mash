package com.github.mdr.mash.screen

object Style {

  implicit class StylableString(s: String) {

    def style: StyledString = StyledString(s.map(StyledCharacter(_)))

    def style(aStyle: Style): StyledString = aStyle(s)

    def style(foregroundColour: Colour = DefaultColour,
              backgroundColour: Colour = DefaultColour,
              bold: Boolean = false,
              inverse: Boolean = false,
              underline: Boolean = false): StyledString =
      style(Style(foregroundColour = foregroundColour,
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

  def withUnderline: Style = copy(underline = true)

  def apply(s: String): StyledString = StyledString(s.map(StyledCharacter(_, this)))

}