package com.github.mdr.mash.screen

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
