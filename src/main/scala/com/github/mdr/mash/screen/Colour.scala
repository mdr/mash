package com.github.mdr.mash.screen

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