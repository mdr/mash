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

case class Colour256(n: Int) extends Colour
