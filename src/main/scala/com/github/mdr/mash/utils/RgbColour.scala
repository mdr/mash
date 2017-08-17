package com.github.mdr.mash.utils

import java.awt.Color

object RgbColour {

  def parse(s: String) = {
    val c = Color.decode(s)
    RgbColour(c.getRed, c.getGreen, c.getBlue)
  }

}

case class RgbColour(red: Int, green: Int, blue: Int) {

  def distance(that: RgbColour) = {
    val rmean = (this.red.toDouble + that.red) / 2
    val r = this.red - that.red
    val g = this.green - that.green
    val b = this.blue - that.blue
    val weightR = 2 + rmean / 256
    val weightG = 4.0
    val weightB = 2 + (255 - rmean) / 256
    Math.sqrt(weightR * r * r + weightG * g * g + weightB * b * b)
  }

  override def toString = "#%02x%02x%02x".format(red, green, blue)

}