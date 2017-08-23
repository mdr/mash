package com.github.mdr.mash.screen

import scala.collection.JavaConverters._
import java.awt.Color
import java.nio.charset.StandardCharsets
import com.github.mdr.mash.utils.Utils
import org.apache.commons.io.IOUtils

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

object Colour256 {

  private val HexCodes: Array[RgbColour] = {
    val lines = IOUtils.readLines(getClass.getResourceAsStream("/256-colour-codes"), StandardCharsets.UTF_8).asScala
    lines.map(RgbColour.parse).toArray
  }

  def nearest(rgbColour: RgbColour): Colour256 = Colour256(HexCodes.indexOf(Utils.minBy(HexCodes, rgbColour.distance).get))

  def nearest(s: String): Colour256 = nearest(RgbColour.parse(s))

}

case class Colour256(n: Int) extends Colour {
  require(0 <= n && n < 256)

  def rgbColour: RgbColour = Colour256.HexCodes(n)

}

object RgbColour {

  def parse(s: String) = {
    val c = Color.decode(s)
    RgbColour(c.getRed, c.getGreen, c.getBlue)
  }

}

case class RgbColour(red: Int, green: Int, blue: Int) extends Colour {

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
