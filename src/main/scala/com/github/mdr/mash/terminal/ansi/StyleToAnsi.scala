package com.github.mdr.mash.terminal.ansi

import com.github.mdr.mash.screen._

import scala.collection.mutable.ArrayBuffer

object StyleToAnsi {

  private val EscapePrefix = "\u001b["

  def apply(style: Style): String = {
    val attribs = ArrayBuffer[Int]()
    attribs ++= fg(style.foregroundColour)
    attribs ++= bg(style.backgroundColour)
    if (style.inverse)
      attribs += 7
    if (style.bold)
      attribs += 1
    if (style.underline)
      attribs += 4
    attribEscape(attribs)
  }

  val Reset: String = attribEscape(Seq(0))

  private def attribEscape(attribs: Seq[Int]): String =
    s"$EscapePrefix${attribs mkString ";"}m"

  private def fg(colour: Colour): Seq[Int] = colour match {
    case BasicColour.Black         ⇒ Seq(30)
    case BasicColour.Red           ⇒ Seq(31)
    case BasicColour.Green         ⇒ Seq(32)
    case BasicColour.Yellow        ⇒ Seq(33)
    case BasicColour.Blue          ⇒ Seq(34)
    case BasicColour.Magenta       ⇒ Seq(35)
    case BasicColour.Cyan          ⇒ Seq(36)
    case BasicColour.Grey          ⇒ Seq(37)
    case BrightColour(basicColour) ⇒ addToFirstAttribute(fg(basicColour), 60)
    case Colour256(n)              ⇒ Seq(38, 5, n)
    case DefaultColour             ⇒ Seq(39)
  }

  private def bg(colour: Colour): Seq[Int] =
    addToFirstAttribute(fg(colour), 10)

  def addToFirstAttribute(attrs: Seq[Int], amount: Int): Seq[Int] =
    attrs.updated(0, attrs.head + amount)

}
