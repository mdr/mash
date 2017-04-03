package com.github.mdr.mash.terminal.ansi

import com.github.mdr.mash.screen._
import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi.Color._

import scala.collection.mutable.ArrayBuffer

object StyleToAnsi {

  private val EscapePrefix = "\u001b["

  def apply(style: Style): String = {
    val attribs: ArrayBuffer[Int] = ArrayBuffer()
    attribs += fg(style.foregroundColour)
    attribs += bg(style.backgroundColour)
    if (style.inverse)
      attribs += 7
    if (style.bold)
      attribs += 1
    attribEscape(attribs)
  }

  val Reset: String = attribEscape(Seq(0))

  private def attribEscape(attribs: Seq[Int]): String =
    s"$EscapePrefix${attribs mkString ";"}m"

  private def fg(colour: Colour): Int = colour match {
    case DefaultColour                     ⇒ 39
    case BasicColour.Black                 ⇒ 30
    case BasicColour.Red                   ⇒ 31
    case BasicColour.Green                 ⇒ 32
    case BasicColour.Yellow                ⇒ 33
    case BasicColour.Blue                  ⇒ 34
    case BasicColour.Magenta               ⇒ 35
    case BasicColour.Cyan                  ⇒ 36
    case BasicColour.Grey                  ⇒ 37
    case BrightColour(BasicColour.Black)   ⇒ 90
    case BrightColour(BasicColour.Red)     ⇒ 91
    case BrightColour(BasicColour.Green)   ⇒ 92
    case BrightColour(BasicColour.Yellow)  ⇒ 93
    case BrightColour(BasicColour.Blue)    ⇒ 94
    case BrightColour(BasicColour.Magenta) ⇒ 95
    case BrightColour(BasicColour.Cyan)    ⇒ 96
    case BrightColour(BasicColour.Grey)    ⇒ 97
  }

  private def bg(colour: Colour): Int = colour match {
    case DefaultColour                     ⇒ 49
    case BasicColour.Black                 ⇒ 40
    case BasicColour.Red                   ⇒ 41
    case BasicColour.Green                 ⇒ 42
    case BasicColour.Yellow                ⇒ 43
    case BasicColour.Blue                  ⇒ 44
    case BasicColour.Magenta               ⇒ 45
    case BasicColour.Cyan                  ⇒ 46
    case BasicColour.Grey                  ⇒ 47
    case BrightColour(BasicColour.Black)   ⇒ 100
    case BrightColour(BasicColour.Red)     ⇒ 101
    case BrightColour(BasicColour.Green)   ⇒ 102
    case BrightColour(BasicColour.Yellow)  ⇒ 103
    case BrightColour(BasicColour.Blue)    ⇒ 104
    case BrightColour(BasicColour.Magenta) ⇒ 105
    case BrightColour(BasicColour.Cyan)    ⇒ 106
    case BrightColour(BasicColour.Grey)    ⇒ 107
  }


}
