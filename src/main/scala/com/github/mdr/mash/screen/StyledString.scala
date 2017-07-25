package com.github.mdr.mash.screen

import com.github.mdr.mash.utils.Utils

case class StyledCharacter(c: Char, style: Style = Style()) {

  def withStyle(style: Style) = copy(style = style)

  def updateStyle(f: Style ⇒ Style) = withStyle(f(style))

}

object StyledString {

  val empty = StyledString(Seq())

  def mkString(strings: Seq[StyledString], separator: StyledString) = separator.join(strings)
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

  def join(strings: Seq[StyledString]): StyledString =
    StyledString(Utils.intercalate(strings.map(_.chars), this.chars))

  def forgetStyling: String = chars.map(_.c).mkString
}
