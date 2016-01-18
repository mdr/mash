package com.github.mdr.mash.printer

trait BoxCharacterSupplier {

  val doubleTopLeft: String

  val doubletopRight: String

  val doubleBottomRight: String

  val doubleBottomLeft: String

  val doubleHorizontalSingleDown: String

  val doubleVertical: String

  val singleVertical: String

  val doubleHorizontal: String

  val doubleVerticalSingleRight: String

  val singleHorizontal: String

  val singleIntersect: String

  val doubleVerticalSingleLeft: String

  val doubleHorizontalSingleUp: String

}

object UnicodeBoxCharacterSupplier extends BoxCharacterSupplier {

  override val doubleTopLeft = "╔"

  override val doubletopRight = "╗"

  override val doubleBottomRight = "╝"

  override val doubleBottomLeft = "╚"

  override val doubleHorizontalSingleDown = "╤"

  override val doubleVertical = "║"

  override val singleVertical = "│"

  override val doubleHorizontal = "═"

  override val doubleVerticalSingleRight = "╟"

  override val singleHorizontal = "─"

  override val singleIntersect = "┼"

  override val doubleVerticalSingleLeft = "╢"

  override val doubleHorizontalSingleUp = "╧"

}

object AsciiBoxCharacterSupplier extends BoxCharacterSupplier {

  override val doubleTopLeft = "+"

  override val doubletopRight = "+"

  override val doubleBottomRight = "+"

  override val doubleBottomLeft = "+"

  override val doubleHorizontalSingleDown = "-"

  override val doubleVertical = "|"

  override val singleVertical = "|"

  override val doubleHorizontal = "-"

  override val doubleVerticalSingleRight = "|"

  override val singleHorizontal = "-"

  override val singleIntersect = "+"

  override val doubleVerticalSingleLeft = "|"

  override val doubleHorizontalSingleUp = "-"

}