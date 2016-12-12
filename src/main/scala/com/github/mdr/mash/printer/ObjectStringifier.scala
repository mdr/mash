package com.github.mdr.mash.printer

import com.github.mdr.mash.printer.model.ObjectModel
import com.github.mdr.mash.terminal.TerminalInfo

class ObjectStringifier(terminalInfo: TerminalInfo) {

  private val boxCharacterSupplier: BoxCharacterSupplier = UnicodeBoxCharacterSupplier

  import boxCharacterSupplier._

  def renderTopRow(model: ObjectModel): String =
    new StringBuilder()
      .append(doubleTopLeft)
      .append(doubleHorizontal * model.fieldColumnWidth)
      .append(doubleHorizontalSingleDown)
      .append(doubleHorizontal * model.valueColumnWidth)
      .append(doubleTopRight)
      .toString

  def renderFieldRow(renderedField: String, renderedValue: String): String =
    new StringBuilder()
      .append(doubleVertical)
      .append(renderedField)
      .append(singleVertical)
      .append(renderedValue)
      .append(doubleVertical)
      .toString

  def renderBottomRow(model: ObjectModel): String =
    new StringBuilder()
      .append(doubleBottomLeft)
      .append(doubleHorizontal * model.fieldColumnWidth)
      .append(doubleHorizontalSingleUp)
      .append(doubleHorizontal * model.valueColumnWidth)
      .append(doubleBottomRight)
      .toString

}