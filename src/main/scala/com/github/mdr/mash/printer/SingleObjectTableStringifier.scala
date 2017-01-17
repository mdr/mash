package com.github.mdr.mash.printer

import com.github.mdr.mash.printer.model.SingleObjectTableModel
import com.github.mdr.mash.terminal.TerminalInfo

class SingleObjectTableStringifier(terminalInfo: TerminalInfo) {

  private val boxCharacterSupplier: BoxCharacterSupplier = UnicodeBoxCharacterSupplier

  import boxCharacterSupplier._

  def renderTopRow(model: SingleObjectTableModel, break: Boolean = true): String =
    new StringBuilder()
      .append(doubleTopLeft)
      .append(doubleHorizontal * model.fieldColumnWidth)
      .append(if (break) doubleHorizontalSingleDown else doubleHorizontal)
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

  def renderBottomRow(model: SingleObjectTableModel): String =
    new StringBuilder()
      .append(doubleBottomLeft)
      .append(doubleHorizontal * model.fieldColumnWidth)
      .append(doubleHorizontalSingleUp)
      .append(doubleHorizontal * model.valueColumnWidth)
      .append(doubleBottomRight)
      .toString

}