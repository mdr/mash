package com.github.mdr.mash.printer

import com.github.mdr.mash.printer.model.{ ObjectTableRow, ObjectsTableModel }
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils
import org.fusesource.jansi.Ansi

class ObjectsTableStringifier(terminalInfo: TerminalInfo, showSelections: Boolean = false) {

  private val boxCharacterSupplier: BoxCharacterSupplier = UnicodeBoxCharacterSupplier
  import boxCharacterSupplier._

  def renderTopRow(model: ObjectsTableModel): String = {
    val sb = new StringBuilder()
    sb.append(doubleTopLeft)
    if (showSelections)
      sb.append(doubleHorizontal + doubleHorizontalSingleDown)
    sb.append(model.columnNames.map(name ⇒ doubleHorizontal * model.columnWidth(name)).mkString(doubleHorizontalSingleDown))
    sb.append(doubleTopRight)
    sb.toString
  }

  def renderHeaderRow(model: ObjectsTableModel): String = {
    def renderColumn(name: String) = {
      val fit = StringUtils.fitToWidth(name, model.columnWidth(name))
      Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a(fit).reset()
    }
    val sb = new StringBuilder()
    sb.append(doubleVertical)
    if (showSelections)
      sb.append(" " + singleVertical)
    sb.append(model.columnNames.map(renderColumn).mkString(singleVertical))
    sb.append(doubleVertical)
    sb.toString
  }

  def renderBelowHeaderRow(model: ObjectsTableModel): String = {
    val sb = new StringBuilder()
    sb.append(doubleVerticalSingleRight)
    if (showSelections)
      sb.append(singleHorizontal + singleIntersect)
    sb.append(model.columnNames.map(name ⇒ singleHorizontal * model.columnWidth(name)).mkString(singleIntersect))
    sb.append(doubleVerticalSingleLeft)
    sb.toString
  }

  def renderObjectRow(model: ObjectsTableModel, renderedObject: ObjectTableRow): String = {
    def renderCell(name: String) = StringUtils.fitToWidth(renderedObject.data(name), model.columnWidth(name))
    new StringBuilder()
      .append(doubleVertical)
      .append(model.columnNames.map(renderCell).mkString(singleVertical))
      .append(doubleVertical)
      .toString
  }

  def renderBottomRow(model: ObjectsTableModel): String = {
    val sb = new StringBuilder
    sb.append(doubleBottomLeft)
    if (showSelections)
      sb.append(doubleHorizontal + doubleHorizontalSingleUp)
    sb.append(model.columnNames.map(name ⇒ doubleHorizontal * model.columnWidths(name)).mkString(doubleHorizontalSingleUp))
    sb.append(doubleBottomRight)
    sb.toString
  }

}