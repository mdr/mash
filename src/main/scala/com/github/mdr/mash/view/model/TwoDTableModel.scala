package com.github.mdr.mash.view.model

import com.github.mdr.mash.view.model.TwoDTableModel._
import com.github.mdr.mash.printer.{ ColumnId, ValueFetch }
import com.github.mdr.mash.runtime.MashValue

object TwoDTableModel {

  val RowLabelColumnId = ColumnId(-1)

  case class Row(cells: Map[ColumnId, Cell], rawValue: MashValue, fetch: ValueFetch) {

    def renderedValue(columnId: ColumnId): String = cells(columnId).renderedValue

  }

  case class Column(name: String, width: Int, fetchOpt: Option[ValueFetch] = None) {
    require(width >= 0, s"width must be non-negative, but was $width")
  }

  case class Cell(renderedValue: String, rawValueOpt: Option[MashValue] = None)

}

case class TwoDTableModel(columnIds: Seq[ColumnId],
                          columns: Map[ColumnId, Column],
                          rows: Seq[Row],
                          rawValue: MashValue) extends DisplayModel {

  def columnName(columnId: ColumnId): String = columns(columnId).name

  def width = columns.values.map(_.width).sum + numberOfColumns + 1

  def numberOfColumns = columns.size

  def numberOfRows = rows.size

  def columnWidth(columnId: ColumnId): Int = columns(columnId).width

  def rowValues = rows.map(_.rawValue)

  def rowValue(row: Int): MashValue = rowValues(row)

  def rowFetch(row: Int): ValueFetch = rows(row).fetch

}
