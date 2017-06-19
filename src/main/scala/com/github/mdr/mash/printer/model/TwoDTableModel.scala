package com.github.mdr.mash.printer.model

import com.github.mdr.mash.printer.{ ColumnFetch, ColumnId }
import com.github.mdr.mash.runtime.{ MashList, MashValue }
import com.github.mdr.mash.printer.model.TwoDTableModel._

object TwoDTableModel {

  case class Row(cells: Map[ColumnId, Cell], rawValue: MashValue) {

    def renderedValue(columnId: ColumnId): String = cells(columnId).renderedValue

  }

  case class Column(name: String, width: Int, fetchOpt: Option[ColumnFetch] = None)

  case class Cell(renderedValue: String, rawValueOpt: Option[MashValue] = None)

}

case class TwoDTableModel(columnIds: Seq[ColumnId],
                          columns: Map[ColumnId, Column],
                          rows: Seq[Row],
                          rawValue: MashValue) extends PrintModel {

  def columnName(columnId: ColumnId): String = columns(columnId).name

  def width = columns.values.map(_.width).sum + numberOfColumns + 1

  def numberOfColumns = columns.size

  def numberOfRows = rows.size

  def columnWidth(columnId: ColumnId): Int = columns(columnId).width

  def rowValues = rows.map(_.rawValue)

  def rowValue(row: Int): MashValue = rowValues(row)

}
