package com.github.mdr.mash.printer.model

import com.github.mdr.mash.printer.ColumnId
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

case class ObjectTableCell(data: String, cellValueOpt: Option[MashValue] = None)

case class ObjectTableRow(rowValue: MashObject,
                          cells: Map[ColumnId, ObjectTableCell])

case class ObjectTableColumn(name: String, width: Int)

case class ObjectsTableModel(columnIds: Seq[ColumnId],
                             columns: Map[ColumnId, ObjectTableColumn],
                             objects: Seq[ObjectTableRow],
                             tableValue: MashList) extends PrintModel {

  def columnName(columnId: ColumnId): String = columns(columnId).name

  def width = columns.values.map(_.width).sum + numberOfColumns + 1

  def numberOfColumns = columns.size

  def numberOfRows = objects.size

  def columnWidth(columnId: ColumnId): Int = columns(columnId).width

  def rowValues = objects.map(_.rowValue)

  def rowValue(row: Int): MashObject = rowValues(row)
}
