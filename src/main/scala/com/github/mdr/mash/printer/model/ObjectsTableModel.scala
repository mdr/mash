package com.github.mdr.mash.printer.model

import com.github.mdr.mash.printer.ColumnId
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

case class ObjectTableRow(data: Map[ColumnId, String],
                          rawObjects: Map[ColumnId, MashValue])

case class ObjectsTableModel(columnIds: Seq[ColumnId],
                             columnNames: Map[ColumnId, String],
                             columnWidths: Map[ColumnId, Int],
                             objects: Seq[ObjectTableRow],
                             wholeTableValue: MashList,
                             rawObjects: Seq[MashObject]) extends PrintModel {

  def columnName(columnId: ColumnId): String = columnNames(columnId)

  def width = columnWidths.values.sum + numberOfColumns + 1

  def numberOfColumns = columnNames.size

  def numberOfRows = objects.size

  def columnWidth(columnId: ColumnId): Int = columnWidths(columnId)

}
