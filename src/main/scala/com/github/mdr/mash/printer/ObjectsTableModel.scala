package com.github.mdr.mash.printer

import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

case class ObjectTableRow(data: Map[String, String], rawObjects: Map[String, MashValue])

case class ObjectsTableModel(
    columnNames: Seq[String],
    columnWidths: Map[String, Int],
    objects: Seq[ObjectTableRow],
    rawValue: MashList,
    rawObjects: Seq[MashObject]) extends PrintModel {

  def width = columnWidths.values.sum + numberOfColumns + 1

  def numberOfColumns = columnNames.size
  def numberOfRows = objects.size

  def columnWidth(columnName: String): Int = columnWidths(columnName)

}
