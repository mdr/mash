package com.github.mdr.mash.printer

case class ObjectTableRow(data: Map[String, String])

case class ObjectTableModel(columnNames: Seq[String], columnWidths: Map[String, Int], objects: Seq[ObjectTableRow]) {

  def width = columnWidths.values.sum + numberOfColumns + 1

  def numberOfColumns = columnNames.size

  def columnWidth(columnName: String): Int = columnWidths(columnName)

}
