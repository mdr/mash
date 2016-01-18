package com.github.mdr.mash.printer

case class ObjectTableModel(columnNames: Seq[String], columnWidths: Map[String, Int], objects: Seq[Map[String, String]]) {

  def width = columnWidths.values.sum + numberOfColumns + 1

  def numberOfColumns = columnNames.size

  def columnWidth(columnName: String): Int = columnWidths(columnName)

}
