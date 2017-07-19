package com.github.mdr.mash.utils

case class Dimensions(rows: Int, columns: Int) {

  def withColumns(columns: Int): Dimensions = copy(columns = columns)

  def withRows(rows: Int): Dimensions = copy(rows = rows)

  def shrink(rows: Int = 0, columns: Int = 0) = Dimensions(this.rows - rows, this.columns - columns)

}
