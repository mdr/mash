package com.github.mdr.mash.utils

case class Dimension(rows: Int, columns: Int) {

  def shrink(rows: Int = 0, columns: Int = 0) = Dimension(this.rows - rows, this.columns - columns)

}
