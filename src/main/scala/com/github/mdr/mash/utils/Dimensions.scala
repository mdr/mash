package com.github.mdr.mash.utils

case class Dimensions(rows: Int, columns: Int) {

  def shrink(rows: Int = 0, columns: Int = 0) = Dimensions(this.rows - rows, this.columns - columns)

}
