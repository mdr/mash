package com.github.mdr.mash.screen

case class Point(row: Int, column: Int) {

  def up(rows: Int = 1): Point = copy(row = row - rows)

  def down(rows: Int = 1): Point = copy(row = row + rows)

}
