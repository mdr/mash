package com.github.mdr.mash.utils

case class Point(row: Int, column: Int) {

  def left(columns: Int = 1): Point = copy(column = column - columns)

  def right(columns: Int = 1): Point = copy(column = column + columns)

  def up(rows: Int = 1): Point = copy(row = row - rows)

  def down(rows: Int = 1): Point = copy(row = row + rows)

}
