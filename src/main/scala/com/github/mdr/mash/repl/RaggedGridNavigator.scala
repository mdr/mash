package com.github.mdr.mash.repl

case class RaggedGridNavigator(total: Int, columns: Int, pos: Int) {
  private val rows = math.ceil((total / columns.toDouble)).toInt
  private val row = pos / columns
  private val column = pos % columns
  private val rowLength = math.min(total - (row * columns), columns)

  def next = (pos + 1) % total

  def previous = (pos - 1 + total) % total

  def right = {
    val newColumn = (column + 1) % rowLength
    row * columns + newColumn
  }

  def left = {
    val newColumn = (column - 1 + rowLength) % rowLength
    row * columns + newColumn
  }

  def up = {
    var nextPos = pos - columns
    if (nextPos < 0)
      nextPos += total - (total % columns) + columns
    if (nextPos >= total)
      nextPos = nextPos - columns
    nextPos
  }

  def down = {
    var nextPos = pos + columns
    if (nextPos >= total)
      nextPos = nextPos % (total - (total % columns)) % columns
    nextPos
  }

}