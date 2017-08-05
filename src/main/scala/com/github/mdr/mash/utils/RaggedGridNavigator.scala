package com.github.mdr.mash.utils

/**
 * Helper class to calculate motion with a ragged grid, including wrapping around the edges.
 *
 * For example, moving down in the last (complete) row:
 *
 * ▢▢▢▢      ▢▢▣▢
 * ▢▢▼▢  ⇒  ▢▢▢▢
 * ▢▢         ▢▢
 *
 * @param total - total number of elements in the grid
 * @param columns - number of columns in the grid
 * @param pos - item number in the grid (elements are numbered left to right, wrapping to the next line when necessary)
 *
 */
case class RaggedGridNavigator(total: Int, columns: Int, pos: Int) {
  private val row = pos / columns
  private val column = pos % columns
  private val rowLength = total - (row * columns) min columns

  def next: Int = (pos + 1) % total

  def previous: Int = (pos - 1 + total) % total

  def right: Int = {
    val newColumn = (column + 1) % rowLength
    row * columns + newColumn
  }

  def left: Int = {
    val newColumn = (column - 1 + rowLength) % rowLength
    row * columns + newColumn
  }

  def up: Int = {
    var nextPos = pos - columns
    if (nextPos < 0)
      nextPos += total - (total % columns) + columns
    if (nextPos >= total)
      nextPos = nextPos - columns
    nextPos
  }

  def down: Int = {
    var nextPos = pos + columns
    if (nextPos >= total)
      nextPos = nextPos % (total - (total % columns)) % columns
    nextPos
  }

}
