package com.github.mdr.mash.utils

object Region {

  def zeroWidth(offset: Int): Region = Region(offset, length = 0)

  def apply(range: Range): Region = Region(range.start, range.length)

  def fromStartEnd(start: Int, end: Int) = Region(start, end - start)
}

case class Region(offset: Int, length: Int) {
  require(length >= 0, s"Length must not be negative (length = $length, offset = $offset)")
  require(offset >= 0, s"Offset must not be negative (offset = $offset, length = $length)")

  def contains(pos: Int): Boolean = offset <= pos && pos < offset + length

  def contains(that: Region): Boolean = this.contains(that.offset) && (that.isEmpty || this.contains(that.lastPos))

  def posAfter = offset + length

  def lastPos = offset + length - 1 // maybe this should be optional (for 0-length regions)

  def of(s: String): String = s.substring(offset, offset + length)

  def of[T](xs: Seq[T]): Seq[T] = xs.slice(offset, offset + length)

  def translate(n: Int) = copy(offset = offset + n)

  def replace(s: String, replacement: String) = StringUtils.replace(s, this, replacement)

  def grow(n: Int) = Region(offset, length + n)

  def overlaps(that: Region): Boolean =
    if (this.length == 0) that contains this.offset
    else if (that.length == 0) this contains that.offset
    else !(this.lastPos < that.offset || that.lastPos < this.offset)

  def merge(that: Region): Region = {
    val offset = this.offset min that.offset
    val posAfter = this.posAfter max that.posAfter
    Region(offset, posAfter - offset)
  }

  def range = offset until posAfter

  def isEmpty: Boolean = length == 0

}

/**
  * A region with a highlighted point
  */
case class PointedRegion(point: Int, region: Region) {

  def this(point: Int, offset: Int, length: Int) = this(point, Region(offset, length))

  def of(s: String): String = region.of(s)

  def contains(pos: Int) = region contains pos

  def posAfter = region.posAfter

  def merge(that: PointedRegion): PointedRegion = copy(region = this.region merge that.region)

  def movePoint(delta: Int) = copy(point = point + delta)

}