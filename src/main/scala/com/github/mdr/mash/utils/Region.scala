package com.github.mdr.mash.utils

object Region {

  def apply(range: Range): Region = Region(range.start, range.length)

}

case class Region(offset: Int, length: Int) {
  require(length >= 0, s"Length must not be negative (length = $length, offset = $offset)")
  require(offset >= 0, s"Offset must not be negative (offset = $offset, length = $length)")

  def contains(pos: Int) = offset <= pos && pos < offset + length

  def posAfter = offset + length

  def lastPos = offset + length - 1

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
    val offset = math.min(this.offset, that.offset)
    val posAfter = math.max(this.posAfter, that.posAfter)
    Region(offset, posAfter - offset)
  }

  def range = offset until posAfter
  
}

/**
 * A region with a highlighted point
 */
case class PointedRegion(point: Int, region: Region) {

  def this(point: Int, offset: Int, length: Int) = this(point, Region(offset, length))

  def contains(pos: Int) = region contains pos

  def posAfter = region.posAfter

  def merge(that: PointedRegion): PointedRegion = copy(region = this.region merge that.region)

}