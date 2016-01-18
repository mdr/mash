package com.github.mdr.mash.utils

case class Region(offset: Int, length: Int) {

  def contains(pos: Int) = offset <= pos && pos < offset + length

  def posAfter = offset + length

  def lastPos = offset + length - 1

  def of(s: String): String = s.substring(offset, offset + length)

  def translate(n: Int) = copy(offset = offset + n)

  def replace(s: String, replacement: String) = StringUtils.replace(s, this, replacement)

}

/**
 * A region with a highlighted point
 */
case class PointedRegion(point: Int, region: Region) {

  def this(point: Int, offset: Int, length: Int) = this(point, Region(offset, length))

  def contains(pos: Int) = region contains pos

  def posAfter = region.posAfter

}