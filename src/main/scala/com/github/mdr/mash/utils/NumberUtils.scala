package com.github.mdr.mash.utils

import java.text.DecimalFormat

import scala.util.Try

object NumberUtils {

  /**
   * Will only Print decimal part only if non-integer
   */
  private val DoubleFormat = new DecimalFormat("##.##################################################")

  def prettyString(d: Double) = DoubleFormat.format(d)

  def asIntOpt(s: String): Option[Int] = Try(s.toInt).toOption

  def isInt(s: String): Boolean = asIntOpt(s).isDefined
}