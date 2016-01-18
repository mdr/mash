package com.github.mdr.mash.utils

import java.text.DecimalFormat

object NumberUtils {

  /**
   * Will only Print decimal part only if non-integer
   */
  private val DoubleFormat = new DecimalFormat("##.##################################################")

  def prettyString(d: Double) = DoubleFormat.format(d)

}