package com.github.mdr.mash.view.printer

import com.ibm.icu.text.DecimalFormat

object BytesPrinter {

  private val KB = 1024
  private val MB = 1024 * KB
  private val GB = 1024 * MB

  def humanReadable(n: Double): String = {
    val f = new DecimalFormat
    f.setMaximumSignificantDigits(3)
    if (n < KB)
      f.format(n) + "B"
    else if (n < MB) {
      val kb = n / KB
      f.format(kb) + "KB"
    } else if (n < GB) {
      val mb = n / MB
      f.format(mb) + "MB"
    } else {
      val gb = n / GB
      f.format(gb) + "GB"
    }
  }

}