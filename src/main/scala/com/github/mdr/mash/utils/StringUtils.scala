package com.github.mdr.mash.utils

import java.util.regex.Pattern

import com.github.mdr.mash.utils.Utils._

object StringUtils {

  /**
    * Insert an ellipsis in the middle of the given string so as to avoid the string exceeding maxLength.
    * For example:
    *
    * ellipsisiseMiddle("1234567890", maxLength = 9) should equal ("123...890")
    *
    * @param maxLength of the string
    */
  def ellipsisiseMiddle(s: String, maxLength: Int) = {
    if (s.size > maxLength)
      if (s.size < 5)
        "." * maxLength
      else {
        val excess = s.size - maxLength + 3
        val (left, right) = s.splitAt(s.size / 2)
        val rightHeavy = s.size % 2 == 1
        val (leftTrim, rightTrim) =
          if (rightHeavy)
            (halveRoundingDown(excess), halveRoundingUp(excess))
          else
            (halveRoundingUp(excess), halveRoundingDown(excess))
        val leftHalf = left.dropRight(leftTrim)
        val rightHalf = right.drop(rightTrim.toInt)
        s"$leftHalf...$rightHalf"
      }
    else
      s
  }

  private def halveRoundingDown(n: Int) = n / 2

  private def halveRoundingUp(n: Int) = math.ceil(n / 2.0).toInt

  /**
    * Ensure the given string is at most maxLength characters long (including 0), by truncating and adding …
    */
  def ellipsisise(s: String, maxLength: Int): String =
    if (s.size < 2 && s.size > maxLength)
      "…" * maxLength
    else {
      val excess = s.length - maxLength
      if (excess <= 0)
        s
      else
        s.take(maxLength - 1) + "…"
    }

  def centre(s: String, width: Int): String =
    if (s.length >= width)
      s
    else {
      val gap = width - s.length
      val half = gap.toDouble / 2
      " " * math.floor(half).toInt + s + " " * math.ceil(half).toInt
    }

  def replace(s: String, region: Region, replacement: String): String =
    new StringBuilder(s).replace(region.offset, region.posAfter, replacement).toString

  def insert(s: String, pos: Int, insertion: String): String =
    new StringBuilder(s).insert(pos, insertion).toString

  /**
    * Pad or truncate the given string to ensure it is exactly the given width in length.
    */
  def fitToWidth(s: String, width: Int): String =
    StringUtils.ellipsisise(s.padTo(width, " ").mkString, width)

  def commonPrefix(s1: String, s2: String): String =
    s1.zip(s2).takeWhile { case (x, y) ⇒ x == y }.map(_._1).mkString

  /**
    * Remove initial whitespace
    */
  def ltrim(s: String) = s.replaceAll("^\\s+", "")

  def splitIntoLines(s: String): Seq[String] =
    if (s.isEmpty)
      Seq()
    else
      s.split("\r?\n", -1).when(s endsWith "\n", _.init)

  private val LineStartPattern = Pattern.compile("^", Pattern.MULTILINE)

  /**
    * Indent every line of the given string by the given number of spaces
    */
  def indent(s: String, spaces: Int): String = LineStartPattern.matcher(s).replaceAll(" " * spaces)

}