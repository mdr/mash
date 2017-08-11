package com.github.mdr.mash.utils

import scala.annotation.tailrec

class LineInfo(s: String) {

  private val lineStarts: Seq[Int] = findLineStarts(0, Seq(0))

  /**
    * Lines exclude newline material
    */
  val lines: Seq[String] = s.split("""\r?\n""", -1)

  /**
    * @return contents of the line, excluding newline material
    */
  def line(lineIndex: Int): String = lines(lineIndex)

  def lineCount: Int = lines.size

  /**
    * @return the length of the given line (excluding newline material)
    */
  def lineLength(line: Int): Int = lines(line).length

  @tailrec
  private def findLineStarts(pos: Int, lineStarts: Seq[Int]): Seq[Int] =
    if (pos >= s.length)
      lineStarts
    else
      findLineStarts(pos + 1, if (s(pos) == '\n') lineStarts :+ (pos + 1) else lineStarts)

  def lineStart(line: Int): Int = lineStarts(line)

  /**
    * @return the position immediately after the end of the line (including newline material)
    */
  def lineEnd(line: Int): Int = if (line >= lines.size - 1) s.length else lineStarts(line + 1)

  /**
    * @return the region of the text covered by the given line (excluding the newline material)
    */
  def lineRegion(line: Int): Region = {
    val offset = lineStarts(line)
    Region(offset, lineLength(line))
  }

  /**
    * @return the regions of the text covered by all lines (excluding newline material)
    */
  def lineRegions: Seq[Region] =
    for (i ← 0 until lineCount)
      yield lineRegion(i)

  def replaceLine(lineIndex: Int, newLine: String): String =
    (lines.take(lineIndex) ++ Seq(newLine) ++ lines.drop(lineIndex + 1)).mkString("\n")

  /**
    * @return number of the line (0-indexed) containing the given pos in the original string
    */
  def lineAndColumn(pos: Int): Point = {
    val line = Utils.indexWhere[Int](lineStarts, _ > pos).getOrElse(lineStarts.length) - 1
    val column = pos - lineStarts(line)
    Point(line, column)
  }

  /**
    * @return pair of the first and last line index that cover the region
    */
  def linesOfRegion(region: Region): (Int, Int) = {
    val firstLine = lineAndColumn(region.offset).row
    val lastLine = lineAndColumn(region.lastPos).row
    (firstLine, lastLine)
  }

  def offset(lineIndex: Int, column: Int): Int =
    lines.take(lineIndex).map(_.length + 1).sum + column

}