package com.github.mdr.mash.utils

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

class LineInfo(s: String) {

  private val lineStarts = findLineStarts(0, Seq(0))

  val lines: Seq[String] = s.split("""\r?\n""")

  def line(lineIndex: Int): String = lines(lineIndex)
  
  def lineCount: Int = lines.size
  
  def lineLength(line: Int): Int = lineRegion(line).length
  
  @tailrec
  private def findLineStarts(pos: Int, lineStarts: Seq[Int]): Seq[Int] =
    if (pos >= s.length)
      lineStarts
    else
      findLineStarts(pos + 1, if (s(pos) == '\n') lineStarts :+ (pos + 1) else lineStarts)

  def lineStart(line: Int) = lineStarts(line)

  def lineEnd(line: Int) = if (line >= lines.size - 1) s.length else lineStarts(line + 1)

  def lineRegion(line: Int) = {
    val offset = lineStarts(line)
    val offsetAfter = lineEnd(line)
    Region(offset, offsetAfter - offset)
  }

  def replaceLine(lineIndex: Int, newLine: String): String =
    (lines.take(lineIndex) ++ Seq(newLine) ++ lines.drop(lineIndex + 1)).mkString("\n")
  
  /**
   * @return number of the line (0-indexed) containing the given pos in the original string
   */
  def lineAndColumn(pos: Int): (Int, Int) = {
    val line = Utils.indexWhere[Int](lineStarts, _ > pos).getOrElse(lineStarts.length) - 1
    val column = pos - lineStarts(line)
    (line, column)
  }

  def offset(lineIndex: Int, column: Int): Int =
    lines.take(lineIndex).map(_.length + 1).sum + column
  
}