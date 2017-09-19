package com.github.mdr.mash.terminal

import java.io.{ ByteArrayOutputStream, PrintStream }

import com.github.mdr.mash.screen._
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.CharUtils.Esc
import com.github.mdr.mash.utils.{ Dimensions, Point }
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

class InMemoryTerminal(size: Dimensions) {

  private val lines = ArrayBuffer[ArrayBuffer[StyledCharacter]]()
  private var wrappedLineIndices: Set[Int] = Set()

  val outputStream = new ByteArrayOutputStream

  private var cursorPos = Point(0, 0)
  private var cursorVisible = true
  private var style = Style.Default
  private var titleOpt: Option[String] = None

  private val CursorUp = s"(?s)$Esc\\[(\\d+)A(.*)".r
  private val CursorForward = s"(?s)$Esc\\[(\\d+)C(.*)".r
  private val CursorBackward = s"(?s)$Esc\\[(\\d+)D(.*)".r
  private val HideCursor = s"(?s)$Esc\\[\\?25l(.*)".r
  private val ShowCursor = s"(?s)$Esc\\[\\?25h(.*)".r
  private val Reset = s"(?s)$Esc\\[0m(.*)".r
  private val MoveCursorToTopLeft = s"(?s)$Esc\\[H(.*)".r
  private val SingleChar = "(?s)(.)(.*)".r
  private val SetTitle = s"(?s)$Esc]0;(.+?)\u0007(.*)".r

  def print(s: String) = new PrintStream(outputStream).print(s)

  def consume() {
    var contents = outputStream.toString
    outputStream.reset()
    while (contents.nonEmpty)
      contents match {
        case SetTitle(title, rest)           ⇒
          titleOpt = Some(title)
          contents = rest
        case HideCursor(rest)                ⇒
          cursorVisible = false
          contents = rest
        case ShowCursor(rest)                ⇒
          cursorVisible = true
          contents = rest
        case CursorUp(n, rest)               ⇒
          cursorPos = cursorPos.up(n.toInt)
          contents = rest
        case CursorForward(n, rest)          ⇒
          cursorPos = cursorPos.right(n.toInt)
          contents = rest
        case CursorBackward(n, rest)         ⇒
          cursorPos = cursorPos.left(n.toInt)
          contents = rest
        case Reset(rest)                     ⇒
          style = Style.Default
          contents = rest
        case MoveCursorToTopLeft(rest)       ⇒
          cursorPos = Point(0, 0)
          contents = rest
        case s if s.startsWith("\n")         ⇒
          println("New line")
          contents = s.tail
          ???
        case s if s.startsWith("\r")         ⇒
          cursorPos = cursorPos.copy(column = 0)
          contents = s.tail
        case SingleChar(c, rest)             ⇒
          val writePos =
            if (cursorPos.column == size.columns) {
              wrappedLineIndices += cursorPos.row
              cursorPos.down().copy(column = 0)
            } else
              cursorPos
          val diff = writePos.row + 1 - lines.size
          if (diff > 0) {
            val newLines = List.fill(diff)(ArrayBuffer[StyledCharacter]())
            lines ++= newLines
          }
          val line = lines(writePos.row)
          val diff2 = writePos.column + 1 - line.length
          if (diff2 > 0)
            line ++= List.fill(diff2)(' '.style)
          line(writePos.column) = c.head.style(style)
          cursorPos = writePos.right()
          contents = rest
        case s if s.startsWith(Esc.toString) ⇒
          throw new IllegalArgumentException(s"Unknown escape sequence: ${makeReadable(s)}")
      }
  }

  private def makeReadable(s: String) =
    s.replaceAll(Esc.toString, "^[").replaceAll("\r", "\\r").replaceAll("\n", "\\n")

  def screen: Screen = {
    val cursorPosOpt = cursorVisible.option(cursorPos)
    val screenLines = lines.zipWithIndex.map { case (chars, index) ⇒
      Line(StyledString(chars), endsInNewline = !(wrappedLineIndices contains index))
    }
    Screen(lines = screenLines, cursorPosOpt, title = titleOpt getOrElse "")
  }

}
