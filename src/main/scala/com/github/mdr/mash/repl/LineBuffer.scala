package com.github.mdr.mash.repl

import com.github.mdr.mash.utils.{ LineInfo, Point, Region }
import com.github.mdr.mash.utils.Utils._

object LineBuffer {

  val Empty = LineBuffer("")

  def apply(s: String): LineBuffer = LineBuffer(s, cursorOffset = s.size)

}

/**
  * The text entered by a user while editing a command
  *
  * @param text               -- the contents of the buffer
  * @param cursorOffset       -- position of the cursor, either within the text, ore one position past the end
  * @param selectionOffsetOpt -- if Some, the position of the start of the selected region
  */
case class LineBuffer(text: String,
                      cursorOffset: Int,
                      selectionOffsetOpt: Option[Int] = None) {
  require(cursorOffset >= 0 && cursorOffset <= text.length,
    s"Cursor offset out of range: offset = $cursorOffset, text length = ${text.length}")

  require(selectionOffsetOpt.forall(offset ⇒ offset >= 0 && offset <= text.length),
    s"Selection offset out of range: offset = $selectionOffsetOpt, text length = ${text.length}")

  require(!selectionOffsetOpt.contains(cursorOffset),
    s"Selection offset must not equal cursor offset: $cursorOffset")

  lazy val lineInfo = new LineInfo(text)

  lazy val cursorPos: Point = lineInfo.lineAndColumn(cursorOffset)

  def cursorRow = cursorPos.row

  def selectedRegionOpt: Option[Region] = selectionOffsetOpt.map(selectionOffset ⇒
    Region.fromStartEnd(cursorOffset min selectionOffset, cursorOffset max selectionOffset))

  def selectedOrCursorRegion: Region = selectedRegionOpt getOrElse Region.zeroWidth(cursorOffset)

  def selectedTextOpt: Option[String] = selectedRegionOpt.map(_ of text)

  def isEmpty = text.isEmpty

  def onFirstLine = cursorRow == 0

  def onLastLine = cursorRow == lineInfo.lineCount - 1

  def cursorAtEnd = cursorOffset == text.length

  def isMultiline = lineInfo.lineCount > 1

  def deleteForwardWord: LineBuffer = {
    val regionToDelete = selectedRegionOpt getOrElse Region.fromStartEnd(cursorOffset, forwardWordOffset)
    deleteRegion(regionToDelete)
  }

  def deleteBackwardWord: LineBuffer = {
    val regionToDelete = selectedRegionOpt getOrElse Region.fromStartEnd(backwardWordOffset, cursorOffset)
    deleteRegion(regionToDelete)
  }

  private def forwardWordOffset: Int = {
    var offset = cursorOffset
    if (offset >= text.length)
      return offset
    while (offset < text.length && !text(offset).isLetterOrDigit)
      offset += 1
    if (offset >= text.length)
      return offset
    while (offset < text.length && text(offset).isLetterOrDigit)
      offset += 1
    offset
  }

  def forwardWord(extendSelection: Boolean = false): LineBuffer = moveCursor(forwardWordOffset, extendSelection)

  private def backwardWordOffset: Int = {
    var offset = cursorOffset
    if (offset <= 0)
      return offset
    offset -= 1
    while (offset > 0 && (offset == text.length || !text(offset).isLetterOrDigit))
      offset -= 1
    if (offset <= 0)
      return offset
    while (offset >= 0 && text(offset).isLetterOrDigit)
      offset -= 1
    offset += 1
    offset min text.length
  }

  def backwardWord(extendSelection: Boolean = false): LineBuffer = moveCursor(backwardWordOffset, extendSelection)

  private def withCursorOffset(offset: Int) = copy(cursorOffset = offset, selectionOffsetOpt = None)

  private def withCursorColumn(column: Int) = withCursorOffset(lineInfo.offset(cursorPos.row, column))

  def backspace: LineBuffer = selectedRegionOpt match {
    case Some(selectedRegion) ⇒ deleteRegion(selectedRegion)
    case None                 ⇒ deleteRegion(Region.fromStartEnd(0 max cursorOffset - 1, cursorOffset))
  }

  def delete: LineBuffer = selectedRegionOpt match {
    case Some(selectedRegion) ⇒ deleteRegion(selectedRegion)
    case None                 ⇒
      if (cursorOffset >= text.length)
        this
      else
        delete(cursorOffset)
  }

  private def moveCursorToStartOfBuffer: LineBuffer = withCursorOffset(0)

  private def moveCursorToStartOfLine: LineBuffer = withCursorColumn(0)

  private def moveCursorToEndOfBuffer: LineBuffer = withCursorOffset(text.length)

  private def moveCursorToEndOfLine: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    withCursorColumn(line.length)
  }

  private def isAtStartOfLine: Boolean = lineInfo.lineStart(cursorPos.row) == cursorOffset

  private def isAtEndOfLine: Boolean = lineInfo.lineEnd(cursorPos.row) - 1 == cursorOffset

  def moveCursorToStart = if (isAtStartOfLine) moveCursorToStartOfBuffer else moveCursorToStartOfLine

  def moveCursorToEnd = if (isAtEndOfLine) moveCursorToEndOfBuffer else moveCursorToEndOfLine

  def deleteToEndOfLine: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    val newLine = line.substring(0, cursorPos.column)
    val newText = lineInfo.replaceLine(cursorPos.row, newLine)
    LineBuffer(newText, cursorOffset)
  }

  def deleteToBeginningOfLine: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    val newLine = line.substring(cursorPos.column)
    val newText = lineInfo.replaceLine(cursorPos.row, newLine)
    val newCursorOffset = cursorOffset - cursorPos.column
    LineBuffer(newText, newCursorOffset)
  }

  def replaceRegion(region: Region, replacement: String): LineBuffer = {
    val newText = text.substring(0, region.offset) + replacement + text.substring(region.posAfter)
    val newCursorOffset =
      if (cursorOffset < region.offset)
        cursorOffset
      else if (cursorOffset >= region.posAfter)
        cursorOffset - region.length + replacement.length
      else
        region.offset + replacement.length
    LineBuffer(newText, newCursorOffset)
  }

  private def deleteRegion(region: Region): LineBuffer = replaceRegion(region, replacement = "")

  def insertAtCursor(c: Char): LineBuffer = insertAtCursor(c.toString)

  def insertAtCursor(chars: String): LineBuffer = replaceRegion(selectedOrCursorRegion, chars)

  def insert(chars: String, offset: Int): LineBuffer = replaceRegion(Region.zeroWidth(offset), chars)

  def delete(deletePos: Int): LineBuffer = deleteRegion(Region(deletePos, 1))

  def cursorLeft(extendSelection: Boolean = false): LineBuffer = moveCursorLeftRightBy(-1, extendSelection)

  def cursorRight(extendSelection: Boolean = false): LineBuffer = moveCursorLeftRightBy(1, extendSelection)

  private def moveCursorLeftRightBy(delta: Int, extendSelection: Boolean = false): LineBuffer = {
    val newCursorOffset = 0 max (cursorOffset + delta) min text.length
    moveCursor(newCursorOffset, extendSelection)
  }

  private def moveCursor(newCursorOffset: Int, extendSelection: Boolean): LineBuffer = {
    val newSelectionOffsetOpt = extendSelection.option(selectionOffsetOpt getOrElse cursorOffset).filterNot(_ == newCursorOffset)
    copy(cursorOffset = newCursorOffset, selectionOffsetOpt = newSelectionOffsetOpt)
  }

  def cursorUp(extendSelection: Boolean = false): LineBuffer = moveCursorUpDownBy(-1, extendSelection)

  def cursorDown(extendSelection: Boolean = false): LineBuffer = moveCursorUpDownBy(1, extendSelection)

  private def moveCursorUpDownBy(delta: Int, extendSelection: Boolean): LineBuffer = {
    val newRow = 0 max cursorPos.row + delta min lineInfo.lineCount - 1
    val line = lineInfo.line(newRow)
    val newColumn = cursorPos.column min line.length
    val newCursorOffset = lineInfo.offset(newRow, newColumn)
    moveCursor(newCursorOffset, extendSelection)
  }

  override def toString = {
    val decoratedChars =
      text.zipWithIndex.map { case (c, i) ⇒ c → i.toDouble } ++
        Seq('▶' → (cursorOffset - 0.5)) ++
        selectionOffsetOpt.toSeq.map(i ⇒ '▷' → (i - 0.5))
    decoratedChars.sortBy(_._2).map(_._1).mkString
  }

  def withoutSelection: LineBuffer = copy(selectionOffsetOpt = None)

  def withSelection(region: Region): LineBuffer = {
    val newCursorOffset = region.posAfter
    val newSelectionOffsetOpt = Some(region.offset).filterNot(_ == newCursorOffset)
    LineBuffer(text, newCursorOffset, newSelectionOffsetOpt)
  }

}
