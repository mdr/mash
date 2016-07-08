package com.github.mdr.mash.repl

import com.github.mdr.mash.utils.LineInfo

object CursorPos {

  def apply(rowAndColumn: (Int, Int)): CursorPos = CursorPos(rowAndColumn._1, rowAndColumn._2)

}

case class CursorPos(row: Int = 0, column: Int = 0)

object LineBuffer {

  val Empty = LineBuffer("", cursorPos = CursorPos())

  def apply(s: String): LineBuffer = LineBuffer(s, s.size)

  def apply(text: String, offset: Int): LineBuffer =
    LineBuffer(text, CursorPos(new LineInfo(text).lineAndColumn(offset)))

}

/**
 * The text entered by a user while editing a command
 *
 * @param text -- the contents of the buffer
 * @param cursorPos -- position of the cursor, either within the text, or one position past the end.
 */
case class LineBuffer(text: String, cursorPos: CursorPos) {

  private val lineInfo = new LineInfo(text)

  { // Sanity checks
    val CursorPos(row, column) = cursorPos
    require(row >= 0 && row < lineInfo.lineCount,
      s"Cursor row out of range: cursor row = $row, total rows = ${lineInfo.lineCount}")
    val cursorLineLength = lineInfo.lineLength(row)
    require(column >= 0 && cursorPos.column <= cursorLineLength,
      s"Cursor column out of range: cursor column = $column, total columns = $cursorLineLength")
  }

  val cursorOffset = lineInfo.offset(cursorPos.row, cursorPos.column)

  def isEmpty = text.isEmpty

  def onLastLine = cursorPos.row == lineInfo.lineCount - 1

  def cursorAtEnd = cursorOffset == text.length
  
  def isMultiline = lineInfo.lineCount > 1
  
  def deleteForwardWord: LineBuffer =
    copy(text = text.substring(0, cursorOffset) + text.substring(forwardWord.cursorOffset))

  def deleteBackwardWord: LineBuffer =
    LineBuffer(text.substring(0, backwardWord.cursorOffset) + text.substring(cursorOffset), backwardWord.cursorOffset)

  def forwardWord: LineBuffer = {
    var offset = cursorOffset
    if (offset >= text.length)
      return this
    while (offset < text.length && !text(offset).isLetterOrDigit)
      offset += 1
    if (offset >= text.length)
      return withCursorOffset(text.length)
    while (offset < text.length && text(offset).isLetterOrDigit)
      offset += 1
    withCursorOffset(offset)
  }

  private def withCursorOffset(offset: Int) = copy(cursorPos = CursorPos(lineInfo.lineAndColumn(offset)))

  private def withCursorColumn(column: Int) = copy(cursorPos = cursorPos.copy(column = column))

  def backwardWord: LineBuffer = {
    var offset = cursorOffset
    if (offset <= 0)
      return this
    offset -= 1
    while (offset > 0 && (offset == text.length || !text(offset).isLetterOrDigit))
      offset -= 1
    if (offset <= 0)
      return withCursorColumn(0)
    while (offset >= 0 && text(offset).isLetterOrDigit)
      offset -= 1
    offset += 1
    offset = math.min(offset, text.length)
    withCursorOffset(offset)
  }

  def backspace: LineBuffer =
    if (cursorOffset <= 0)
      this
    else
      LineBuffer(text.substring(0, cursorOffset - 1) + text.substring(cursorOffset), cursorOffset - 1)

  def delete: LineBuffer =
    if (cursorOffset >= text.length)
      this
    else
      delete(cursorOffset)

  def moveCursorToStart: LineBuffer = withCursorColumn(0)

  def moveCursorToEnd: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    withCursorColumn(line.size)
  }

  def up: LineBuffer =
    cursorPos.row match {
      case 0 ⇒ this
      case row ⇒
        val newRow = row - 1
        val line = lineInfo.line(newRow)
        val newColumn =
          if (cursorPos.column > line.length)
            line.length
          else
            cursorPos.column
        copy(cursorPos = CursorPos(newRow, newColumn))
    }

  def down: LineBuffer =
    if (onLastLine)
      this
    else {
      val newRow = cursorPos.row + 1
      val line = lineInfo.line(newRow)
      val newColumn =
        if (cursorPos.column > line.length)
          line.length
        else
          cursorPos.column
      copy(cursorPos = CursorPos(newRow, newColumn))
    }

  def deleteToEndOfLine: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    val newLine = line.substring(0, cursorPos.column)
    copy(text = lineInfo.replaceLine(cursorPos.row, newLine))
  }

  def addCharacterAtCursor(c: Char): LineBuffer =
    insertCharacters(c.toString, cursorOffset)

  def addCharactersAtCursor(chars: String): LineBuffer =
    insertCharacters(chars, cursorOffset)

  def insertCharacters(chars: String, insertPos: Int): LineBuffer = {
    val newText = text.substring(0, insertPos) + chars + text.substring(insertPos)
    val newCursorOffset =
      if (cursorOffset < insertPos)
        cursorOffset
      else
        cursorOffset + chars.length
    LineBuffer(newText, newCursorOffset)
  }

  def delete(deletePos: Int): LineBuffer = {
    val newText = text.substring(0, deletePos) + text.substring(deletePos + 1)
    val newCursorOffset =
      if (cursorOffset <= deletePos)
        cursorOffset
      else
        cursorOffset - 1
    LineBuffer(newText, newCursorOffset)
  }

  def cursorLeft: LineBuffer =
    if (cursorOffset <= 0)
      this
    else
      LineBuffer(text, cursorOffset - 1)

  def cursorRight: LineBuffer =
    if (cursorOffset >= text.length)
      this
    else
      LineBuffer(text, cursorOffset + 1)

}