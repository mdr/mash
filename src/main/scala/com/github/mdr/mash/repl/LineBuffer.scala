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

  def deleteForwardWord: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    val newLine = line.substring(0, cursorPos.column) + text.substring(forwardWord.cursorPos.column)
    copy(text = lineInfo.replaceLine(cursorPos.row, newLine))
  }

  def deleteBackwardWord: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    val newLine = line.substring(0, backwardWord.cursorPos.column) + text.substring(cursorPos.column)
    copy(text = lineInfo.replaceLine(cursorPos.row, newLine), cursorPos = backwardWord.cursorPos)
  }

  def forwardWord: LineBuffer = {
    var offset = cursorOffset
    if (offset >= text.length)
      return this
    while (offset < text.length && !text(offset).isLetterOrDigit)
      offset += 1
    if (offset >= text.length)
      return withCursorColumn(text.length)
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
    withCursorColumn(offset)
  }

  def backspace: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    val column = cursorPos.column
    if (column == 0)
      this
    else {
      val newLine = line.substring(0, column - 1) + line.substring(column)
      copy(text = lineInfo.replaceLine(cursorPos.row, newLine), cursorPos = cursorPos.copy(column = column - 1))
    }
  }

  def delete: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    val column = cursorPos.column
    if (column >= line.length)
      this
    else
      delete(cursorPos)
  }

  def moveCursorToStart: LineBuffer = withCursorColumn(0)

  def moveCursorToEnd: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    withCursorColumn(line.size)
  }

  def deleteToEndOfLine: LineBuffer = {
    val line = lineInfo.line(cursorPos.row)
    val newLine = line.substring(0, cursorPos.column)
    copy(text = lineInfo.replaceLine(cursorPos.row, newLine))
  }

  def addCharacterAtCursor(c: Char): LineBuffer =
    insertCharacters(c.toString, cursorPos)

  def addCharactersAtCursor(chars: String): LineBuffer =
    insertCharacters(chars, cursorPos)

  def insertCharacters(chars: String, insertPos: CursorPos): LineBuffer = {
    val line = lineInfo.line(insertPos.row)
    val newLine = line.substring(0, insertPos.column) + chars + line.substring(insertPos.column)
    val newCursorPos =
      if (insertPos.row != cursorPos.row || cursorPos.column < insertPos.column)
        cursorPos
      else
        cursorPos.copy(column = insertPos.column + chars.length)
    LineBuffer(lineInfo.replaceLine(cursorPos.row, newLine), newCursorPos)
  }

  def delete(deletePos: CursorPos): LineBuffer = {
    val line = lineInfo.line(deletePos.row)
    val newLine = line.substring(0, deletePos.column) + line.substring(deletePos.column + 1)

    val newCursorPos =
      if (deletePos.row != cursorPos.row || cursorPos.column <= deletePos.column)
        cursorPos
      else
        cursorPos.copy(column = cursorPos.column - 1)
    LineBuffer(lineInfo.replaceLine(cursorPos.row, newLine), newCursorPos)
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