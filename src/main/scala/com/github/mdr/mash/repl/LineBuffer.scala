package com.github.mdr.mash.repl

object LineBuffer {

  val Empty = LineBuffer("", cursorPos = 0)

  def apply(s: String): LineBuffer = LineBuffer(s, s.size)

}

/**
 * The text entered by a user while editing a command
 */
case class LineBuffer(text: String, cursorPos: Int) {
  require(cursorPos >= 0 && cursorPos <= text.length,
    s"Cursor out of range: cursor = $cursorPos, length = ${text.length}")

  def isEmpty = text.isEmpty

  def deleteForwardWord: LineBuffer =
    copy(text = text.substring(0, cursorPos) + text.substring(forwardWord.cursorPos))

  def deleteBackwardWord: LineBuffer =
    copy(text = text.substring(0, backwardWord.cursorPos) + text.substring(cursorPos), backwardWord.cursorPos)

  def forwardWord: LineBuffer = {
    var pos = cursorPos
    if (pos >= text.length)
      return this
    while (pos < text.length && !text(pos).isLetterOrDigit)
      pos += 1
    if (pos >= text.length)
      return LineBuffer(text)
    while (pos < text.length && text(pos).isLetterOrDigit)
      pos += 1
    return LineBuffer(text, pos)
  }

  def backwardWord: LineBuffer = {
    var pos = cursorPos
    if (pos <= 0)
      return this
    pos -= 1
    while (pos > 0 && (pos == text.length || !text(pos).isLetterOrDigit))
      pos -= 1
    if (pos <= 0)
      return LineBuffer(text, 0)
    while (pos >= 0 && text(pos).isLetterOrDigit)
      pos -= 1
    pos += 1
    pos = math.min(pos, text.length)
    return LineBuffer(text, pos)
  }

  def backspace: LineBuffer =
    if (cursorPos <= 0)
      this
    else
      LineBuffer(text.substring(0, cursorPos - 1) + text.substring(cursorPos), cursorPos - 1)

  def delete: LineBuffer =
    if (cursorPos >= text.length)
      this
    else
      delete(cursorPos)

  def moveCursorToStart: LineBuffer = copy(cursorPos = 0)

  def moveCursorToEnd: LineBuffer = copy(cursorPos = text.size)

  def deleteToEndOfLine: LineBuffer = copy(text.substring(0, cursorPos))

  def addCharacterAtCursor(c: Char): LineBuffer =
    insertCharacters(c.toString, cursorPos)

  def addCharactersAtCursor(chars: String): LineBuffer =
    insertCharacters(chars, cursorPos)

  def insertCharacters(chars: String, insertPos: Int): LineBuffer = {
    val newText = text.substring(0, insertPos) + chars + text.substring(insertPos)
    val newCursorPos =
      if (cursorPos < insertPos)
        cursorPos
      else
        cursorPos + chars.length
    LineBuffer(newText, newCursorPos)
  }

  def delete(deletePos: Int): LineBuffer = {
    val newText = text.substring(0, deletePos) + text.substring(deletePos + 1)
    val newCursorPos =
      if (cursorPos < deletePos)
        cursorPos
      else
        cursorPos - 1
    LineBuffer(newText, newCursorPos)
  }

  def cursorLeft: LineBuffer = if (cursorPos <= 0) this else copy(cursorPos = cursorPos - 1)

  def cursorRight: LineBuffer = if (cursorPos >= text.length) this else copy(cursorPos = cursorPos + 1)

}