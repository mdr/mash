package com.github.mdr.mash

object LineBuffer {

  val Empty = LineBuffer("", 0)

  def apply(s: String): LineBuffer = LineBuffer(s, s.size)

}

/**
 * The text entered by a user while editing a command
 */
case class LineBuffer(s: String, cursorPos: Int) {
  require(cursorPos >= 0 && cursorPos <= s.length)

  def isEmpty = s.isEmpty

  def deleteForwardWord: LineBuffer =
    copy(s = s.substring(0, cursorPos) + s.substring(forwardWord.cursorPos))

  def deleteBackwardWord: LineBuffer =
    copy(s = s.substring(0, backwardWord.cursorPos) + s.substring(cursorPos), backwardWord.cursorPos)

  def forwardWord: LineBuffer = {
    var pos = cursorPos
    if (pos >= s.length)
      return this
    while (pos < s.length && !s(pos).isLetterOrDigit)
      pos += 1
    if (pos >= s.length)
      return LineBuffer(s)
    while (pos < s.length && s(pos).isLetterOrDigit)
      pos += 1
    return LineBuffer(s, pos)
  }

  def backwardWord: LineBuffer = {
    var pos = cursorPos
    if (pos <= 0)
      return this
    pos -= 1
    while (pos > 0 && (pos == s.length || !s(pos).isLetterOrDigit))
      pos -= 1
    if (pos <= 0)
      return LineBuffer(s, 0)
    while (pos >= 0 && s(pos).isLetterOrDigit)
      pos -= 1
    pos += 1
    pos = math.min(pos, s.length)
    return LineBuffer(s, pos)
  }

  def backspace: LineBuffer =
    if (cursorPos <= 0)
      this
    else
      LineBuffer(s.substring(0, cursorPos - 1) + s.substring(cursorPos), cursorPos - 1)

  def delete: LineBuffer =
    if (cursorPos >= s.length)
      this
    else LineBuffer(s.substring(0, cursorPos) + s.substring(cursorPos + 1), cursorPos)

  def moveCursorToStart: LineBuffer = copy(cursorPos = 0)

  def moveCursorToEnd: LineBuffer = copy(cursorPos = s.size)

  def deleteToEndOfLine: LineBuffer = copy(s.substring(0, cursorPos))

  def addCharacterAtCursor(c: Char): LineBuffer =
    insertCharacters(c.toString, cursorPos)

  def addCharactersAtCursor(chars: String): LineBuffer =
    insertCharacters(chars, cursorPos)

  def insertCharacters(chars: String, insertPos: Int): LineBuffer = {
    val newText = s.substring(0, insertPos) + chars + s.substring(insertPos)
    val newCursorPos =
      if (cursorPos < insertPos)
        cursorPos
      else
        cursorPos + chars.length
    LineBuffer(newText, newCursorPos)
  }

  def cursorLeft: LineBuffer = if (cursorPos <= 0) this else copy(cursorPos = cursorPos - 1)

  def cursorRight: LineBuffer = if (cursorPos >= s.length) this else copy(cursorPos = cursorPos + 1)

}