package com.github.mdr.mash.render

  import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ BasicColour, Style, StyledString }

object KeyHint {

  val Exit = KeyHint("q", "exit")
  val Focus = KeyHint("f", "focus")
  val Back = KeyHint("b", "back")
  val Insert = KeyHint("i", "insert")
  val InsertWhole = KeyHint("I", "insert whole")
  val Mark = KeyHint("m", "mark")
  val Row = KeyHint("R", "row")
  val Tree = KeyHint("t", "tree")
  val Table = KeyHint("t", "table")
  val HideColumn = KeyHint("h", "hide col")
  val Search = KeyHint("/", "find")
  val Expression = KeyHint("e", "expr")
  val Dir = KeyHint("d", "dir")
  val Read = KeyHint("r", "read")
  val Open = KeyHint("o", "open")
  val Copy = KeyHint("c", "copy")

  val NextHit = KeyHint("^N", "next")
  val PreviousHit = KeyHint("^P", "prev")
  val CaseSensitive = KeyHint("^T", "case sensitive")
  val CaseInsensitive = KeyHint("^T", "ignore case")
  val DoneSearch = KeyHint("ret", "done")
  val Quit = KeyHint("^G", "quit")

  val NextHistoryHit = KeyHint("Up", "next")
  val ChangeDirectory = KeyHint("^D", "cd")

  def renderKeyHint(hint: KeyHint): StyledString =
    hint.key.style(hintStyle) + " ".style + hint.description.style

  def renderKeyHints(hints: Seq[KeyHint]): StyledString =
    StyledString.join(hints.map(renderKeyHint), ", ".style)

  val hintStyle = Style(inverse = true, foregroundColour = BasicColour.Cyan)

}

case class KeyHint(key: String, description: String)
