package com.github.mdr.mash.screen

import com.github.mdr.mash.screen.Style.StylableString

object KeyHint {

  val Exit = KeyHint("q", "exit")
  val Focus = KeyHint("f", "focus")
  val Back = KeyHint("b", "back")
  val Insert = KeyHint("i", "insert")
  val InsertWhole = KeyHint("I", "insert whole")
  val Mark = KeyHint("m", "mark")
  val Row = KeyHint("r", "row")
  val Tree = KeyHint("t", "tree")
  val Table = KeyHint("t", "table")
  val HideColumn = KeyHint("h", "hide col")
  val Search = KeyHint("/", "find")
  val Expression = KeyHint("e", "expr")

  val NextHit = KeyHint("^n", "next")
  val PreviousHit = KeyHint("^p", "prev")
  val DoneSearch = KeyHint("ret", "done")
  val CaseSensitive = KeyHint("^t", "case sensitive")
  val CaseInsensitive = KeyHint("^t", "ignore case")

  def renderKeyHints(hints: Seq[KeyHint]): StyledString = {
    def renderHint(hint: KeyHint) = hint.key.style(hintStyle) + " ".style + hint.description.style
    StyledString.mkString(hints.map(renderHint), ", ".style)
  }

  val hintStyle = Style(inverse = true, foregroundColour = Colour.Cyan)

}

case class KeyHint(key: String, description: String)
