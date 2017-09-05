package com.github.mdr.mash.view.render

import com.github.mdr.mash.input.InputSequence.KeyPress
import com.github.mdr.mash.input.{ Key, KeyDescriber }
import com.github.mdr.mash.input.Key.BasicKey
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ BasicColour, DefaultColours, Style, StyledString }
import com.github.mdr.mash.input.KeyDsl._

object KeyHint {

  val Exit = KeyHint(BasicKey('q'), "exit")
  val Focus = KeyHint(BasicKey('f'), "focus")
  val Back = KeyHint(BasicKey('b'), "back")
  val Insert = KeyHint(BasicKey('i'), "insert")
  val InsertWhole = KeyHint(BasicKey('I'), "insert whole")
  val Mark = KeyHint(BasicKey('m'), "mark")
  val Row = KeyHint(BasicKey('R'), "row")
  val Tree = KeyHint(BasicKey('t'), "tree")
  val Table = KeyHint(BasicKey('t'), "table")
  val HideColumn = KeyHint(BasicKey('h'), "hide col")
  val Search = KeyHint(BasicKey('/'), "find")
  val Expression = KeyHint(BasicKey('e'), "expr")
  val Dir = KeyHint(BasicKey('d'), "dir")
  val Read = KeyHint(BasicKey('r'), "read")
  val Open = KeyHint(BasicKey('o'), "open")
  val Copy = KeyHint(BasicKey('c'), "copy")
  val NextParentResult = KeyHint(BasicKey('N'), "next prnt")
  val PreviousParentResult = KeyHint(BasicKey('P'), "prev prnt")
  
  val NextHit = KeyHint(control('n'), "next")
  val PreviousHit = KeyHint(control('p'), "prev")
  val CaseSensitive = KeyHint(control('t'), "case sensitive")
  val CaseInsensitive = KeyHint(control('t'), "ignore case")
  val DoneSearch = KeyHint(Key.Enter, "done")
  val Quit = KeyHint(control('g'), "quit")

  val NextHistoryHit = KeyHint(Key.Up, "next")
  val ChangeDirectory = KeyHint(control('d'), "cd")
  val ThisDirOnly = KeyHint(alt('d'), "this dir only")
  val AllDirs = KeyHint(alt('d'), "all dirs")

  def renderKeyHint(hint: KeyHint): StyledString =
    KeyDescriber.describe(hint.keyPress).style(hintStyle) + " ".style + hint.description.style

  def renderKeyHints(hints: Seq[KeyHint]): StyledString =
    StyledString.join(hints.map(renderKeyHint), ", ".style)

  val hintStyle = Style(inverse = true, foregroundColour = DefaultColours.Cyan)

}

case class KeyHint(keyPress: KeyPress, description: String)
