package com.github.mdr.mash.input
import com.github.mdr.mash.repl.ObjectBrowserActions._
import InputSequence._
import com.github.mdr.mash.input.Key._

object ObjectBrowserKeyMap extends KeyMap(Map(
  KeyPress(Down) -> NextItem,
  KeyPress(BasicKey('n'), control = true) -> NextItem,
  KeyPress(Up) -> PreviousItem,
  KeyPress(BasicKey('p'), control = true) -> PreviousItem,
  KeyPress(PageUp) -> PreviousPage,
  KeyPress(PageDown) -> NextPage,
  OtherSequence(" ") -> NextPage,
  OtherSequence("q") -> ExitBrowser,
  OtherSequence("g") -> FirstItem,
  OtherSequence("G") -> LastItem,
  OtherSequence("s") -> ToggleSelected,
  OtherSequence("i") -> InsertItem,
  TerminalWindowChanged -> Rerender))