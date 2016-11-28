package com.github.mdr.mash.input

import com.github.mdr.mash.input.InputSequence._
import com.github.mdr.mash.input.Key._
import com.github.mdr.mash.repl.ObjectBrowserActions._

object ObjectBrowserKeyMap extends KeyMap(Map(
  KeyPress(Right) -> NextColumn,
  KeyPress(Left) -> PreviousColumn,
  KeyPress(BasicKey('f'), control = true) -> NextColumn,
  KeyPress(BasicKey('b'), control = true) -> PreviousColumn,
  KeyPress(Down) -> NextItem,
  KeyPress(BasicKey('e'), control = true) -> FirstColumn,
  KeyPress(BasicKey('a'), control = true) -> LastColumn,
  OtherSequence("r") -> UnfocusColumn,
  OtherSequence("f") -> Focus,
  KeyPress(Enter) -> Focus,
  OtherSequence("b") -> Back,
  KeyPress(BasicKey('n'), control = true) -> NextItem,
  KeyPress(Up) -> PreviousItem,
  KeyPress(BasicKey('p'), control = true) -> PreviousItem,
  KeyPress(PageUp) -> PreviousPage,
  KeyPress(PageDown) -> NextPage,
  OtherSequence(" ") -> NextPage,
  OtherSequence("q") -> ExitBrowser,
  OtherSequence("g") -> FirstItem,
  OtherSequence("G") -> LastItem,
  OtherSequence("m") -> ToggleMarked,
  OtherSequence("i") -> InsertItem,
  OtherSequence("t") -> ViewAsTree,
  TerminalWindowChanged -> Rerender))