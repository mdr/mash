package com.github.mdr.mash.input

import com.github.mdr.mash.input.InputSequence._
import com.github.mdr.mash.input.Key._
import com.github.mdr.mash.repl.NormalActions.{ AssistInvocation, RedrawScreen }
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.ExpressionInput._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.IncrementalSearch._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ NextParentItem, _ }

object ObjectBrowserKeyMap extends KeyMap(Map(
  KeyPress(BasicKey('l'), control = true) -> RedrawScreen,
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
  OtherSequence("d") -> FocusDirectory,
  OtherSequence("r") -> ReadFile,
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
  OtherSequence("I") -> InsertWholeItem,
  OtherSequence("t") -> ViewAsTree,
  OtherSequence("1") -> View1D,
  OtherSequence("2") -> View2D,
  OtherSequence("h") -> HideColumn,
  OtherSequence("o") -> Open,
  OtherSequence("c") -> Copy,
  OtherSequence("/") -> BeginSearch,
  OtherSequence("e") -> BeginExpression,
  OtherSequence("N") -> NextParentItem,
  OtherSequence("P") -> PreviousParentItem,
  TerminalWindowChanged -> Rerender)) {

  object ExpressionInput extends KeyMap(LineBufferKeyMap.map ++ Map(
    KeyPress(Space, shift = true) -> AssistInvocation))

  object IncrementalSearch extends KeyMap(Map(
    KeyPress(Enter) -> ExitSearch,
    KeyPress(Backspace) -> Unsearch,
    KeyPress(BasicKey('t'), control = true) -> ToggleCase,
    KeyPress(Down) -> NextHit,
    KeyPress(BasicKey('n'), control = true) -> NextHit,
    KeyPress(Up) -> PreviousHit,
    KeyPress(BasicKey('p'), control = true) -> PreviousHit))

}