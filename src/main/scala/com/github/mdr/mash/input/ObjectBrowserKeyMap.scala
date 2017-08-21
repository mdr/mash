package com.github.mdr.mash.input

import com.github.mdr.mash.input.InputSequence._
import com.github.mdr.mash.input.Key._
import com.github.mdr.mash.repl.NormalActions.{AssistInvocation, Paste, RedrawScreen}
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.ExpressionInput._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.IncrementalSearch._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{NextParentItem, _}
import com.github.mdr.mash.input.KeyDsl._
import com.github.mdr.mash.repl.NormalActions

object ObjectBrowserKeyMap extends KeyMap(Map(
  control('l') → RedrawScreen,
  KeyPress(Right) → NextColumn,
  KeyPress(Left) → PreviousColumn,
  control('f') → NextColumn,
  control('b') → PreviousColumn,
  KeyPress(Down) → NextItem,
  control('e') → FirstColumn,
  control('a') → LastColumn,
  OtherSequence("R") → UnfocusColumn,
  OtherSequence("f") → Focus,
  KeyPress(Enter) → Focus,
  OtherSequence("d") → FocusDirectory,
  OtherSequence("r") → ReadFile,
  OtherSequence("b") → Back,
  control('n') → NextItem,
  KeyPress(Up) → PreviousItem,
  control('p') → PreviousItem,
  KeyPress(PageUp) → PreviousPage,
  KeyPress(PageDown) → NextPage,
  OtherSequence(" ") → NextPage,
  OtherSequence("q") → ExitBrowser,
  OtherSequence("g") → FirstItem,
  KeyPress(Home) → FirstItem,
  OtherSequence("G") → LastItem,
  KeyPress(End) → LastItem,
  OtherSequence("m") → ToggleMarked,
  OtherSequence("i") → InsertItem,
  OtherSequence("I") → InsertWholeItem,
  OtherSequence("t") → ViewAsTree,
  OtherSequence("1") → View1D,
  OtherSequence("2") → View2D,
  OtherSequence("h") → HideColumn,
  OtherSequence("o") → Open,
  OtherSequence("c") → Copy,
  OtherSequence("/") → BeginSearch,
  OtherSequence("e") → BeginExpression,
  OtherSequence("N") → NextParentItem,
  OtherSequence("P") → PreviousParentItem,
  TerminalWindowChanged → Rerender)) {

  object ExpressionInput extends KeyMap(LineBufferKeyMap.map ++ Map(
    alt('w') → NormalActions.Copy,
    shift(Space) → AssistInvocation,
    control('y') → Paste))

  object IncrementalSearch extends KeyMap(Map(
    control('l') → RedrawScreen,
    TerminalWindowChanged → Rerender,
    KeyPress(Enter) → ExitSearch,
    KeyPress(Backspace) → Unsearch,
    control('t') → ToggleCase,
    KeyPress(Down) → NextHit,
    control('n') → NextHit,
    KeyPress(Up) → PreviousHit,
    control('p') → PreviousHit))

}