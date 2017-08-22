package com.github.mdr.mash.input

import com.github.mdr.mash.input.InputSequence._
import com.github.mdr.mash.input.Key._
import com.github.mdr.mash.repl.NormalActions.{ AssistInvocation, Paste, Quit, RedrawScreen }
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.ExpressionInput._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.IncrementalSearch._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ NextParentItem, _ }
import com.github.mdr.mash.input.KeyDsl._
import com.github.mdr.mash.repl.NormalActions

object ObjectBrowserKeyMap extends KeyMap(Map(
  control('l') → RedrawScreen,
  KeyPress(Right) → NextColumn,
  KeyPress(Left) → PreviousColumn,
  control('f') → NextColumn,
  control('b') → PreviousColumn,
  control('e') → FirstColumn,
  control('a') → LastColumn,
  control('p') → PreviousItem,
  control('n') → NextItem,
  KeyPress(Up) → PreviousItem,
  KeyPress(Down) → NextItem,
  KeyPress(PageUp) → PreviousPage,
  KeyPress(PageDown) → NextPage,
  OtherSequence(" ") → NextPage,
  OtherSequence("q") → ExitBrowser,
  control('g') → ExitBrowser,
  OtherSequence("g") → FirstItem,
  OtherSequence("G") → LastItem,
  KeyPress(Home) → FirstItem,
  KeyPress(End) → LastItem,
  alt('<') → FirstItem,
  alt('>') → LastItem,
  OtherSequence("N") → NextParentItem,
  OtherSequence("P") → PreviousParentItem,
  OtherSequence("R") → UnfocusColumn,
  OtherSequence("f") → Focus,
  KeyPress(Enter) → Focus,
  OtherSequence("d") → FocusDirectory,
  OtherSequence("r") → ReadFile,
  OtherSequence("b") → Back,
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
  TerminalWindowChanged → Rerender)) {

  object ExpressionInput extends KeyMap(LineBufferKeyMap.map ++ Map(
    alt('w') → NormalActions.Copy,
    shift(Space) → AssistInvocation,
    control('y') → Paste,
    TerminalWindowChanged → Rerender))

  object IncrementalSearch extends KeyMap(Map(
    control('l') → RedrawScreen,
    TerminalWindowChanged → Rerender,
    KeyPress(Enter) → ExitSearch,
    control('g') → ExitSearch,
    KeyPress(Backspace) → Unsearch,
    control('t') → ToggleCase,
    KeyPress(Up) → PreviousHit,
    KeyPress(Down) → NextHit,
    control('p') → PreviousHit,
    control('n') → NextHit))

}