package com.github.mdr.mash.input

import InputSequence._
import com.github.mdr.mash.input.Key._
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.NormalActions

object NormalKeyMap extends KeyMap(Map(
  KeyPress(BasicKey('l'), control = true) -> ClearScreen,
  KeyPress(Enter) -> AcceptLine,
  KeyPress(BasicKey('a'), control = true) -> BeginningOfLine,
  KeyPress(Home) -> BeginningOfLine,
  KeyPress(BasicKey('e'), control = true) -> EndOfLine,
  KeyPress(End) -> EndOfLine,
  KeyPress(BasicKey('f'), control = true) -> ForwardChar,
  KeyPress(Right) -> ForwardChar,
  KeyPress(BasicKey('b'), control = true) -> BackwardChar,
  KeyPress(Left) -> BackwardChar,
  KeyPress(BasicKey('f'), alt = true) -> ForwardWord,
  KeyPress(BasicKey('b'), alt = true) -> BackwardWord,
  KeyPress(BasicKey('p'), control = true) -> PreviousHistory,
  KeyPress(Up) -> PreviousHistory,
  KeyPress(BasicKey('n'), control = true) -> NextHistory,
  KeyPress(Down) -> NextHistory,
  KeyPress(BasicKey('d'), control = true) -> DeleteChar,
  KeyPress(Delete) -> DeleteChar,
  KeyPress(Backspace) -> BackwardDeleteChar,
  KeyPress(BasicKey('k'), control = true) -> KillLine,
  KeyPress(BasicKey('d'), alt = true) -> KillWord,
  KeyPress(Backspace, alt = true) -> BackwardKillWord,
  KeyPress(Tab) -> Complete,
  KeyPress(Tab, shift = true) -> BackwardComplete,
  KeyPress(Space, shift = true) -> AssistInvocation,
  KeyPress(BasicKey('r'), control = true) -> IncrementalHistorySearch,
  KeyPress(Key.PageUp) -> NormalActions.PageUp,
  KeyPress(Key.PageDown) -> NormalActions.PageDown,
  KeyPress(BasicKey('.'), alt = true) -> YankLastArg,
  KeyPress(BasicKey(','), alt = true) -> ToggleMish,
  KeyPress(BasicKey('q'), control = true) -> ToggleQuote))
