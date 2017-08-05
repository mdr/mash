package com.github.mdr.mash.input

import com.github.mdr.mash.input.InputSequence._
import com.github.mdr.mash.input.Key._
import com.github.mdr.mash.repl.IncrementalHistorySearchActions.ChangeDirectory
import com.github.mdr.mash.repl.NormalActions
import com.github.mdr.mash.repl.NormalActions._

object NormalKeyMap extends KeyMap(LineBufferKeyMap.map ++ Map(
  KeyPress(BasicKey('t'), control = true) → Inline,
  KeyPress(BasicKey('p'), control = true) → NormalActions.Up,
  KeyPress(Key.Up) → NormalActions.Up,
  KeyPress(BasicKey('n'), control = true) → NormalActions.Down,
  KeyPress(Key.Down) → NormalActions.Down,
  KeyPress(Tab, shift = true) → BackwardComplete,
  KeyPress(Space, shift = true) → AssistInvocation,
  KeyPress(BasicKey('r'), control = true) → IncrementalHistorySearch,
  KeyPress(Key.PageUp) → NormalActions.PageUp,
  KeyPress(Key.PageDown) → NormalActions.PageDown,
  KeyPress(BasicKey('.'), alt = true) → InsertLastArg,
  KeyPress(BasicKey(','), alt = true) → ToggleMish,
  KeyPress(BasicKey('v'), control = true) → BrowseLastResult))

object IncrementalHistorySearchKeyMap extends KeyMap(Map(
  KeyPress(BasicKey('p'), control = true) → NormalActions.Up,
  KeyPress(Key.Up) → NormalActions.Up,
  KeyPress(BasicKey('n'), control = true) → NormalActions.Down,
  KeyPress(Key.Down) → NormalActions.Down,
  KeyPress(BasicKey('r'), control = true) → IncrementalHistorySearch,
  KeyPress(Backspace) → BackwardDeleteChar,
  KeyPress(Key.Enter) → NormalActions.Enter,
  KeyPress(BasicKey('d'), control = true) → ChangeDirectory,
  KeyPress(BasicKey('q'), control = true) → AbandonHistorySearch))

object LineBufferKeyMap extends KeyMap(Map(
  KeyPress(BasicKey('l'), control = true) → RedrawScreen,
  KeyPress(Tab) → Complete,
  KeyPress(Key.Enter) → NormalActions.Enter,
  KeyPress(BasicKey('a'), control = true) → BeginningOfLine,
  KeyPress(Home) → BeginningOfLine,
  KeyPress(BasicKey('e'), control = true) → EndOfLine,
  KeyPress(End) → EndOfLine,
  KeyPress(BasicKey('f'), control = true) → ForwardChar,
  KeyPress(Right) → ForwardChar,
  KeyPress(BasicKey('b'), control = true) → BackwardChar,
  KeyPress(Left) → BackwardChar,
  KeyPress(Right, shift = true) → ForwardCharExtendingSelection,
  KeyPress(Left, shift = true) → BackwardCharExtendingSelection,
  KeyPress(BasicKey('f'), alt = true) → ForwardWord,
  KeyPress(BasicKey('b'), alt = true) → BackwardWord,
  KeyPress(BasicKey('f'), alt = true, shift = true) → ForwardWordExtendingSelection,
  KeyPress(BasicKey('b'), alt = true, shift = true) → BackwardWordExtendingSelection,
  KeyPress(BasicKey('d'), control = true) → DeleteChar,
  KeyPress(Delete) → DeleteChar,
  KeyPress(Backspace) → BackwardDeleteChar,
  KeyPress(BasicKey('k'), control = true) → KillLine,
  KeyPress(BasicKey('u'), control = true) → BackwardKillLine,
  KeyPress(BasicKey('d'), alt = true) → KillWord,
  KeyPress(Backspace, alt = true) → BackwardKillWord,
  KeyPress(BasicKey('q'), control = true) → ToggleQuote,
  KeyPress(BasicKey('y'), control = true) → ExpandSelection))