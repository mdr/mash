package com.github.mdr.mash.input

import com.github.mdr.mash.input.InputSequence._
import com.github.mdr.mash.input.Key._
import com.github.mdr.mash.repl.IncrementalHistorySearchActions.{ ChangeDirectory, FirstHit, LastHit, ToggleCurrentDirOnly }
import com.github.mdr.mash.repl.NormalActions
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.input.KeyDsl._

object NormalKeyMap extends KeyMap(LineBufferKeyMap.map ++ Map(
  alt('w') → Copy,
  control('y') → Paste,
  control('p') → NormalActions.Up,
  KeyPress(Key.Up) → NormalActions.Up,
  control('n') → NormalActions.Down,
  KeyPress(Key.Down) → NormalActions.Down,
  KeyPress(Tab, shift = true) → BackwardComplete,
  KeyPress(Space, shift = true) → AssistInvocation,
  control('r') → IncrementalHistorySearch,
  KeyPress(Key.PageUp) → NormalActions.PageUp,
  KeyPress(Key.PageDown) → NormalActions.PageDown,
  alt('.') → InsertLastArg,
  alt(',') → ToggleMish,
  control('v') → BrowseLastResult,
  control('_') → Undo,
  alt('_') → Redo))

object IncrementalHistorySearchKeyMap extends KeyMap(Map(
  control('p') → NormalActions.Up,
  control('n') → NormalActions.Down,
  KeyPress(Key.Up) → NormalActions.Up,
  KeyPress(Key.Down) → NormalActions.Down,
  control('r') → IncrementalHistorySearch,
  KeyPress(Backspace) → BackwardDeleteChar,
  KeyPress(Key.Enter) → NormalActions.Enter,
  control('d') → ChangeDirectory,
  alt('d') → ToggleCurrentDirOnly,
  control('g') → Quit,
  KeyPress(Home) → LastHit,
  KeyPress(End) → FirstHit,
  alt('<') → LastHit,
  alt('>') → FirstHit))

object LineBufferKeyMap extends KeyMap(Map(
  alt(Key.Enter) → Newline,
  alt('e') → Inline,
  control('l') → RedrawScreen,
  KeyPress(Tab) → Complete,
  KeyPress(Key.Enter) → NormalActions.Enter,
  control('a') → BeginningOfLine,
  KeyPress(Home) → BeginningOfLine,
  control('e') → EndOfLine,
  KeyPress(End) → EndOfLine,
  shift(Home) → BeginningOfLineExtendingSelection,
  shift(End) → EndOfLineExtendingSelection,
  control('f') → ForwardChar,
  KeyPress(Right) → ForwardChar,
  control('b') → BackwardChar,
  KeyPress(Left) → BackwardChar,
  shift(Right) → ForwardCharExtendingSelection,
  shift(Left) → BackwardCharExtendingSelection,
  shift(Key.Down) → DownExtendingSelection,
  shift(Key.Up) → UpExtendingSelection,
  alt('f') → ForwardWord,
  alt('b') → BackwardWord,
  altShift('f') → ForwardWordExtendingSelection,
  altShift('b') → BackwardWordExtendingSelection,
  altShift(Key.Right) → ForwardWordExtendingSelection,
  altShift(Key.Left) → BackwardWordExtendingSelection,
  control('d') → DeleteChar,
  KeyPress(Delete) → DeleteChar,
  KeyPress(Backspace) → BackwardDeleteChar,
  control('k') → KillLine,
  control('u') → BackwardKillLine,
  alt('d') → KillWord,
  alt(Backspace) → BackwardKillWord,
  control('w') → BackwardKillWord,
  control('q') → ToggleQuote,
  alt(Key.Up) → ExpandSelection,
  alt(Key.Down) → UnexpandSelection,
  control('g') → Quit,
  alt('<') → BeginningOfBuffer,
  alt('>') → EndOfBuffer))