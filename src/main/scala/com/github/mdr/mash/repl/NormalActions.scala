package com.github.mdr.mash.repl

import com.github.mdr.mash.input.InputAction

object NormalActions {

  case class SelfInsert(s: String) extends InputAction
  case object RedrawScreen extends InputAction
  case object Enter extends InputAction
  case object BeginningOfLine extends InputAction
  case object EndOfLine extends InputAction
  case object ForwardChar extends InputAction
  case object BackwardChar extends InputAction
  case object ForwardCharExtendingSelection extends InputAction
  case object BackwardCharExtendingSelection extends InputAction
  case object ForwardWord extends InputAction
  case object BackwardWord extends InputAction
  case object ForwardWordExtendingSelection extends InputAction
  case object BackwardWordExtendingSelection extends InputAction
  case object Up extends InputAction
  case object Down extends InputAction
  case object DeleteChar extends InputAction
  case object EndOfFile extends InputAction
  case object BackwardDeleteChar extends InputAction
  case object KillLine extends InputAction
  case object BackwardKillLine extends InputAction
  case object KillWord extends InputAction
  case object BackwardKillWord extends InputAction
  case object Complete extends InputAction
  case object BackwardComplete extends InputAction
  case object Noop extends InputAction
  case object AssistInvocation extends InputAction
  case object IncrementalHistorySearch extends InputAction
  case object AbandonHistorySearch extends InputAction
  case object PageUp extends InputAction
  case object PageDown extends InputAction
  case object InsertLastArg extends InputAction
  case object ToggleQuote extends InputAction
  case object ExpandSelection extends InputAction
  case object ToggleMish extends InputAction
  case object BrowseLastResult extends InputAction
  case object Inline extends InputAction

}

object IncrementalHistorySearchActions {
  case object ChangeDirectory extends InputAction

}