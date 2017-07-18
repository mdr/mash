package com.github.mdr.mash.repl.history

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.{ LineBuffer, ReplState }

/**
  * Incremental history search state.
  *
  * @param resultIndex -- when there are multiple results that match the searchString, the user can select through them.
  *                    This records which result the user is currently viewing.
  */
case class HistorySearchState(searchString: String = "", resultIndex: Int = 0)

object HistorySearchActionHandler {

  case class Result(newState: ReplState, actionConsumed: Boolean = true)

}

case class HistorySearchActionHandler(history: History) {

  import HistorySearchActionHandler._

  def begin(state: ReplState): ReplState = {
    history.resetHistoryPosition()
    state.copy(
      assistanceStateOpt = None,
      historySearchStateOpt = Some(HistorySearchState()))
  }

  def handleAction(action: InputAction, state: ReplState, searchState: HistorySearchState): Result = {
    val HistorySearchState(searchString, resultIndex) = searchState
    action match {
      case SelfInsert(c)            ⇒ updateSearch(searchString + c, resultIndex = 0, state)
      case BackwardDeleteChar       ⇒ handleDeleteChar(searchString, state)
      case AcceptLine               ⇒ Result(state.copy(historySearchStateOpt = None))
      case IncrementalHistorySearch ⇒ updateSearch(searchString, resultIndex + 1, state)
      case _                        ⇒ exitSearchAndHandleNormally(action, state)
    }
  }

  private def handleDeleteChar(searchString: String, state: ReplState): Result =
    searchString match {
      case "" ⇒ Result(state.copy(historySearchStateOpt = None))
      case _  ⇒ updateSearch(searchString.init, resultIndex = 0, state)
    }

  private def updateSearch(searchString: String, resultIndex: Int = 0, state: ReplState): Result = {
    val text =
      if (searchString.isEmpty) ""
      else history.findMatches(searchString).distinct.drop(resultIndex).headOption.getOrElse("")
    Result(state.copy(
      lineBuffer = LineBuffer(text),
      historySearchStateOpt = Some(HistorySearchState(searchString, resultIndex))))
  }

  private def exitSearchAndHandleNormally(action: InputAction, state: ReplState): Result =
    Result(state.copy(historySearchStateOpt = None), actionConsumed = false)

}