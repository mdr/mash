package com.github.mdr.mash.repl.history

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.{ LineBuffer, ReplState }

/**
  * @param resultIndexOpt -- when there are multiple results that match the searchString, the user can select through them.
  *                       This records the index of which result the user is currently viewing (if any).
  */
case class IncrementalHistorySearchState(searchString: String = "", resultIndexOpt: Option[Int] = None)

object IncrementalHistorySearchActionHandler {

  case class Result(newState: ReplState, actionConsumed: Boolean = true)

}

case class IncrementalHistorySearchActionHandler(history: History) {

  import IncrementalHistorySearchActionHandler._

  def beginIncrementalSearch(state: ReplState): ReplState = {
    history.resetHistoryPosition()
    state.copy(
      assistanceStateOpt = None,
      historySearchStateOpt = Some(IncrementalHistorySearchState()))
  }

  def handleAction(action: InputAction, state: ReplState): Result =
    state.historySearchStateOpt match {
      case Some(IncrementalHistorySearchState(searchString, resultIndexOpt)) ⇒
        action match {
          case SelfInsert(c)            ⇒ updateSearch(searchString + c, resultIndexOpt = None, state)
          case BackwardDeleteChar       ⇒ handleDeleteChar(searchString, state)
          case AcceptLine               ⇒ Result(state.copy(historySearchStateOpt = None))
          case IncrementalHistorySearch ⇒ updateSearch(searchString, resultIndexOpt, state)
          case _                        ⇒ exitSearchAndHandleNormally(action, state)
        }
      case None                                                              ⇒
        Result(state, actionConsumed = false)
    }

  private def handleDeleteChar(searchString: String, state: ReplState): Result =
    searchString match {
      case ""                            ⇒
        Result(state.copy(historySearchStateOpt = None))
      case _ if searchString.length == 1 ⇒
        Result(state.copy(lineBuffer = LineBuffer.Empty, historySearchStateOpt = Some(IncrementalHistorySearchState(""))))
      case _                             ⇒
        updateSearch(searchString.init, resultIndexOpt = None, state)
    }

  private def updateSearch(searchString: String, resultIndexOpt: Option[Int], state: ReplState): Result = {
    val nextResultIndex = resultIndexOpt.map(_ + 1).getOrElse(0)
    val nextHitOpt = history.findMatches(searchString).distinct.drop(nextResultIndex).headOption
    val nextState = nextHitOpt match {
      case Some(nextHit) ⇒
        state.copy(
          lineBuffer = LineBuffer(nextHit),
          historySearchStateOpt = Some(IncrementalHistorySearchState(searchString, Some(nextResultIndex))))
      case None          ⇒
        state.copy(lineBuffer = LineBuffer.Empty)
    }
    Result(nextState)
  }

  private def exitSearchAndHandleNormally(action: InputAction, state: ReplState): Result =
    Result(state.copy(historySearchStateOpt = None), actionConsumed = false)

}