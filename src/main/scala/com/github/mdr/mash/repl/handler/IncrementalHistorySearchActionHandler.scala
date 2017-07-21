package com.github.mdr.mash.repl.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.IncrementalHistorySearchState._
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.history.History
import com.github.mdr.mash.repl.{ IncrementalHistorySearchState, LineBuffer, ReplState }

object IncrementalHistorySearchActionHandler {

  case class Result(newState: ReplState, actionConsumed: Boolean = true)

}

case class IncrementalHistorySearchActionHandler(history: History) {

  import IncrementalHistorySearchActionHandler._

  def beginIncrementalSearch(state: ReplState): ReplState = {
    history.resetHistoryPosition()
    state.copy(
      assistanceStateOpt = None,
      historySearchStateOpt = Some(IncrementalHistorySearchState(searchString = "", BeforeFirstHit)))
  }

  def handleAction(action: InputAction, state: ReplState): Result =
    state.historySearchStateOpt match {
      case Some(IncrementalHistorySearchState(searchString, hitStatus)) ⇒
        action match {
          case SelfInsert(c)            ⇒ updateSearch(searchString + c, BeforeFirstHit, state)
          case BackwardDeleteChar       ⇒ handleDeleteChar(searchString, state)
          case AcceptLine               ⇒ Result(state.copy(historySearchStateOpt = None))
          case IncrementalHistorySearch ⇒ updateSearch(searchString, hitStatus, state)
          case _                        ⇒ exitSearchAndHandleNormally(action, state)
        }
      case None                                                         ⇒
        Result(state, actionConsumed = false)
    }

  private def handleDeleteChar(searchString: String, state: ReplState): Result =
    searchString match {
      case ""                            ⇒
        Result(state.copy(historySearchStateOpt = None))
      case _ if searchString.length == 1 ⇒
        Result(state.copy(lineBuffer = LineBuffer.Empty, historySearchStateOpt = Some(IncrementalHistorySearchState(searchString = "", BeforeFirstHit))))
      case _                             ⇒
        updateSearch(searchString.init, BeforeFirstHit, state)
    }

  private def updateSearch(searchString: String, hitStatus: HitStatus, state: ReplState): Result =
    if (hitStatus == AfterLastHit)
      Result(state.copy(
        historySearchStateOpt = Some(IncrementalHistorySearchState(searchString, AfterLastHit))))
    else {
      val nextResultIndex = hitStatus match {
        case Hit(previousResultIndex, _) ⇒ previousResultIndex + 1
        case _                           ⇒ 0
      }
      val nextMatchOpt = history.findMatch(searchString, nextResultIndex)
      val nextState = nextMatchOpt match {
        case Some(nextMatch) ⇒
          state.copy(
            lineBuffer = LineBuffer(nextMatch.command),
            historySearchStateOpt = Some(IncrementalHistorySearchState(searchString, Hit(nextResultIndex, nextMatch.region))))
        case None            ⇒
          state.copy(
            lineBuffer = LineBuffer.Empty,
            historySearchStateOpt = Some(IncrementalHistorySearchState(searchString, AfterLastHit)))
      }
      Result(nextState)
    }

  private def exitSearchAndHandleNormally(action: InputAction, state: ReplState): Result =
    Result(state.copy(historySearchStateOpt = None), actionConsumed = false)

}