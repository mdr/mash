package com.github.mdr.mash.repl.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.IncrementalHistorySearchState._
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.history.History
import com.github.mdr.mash.repl.history.History.Match
import com.github.mdr.mash.repl.{ IncrementalHistorySearchState, LineBuffer, ReplState }

object IncrementalHistorySearchActionHandler {

  case class Result(newState: ReplState, actionConsumed: Boolean = true)

}

case class IncrementalHistorySearchActionHandler(history: History) {

  import IncrementalHistorySearchActionHandler._

  def beginFreshIncrementalSearch(state: ReplState): ReplState = {
    history.resetHistoryPosition()
    state.copy(
      assistanceStateOpt = None,
      historySearchStateOpt = Some(IncrementalHistorySearchState(searchString = "", BeforeFirstHit)))
  }

  def beginIncrementalSearchFromLine(state: ReplState): ReplState =
    updateSearch(state.lineBuffer.text, BeforeFirstHit, beginFreshIncrementalSearch(state))

  def handleAction(action: InputAction, state: ReplState): Result =
    state.historySearchStateOpt match {
      case Some(IncrementalHistorySearchState(searchString, hitStatus)) ⇒
        action match {
          case SelfInsert(c)                 ⇒ Result(updateSearch(searchString + c, BeforeFirstHit, state))
          case BackwardDeleteChar            ⇒ Result(handleDeleteChar(searchString, state))
          case Enter                         ⇒ Result(state.copy(historySearchStateOpt = None))
          case IncrementalHistorySearch | Up ⇒ Result(updateSearch(searchString, hitStatus, state))
          case _                             ⇒ exitSearchAndHandleNormally(action, state)
        }
      case None                                                         ⇒
        Result(state, actionConsumed = false)
    }

  private def handleDeleteChar(searchString: String, state: ReplState): ReplState =
    searchString match {
      case ""                            ⇒
        state.copy(historySearchStateOpt = None)
      case _ if searchString.length == 1 ⇒
        state.copy(
          lineBuffer = LineBuffer.Empty,
          historySearchStateOpt = Some(IncrementalHistorySearchState(searchString = "", BeforeFirstHit)))
      case _                             ⇒
        updateSearch(searchString.init, BeforeFirstHit, state)
    }

  private def updateSearch(searchString: String, hitStatus: HitStatus, state: ReplState): ReplState =
    if (hitStatus == AfterLastHit)
      state.copy(
        historySearchStateOpt = Some(IncrementalHistorySearchState(searchString, AfterLastHit)))
    else {
      val nextResultIndex = hitStatus match {
        case hit: Hit ⇒ hit.resultIndex + 1
        case _        ⇒ 0
      }
      val nextMatchOpt = history.findMatch(searchString, nextResultIndex)
      nextMatchOpt match {
        case Some(Match(command, matchRegion, timestamp, workingDirectory)) ⇒
          val hit = Hit(nextResultIndex, matchRegion, timestamp, workingDirectory.toString)
          state.copy(
            lineBuffer = LineBuffer(command),
            historySearchStateOpt = Some(IncrementalHistorySearchState(searchString, hit)))
        case None                                                           ⇒
          state.copy(
            lineBuffer = LineBuffer.Empty,
            historySearchStateOpt = Some(IncrementalHistorySearchState(searchString, AfterLastHit)))
      }
    }

  private def exitSearchAndHandleNormally(action: InputAction, state: ReplState): Result =
    Result(state.copy(historySearchStateOpt = None), actionConsumed = false)

}