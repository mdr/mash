package com.github.mdr.mash.repl.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.ns.os.ChangeDirectoryFunction
import com.github.mdr.mash.repl.IncrementalHistorySearchActions.ChangeDirectory
import com.github.mdr.mash.repl.IncrementalHistorySearchState._
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.history.History
import com.github.mdr.mash.repl.history.History.Match
import com.github.mdr.mash.repl.{ IncrementalHistorySearchState, LineBuffer, ReplState }

import scala.util.control.NonFatal

object IncrementalHistorySearchActionHandler {

  case class Result(newState: ReplState, actionConsumed: Boolean = true)

}

case class IncrementalHistorySearchActionHandler(history: History) {

  import IncrementalHistorySearchActionHandler._

  def beginFreshIncrementalSearch(state: ReplState): ReplState = {
    history.resetHistoryPosition()
    val originalLineBuffer = state.lineBuffer.text
    val searchState = IncrementalHistorySearchState(searchString = "", originalLineBuffer, BeforeFirstHit)
    state.copy(
      assistanceStateOpt = None,
      incrementalHistorySearchStateOpt = Some(searchState))
  }

  def beginIncrementalSearchFromLine(state: ReplState): ReplState = {
    val initialState = beginFreshIncrementalSearch(state)
    val searchState = initialState.incrementalHistorySearchStateOpt.get // safe, just initialised
    freshSearch(state.lineBuffer.text, searchState, initialState)
  }

  def handleAction(action: InputAction, state: ReplState): Result =
    state.incrementalHistorySearchStateOpt match {
      case Some(searchState@IncrementalHistorySearchState(searchString, _, hitStatus)) ⇒
        action match {
          case SelfInsert(c)                 ⇒ Result(freshSearch(searchString + c, searchState, state))
          case BackwardDeleteChar            ⇒ Result(handleDeleteChar(searchState, state))
          case IncrementalHistorySearch | Up ⇒ Result(nextHit(searchState, state))
          case Down                          ⇒ Result(previousHit(searchState, state))
          case Enter                         ⇒ Result(state.copy(incrementalHistorySearchStateOpt = None))
          case AbandonHistorySearch          ⇒ Result(handleAbandonSearch(searchState, state))
          case ChangeDirectory               ⇒ Result(handleChangeDirectory(hitStatus, state))
          case _                             ⇒ exitSearchAndHandleNormally(action, state)
        }
      case None                                                                        ⇒
        Result(state, actionConsumed = false)
    }

  private def handleAbandonSearch(searchState: IncrementalHistorySearchState, state: ReplState): ReplState =
    state.copy(
      lineBuffer = LineBuffer(searchState.originalLineBuffer),
      incrementalHistorySearchStateOpt = None)

  private def handleDeleteChar(searchState: IncrementalHistorySearchState, state: ReplState): ReplState = {
    val searchString = searchState.searchString
    searchString match {
      case ""                            ⇒
        state.copy(incrementalHistorySearchStateOpt = None)
      case _ if searchString.length == 1 ⇒
        state.copy(
          lineBuffer = LineBuffer.Empty,
          incrementalHistorySearchStateOpt = Some(searchState.copy(searchString = "", hitStatus = BeforeFirstHit)))
      case _                             ⇒
        freshSearch(searchString.init, searchState, state)
    }
  }

  private def freshSearch(newSearchString: String, searchState: IncrementalHistorySearchState, state: ReplState): ReplState =
    history.findMatch(newSearchString, index = 0) match {
      case Some(historyMatch) ⇒
        newMatchFound(newSearchString, searchState, state, resultIndex = 0, historyMatch)
      case None               ⇒
        state.copy(
          lineBuffer = LineBuffer.Empty,
          incrementalHistorySearchStateOpt = Some(searchState.copy(searchString = newSearchString, hitStatus = NoHits)))
    }

  private def previousHit(searchState: IncrementalHistorySearchState, state: ReplState): ReplState =
    getHitAtIndex(searchState, state, previousResultIndex(searchState.hitStatus))

  private def nextHit(searchState: IncrementalHistorySearchState, state: ReplState): ReplState =
    getHitAtIndex(searchState, state, nextResultIndex(searchState.hitStatus))

  private def getHitAtIndex(searchState: IncrementalHistorySearchState, state: ReplState, resultIndex: Int): ReplState =
    if (searchState.hitStatus == NoHits)
      state
    else
      history.findMatch(searchState.searchString, resultIndex) match {
        case Some(historyMatch) ⇒ newMatchFound(searchState.searchString, searchState, state, resultIndex, historyMatch)
        case None               ⇒ state
      }

  private def newMatchFound(searchString: String,
                            searchState: IncrementalHistorySearchState,
                            state: ReplState,
                            resultIndex: Int,
                            historyMatch: Match): ReplState = {
    val Match(command, matchRegion, timestamp, workingDirectory) = historyMatch
    val hit = Hit(resultIndex, matchRegion, timestamp, workingDirectory)
    state.copy(
      lineBuffer = LineBuffer(command),
      incrementalHistorySearchStateOpt = Some(searchState.copy(searchString = searchString, hitStatus = hit)))
  }

  private def nextResultIndex(hitStatus: HitStatus): Int =
    hitStatus match {
      case hit: Hit ⇒ hit.resultIndex + 1
      case _        ⇒ 0
    }

  private def previousResultIndex(hitStatus: HitStatus): Int =
    hitStatus match {
      case hit: Hit ⇒ 0 max hit.resultIndex - 1
      case _        ⇒ 0
    }

  private def exitSearchAndHandleNormally(action: InputAction, state: ReplState): Result =
    Result(state.copy(incrementalHistorySearchStateOpt = None), actionConsumed = false)

  private def handleChangeDirectory(hitStatus: HitStatus, state: ReplState) = {
    hitStatus match {
      case hit: Hit ⇒
        try
          ChangeDirectoryFunction.changeDirectory(hit.workingDirectory)
        catch {
          case NonFatal(_) ⇒ // ignore
        }
      case _        ⇒
    }
    state
  }

}