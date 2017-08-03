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
    state.copy(
      assistanceStateOpt = None,
      incrementalHistorySearchStateOpt = Some(IncrementalHistorySearchState(searchString = "", BeforeFirstHit)))
  }

  def beginIncrementalSearchFromLine(state: ReplState): ReplState =
    freshSearch(state.lineBuffer.text, beginFreshIncrementalSearch(state))

  def handleAction(action: InputAction, state: ReplState): Result =
    state.incrementalHistorySearchStateOpt match {
      case Some(IncrementalHistorySearchState(searchString, hitStatus)) ⇒
        action match {
          case SelfInsert(c)                 ⇒ Result(freshSearch(searchString + c, state))
          case BackwardDeleteChar            ⇒ Result(handleDeleteChar(searchString, state))
          case IncrementalHistorySearch | Up ⇒ Result(nextHit(searchString, hitStatus, state))
          case Down                          ⇒ Result(previousHit(searchString, hitStatus, state))
          case Enter                         ⇒ Result(state.copy(incrementalHistorySearchStateOpt = None))
          case ChangeDirectory               ⇒ Result(handleChangeDirectory(hitStatus, state))
          case _                             ⇒ exitSearchAndHandleNormally(action, state)
        }
      case None                                                         ⇒
        Result(state, actionConsumed = false)
    }

  private def handleDeleteChar(searchString: String, state: ReplState): ReplState =
    searchString match {
      case ""                            ⇒
        state.copy(incrementalHistorySearchStateOpt = None)
      case _ if searchString.length == 1 ⇒
        state.copy(
          lineBuffer = LineBuffer.Empty,
          incrementalHistorySearchStateOpt = Some(IncrementalHistorySearchState(searchString = "", BeforeFirstHit)))
      case _                             ⇒
        freshSearch(searchString.init, state)
    }

  private def freshSearch(searchString: String, state: ReplState): ReplState =
    history.findMatch(searchString, index = 0) match {
      case Some(historyMatch) ⇒
        newMatchFound(searchString, state, resultIndex = 0, historyMatch)
      case None               ⇒
        state.copy(
          lineBuffer = LineBuffer.Empty,
          incrementalHistorySearchStateOpt = Some(IncrementalHistorySearchState(searchString, NoHits)))
    }

  private def previousHit(searchString: String, hitStatus: HitStatus, state: ReplState): ReplState =
    getHitAtIndex(searchString, hitStatus, state, previousResultIndex(hitStatus))

  private def nextHit(searchString: String, hitStatus: HitStatus, state: ReplState): ReplState =
    getHitAtIndex(searchString, hitStatus, state, nextResultIndex(hitStatus))

  private def getHitAtIndex(searchString: String, hitStatus: HitStatus, state: ReplState, resultIndex: Int): ReplState =
    if (hitStatus == NoHits)
      state
    else
      history.findMatch(searchString, resultIndex) match {
        case Some(historyMatch) ⇒ newMatchFound(searchString, state, resultIndex, historyMatch)
        case None               ⇒ state
      }

  private def newMatchFound(searchString: String, state: ReplState, resultIndex: Int, historyMatch: Match): ReplState = {
    val Match(command, matchRegion, timestamp, workingDirectory) = historyMatch
    val hit = Hit(resultIndex, matchRegion, timestamp, workingDirectory)
    state.copy(
      lineBuffer = LineBuffer(command),
      incrementalHistorySearchStateOpt = Some(IncrementalHistorySearchState(searchString, hit)))
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