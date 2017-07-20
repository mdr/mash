package com.github.mdr.mash.repl.history

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.history.IncrementalHistorySearchState._
import com.github.mdr.mash.repl.{ LineBuffer, ReplState }
import com.github.mdr.mash.utils.Region

object IncrementalHistorySearchState {

  sealed trait HitStatus {

    def matchRegionOpt: Option[Region] = None

  }

  case object BeforeFirstHit extends HitStatus

  /**
    * @param resultIndex index into the stream of results that match the searchString
    * @param matchRegion region in the line buffer that matches the search
    */
  case class Hit(resultIndex: Int, matchRegion: Region) extends HitStatus {

    override def matchRegionOpt: Option[Region] = Some(matchRegion)

  }

  case object AfterLastHit extends HitStatus

}

case class IncrementalHistorySearchState(searchString: String = "",
                                         hitStatus: HitStatus)

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
      val nextMatchOpt = history.findMatches(searchString).distinct.drop(nextResultIndex).headOption
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