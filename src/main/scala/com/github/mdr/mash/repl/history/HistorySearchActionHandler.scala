package com.github.mdr.mash.repl.history

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.{ LineBuffer, Repl }

trait HistorySearchActionHandler {
  self: Repl ⇒

  protected def handleHistorySearchAction(action: InputAction, searchState: HistorySearchState) {
    val HistorySearchState(searchString, resultIndex) = searchState
    action match {
      case SelfInsert(c)            ⇒ updateSearch(searchString + c)
      case BackwardDeleteChar       ⇒ handleDeleteChar(searchString)
      case AcceptLine               ⇒ state.historySearchStateOpt = None
      case IncrementalHistorySearch ⇒ updateSearch(searchString, resultIndex + 1)
      case _                        ⇒ exitSearchAndHandleNormally(action)
    }
  }

  private def handleDeleteChar(searchString: String) =
    searchString match {
      case "" ⇒ state.historySearchStateOpt = None
      case _  ⇒ updateSearch(searchString.init)
    }

  private def updateSearch(searchString: String, resultIndex: Int = 0) {
    val text =
      if (searchString.isEmpty) ""
      else history.findMatches(searchString).distinct.drop(resultIndex).headOption.getOrElse("")
    state.lineBuffer = LineBuffer(text)
    state.historySearchStateOpt = Some(HistorySearchState(searchString, resultIndex))
  }

  private def exitSearchAndHandleNormally(action: InputAction) {
    state.historySearchStateOpt = None
    handleNormalAction(action)
  }

}