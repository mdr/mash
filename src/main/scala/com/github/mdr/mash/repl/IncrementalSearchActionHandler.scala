package com.github.mdr.mash.repl

import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.NormalActions._

trait IncrementalSearchActionHandler {
  self: Repl ⇒

  protected def handleIncrementalSearchAction(action: InputAction, searchState: IncrementalSearchState) {
    val IncrementalSearchState(searchString, resultIndex) = searchState
    action match {
      case SelfInsert(c)            ⇒ updateSearch(searchString + c)
      case BackwardDeleteChar       ⇒ handleDeleteChar(searchString)
      case AcceptLine               ⇒ state.incrementalSearchStateOpt = None
      case IncrementalHistorySearch ⇒ updateSearch(searchString, resultIndex + 1)
      case _                        ⇒ exitSearchAndHandleNormally(action)
    }
  }

  private def handleDeleteChar(searchString: String) =
    searchString match {
      case "" ⇒ state.incrementalSearchStateOpt = None
      case _  ⇒ updateSearch(searchString.init)
    }

  private def updateSearch(searchString: String, resultIndex: Int = 0) {
    val text =
      if (searchString.isEmpty) ""
      else history.findMatches(searchString).distinct.drop(resultIndex).headOption.getOrElse("")
    state.lineBuffer = LineBuffer(text)
    state.incrementalSearchStateOpt = Some(IncrementalSearchState(searchString, resultIndex))
  }

  private def exitSearchAndHandleNormally(action: InputAction) {
    state.incrementalSearchStateOpt = None
    handleNormalAction(action)
  }

}