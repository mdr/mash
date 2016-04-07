package com.github.mdr.mash.repl

import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import com.github.mdr.mash.input.InputAction

trait IncrementalSearchActionHandler { self: Repl ⇒

  import InputAction._

  protected def handleIncrementalSearchAction(action: InputAction, searchState: IncrementalSearchState) {
    val IncrementalSearchState(searchString, resultIndex) = searchState
    action match {
      case SelfInsert(c) ⇒
        val newSearchString = searchString + c
        updateSearch(newSearchString, 0)
      case BackwardDeleteChar ⇒
        searchString match {
          case "" ⇒
            state.incrementalSearchStateOpt = None
          case _ ⇒
            val newSearchString = searchString.init
            updateSearch(newSearchString, 0)
        }
      case AcceptLine ⇒
        state.incrementalSearchStateOpt = None
      case IncrementalHistorySearch ⇒
        updateSearch(searchString, resultIndex + 1)
      case _ ⇒
        state.incrementalSearchStateOpt = None
        handleNormalAction(action)
    }
  }

  private def updateSearch(searchString: String, resultIndex: Int) {
    state.lineBuffer = LineBuffer(
      if (searchString.isEmpty)
        ""
      else
        state.history.findMatches(searchString).distinct.drop(resultIndex).headOption.getOrElse(""))
    state.incrementalSearchStateOpt = Some(IncrementalSearchState(searchString, resultIndex))
  }
}