package com.github.mdr.mash

import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import BrowseCompletionActions._
import com.github.mdr.mash.input.InputSequence._
import com.github.mdr.mash.input.Key._
import com.github.mdr.mash.input.InputSequence
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.input.KeyMap
import com.github.mdr.mash.input.BasicKeyMap

object BrowseCompletionActions {

  case object NextCompletion extends InputAction
  case object PreviousCompletion extends InputAction
  case object NavigateDown extends InputAction
  case object NavigateUp extends InputAction
  case object NavigateLeft extends InputAction
  case object NavigateRight extends InputAction
  case object AcceptCompletion extends InputAction

}

trait BrowseCompletionActionHandler { self: Repl ⇒

  protected def browseCompletion(completionState: CompletionState, activeCompletion: Int) {
    val Region(offset, length) = completionState.replacementLocation
    val s = state.lineBuffer.text
    val completion = completionState.completions(activeCompletion)
    val replacement = completion.replacement
    val newReplacementLocation = Region(offset, replacement.length)
    val newCursorPos = newReplacementLocation.posAfter
    val newS = StringUtils.replace(s, completionState.replacementLocation, replacement)
    state.lineBuffer = LineBuffer(newS, newCursorPos)
    state.completionStateOpt = Some(
      BrowserCompletionState(
        completions = completionState.completions,
        activeCompletion = activeCompletion,
        replacementLocation = newReplacementLocation))
  }

  protected def handleBrowserCompletionAction(action: InputAction, completionState: BrowserCompletionState) {
    val pos = completionState.activeCompletion
    val total = completionState.completions.length
    val columns = previousReplRenderResultOpt.map(_.completionColumns).getOrElse(1)
    val rows = math.ceil((total / columns.toDouble)).toInt
    val row = pos / columns
    val column = pos % columns
    val rowLength = math.min(total - (row * columns), columns)

    action match {
      case NextCompletion ⇒
        val newPos = (pos + 1) % total
        browseCompletion(completionState, newPos)
      case PreviousCompletion ⇒
        val newPos = (pos - 1 + total) % total
        browseCompletion(completionState, newPos)
      case NavigateRight ⇒
        val newColumn = (column + 1) % rowLength
        val newPos = row * columns + newColumn
        browseCompletion(completionState, newPos)
      case NavigateLeft ⇒
        val newColumn = (column - 1 + rowLength) % rowLength
        val newPos = row * columns + newColumn
        browseCompletion(completionState, newPos)
      case NavigateDown ⇒
        var nextPos = pos + columns
        if (nextPos >= total)
          nextPos = nextPos % (total - (total % columns)) % columns
        browseCompletion(completionState, nextPos)
      case NavigateUp ⇒
        var nextPos = pos - columns
        if (nextPos < 0)
          nextPos += total - (total % columns) + columns
        if (nextPos >= total)
          nextPos = nextPos - columns
        browseCompletion(completionState, nextPos)
      case AcceptCompletion ⇒
        state.completionStateOpt = None
      case _ ⇒
        state.completionStateOpt = None
        handleNormalAction(action)
    }
  }

}

object BrowseCompletionsKeyMap extends KeyMap(BasicKeyMap.map ++ Map(
  KeyPress(Tab) -> NextCompletion,
  KeyPress(Tab, shift = true) -> PreviousCompletion,
  KeyPress(Right) -> NavigateRight,
  KeyPress(BasicKey('b'), control = true) -> NavigateLeft,
  KeyPress(Left) -> NavigateLeft,
  KeyPress(BasicKey('p'), control = true) -> NavigateUp,
  KeyPress(Up) -> NavigateUp,
  KeyPress(BasicKey('n'), control = true) -> NavigateDown,
  KeyPress(Down) -> NavigateDown,
  KeyPress(Enter) -> AcceptCompletion))

