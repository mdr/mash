package com.github.mdr.mash.repl

import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import BrowseCompletionActions._
import com.github.mdr.mash.input.InputSequence._
import com.github.mdr.mash.input.Key._
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
    val text = state.lineBuffer.text
    val completion = completionState.completions(activeCompletion)
    val replacement = completion.replacement
    val newReplacementLocation = Region(offset, replacement.length)
    val newCursorPos = newReplacementLocation.posAfter
    val newText = StringUtils.replace(text, completionState.replacementLocation, replacement)
    state.lineBuffer = LineBuffer(newText, newCursorPos)
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
    val navigator = RaggedGridNavigator(total, columns, pos)
    action match {
      case NextCompletion     ⇒ browseCompletion(completionState, navigator.next)
      case PreviousCompletion ⇒ browseCompletion(completionState, navigator.previous)
      case NavigateRight      ⇒ browseCompletion(completionState, navigator.right)
      case NavigateLeft       ⇒ browseCompletion(completionState, navigator.left)
      case NavigateDown       ⇒ browseCompletion(completionState, navigator.down)
      case NavigateUp         ⇒ browseCompletion(completionState, navigator.up)
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

