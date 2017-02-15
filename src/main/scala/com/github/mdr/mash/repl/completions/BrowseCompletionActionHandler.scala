package com.github.mdr.mash.repl.completions

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.completions.BrowseCompletionActions._
import com.github.mdr.mash.utils.{ RaggedGridNavigator, Region, StringUtils }

trait BrowseCompletionActionHandler { self: Repl ⇒

  protected def handleBrowserCompletionAction(action: InputAction, completionState: BrowserCompletionState) {
    val navigator = gridNavigator(completionState)
    action match {
      case NextCompletion     ⇒ browseCompletions(completionState, navigator.next)
      case PreviousCompletion ⇒ browseCompletions(completionState, navigator.previous)
      case NavigateRight      ⇒ browseCompletions(completionState, navigator.right)
      case NavigateLeft       ⇒ browseCompletions(completionState, navigator.left)
      case NavigateDown       ⇒ browseCompletions(completionState, navigator.down)
      case NavigateUp         ⇒ browseCompletions(completionState, navigator.up)
      case AcceptCompletion ⇒
        state.completionStateOpt = None
      case _ ⇒
        state.completionStateOpt = None
        handleNormalAction(action)
    }
  }

  /**
   * Browse completions with the given active completion index
   */
  protected def browseCompletions(completionState: CompletionState, activeCompletion: Int = 0) {
    val offset = completionState.replacementLocation.offset
    val completion = completionState.completions(activeCompletion)
    val replacement = completion.replacement
    val newReplacementLocation = Region(offset, replacement.length)
    val text = state.lineBuffer.text
    val newText = StringUtils.replace(text, completionState.replacementLocation, replacement)
    val newCursorPos = newReplacementLocation.posAfter
    state.lineBuffer = LineBuffer(newText, newCursorPos)
    val newCompletionState = BrowserCompletionState(
      completions = completionState.completions,
      activeCompletion = activeCompletion,
      replacementLocation = newReplacementLocation)
    state.completionStateOpt = Some(newCompletionState)
  }

  private def gridNavigator(completionState: BrowserCompletionState): RaggedGridNavigator = {
    val pos = completionState.activeCompletion
    val total = completionState.completions.length
    val columns = previousReplRenderResultOpt.map(_.completionColumns).getOrElse(1)
    RaggedGridNavigator(total, columns, pos)
  }

}

