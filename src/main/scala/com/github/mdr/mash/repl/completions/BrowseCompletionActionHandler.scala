package com.github.mdr.mash.repl.completions

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.completions.BrowseCompletionActions._
import com.github.mdr.mash.screen.CompletionRenderer
import com.github.mdr.mash.utils.{ RaggedGridNavigator, Region, StringUtils }

trait BrowseCompletionActionHandler {
  self: Repl ⇒

  protected def handleBrowserCompletionAction(action: InputAction, completionState: BrowserCompletionState) {
    val navigator = gridNavigator(completionState)
    action match {
      case NextCompletion     ⇒ browseCompletions(completionState, navigator.next)
      case PreviousCompletion ⇒ browseCompletions(completionState, navigator.previous)
      case NavigateRight      ⇒ browseCompletions(completionState, navigator.right)
      case NavigateLeft       ⇒ browseCompletions(completionState, navigator.left)
      case NavigateDown       ⇒ browseCompletions(completionState, navigator.down)
      case NavigateUp         ⇒ browseCompletions(completionState, navigator.up)
      case AcceptCompletion   ⇒
        state.cloneFrom(state.exitCompletion)
      case _                  ⇒
        state.cloneFrom(state.exitCompletion)
        handleNormalAction(action)
    }
  }

  /**
    * Browse completions with the given active completion index
    */
  protected def browseCompletions(completionState: CompletionState, activeCompletion: Int = 0) {
    val (newCompletionState, newLineBuffer) = getBrowseCompletionState(completionState, activeCompletion, state.lineBuffer)
    state.cloneFrom(state.copy(
      lineBuffer = newLineBuffer,
      completionStateOpt = Some(newCompletionState)))
  }

  protected def getBrowseCompletionState(completionState: CompletionState,
                                         activeCompletion: Int,
                                         lineBuffer: LineBuffer): (BrowserCompletionState, LineBuffer) = {
    val text = lineBuffer.text
    val offset = completionState.replacementLocation.offset
    val completion = completionState.completions(activeCompletion)
    val replacement = completion.replacement
    val newReplacementLocation = Region(offset, replacement.length)
    val newText = StringUtils.replace(text, completionState.replacementLocation, replacement)
    val newCursorPos = newReplacementLocation.posAfter
    val newCompletionState = BrowserCompletionState(
      completions = completionState.completions,
      activeCompletion = activeCompletion,
      replacementLocation = newReplacementLocation)
    val newLineBuffer = LineBuffer(newText, newCursorPos)
    (newCompletionState, newLineBuffer)
  }

  protected def gridNavigator(completionState: BrowserCompletionState): RaggedGridNavigator = {
    val pos = completionState.activeCompletion
    val total = completionState.completions.length
    val columns = CompletionRenderer.getNumberOfCompletionColumns(completionState, terminal.size)
    RaggedGridNavigator(total, columns, pos)
  }

}

