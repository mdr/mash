package com.github.mdr.mash.repl

import com.github.mdr.mash.input.InputAction

object BrowseCompletionActions {

  case object NextCompletion extends InputAction
  case object PreviousCompletion extends InputAction
  case object NavigateDown extends InputAction
  case object NavigateUp extends InputAction
  case object NavigateLeft extends InputAction
  case object NavigateRight extends InputAction
  case object AcceptCompletion extends InputAction

}