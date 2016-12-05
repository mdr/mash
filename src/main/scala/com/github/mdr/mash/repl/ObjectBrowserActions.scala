package com.github.mdr.mash.repl

import com.github.mdr.mash.input.InputAction

object ObjectBrowserActions {

  case object NextPage extends InputAction
  case object PreviousPage extends InputAction
  case object NextItem extends InputAction
  case object PreviousItem extends InputAction
  case object PreviousColumn extends InputAction
  case object NextColumn extends InputAction
  case object UnfocusColumn extends InputAction
  case object Focus extends InputAction
  case object Back extends InputAction
  case object FirstColumn extends InputAction
  case object LastColumn extends InputAction
  case object FirstItem extends InputAction
  case object LastItem extends InputAction
  case object ExitBrowser extends InputAction
  case object InsertItem extends InputAction
  case object InsertWholeItem extends InputAction
  case object ToggleMarked extends InputAction
  case object Rerender extends InputAction
  case object ViewAsTree extends InputAction
  case object HideColumn extends InputAction
  case object Open extends InputAction
  case object BeginSearch extends InputAction
  case object Unsearch extends InputAction
  case object ExitSearch extends InputAction
}