package com.github.mdr.mash.repl

import com.github.mdr.mash.input.InputAction

object ObjectBrowserActions {

  case object NextPage extends InputAction
  case object PreviousPage extends InputAction
  case object NextItem extends InputAction
  case object PreviousItem extends InputAction
  case object FirstItem extends InputAction
  case object LastItem extends InputAction
  case object ExitBrowser extends InputAction
  case object InsertItem extends InputAction
  case object ToggleSelected extends InputAction

}